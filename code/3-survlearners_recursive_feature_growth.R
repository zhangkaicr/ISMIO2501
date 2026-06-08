# ==========================================
# 脚本名称：3-survlearners_recursive_feature_growth.R
# 核心用途：
# 1) 在观察性数据中，使用倾向性评分（PS）校正后估计 CATE；
# 2) 依据既有变量重要性排序，执行“递归特征增加”；
# 3) 每次增加一个变量后，使用 survlearners 可用算法重新拟合；
# 4) 为每个算法输出一张结果表，并输出总汇总表。
#
# 使用方法（在项目根目录）：
# Rscript --version
# Rscript 3-survlearners_recursive_feature_growth.R
#
# 可选环境变量：
# MAX_FEATURES      默认 -1（使用全部排序变量）；可设置为正整数仅跑前N个变量
# RMST_HORIZON      默认 24；用于设置 survlearners t0
# QINI_RANK_FILE    默认 output/2-rmst24_qini_rank_by_variable_train/06_rmst{horizon}_qini_by_variable_ranked_train.csv
# PS_GROUP_NAME     默认空；若 PS 文件为多组格式，则指定要使用的组名
# ==========================================

# 加载 pacman，便于统一管理依赖包。
library(pacman, help, pos = 2, lib.loc = NULL)
# 加载分析所需依赖：tidyverse/openxlsx/here/grf/survlearners。
p_load(tidyverse, openxlsx, here, grf, survlearners)

# 固定随机种子，提升结果复现性。
set.seed(20260423)

# -----------------------------
# 0A. 路径与PS解析辅助函数
# -----------------------------

# 解析 Rscript 调用时的当前脚本路径，便于按脚本名创建输出目录。
get_current_script_path <- function(default_path = here("3-survlearners_recursive_feature_growth.R")) {
  # 获取完整命令行参数。
  args_full <- commandArgs(trailingOnly = FALSE)
  # 找出 --file=xxx 形式的参数。
  file_arg <- args_full[str_detect(args_full, "^--file=")]
  # 若存在脚本路径参数，则优先使用该路径。
  if (length(file_arg) > 0) {
    return(normalizePath(str_remove(file_arg[1], "^--file="), winslash = "/", mustWork = FALSE))
  }
  # 否则回退为默认脚本路径，兼容 source 调用。
  normalizePath(default_path, winslash = "/", mustWork = FALSE)
}

# 根据脚本名创建 output/脚本名 目录，保证每个脚本结果独立存放。
build_output_dir <- function(script_path) {
  # 提取脚本主名（不含扩展名）。
  script_stem <- tools::file_path_sans_ext(basename(script_path))
  # 生成输出目录。
  out_dir <- here("output", script_stem)
  # 若目录不存在则创建。
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  # 返回输出目录。
  out_dir
}

# 解析 PS变量最终确定.txt，兼容分组文本格式和 R 向量格式。
parse_ps_groups <- function(ps_file) {
  # 检查文件是否存在。
  if (!file.exists(ps_file)) {
    stop(paste0("未找到PS变量文件：", ps_file))
  }

  # 读取原始文本行。
  lines_raw <- readr::read_lines(ps_file, lazy = FALSE)
  # 去掉首尾空白，便于模式识别。
  lines_trim <- stringr::str_trim(lines_raw)
  # 识别“PS评分计算变量X：”标题行。
  header_idx <- which(stringr::str_detect(lines_trim, "^PS评分计算变量\\d+：$"))

  # 若识别到分组标题，则按分组文本格式解析。
  if (length(header_idx) > 0) {
    return(
      map_dfr(seq_along(header_idx), function(i) {
        # 当前分组起始行。
        start_idx <- header_idx[i]
        # 当前分组结束行。
        end_idx <- if (i < length(header_idx)) header_idx[i + 1] - 1 else length(lines_trim)
        # 记录组名。
        group_name <- lines_trim[start_idx]
        # 提取组内正文。
        block_lines <- lines_trim[seq.int(start_idx + 1, end_idx)]
        # 提取形如 1.age_grade 的变量定义行。
        variable_lines <- block_lines[stringr::str_detect(block_lines, "^\\d+\\.[^[:space:]].*$")]

        # 若组内无变量，则返回空表。
        if (length(variable_lines) == 0) {
          return(tibble())
        }

        # 解析顺序号与变量名。
        tibble(
          group_name = group_name,
          order_id = as.integer(stringr::str_extract(variable_lines, "^\\d+")),
          variable = stringr::str_trim(stringr::str_replace(variable_lines, "^\\d+\\.", ""))
        )
      }) %>%
        filter(!is.na(variable), variable != "") %>%
        distinct(group_name, order_id, variable)
    )
  }

  # 若没有分组标题，则尝试解析 R 向量格式中的双引号变量名。
  quoted_vars <- stringr::str_match_all(paste(lines_raw, collapse = "\n"), '"([^"]+)"')[[1]]
  variable_values <- quoted_vars[, 2]

  # 若能提取出变量，则按单个固定组返回。
  if (length(variable_values) > 0) {
    return(
      tibble(
        group_name = "ps_feature_fixed",
        order_id = seq_along(variable_values),
        variable = variable_values
      ) %>%
        distinct(group_name, order_id, variable)
    )
  }

  # 两种格式都无法识别时，明确报错。
  stop("PS变量文件格式无法识别；请使用“PS评分计算变量X：”分组格式或R向量格式。")
}

# -----------------------------
# 0. 读取与路径设置
# -----------------------------

# 定义输入数据路径（按用户要求使用 train01 的 tidy sheet）。
data_file <- here("data", "01_ISMIO2501_train_tidy.xlsx")
# 获取当前脚本路径。
script_path <- get_current_script_path()
# 创建当前脚本的独立输出目录。
out_dir <- build_output_dir(script_path)
# 指定 PS 变量文件路径。
ps_file_path <- here("data", "PS变量最终确定.txt")
# 读取 RMST horizon（用于 t0）。
rmst_horizon <- suppressWarnings(as.numeric(Sys.getenv("RMST_HORIZON", unset = "24")))
if (!is.finite(rmst_horizon) || rmst_horizon <= 0) {
  stop("RMST_HORIZON 必须是正数。")
}
horizon_tag <- gsub("\\.", "p", as.character(rmst_horizon))

# 定义变量重要性排序文件路径，默认读取 2 号脚本在 output 下生成的结果。
rank_file <- Sys.getenv(
  "QINI_RANK_FILE",
  unset = here(
    "output",
    "2-rmst24_qini_rank_by_variable_train",
    paste0("06_rmst", horizon_tag, "_qini_by_variable_ranked_train.csv")
  )
)

# 读取训练集 tidy 工作表。
df <- read.xlsx(data_file, sheet = "tidy")
# 读取变量重要性排序表。
rank_df <- readr::read_csv(rank_file, show_col_types = FALSE)

# 解析 PS 变量文本文件。
ps_group_table <- parse_ps_groups(ps_file_path)
# 保存全部解析出的 PS 变量组，便于复核。
write_csv(ps_group_table, file.path(out_dir, "01_ps_groups_long.csv"))

# 默认使用解析结果中的第一个变量组；若用户传入环境变量则优先使用指定组。
ps_group_name_default <- unique(ps_group_table$group_name)[1]
ps_group_name <- Sys.getenv("PS_GROUP_NAME", unset = ps_group_name_default)
# 提取选中的 PS 变量组。
ps_feature_selected <- ps_group_table %>%
  filter(group_name == ps_group_name) %>%
  arrange(order_id)

# 若指定组不存在，则直接报错。
if (nrow(ps_feature_selected) == 0) {
  stop(paste0("未在PS变量文件中找到指定变量组：", ps_group_name))
}

# 保存本次选中的 PS 变量组。
write_csv(ps_feature_selected, file.path(out_dir, "02_ps_group_selected.csv"))

# -----------------------------
# 1. 基础检查与预处理
# -----------------------------

# 定义关键字段。
required_cols <- c("arms", "Event", "OS")
# 检查缺失关键字段并在缺失时终止。
miss_required <- setdiff(required_cols, names(df))
if (length(miss_required) > 0) {
  stop(paste0("输入数据缺少关键字段：", paste(miss_required, collapse = ", ")))
}

# 从变量排序表中提取“可用且成功计算”的变量顺序。
feature_order <- rank_df %>%
  filter(status == "ok", !is.na(qini)) %>%
  arrange(desc(qini)) %>%
  pull(variable) %>%
  unique()

# 只保留当前数据中实际存在的变量，避免后续报错。
feature_order <- intersect(feature_order, names(df))
# 按需求：模型筛选过程中移除 HBsAb。
feature_order <- setdiff(feature_order, "HBsAb")

# 环境变量控制最多使用多少个特征。
max_features_env <- suppressWarnings(as.integer(Sys.getenv("MAX_FEATURES", unset = "-1")))
# 解释逻辑：当 MAX_FEATURES 为正时截断；否则使用全部变量。
if (is.finite(max_features_env) && max_features_env > 0) {
  feature_order <- feature_order[seq_len(min(length(feature_order), max_features_env))]
}

# 若特征列表为空，直接终止。
if (length(feature_order) == 0) {
  stop("变量排序结果为空，无法执行递归特征增加。")
}

# 仅保留当前数据中可用的固定PS变量。
ps_feature_use <- intersect(ps_feature_selected$variable, names(df))
if (length(ps_feature_use) < 2) {
  stop("固定PS变量在当前数据中可用列不足，无法进行观察性校正。")
}

# 保存训练数据中实际可用的 PS 变量。
write_csv(
  tibble(variable = ps_feature_use),
  file.path(out_dir, "03_ps_variables_available_in_train.csv")
)

# 定义 treated 组标签：优先使用 TACE_TA，否则使用排序后第二组。
if ("TACE_TA" %in% unique(as.character(df$arms))) {
  treated_label <- "TACE_TA"
} else {
  treated_label <- sort(unique(as.character(df$arms)))[2]
}

# 构建用于建模的基础字段：W（二值处理）、Y（时间）、D（事件）。
df <- df %>%
  mutate(
    W = as.numeric(as.character(arms) == treated_label),
    Y = as.numeric(OS),
    D = as.numeric(Event > 0)
  ) %>%
  filter(!is.na(W), !is.na(Y), !is.na(D))

# -----------------------------
# 2. 工具函数定义
# -----------------------------

# 定义函数：把当前特征集合编码成数值矩阵（one-hot）。
make_x_matrix <- function(dat, vars) {
  # 提取当前特征子集。
  x_raw <- dat %>%
    select(all_of(vars))

  # 将字符/逻辑列转为因子，便于 model.matrix 编码。
  x_raw <- x_raw %>%
    mutate(across(where(~ is.character(.x) || is.logical(.x)), as.factor))

  # 构建设计矩阵（无截距）。
  x_terms <- terms(~ . - 1, data = x_raw)
  x_mat <- model.matrix(x_terms, data = x_raw)
  x_mat
}

sanitize_x_matrix <- function(x_mat) {
  x_mat <- as.matrix(x_mat)
  if (!is.matrix(x_mat)) x_mat <- matrix(x_mat, nrow = length(x_mat), ncol = 1)
  if (ncol(x_mat) == 0) return(matrix(0, nrow = nrow(x_mat), ncol = 1))

  x_mat[is.na(x_mat)] <- 0

  keep_nonconst <- apply(x_mat, 2, function(v) {
    sd_v <- suppressWarnings(sd(v))
    is.finite(sd_v) && sd_v > 0
  })
  x_mat <- x_mat[, keep_nonconst, drop = FALSE]
  if (ncol(x_mat) == 0) return(matrix(0, nrow = nrow(x_mat), ncol = 1))

  if (ncol(x_mat) >= 2) {
    qr_x <- qr(x_mat)
    r <- qr_x$rank
    piv <- qr_x$pivot
    if (is.finite(r) && r >= 1) {
      keep_idx <- sort(piv[seq_len(r)])
      x_mat <- x_mat[, keep_idx, drop = FALSE]
    }
  }

  x_mat
}

# 定义函数：给定 data/W 与固定PS变量估计倾向性评分，并截断到 [0.01, 0.99]。
estimate_ps <- function(dat, ps_vars, w_col = "W") {
  # 使用固定PS变量构建设计矩阵。
  X_ps <- sanitize_x_matrix(make_x_matrix(dat, ps_vars))
  W <- dat[[w_col]]
  # 用 grf 的回归森林拟合 W~X_ps，获得个体化 PS。
  ps_fit <- regression_forest(X = X_ps, Y = W, num.trees = 300, honesty = TRUE)
  # 预测并截断极端概率。
  ps_hat <- pmin(pmax(as.numeric(predict(ps_fit)$predictions), 0.01), 0.99)
  ps_hat
}

# 定义函数：根据函数是否支持 W.hat，自动决定如何传入 PS。
# 说明：
# - 对支持 W.hat 的算法：直接传 W.hat=ps_hat；
# - 对不支持 W.hat 的算法（如部分 S/T 实现）：将 ps_hat 作为附加协变量并入 X。
fit_one_algorithm <- function(algo_name, X_base, Y, W, D, t0, ps_hat) {
  # 从 survlearners 命名空间取函数对象。
  fn <- get(algo_name, envir = asNamespace("survlearners"))
  # 提取该函数形参名，便于做自适应传参。
  fn_args <- names(formals(fn))

  # 判定是否可直接传 W.hat。
  supports_what <- "W.hat" %in% fn_args
  # 若不支持 W.hat，则将 PS 作为额外协变量并入 X（观察性校正替代方案）。
  X_use <- if (supports_what) X_base else cbind(X_base, ps_hat = ps_hat)
  if (grepl("coxph", algo_name, fixed = TRUE)) {
    X_use <- sanitize_x_matrix(X_use)
  }

  # 组装通用参数。
  call_args <- list(
    X = X_use,
    Y = Y,
    W = W,
    D = D,
    t0 = t0
  )

  # 若支持 W.hat，加入倾向评分。
  if (supports_what) call_args$W.hat <- ps_hat
  # 若支持删失拟合选项，优先使用 survival.forest（更适合依赖删失）。
  if ("cen.fit" %in% fn_args) call_args$cen.fit <- "survival.forest"
  # 若支持折数参数，适当降低折数提升可运行性。
  if ("k.folds" %in% fn_args) call_args$k.folds <- 5

  # 执行拟合并捕获异常，保证单算法失败不影响整体流程。
  fit_obj <- tryCatch(
    do.call(fn, call_args),
    error = function(e) e
  )

  # 若拟合失败，返回失败状态。
  if (inherits(fit_obj, "error")) {
    return(list(
      status = "fit_error",
      msg = conditionMessage(fit_obj),
      cate = rep(NA_real_, length(Y))
    ))
  }

  pred_obj <- tryCatch(
    predict(fit_obj),
    error = function(e) e
  )
  if (inherits(pred_obj, "error")) {
    return(list(
      status = "predict_error",
      msg = conditionMessage(pred_obj),
      cate = rep(NA_real_, length(Y))
    ))
  }

  cate_vec <- NULL

  if (is.numeric(pred_obj) && length(pred_obj) == length(Y)) {
    cate_vec <- as.numeric(pred_obj)
  } else if (is.matrix(pred_obj) || is.data.frame(pred_obj)) {
    if (nrow(pred_obj) == length(Y) && ncol(pred_obj) >= 1) {
      cate_vec <- as.numeric(pred_obj[, 1])
    } else if (length(pred_obj) == length(Y)) {
      cate_vec <- as.numeric(pred_obj)
    }
  } else if (is.list(pred_obj)) {
    candidate_names <- c(
      "predictions", "prediction", "pred", "cate", "CATE",
      "tau_hat", "tau.hat", "tau", "tauHat"
    )
    for (nm in candidate_names) {
      if (!is.null(pred_obj[[nm]])) {
        v <- pred_obj[[nm]]
        if ((is.matrix(v) || is.data.frame(v)) && nrow(v) == length(Y) && ncol(v) >= 1) {
          v <- v[, 1]
        }
        if (is.numeric(v) && length(v) == length(Y)) {
          cate_vec <- as.numeric(v)
          break
        }
      }
    }
  }

  if (is.null(cate_vec)) {
    return(list(
      status = "predict_error",
      msg = paste0("predict() 返回类型无法解析：", paste(class(pred_obj), collapse = "|")),
      cate = rep(NA_real_, length(Y))
    ))
  }

  if (all(is.na(cate_vec))) {
    return(list(
      status = "predict_all_na",
      msg = "predict() 返回全 NA",
      cate = cate_vec
    ))
  }

  list(status = "ok", msg = "", cate = cate_vec)
}

# -----------------------------
# 3. 配置 survlearners 算法列表
# -----------------------------

# 定义希望覆盖的 survlearners 算法全集（按包文档常用命名）。
algo_candidates <- c(
  "surv_sl_lasso", "surv_sl_grf", "surv_sl_coxph",
  "surv_tl_lasso", "surv_tl_grf", "surv_tl_coxph",
  "surv_xl_lasso", "surv_xl_grf", "surv_xl_grf_lasso",
  "surv_fl_lasso", "surv_fl_grf",
  "surv_rl_lasso", "surv_rl_grf", "surv_rl_grf_lasso"
)

# 仅保留当前安装版本中实际存在的函数，避免“函数不存在”报错。
algo_available <- algo_candidates[
  vapply(
    algo_candidates,
    function(x) exists(x, envir = asNamespace("survlearners"), mode = "function", inherits = FALSE),
    logical(1)
  )
]

# 若无可用算法，直接终止。
if (length(algo_available) == 0) {
  stop("当前 survlearners 版本未检测到可用算法函数。")
}

# 保存当前版本中检测到的可用算法列表。
write_csv(
  tibble(algorithm = algo_available),
  file.path(out_dir, "04_survlearners_algorithms_available.csv")
)

# -----------------------------
# 4. 递归特征增加主循环
# -----------------------------

# t0 使用 RMST_HORIZON（默认24）。
t0 <- rmst_horizon

# 初始化总汇总结果容器。
all_summary <- tibble()
# 初始化“患者级 CATE 明细”容器。
all_patient_cate <- tibble()

# 外层循环：按变量重要性顺序逐步增加特征。
for (k in seq_along(feature_order)) {
  # 当前步特征集合（前 k 个）。
  vars_k <- feature_order[seq_len(k)]
  # 当前新增变量（用于记录）。
  added_var <- feature_order[k]

  # 提取当前步完整建模数据（Y/D/W + vars_k），并做完整案例过滤。
  dat_k <- df %>%
    mutate(
      # 若存在 raw_id 则用 raw_id 作为患者ID；否则使用行号。
      patient_id = if ("raw_id" %in% names(.)) as.character(raw_id) else as.character(row_number())
    ) %>%
    select(all_of(c("patient_id", "Y", "D", "W", vars_k, ps_feature_use))) %>%
    filter(complete.cases(.))

  # 若样本过少或处理组不平衡到不可用，则整步跳过。
  if (nrow(dat_k) < 120 || dplyr::n_distinct(dat_k$W) < 2) {
    all_summary <- bind_rows(
      all_summary,
      tibble(
        algorithm = algo_available,
        step = k,
        n_features = k,
        added_feature = added_var,
        used_features = paste(vars_k, collapse = "|"),
        n_used = nrow(dat_k),
        cate_mean = NA_real_,
        cate_sd = NA_real_,
        cate_q25 = NA_real_,
        cate_q50 = NA_real_,
        cate_q75 = NA_real_,
        cate_min = NA_real_,
        cate_max = NA_real_,
        ps_mean = NA_real_,
        ps_sd = NA_real_,
        status = "skip_low_sample"
      )
    )

    # 当整步因样本不足跳过时，患者级明细也写入占位记录（cate=NA）。
    all_patient_cate <- bind_rows(
      all_patient_cate,
      tidyr::crossing(
        algorithm = algo_available,
        patient_id = if ("raw_id" %in% names(df)) as.character(df$raw_id) else as.character(seq_len(nrow(df)))
      ) %>%
        mutate(
          step = k,
          n_features = k,
          added_feature = added_var,
          cate = NA_real_,
          in_model = 0L,
          status = "skip_low_sample"
        )
    )

    next
  }

  # 构建当前步特征矩阵。
  X_k <- make_x_matrix(dat_k, vars_k)
  # 用固定PS变量估计当前步 PS（观察性校正核心）。
  ps_k <- estimate_ps(dat_k, ps_vars = ps_feature_use, w_col = "W")

  # 内层循环：遍历每个可用 survlearners 算法。
  for (algo in algo_available) {
    # 拟合并预测当前算法的 CATE。
    fit_res <- fit_one_algorithm(
      algo_name = algo,
      X_base = X_k,
      Y = dat_k$Y,
      W = dat_k$W,
      D = dat_k$D,
      t0 = t0,
      ps_hat = ps_k
    )

    # 计算 CATE 描述统计。
    cate_mean <- mean(fit_res$cate, na.rm = TRUE)
    cate_sd <- sd(fit_res$cate, na.rm = TRUE)
    cate_q <- as.numeric(quantile(fit_res$cate, probs = c(0.25, 0.5, 0.75), na.rm = TRUE))
    cate_min <- suppressWarnings(min(fit_res$cate, na.rm = TRUE))
    cate_max <- suppressWarnings(max(fit_res$cate, na.rm = TRUE))

    # 若全 NA，min/max 会返回 Inf/-Inf，这里统一改成 NA。
    if (!is.finite(cate_min)) cate_min <- NA_real_
    if (!is.finite(cate_max)) cate_max <- NA_real_

    # 记录当前（算法×步）的结果。
    all_summary <- bind_rows(
      all_summary,
      tibble(
        algorithm = algo,
        step = k,
        n_features = k,
        added_feature = added_var,
        used_features = paste(vars_k, collapse = "|"),
        n_used = nrow(dat_k),
        cate_mean = cate_mean,
        cate_sd = cate_sd,
        cate_q25 = cate_q[1],
        cate_q50 = cate_q[2],
        cate_q75 = cate_q[3],
        cate_min = cate_min,
        cate_max = cate_max,
        ps_mean = mean(ps_k, na.rm = TRUE),
        ps_sd = sd(ps_k, na.rm = TRUE),
        status = fit_res$status
      )
    )

    # 构建“当前算法×当前步”的患者级 CATE 表：
    # 1) 对于进入当前步 complete-case 建模的患者，写入预测 cate；
    # 2) 对于未进入建模的患者，cate 记 NA，in_model=0。
    cate_in_model <- tibble(
      patient_id = dat_k$patient_id,
      cate = fit_res$cate,
      in_model = 1L
    )

    one_step_algo_patient <- tibble(
      patient_id = if ("raw_id" %in% names(df)) as.character(df$raw_id) else as.character(seq_len(nrow(df)))
    ) %>%
      left_join(cate_in_model, by = "patient_id") %>%
      mutate(
        in_model = if_else(is.na(in_model), 0L, in_model),
        algorithm = algo,
        step = k,
        n_features = k,
        added_feature = added_var,
        status = fit_res$status
      ) %>%
      select(algorithm, step, n_features, added_feature, patient_id, cate, in_model, status)

    # 追加写入总患者级结果容器。
    all_patient_cate <- bind_rows(all_patient_cate, one_step_algo_patient)
  }

  # 控制台打印进度，便于长任务监控。
  cat(sprintf("完成 step %d / %d，新增变量：%s\n", k, length(feature_order), added_var))
}

# -----------------------------
# 5. 写出结果表
# -----------------------------

# 保存本次运行的元信息，便于后续复现。
run_meta <- tibble(
  script_path = script_path,
  output_dir = output_dir <- out_dir,
  data_file = normalizePath(data_file, winslash = "/", mustWork = FALSE),
  rank_file = normalizePath(rank_file, winslash = "/", mustWork = FALSE),
  ps_file = normalizePath(ps_file_path, winslash = "/", mustWork = FALSE),
  ps_group_name = ps_group_name,
  rmst_horizon = rmst_horizon,
  total_ranked_features = nrow(rank_df),
  used_feature_n = length(feature_order),
  ps_var_n = length(ps_feature_use),
  algorithm_n = length(algo_available)
)
write_csv(run_meta, file.path(out_dir, "05_run_meta.csv"))

# 保存最终采用的递归特征顺序。
write_csv(
  tibble(step = seq_along(feature_order), variable = feature_order),
  file.path(out_dir, "06_feature_order_used.csv")
)

# 写出总汇总表（所有算法合并）。
write_csv(
  all_summary,
  file.path(out_dir, "07_survlearners_recursive_feature_growth_all_algorithms.csv")
)

# 写出患者级 CATE 总表（算法×步×患者）。
write_csv(
  all_patient_cate,
  file.path(out_dir, "08_survlearners_recursive_feature_growth_all_algorithms_patient_cate.csv")
)

# 按算法拆分输出“每算法一张表”。
for (algo in unique(all_summary$algorithm)) {
  one_algo <- all_summary %>%
    filter(algorithm == algo) %>%
    arrange(step)

  write_csv(
    one_algo,
    file.path(out_dir, paste0("09_", algo, "_recursive_feature_growth_table.csv"))
  )

  # 同步输出该算法的患者级 CATE 明细表。
  one_algo_patient <- all_patient_cate %>%
    filter(algorithm == algo) %>%
    arrange(step, patient_id)

  write_csv(
    one_algo_patient,
    file.path(out_dir, paste0("10_", algo, "_recursive_feature_growth_patient_cate.csv"))
  )
}

# 控制台输出完成信息。
cat("递归特征增加 + survlearners 全算法运行完成。\n")
cat("输出目录：", out_dir, "\n")
