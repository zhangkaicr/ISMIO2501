# ==========================================
# 脚本名称：2-rmst24_qini_rank_by_variable_train.R
# 核心用途：
# 1) 加载 1-rmst_hte_qini_fun.R 中的函数；
# 2) 使用 01_ISMIO2501_train_tidy.xlsx 的变量逐一计算 Qini；
# 3) 固定 RMST 截断时间为 24 个月；
# 4) 按 Qini 从高到低排序并输出结果数据框。
#
# 使用方法（在项目根目录）：
# Rscript --version
# Rscript 2-rmst24_qini_rank_by_variable_train.R
# ==========================================

# 加载 pacman 包，便于统一依赖加载。
library(pacman, help, pos = 2, lib.loc = NULL)
# 加载数据处理、读 Excel、路径管理与 GRF 建模需要的包。
p_load(tidyverse, openxlsx, here, grf)

# 固定随机种子，保证结果可复现。
set.seed(20260423)

# 加载原始函数脚本（按需求：原脚本不修改，只加载使用）。
source(here("1-rmst_hte_qini_fun.R"))

# ---------------------------------------------------------
# 本地函数：获取当前脚本路径与输出目录
# ---------------------------------------------------------
# 解析 Rscript 调用时传入的 --file 参数。
get_current_script_path <- function(default_path = here("2-rmst24_qini_rank_by_variable_train.R")) {
  # 读取完整命令行参数。
  args_full <- commandArgs(trailingOnly = FALSE)
  # 提取形如 --file=xxx 的参数。
  file_arg <- args_full[str_detect(args_full, "^--file=")]
  # 若存在脚本参数，则使用实际脚本路径；否则回退到默认路径。
  if (length(file_arg) > 0) {
    return(normalizePath(str_remove(file_arg[1], "^--file="), winslash = "/", mustWork = FALSE))
  }
  # 返回默认脚本路径，兼容交互环境 source 调用。
  normalizePath(default_path, winslash = "/", mustWork = FALSE)
}

# 根据脚本名创建 output/脚本名 目录，保证每个脚本结果独立存放。
build_output_dir <- function(script_path) {
  # 去掉扩展名，仅保留脚本主名。
  script_stem <- tools::file_path_sans_ext(basename(script_path))
  # 生成输出目录路径。
  out_dir <- here("output", script_stem)
  # 若目录不存在则递归创建。
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  # 返回输出目录。
  out_dir
}

# ---------------------------------------------------------
# 本地函数：解析 PS变量最终确定.txt
# ---------------------------------------------------------
# 从文本文件中提取多个“PS评分计算变量X”变量组。
parse_ps_groups <- function(ps_file) {
  # 检查文件是否存在。
  if (!file.exists(ps_file)) {
    stop(paste0("未找到PS变量文件：", ps_file))
  }

  # 逐行读取文本，关闭警告以兼容中文内容。
  lines_raw <- readr::read_lines(ps_file, lazy = FALSE)
  # 去掉首尾空白，便于后续规则匹配。
  lines_trim <- stringr::str_trim(lines_raw)
  # 找出每个变量组标题所在位置。
  header_idx <- which(stringr::str_detect(lines_trim, "^PS评分计算变量\\d+：$"))

  # 若识别到分组标题，则按原始分组文本格式解析。
  if (length(header_idx) > 0) {
    return(
      map_dfr(seq_along(header_idx), function(i) {
        # 当前变量组标题所在行。
        start_idx <- header_idx[i]
        # 当前变量组终止行：取下一个标题前一行，或文件末尾。
        end_idx <- if (i < length(header_idx)) header_idx[i + 1] - 1 else length(lines_trim)
        # 提取当前变量组标题文本。
        group_name <- lines_trim[start_idx]
        # 截取当前变量组正文行。
        block_lines <- lines_trim[seq.int(start_idx + 1, end_idx)]
        # 仅保留形如“1.age_grade”的变量定义行。
        variable_lines <- block_lines[stringr::str_detect(block_lines, "^\\d+\\.[^[:space:]].*$")]

        # 若该变量组没有解析出变量，则返回空结果。
        if (length(variable_lines) == 0) {
          return(tibble())
        }

        # 拆出变量顺序号与变量名。
        tibble(
          group_name = group_name,
          order_id = as.integer(stringr::str_extract(variable_lines, "^\\d+")),
          variable = stringr::str_trim(stringr::str_replace(variable_lines, "^\\d+\\.", ""))
        )
      }) %>%
        # 去掉空变量名与重复记录。
        filter(!is.na(variable), variable != "") %>%
        distinct(group_name, order_id, variable)
    )
  }

  # 若未识别到分组标题，则尝试解析 R 向量格式，如 ps_feature_fixed <- c("a","b")。
  quoted_vars <- stringr::str_match_all(paste(lines_raw, collapse = "\n"), '"([^"]+)"')[[1]]
  # 提取被双引号包裹的变量名。
  variable_values <- quoted_vars[, 2]

  # 若能解析出变量名，则按单组固定变量处理。
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

  # 两种格式都无法解析时，给出明确错误。
  stop("PS变量文件格式无法识别；请使用“PS评分计算变量X：”分组格式或R向量格式。")
}

# ---------------------------------------------------------
# 本地函数：指定 horizon 的 RMST-Qini 计算函数
# ---------------------------------------------------------
compute_rmst_hte_qini_h <- function(
    data,
    time_col,
    event_col,
    treatment_col,
    covariate_cols,
    ps_covariate_cols,
    horizon = 24,
    q_grid = seq(0.1, 1, by = 0.1),
    train_ratio = 0.5,
    num_trees_ps = 300,
    num_trees_csf = 600,
    qini_boot_R = 50
) {
  # 检查 horizon 是否为有效正数。
  if (!is.numeric(horizon) || length(horizon) != 1 || !is.finite(horizon) || horizon <= 0) {
    stop("horizon 必须是正数。")
  }
  # 检查训练集比例是否在 (0,1) 区间内。
  if (!is.numeric(train_ratio) || !is.finite(train_ratio) || train_ratio <= 0 || train_ratio >= 1) {
    stop("train_ratio 必须是 (0,1) 内的数值。")
  }

  # 汇总必需字段并检查是否缺失。
  required_cols <- c(time_col, event_col, treatment_col, covariate_cols, ps_covariate_cols)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste0("输入数据缺少必要列：", paste(missing_cols, collapse = ", ")))
  }

  # 只保留分析字段，并标准化时间/事件/处理变量格式。
  df <- data %>%
    select(all_of(required_cols)) %>%
    mutate(
      .Y = as.numeric(.data[[time_col]]),
      .D = as.numeric(.data[[event_col]] > 0),
      .W = as.numeric(.data[[treatment_col]] > 0)
    ) %>%
    filter(!is.na(.Y), !is.na(.D), !is.na(.W))

  # 检查处理变量是否二值化成功。
  if (!all(df$.W %in% c(0, 1))) {
    stop("treatment_col 必须可转换为二值变量（0/1）。")
  }

  # 提取协变量并把字符/逻辑变量转为因子。
  x_raw <- df %>%
    select(all_of(covariate_cols)) %>%
    mutate(across(where(~ is.character(.x) || is.logical(.x)), as.factor))

  # 使用完整案例，避免模型输入缺失。
  cc_idx <- complete.cases(x_raw)
  df <- df[cc_idx, , drop = FALSE]
  x_raw <- x_raw[cc_idx, , drop = FALSE]

  # 构建设计矩阵（one-hot，无截距）。
  x_terms <- terms(~ . - 1, data = x_raw)
  X <- model.matrix(x_terms, data = x_raw)

  # 样本量检查，太小则直接报错。
  n <- nrow(X)
  if (n < 80) {
    stop("样本量过小（<80），建议增大样本后再进行 RMST-HTE Qini 评估。")
  }

  # 划分训练集与评估集，做训练-评估分离。
  idx_train <- sample.int(n, size = floor(train_ratio * n), replace = FALSE)
  idx_eval <- setdiff(seq_len(n), idx_train)
  if (length(idx_train) < 50 || length(idx_eval) < 30) {
    stop("训练集或评估集样本过小，请调大样本量或调整 train_ratio。")
  }

  # 拆分训练/评估数据。
  X_train <- X[idx_train, , drop = FALSE]
  X_eval <- X[idx_eval, , drop = FALSE]
  Y_train <- df$.Y[idx_train]
  Y_eval <- df$.Y[idx_eval]
  W_train <- df$.W[idx_train]
  W_eval <- df$.W[idx_eval]
  D_train <- df$.D[idx_train]
  D_eval <- df$.D[idx_eval]

  # 使用固定PS变量构建设计矩阵（与模型筛选变量分离）。
  ps_raw <- data %>%
    select(all_of(ps_covariate_cols)) %>%
    mutate(across(where(~ is.character(.x) || is.logical(.x)), as.factor))
  ps_raw <- ps_raw[cc_idx, , drop = FALSE]
  ps_terms <- terms(~ . - 1, data = ps_raw)
  X_ps <- model.matrix(ps_terms, data = ps_raw)
  X_ps_train <- X_ps[idx_train, , drop = FALSE]
  X_ps_eval <- X_ps[idx_eval, , drop = FALSE]

  # 拟合倾向评分模型并截断概率范围。
  ps_forest <- regression_forest(
    X = X_ps_train,
    Y = W_train,
    num.trees = num_trees_ps,
    honesty = TRUE
  )
  ps_train <- pmin(pmax(predict(ps_forest, X_ps_train)$predictions, 0.01), 0.99)
  ps_eval <- pmin(pmax(predict(ps_forest, X_ps_eval)$predictions, 0.01), 0.99)

  # 在训练集拟合 CSF，用于给评估集预测 tau 排序分数。
  csf_train <- causal_survival_forest(
    X = X_train,
    Y = Y_train,
    W = W_train,
    D = D_train,
    W.hat = ps_train,
    target = "RMST",
    horizon = horizon,
    num.trees = num_trees_csf,
    honesty = TRUE
  )

  # 在评估集拟合 CSF，用于计算 DR score 和 Qini 指标。
  csf_eval <- causal_survival_forest(
    X = X_eval,
    Y = Y_eval,
    W = W_eval,
    D = D_eval,
    W.hat = ps_eval,
    target = "RMST",
    horizon = horizon,
    num.trees = num_trees_csf,
    honesty = TRUE
  )

  # 获取评估集 tau 预测值（用于排序）。
  tau_eval <- as.numeric(predict(csf_train, X_eval)$predictions)
  # 获取评估集 DR score（用于 RATE 计算）。
  dr_score_eval <- as.numeric(get_scores(csf_eval))

  # 计算 Qini 标量（grf 口径）。
  qini_scalar_raw <- rank_average_treatment_effect(
    forest = csf_eval,
    priorities = tau_eval,
    target = "QINI",
    q = q_grid,
    R = qini_boot_R
  )

  # 整理 Qini 标量结果。
  qini_scalar <- tibble(
    metric = "QINI",
    estimate = as.numeric(qini_scalar_raw$estimate[1]),
    std_err = as.numeric(qini_scalar_raw$std.err[1]),
    z_value = estimate / std_err,
    p_value = 2 * pnorm(-abs(z_value))
  )

  # 额外构建 Qini 曲线点（用于诊断与可视化）。
  ord <- order(tau_eval, decreasing = TRUE)
  s_ord <- dr_score_eval[ord]
  cum_s <- cumsum(s_ord)
  mu_s <- mean(s_ord)
  n_eval <- length(s_ord)

  qini_curve_points <- map_dfr(q_grid, function(qi) {
    m <- max(1L, floor(qi * n_eval))
    gain_targeted <- cum_s[m] / n_eval
    gain_random <- qi * mu_s
    tibble(
      q = as.numeric(qi),
      qini = as.numeric(gain_targeted - gain_random)
    )
  })

  # 返回结构化结果。
  list(
    qini_scalar = qini_scalar,
    qini_curve_points = qini_curve_points,
    tau_eval = tau_eval,
    dr_score_eval = dr_score_eval,
    meta = tibble(
      n_total = n,
      n_train = length(idx_train),
      n_eval = length(idx_eval),
      horizon = horizon
    )
  )
}

# ---------------------------------------------------------
# 主流程：逐变量计算 Qini 并排序
# ---------------------------------------------------------

# 可选环境变量：RMST_HORIZON，默认 24（月）。
rmst_horizon <- suppressWarnings(as.numeric(Sys.getenv("RMST_HORIZON", unset = "24")))
if (!is.finite(rmst_horizon) || rmst_horizon <= 0) {
  stop("RMST_HORIZON 必须是正数。")
}

# 获取当前脚本路径，供输出目录命名使用。
script_path <- get_current_script_path()
# 创建按脚本名分隔的输出目录。
output_dir <- build_output_dir(script_path)

# 指定 PS 变量文本文件路径。
ps_file_path <- here("data", "PS变量最终确定.txt")
# 解析文本中的全部 PS 变量组。
ps_group_table <- parse_ps_groups(ps_file_path)
# 将解析后的长表保存下来，便于留痕与复核。
write_csv(ps_group_table, file.path(output_dir, "01_ps_groups_long.csv"))

# 指定默认使用的 PS 变量组名称。
# 若环境变量未指定，则默认采用解析结果中的第一个变量组。
ps_group_name_default <- unique(ps_group_table$group_name)[1]
ps_group_name <- Sys.getenv("PS_GROUP_NAME", unset = ps_group_name_default)
# 提取目标 PS 变量组。
ps_feature_selected <- ps_group_table %>%
  filter(group_name == ps_group_name) %>%
  arrange(order_id)

# 若指定组不存在，则给出明确报错。
if (nrow(ps_feature_selected) == 0) {
  stop(paste0("未在PS变量文件中找到指定变量组：", ps_group_name))
}

# 保存当前运行实际采用的 PS 变量列表。
write_csv(ps_feature_selected, file.path(output_dir, "02_ps_group_selected.csv"))

# 读取指定训练集文件的 tidy 工作表（按用户要求固定 sheet）。
df_train <- read.xlsx(
  xlsxFile = here("data", "01_ISMIO2501_train_tidy.xlsx"),
  sheet = "tidy"
)

# 检查关键字段是否存在。
required_cols <- c("arms", "Event", "OS")
missing_required <- setdiff(required_cols, names(df_train))
if (length(missing_required) > 0) {
  stop(paste0("训练集缺少必要列：", paste(missing_required, collapse = ", ")))
}

# 定义 treated 组：优先使用 TACE_TA，否则使用排序后第二组。
if ("TACE_TA" %in% unique(as.character(df_train$arms))) {
  treated_label <- "TACE_TA"
} else {
  treated_label <- sort(unique(as.character(df_train$arms)))[2]
}

# 构建二值处理变量，供函数使用。
df_train <- df_train %>%
  mutate(
    W_bin = as.numeric(as.character(arms) == treated_label)
  )

# 定义候选变量：排除 ID、处理和结局字段；按需求移除 HBsAb。
exclude_cols <- c("raw_id", "arms", "W_bin", "Event", "OS", "PFS_Event", "PFS", "HBsAb")
candidate_vars <- setdiff(names(df_train), exclude_cols)

# 基于“PS变量最终确定.txt”中的指定变量组，生成本次运行的 PS 评分变量。
ps_feature_use <- intersect(ps_feature_selected$variable, names(df_train))
if (length(ps_feature_use) < 2) {
  stop("固定PS变量在训练数据中可用列不足，无法进行PS估计。")
}

# 保存训练集实际可用的 PS 变量列表，避免文本变量与数据字段不一致时难以追踪。
write_csv(
  tibble(variable = ps_feature_use),
  file.path(output_dir, "03_ps_variables_available_in_train.csv")
)

# 逐变量计算 Qini，并做容错处理。
qini_result <- map_dfr(candidate_vars, function(v) {
  # 提取当前变量并计算基本信息量指标。
  x <- df_train[[v]]
  n_non_na <- sum(!is.na(x))
  n_unique <- dplyr::n_distinct(x[!is.na(x)])

  # 信息量不足的变量直接跳过。
  if (n_non_na < 80 || n_unique < 2) {
    return(tibble(
      variable = v,
      n_non_na = n_non_na,
      n_unique = n_unique,
      qini = NA_real_,
      std_err = NA_real_,
      z_value = NA_real_,
      p_value = NA_real_,
      horizon = rmst_horizon,
      status = "skip_low_information"
    ))
  }

  # 安全计算：单变量失败不影响全局流程。
  out <- tryCatch(
    compute_rmst_hte_qini_h(
      data = df_train,
      time_col = "OS",
      event_col = "Event",
      treatment_col = "W_bin",
      covariate_cols = v,
      ps_covariate_cols = ps_feature_use,
      horizon = rmst_horizon,
      q_grid = seq(0.1, 1, by = 0.1),
      train_ratio = 0.7,
      num_trees_ps = 300,
      num_trees_csf = 600,
      qini_boot_R = 50
    ),
    error = function(e) e
  )

  # 若失败则记录错误信息。
  if (inherits(out, "error")) {
    return(tibble(
      variable = v,
      n_non_na = n_non_na,
      n_unique = n_unique,
      qini = NA_real_,
      std_err = NA_real_,
      z_value = NA_real_,
      p_value = NA_real_,
      horizon = rmst_horizon,
      status = paste0("error: ", conditionMessage(out))
    ))
  }

  # 成功时返回该变量 Qini 结果。
  tibble(
    variable = v,
    n_non_na = n_non_na,
    n_unique = n_unique,
    qini = as.numeric(out$qini_scalar$estimate[1]),
    std_err = as.numeric(out$qini_scalar$std_err[1]),
    z_value = as.numeric(out$qini_scalar$z_value[1]),
    p_value = as.numeric(out$qini_scalar$p_value[1]),
    horizon = as.numeric(out$meta$horizon[1]),
    status = "ok"
  )
})

# 先输出未排序原始结果，便于后续排查每个变量的状态。
write_csv(qini_result, file.path(output_dir, "04_qini_by_variable_raw_train.csv"))

# 按 Qini 从高到低排序。
qini_result_sorted <- qini_result %>%
  arrange(desc(qini))

# 记录本次运行配置，便于复现。
run_meta <- tibble(
  script_path = script_path,
  output_dir = output_dir,
  data_file = normalizePath(here("data", "01_ISMIO2501_train_tidy.xlsx"), winslash = "/", mustWork = FALSE),
  sheet_name = "tidy",
  ps_file = normalizePath(ps_file_path, winslash = "/", mustWork = FALSE),
  ps_group_name = ps_group_name,
  rmst_horizon = rmst_horizon,
  candidate_var_n = length(candidate_vars),
  ps_var_n = length(ps_feature_use)
)
write_csv(run_meta, file.path(output_dir, "05_run_meta.csv"))

# 输出最终排序结果到 output/脚本名 目录。
horizon_tag <- gsub("\\.", "p", as.character(rmst_horizon))
out_csv <- file.path(output_dir, paste0("06_rmst", horizon_tag, "_qini_by_variable_ranked_train.csv"))
write_csv(qini_result_sorted, out_csv)

# 控制台展示前 20 行结果。
cat("==== RMST=", rmst_horizon, " 逐变量Qini排序（前20） ====\n", sep = "")
print(qini_result_sorted %>% slice_head(n = 20))
cat("\n结果文件：", out_csv, "\n")
