#!/usr/bin/env Rscript

# ==========================================
# 脚本名称：9-surv_fl_grf_top24_train_and_decile.R
# 核心用途：
# 1) 固定最终模型为 surv_fl_grf；
# 2) 固定使用变量重要性排序前24个变量作为建模变量；
# 3) 固定 RMST 时间为 24 个月；
# 4) 固定使用 PS变量最终确定.txt 中的 PS 变量；
# 5) 只在训练集拟合一次模型，再分别应用到 train / validation / prevalidation；
# 6) 输出每位患者的 CATE、PS、OW 权重与 IPW 权重；
# 7) 将患者级结果追加到原始数据末尾，输出类似第7步的3个新表；
# 8) 基于第9步新模型结果继续做类似第8步的10分位 HR 结果与森林图。
# ==========================================

# 加载 pacman，统一依赖管理。
library(pacman, help, pos = 2, lib.loc = NULL)
# 加载脚本所需依赖。
p_load(tidyverse, openxlsx, here, grf, survlearners, survival)
# 加载 PS 权重函数，便于输出患者级 OW / IPW 权重与 decile 加权 HR。
source(here("4-ps_weighted_hr_function.R"))

# 固定随机种子，保证结果可复现。
set.seed(20260513)

# ------------------------------------------
# 0) 路径与输出目录辅助函数
# ------------------------------------------

# 定义函数：解析当前脚本路径，便于按脚本名创建 output 子目录。
get_current_script_path <- function(default_path = here("9-surv_fl_grf_top24_train_and_decile.R")) {
  # 获取完整命令行参数。
  args_full <- commandArgs(trailingOnly = FALSE)
  # 提取 --file=xxx 参数。
  file_arg <- args_full[stringr::str_detect(args_full, "^--file=")]
  # 若存在脚本路径，则优先使用。
  if (length(file_arg) > 0) {
    return(normalizePath(stringr::str_remove(file_arg[1], "^--file="), winslash = "/", mustWork = FALSE))
  }
  # 否则回退到默认脚本路径。
  normalizePath(default_path, winslash = "/", mustWork = FALSE)
}

# 定义函数：根据脚本名创建 output/脚本名 目录。
build_output_dir <- function(script_path) {
  # 提取脚本主名。
  script_stem <- tools::file_path_sans_ext(basename(script_path))
  # 拼接输出目录。
  out_dir <- here("output", script_stem)
  # 若目录不存在，则递归创建。
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  # 返回输出目录。
  out_dir
}

# ------------------------------------------
# 1) 参数与路径设置
# ------------------------------------------

# 固定最终算法名称。
algo_name <- "surv_fl_grf"

# 固定只纳入变量重要性排序前 24 个变量。
top_n_features <- 24L

# 固定 RMST horizon；若外部传入环境变量则优先使用，否则默认为 24。
rmst_horizon <- suppressWarnings(as.numeric(Sys.getenv("RMST_HORIZON", unset = "24")))

# 若 horizon 非法则停止。
if (!is.finite(rmst_horizon) || rmst_horizon <= 0) {
  stop("RMST_HORIZON 必须是正数。")
}

# 将 horizon 赋值给 t0，保持与 survlearners 接口一致。
t0 <- rmst_horizon

# 获取当前脚本路径，并建立输出目录。
script_path <- get_current_script_path()
out_dir <- build_output_dir(script_path)

# 报告根目录；支持可选子目录，保持与第7步脚本一致。
report_subdir <- Sys.getenv("REPORT_SUBDIR", unset = "")
report_root <- if (nzchar(report_subdir)) here("reports", report_subdir) else here("reports")

# 数据目录。
data_dir <- here("data")

# 若数据目录不存在则停止。
if (!dir.exists(data_dir)) {
  stop("未找到 data/ 目录：", data_dir)
}

# ------------------------------------------
# 2) 工具函数：定位并读取 PS 变量文件
# ------------------------------------------

# 定义函数：自动定位“PS变量最终确定”文本文件。
locate_ps_file <- function() {
  # 在 data/ 下搜索名称中包含“PS变量最终确定”的 txt 文件。
  cand <- list.files(
    path = data_dir,
    pattern = "PS变量最终确定.*\\.txt$",
    full.names = TRUE
  )

  # 若没有找到则报错。
  if (length(cand) < 1) {
    stop("在 data/ 下未找到“PS变量最终确定*.txt”文件。")
  }

  # 若找到多个，则优先选择文件名最短者。
  cand <- cand[order(nchar(basename(cand)), basename(cand))]

  # 返回首个候选路径。
  cand[1]
}

# 定义函数：按 set_id 从 PS 文本中读取变量名称。
read_ps_feature_final_local <- function(ps_file, set_id = 1) {
  # 若文件不存在则返回空向量。
  if (!file.exists(ps_file)) {
    return(character())
  }

  # 按 UTF-8 读取文本，避免中文乱码。
  lines <- readLines(ps_file, warn = FALSE, encoding = "UTF-8")

  # 构造目标标题块，例如“PS评分计算变量1”。
  header_pat <- paste0("^\\s*PS评分计算变量\\s*", set_id, "\\s*[:：]?\\s*$")

  # 定位标题行。
  start_idx <- which(grepl(header_pat, lines))

  # 若存在对应标题块，则只解析该块；否则退回解析全文。
  if (length(start_idx) >= 1) {
    start <- start_idx[1] + 1
    end_candidates <- which(grepl("^\\s*PS评分计算变量\\s*\\d+\\s*[:：]?\\s*$", lines))
    end_candidates <- end_candidates[end_candidates > start_idx[1]]
    end <- if (length(end_candidates) >= 1) end_candidates[1] - 1 else length(lines)
    block <- lines[start:end]
  } else {
    block <- lines
  }

  # 优先解析双引号中的变量名，兼容 R 向量写法。
  quoted_list <- regmatches(block, gregexpr("\"[A-Za-z0-9_]+\"", block, perl = TRUE))
  quoted_tokens <- unique(unlist(quoted_list))
  quoted_vars <- gsub("^\"|\"$", "", quoted_tokens)
  quoted_vars <- quoted_vars[nzchar(quoted_vars)]

  # 若成功解析到双引号变量名，则直接返回。
  if (length(quoted_vars) > 0) {
    return(quoted_vars)
  }

  # 优先解析“1.var_name”这种格式。
  token_list <- regmatches(block, gregexpr("[0-9]+\\.[A-Za-z0-9_]+", block, perl = TRUE))
  tokens <- unique(unlist(token_list))
  vars <- sub("^[0-9]+\\.", "", tokens)
  vars <- vars[nzchar(vars)]

  # 若未解析到编号格式，则按逐行变量名再解析一次。
  if (length(vars) == 0) {
    plain <- trimws(block)
    plain <- plain[nzchar(plain)]
    plain <- plain[!grepl("^PS评分计算变量\\s*\\d+\\s*[:：]?$", plain)]
    plain <- sub("^\\d+\\s*[\\.|、\\)]\\s*", "", plain)
    plain <- trimws(plain)
    plain <- plain[nzchar(plain)]
    vars <- unique(plain)
  }

  # 返回变量名向量。
  vars
}

# ------------------------------------------
# 3) 工具函数：按变量重要性排序读取前 24 个变量
# ------------------------------------------

# 定义函数：从变量重要性排序文件中读取前 n 个变量。
get_top_feature_vec_by_qini <- function(report_root, n_features = 24L) {
  # 默认优先读取 reports 根目录下的 rmst24 排序表。
  rank_file_default <- file.path(report_root, "rmst24_qini_by_variable_ranked_train.csv")

  # 若设置了自定义排序文件，则优先使用环境变量。
  rank_file_env <- Sys.getenv("QINI_RANK_FILE", unset = "")

  # 若 reports 文件不存在，则回退到当前主线 output 文件。
  rank_file_fallback <- here("output", "2-rmst24_qini_rank_by_variable_train", "06_rmst24_qini_by_variable_ranked_train.csv")

  # 确定最终读取路径。
  rank_file <- if (nzchar(rank_file_env)) {
    rank_file_env
  } else if (file.exists(rank_file_default)) {
    rank_file_default
  } else {
    rank_file_fallback
  }

  # 若排序文件不存在则报错。
  if (!file.exists(rank_file)) {
    stop("未找到变量重要性排序文件：", rank_file)
  }

  # 读取排序表。
  tab <- readr::read_csv(rank_file, show_col_types = FALSE)

  # 仅保留有效记录，并按表中已有顺序取前 n 个变量。
  top_tab <- tab %>%
    filter(status == "ok", !is.na(variable), variable != "") %>%
    slice_head(n = n_features)

  # 若不足 n 个变量则报错。
  if (nrow(top_tab) < n_features) {
    stop("变量重要性排序表中的有效变量少于前 ", n_features, " 个。")
  }

  # 返回变量向量与实际文件路径。
  list(
    feature_vec = top_tab$variable,
    rank_file_used = rank_file
  )
}

# ------------------------------------------
# 4) 工具函数：构建设计矩阵，并保证训练/预测列一致
# ------------------------------------------

# 定义函数：根据训练数据建立设计矩阵模板。
build_design_blueprint <- function(dat, vars) {
  # 提取原始特征。
  x_raw <- dat %>%
    select(all_of(vars))

  # 将字符型与逻辑型变量转为因子，保证 model.matrix 可稳定展开哑变量。
  x_raw <- x_raw %>%
    mutate(across(where(~ is.character(.x) || is.logical(.x)), as.factor))

  # 记录各因子变量水平，便于外部数据集沿用训练集编码规则。
  factor_levels <- purrr::map(
    x_raw %>% select(where(is.factor)),
    levels
  )

  # 建立无截距设计公式。
  mm_formula <- terms(~ . - 1, data = x_raw)

  # 生成训练集设计矩阵。
  X_train <- model.matrix(mm_formula, data = x_raw)

  # 返回模板对象。
  list(
    vars = vars,
    formula = mm_formula,
    factor_levels = factor_levels,
    colnames = colnames(X_train),
    X_train = X_train
  )
}

# 定义函数：用训练集模板把任意数据集转换成同列设计矩阵。
make_x_matrix_from_blueprint <- function(dat, blueprint) {
  # 取出目标变量。
  x_raw <- dat %>%
    select(all_of(blueprint$vars))

  # 对训练中为因子的列，强制使用训练集水平。
  for (nm in names(blueprint$factor_levels)) {
    x_raw[[nm]] <- factor(as.character(x_raw[[nm]]), levels = blueprint$factor_levels[[nm]])
  }

  # 其他字符/逻辑型变量仍转为因子。
  x_raw <- x_raw %>%
    mutate(across(where(~ is.character(.x) || is.logical(.x)), as.factor))

  # 生成当前数据集设计矩阵。
  X_now <- model.matrix(blueprint$formula, data = x_raw)

  # 创建与训练集完全同列的零矩阵。
  X_aligned <- matrix(
    0,
    nrow = nrow(X_now),
    ncol = length(blueprint$colnames),
    dimnames = list(NULL, blueprint$colnames)
  )

  # 仅把当前矩阵中存在的列写回；缺失列保持 0。
  common_cols <- intersect(colnames(X_now), blueprint$colnames)
  X_aligned[, common_cols] <- X_now[, common_cols, drop = FALSE]

  # 返回对齐后的矩阵。
  X_aligned
}

# ------------------------------------------
# 5) 工具函数：数据预处理、PS 估计、模型拟合与预测
# ------------------------------------------

# 定义函数：从原始数据集中提取建模所需字段，并保留原始行号用于回填。
build_analysis_df <- function(raw_df, feature_vec, ps_vec, treated_label) {
  # 构造分析数据，并保留原始行号。
  raw_df %>%
    mutate(.row_id = row_number()) %>%
    transmute(
      .row_id = .row_id,
      W = as.numeric(as.character(arms) == treated_label),
      Y = as.numeric(OS),
      D = as.numeric(Event > 0),
      across(all_of(unique(c(feature_vec, ps_vec))))
    )
}

# 定义函数：在训练集上估计 PS，并同时返回拟合对象与训练集 PS。
estimate_ps_grf <- function(dat, ps_blueprint) {
  # 用训练集 PS 模板构建设计矩阵。
  X_ps <- make_x_matrix_from_blueprint(dat, ps_blueprint)

  # 用回归森林拟合处理指派机制。
  ps_fit <- regression_forest(
    X = X_ps,
    Y = dat$W,
    num.trees = 300,
    honesty = TRUE
  )

  # 预测训练集 PS，并做截断。
  ps_hat <- as.numeric(predict(ps_fit)$predictions)
  ps_hat <- pmin(pmax(ps_hat, 0.01), 0.99)

  # 返回拟合对象与 PS。
  list(
    ps_fit = ps_fit,
    ps_hat = ps_hat
  )
}

# 定义函数：拟合 surv_fl_grf。
fit_surv_fl_grf_once <- function(X_train, Y, W, D, t0, ps_hat) {
  # 调用 surv_fl_grf；使用与主线一致的 censoring 拟合方式。
  survlearners::surv_fl_grf(
    X = X_train,
    Y = Y,
    W = W,
    D = D,
    t0 = t0,
    W.hat = ps_hat,
    cen.fit = "survival.forest",
    k.folds = 5
  )
}

# 定义函数：对任意数据集预测 CATE。
predict_cate_on_dataset <- function(fit_obj, X_new) {
  # 使用 survlearners 标准接口预测。
  as.numeric(predict(fit_obj, X_new))
}

# 定义函数：对单个数据集评分，并回填 CATE / PS / 权重。
score_one_dataset <- function(
    raw_df,
    dataset_name,
    treated_label,
    feature_vec,
    ps_feature_fixed,
    feature_blueprint,
    ps_blueprint,
    ps_fit,
    fit_obj,
    t0,
    cate_col_name
) {
  # 保留原始行号，便于结果回填。
  raw_df <- raw_df %>% mutate(.row_id = row_number())

  # 仅取训练集中也存在于当前数据集的 PS 变量。
  ps_vec_now <- intersect(ps_feature_fixed, names(raw_df))

  # 构造分析数据。
  dat_full <- build_analysis_df(
    raw_df = raw_df,
    feature_vec = feature_vec,
    ps_vec = ps_vec_now,
    treated_label = treated_label
  )

  # 做完整病例筛选；要求模型变量与当前可用 PS 变量都完整。
  dat_now <- dat_full %>%
    filter(complete.cases(.))

  # 若当前数据集没有可评分患者，则返回原始表并全部记 NA。
  if (nrow(dat_now) < 1) {
    out_df <- raw_df %>%
      mutate(
        patient_id = as.character(.row_id),
        final_model_complete_case = FALSE,
        W = as.numeric(as.character(arms) == treated_label),
        Y = as.numeric(OS),
        D = as.numeric(Event > 0),
        final_model_ps = NA_real_,
        weight_OW = NA_real_,
        weight_IPW = NA_real_
      ) %>%
      mutate(!!cate_col_name := NA_real_) %>%
      select(-.row_id)

    return(
      list(
        data_with_cate = out_df,
        cate_only = tibble(),
        meta = tibble(
          dataset = dataset_name,
          n_total = nrow(raw_df),
          n_scored = 0L,
          n_unscored = nrow(raw_df),
          rmst_horizon = t0,
          ps_variable_n_current_dataset = length(ps_vec_now)
        )
      )
    )
  }

  # 按训练集模板构建当前数据集的模型矩阵。
  X_now <- make_x_matrix_from_blueprint(dat_now, feature_blueprint)

  # 用训练集 PS 模板构建当前数据集 PS 设计矩阵，并预测个体化 PS。
  X_ps_now <- make_x_matrix_from_blueprint(dat_now, ps_blueprint)
  ps_hat_now <- as.numeric(predict(ps_fit, X_ps_now)$predictions)
  ps_hat_now <- pmin(pmax(ps_hat_now, 0.01), 0.99)

  # 用训练好的模型在当前数据集上预测 CATE。
  cate_hat <- predict_cate_on_dataset(fit_obj, X_now)

  # 基于当前数据集的处理指示与 PS，计算患者级 OW 与 IPW 权重。
  weight_tbl <- make_ps_weights(
    w = dat_now$W,
    ps = ps_hat_now,
    methods = c("OW", "IPW")
  )

  # 将 CATE / PS / 权重回填到原始行号。
  cate_tbl <- tibble(
    .row_id = dat_now$.row_id,
    patient_id = as.character(dat_now$.row_id),
    final_model_complete_case = TRUE,
    W = as.numeric(dat_now$W),
    Y = as.numeric(dat_now$Y),
    D = as.numeric(dat_now$D),
    final_model_ps = as.numeric(ps_hat_now),
    weight_OW = as.numeric(weight_tbl$OW),
    weight_IPW = as.numeric(weight_tbl$IPW)
  ) %>%
    mutate(!!cate_col_name := as.numeric(cate_hat))

  # 合并回原始表；未进入模型的患者保留 NA。
  out_df <- raw_df %>%
    mutate(
      patient_id = as.character(.row_id),
      final_model_complete_case = FALSE,
      W = as.numeric(as.character(arms) == treated_label),
      Y = as.numeric(OS),
      D = as.numeric(Event > 0)
    ) %>%
    left_join(
      cate_tbl %>%
        select(
          .row_id,
          final_model_complete_case,
          final_model_ps,
          weight_OW,
          weight_IPW,
          all_of(cate_col_name)
        ),
      by = ".row_id",
      suffix = c("", ".new")
    ) %>%
    mutate(
      final_model_complete_case = if_else(is.na(final_model_complete_case.new), final_model_complete_case, final_model_complete_case.new)
    ) %>%
    select(-final_model_complete_case.new, -.row_id)

  # 返回结果对象。
  list(
    data_with_cate = out_df,
    cate_only = cate_tbl %>%
      select(
        patient_id, W, Y, D,
        final_model_ps, weight_OW, weight_IPW,
        all_of(cate_col_name)
      ),
    meta = tibble(
      dataset = dataset_name,
      n_total = nrow(raw_df),
      n_scored = nrow(dat_now),
      n_unscored = nrow(raw_df) - nrow(dat_now),
      rmst_horizon = t0,
      ps_variable_n_current_dataset = length(ps_vec_now)
    )
  )
}

# ------------------------------------------
# 6) 工具函数：第8步风格的10分位 HR 与森林图
# ------------------------------------------

# 定义函数：按 CATE 从高到低做10等分。
assign_decile_by_rank <- function(x) {
  # 若全为缺失，则直接返回 NA。
  if (all(is.na(x))) return(rep(NA_integer_, length(x)))
  # 使用 rank(method='first') 打破并列，保证可稳定分组。
  rk <- rank(-x, ties.method = "first", na.last = "keep")
  # 依据秩切成10组，1=最高CATE组，10=最低CATE组。
  as.integer(dplyr::ntile(rk, 10))
}

# 定义函数：计算未调整 Cox HR。
calc_unadjusted_hr <- function(data, time_col, event_col, treat_col, robust_se = TRUE) {
  # 整理基础字段。
  df <- data %>%
    transmute(
      .time = as.numeric(.data[[time_col]]),
      .event = as.numeric(.data[[event_col]] > 0),
      .treat = as.numeric(.data[[treat_col]] > 0)
    ) %>%
    filter(complete.cases(.))

  # 若样本过少或仅单组，则返回 NA 结果。
  if (nrow(df) < 5 || dplyr::n_distinct(df$.treat) < 2) {
    return(
      tibble(
        method = "Unadjusted",
        n = nrow(df),
        n_treated = sum(df$.treat == 1, na.rm = TRUE),
        n_control = sum(df$.treat == 0, na.rm = TRUE),
        hr = NA_real_,
        lcl95 = NA_real_,
        ucl95 = NA_real_,
        p_value = NA_real_
      )
    )
  }

  # 拟合未调整 Cox 模型。
  fit <- tryCatch(
    coxph(
      Surv(.time, .event) ~ .treat,
      data = df,
      robust = robust_se
    ),
    error = function(e) NULL
  )

  # 若拟合失败，则返回 NA 结果。
  if (is.null(fit)) {
    return(
      tibble(
        method = "Unadjusted",
        n = nrow(df),
        n_treated = sum(df$.treat == 1, na.rm = TRUE),
        n_control = sum(df$.treat == 0, na.rm = TRUE),
        hr = NA_real_,
        lcl95 = NA_real_,
        ucl95 = NA_real_,
        p_value = NA_real_
      )
    )
  }

  # 提取模型摘要。
  sm <- summary(fit)
  # 提取 log(HR)。
  beta <- as.numeric(sm$coefficients[".treat", "coef"])
  # 优先提取 robust se。
  se_col <- if ("robust se" %in% colnames(sm$coefficients)) "robust se" else "se(coef)"
  se <- as.numeric(sm$coefficients[".treat", se_col])

  # 计算 HR、95%CI 与 p 值。
  hr <- exp(beta)
  lcl <- exp(beta - 1.96 * se)
  ucl <- exp(beta + 1.96 * se)
  z <- beta / se
  p <- 2 * pnorm(-abs(z))

  # 返回未调整结果。
  tibble(
    method = "Unadjusted",
    n = nrow(df),
    n_treated = sum(df$.treat == 1, na.rm = TRUE),
    n_control = sum(df$.treat == 0, na.rm = TRUE),
    hr = hr,
    lcl95 = lcl,
    ucl95 = ucl,
    p_value = p
  )
}

# 定义函数：对单个数据集按 decile 计算 OW/IPW/未调整 HR。
analyze_one_dataset <- function(
    data_input,
    dataset_name,
    cate_col,
    feature_vars,
    ps_vars,
    time_col = "Y",
    event_col = "D",
    treat_col = "W"
) {
  # 转为 tibble。
  dat <- tibble::as_tibble(data_input)

  # 针对当前数据集，仅保留实际存在的 PS 变量。
  ps_vars_use <- intersect(ps_vars, names(dat))
  # 若当前数据集可用 PS 变量太少，则无法做加权 HR。
  if (length(ps_vars_use) < 2) {
    stop(paste0(dataset_name, " 可用的PS变量不足2个，无法计算 OW/IPW。"))
  }

  # 检查关键字段是否存在。
  required_cols <- c(time_col, event_col, treat_col, cate_col, feature_vars, ps_vars_use)
  missing_cols <- setdiff(required_cols, names(dat))
  if (length(missing_cols) > 0) {
    stop(paste0(dataset_name, " 缺少必要列：", paste(missing_cols, collapse = ", ")))
  }

  # 保留完整 CATE 样本，并计算 decile。
  dat_decile <- dat %>%
    filter(!is.na(.data[[cate_col]])) %>%
    mutate(
      cate_decile = assign_decile_by_rank(.data[[cate_col]])
    )

  # 初始化结果容器。
  hr_result <- tibble()

  # 按 decile 循环计算。
  for (g in 1:10) {
    # 当前 decile 子集。
    sub_g <- dat_decile %>%
      filter(cate_decile == g)

    # 先计算 OW/IPW。
    weighted_res <- tryCatch(
      calc_weighted_hr_ps(
        data = sub_g,
        time_col = time_col,
        event_col = event_col,
        treat_col = treat_col,
        covariate_cols = feature_vars,
        ps_covariate_cols = ps_vars_use,
        methods = c("OW", "IPW"),
        robust_se = TRUE
      ),
      error = function(e) e
    )

    # 若加权模型失败，则写入占位结果。
    if (inherits(weighted_res, "error")) {
      weighted_tbl <- tibble(
        method = c("OW", "IPW"),
        n = nrow(sub_g),
        n_treated = sum(sub_g[[treat_col]] == 1, na.rm = TRUE),
        n_control = sum(sub_g[[treat_col]] == 0, na.rm = TRUE),
        hr = NA_real_,
        lcl95 = NA_real_,
        ucl95 = NA_real_,
        p_value = NA_real_
      )
    } else {
      # 取 OW / IPW 两种方法结果。
      weighted_tbl <- weighted_res$hr_table %>%
        filter(method %in% c("OW", "IPW"))
    }

    # 计算未调整 HR。
    unadjusted_tbl <- calc_unadjusted_hr(
      data = sub_g,
      time_col = time_col,
      event_col = event_col,
      treat_col = treat_col,
      robust_se = TRUE
    )

    # 合并当前 decile 三种方法结果，并补充元信息。
    one_decile_tbl <- bind_rows(weighted_tbl, unadjusted_tbl) %>%
      mutate(
        dataset = dataset_name,
        cate_decile = g,
        cate_decile_label = paste0("D", g),
        cate_rank_direction = "1=highest_CATE,10=lowest_CATE",
        neg_log10_p = if_else(is.na(p_value), NA_real_, -log10(pmax(p_value, 1e-300)))
      ) %>%
      select(
        dataset, cate_decile, cate_decile_label, cate_rank_direction,
        method, n, n_treated, n_control,
        hr, lcl95, ucl95, p_value, neg_log10_p
      )

    # 追加到总结果容器。
    hr_result <- bind_rows(hr_result, one_decile_tbl)
  }

  # 返回当前数据集结果。
  hr_result
}

# 定义函数：为单个数据集绘制森林图。
plot_one_dataset_forest <- function(hr_df, dataset_name) {
  # 保留未调整结果与加权结果，便于同图比较。
  plot_df <- hr_df %>%
    filter(method %in% c("Unadjusted", "OW", "IPW")) %>%
    filter(is.finite(hr), is.finite(lcl95), is.finite(ucl95), hr > 0, lcl95 > 0, ucl95 > 0) %>%
    mutate(
      # 将 decile 反向显示，使 D1（最高CATE）出现在最上方。
      cate_decile_label = factor(cate_decile_label, levels = paste0("D", 10:1)),
      # 图中使用 IPTW 作为 IPW 的展示名称，同时保留未调整原始结果面板。
      method_label = dplyr::recode(method, "Unadjusted" = "Unadjusted", "OW" = "OW", "IPW" = "IPTW"),
      method_label = factor(method_label, levels = c("Unadjusted", "OW", "IPTW")),
      # 生成右侧显示文字。
      hr_label = sprintf("%.3f (%.3f-%.3f); P=%s", hr, lcl95, ucl95, format.pval(p_value, digits = 3, eps = 1e-300))
    )

  # 若无可绘图结果，则返回空图对象。
  if (nrow(plot_df) == 0) {
    return(
      ggplot() +
        annotate("text", x = 1, y = 1, label = paste0(dataset_name, ": no valid HR results")) +
        theme_void()
    )
  }

  # 计算文本右侧放置位置，并为 log 坐标预留展示空间。
  text_x <- max(plot_df$ucl95, na.rm = TRUE) * 2.2
  x_upper <- max(text_x * 1.35, 10)
  x_breaks <- c(0.1, 1, 10, 100)
  x_breaks <- x_breaks[x_breaks >= min(plot_df$lcl95, na.rm = TRUE) / 1.2 & x_breaks <= x_upper]
  if (!1 %in% x_breaks) x_breaks <- sort(unique(c(x_breaks, 1)))

  # 绘制森林图，X轴采用对数尺度，并在右侧添加文本标签。
  ggplot(plot_df, aes(x = hr, y = cate_decile_label, color = method_label)) +
    geom_vline(xintercept = 1, linetype = 2, color = "grey45", linewidth = 0.5) +
    geom_segment(aes(x = lcl95, xend = ucl95, y = cate_decile_label, yend = cate_decile_label), linewidth = 0.65) +
    geom_point(size = 2.4) +
    geom_text(
      aes(x = text_x, y = cate_decile_label, label = hr_label),
      color = "grey20",
      hjust = 0,
      size = 3.1,
      inherit.aes = FALSE,
      data = plot_df
    ) +
    scale_x_log10(
      breaks = x_breaks,
      labels = scales::label_number(accuracy = 0.1)
    ) +
    scale_color_manual(
      values = c("Unadjusted" = "#4D4D4D", "OW" = "#2C7FB8", "IPTW" = "#31A354")
    ) +
    facet_wrap(~ method_label, ncol = 3) +
    labs(
      title = paste0(dataset_name, " Decile-wise HR Forest Plot"),
      subtitle = "Decile 1 = Highest CATE",
      x = "Hazard Ratio (log scale)",
      y = "CATE decile"
    ) +
    coord_cartesian(xlim = c(min(plot_df$lcl95, na.rm = TRUE) / 1.25, x_upper), clip = "off") +
    theme_bw() +
    theme(
      panel.background = element_rect(fill = "#F4F4F4", color = NA),
      strip.background = element_rect(fill = "#D9D9D9", color = "grey45"),
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#DDDDDD"),
      axis.title.y = element_blank(),
      legend.position = "none",
      plot.margin = margin(10, 100, 10, 10)
    )
}

# ------------------------------------------
# 7) 读取训练集、PS 变量、前 24 个变量并拟合最终模型
# ------------------------------------------

# 定位 PS 文件。
ps_file <- locate_ps_file()

# 读取 set_id=1 的 PS 变量。
ps_feature_fixed <- read_ps_feature_final_local(ps_file = ps_file, set_id = 1)

# 若 PS 变量为空则停止。
if (length(ps_feature_fixed) < 2) {
  stop("PS 变量读取为空或不足，请检查文件：", ps_file)
}

# 直接从变量重要性排序表中读取前 24 个变量。
feature_info <- get_top_feature_vec_by_qini(
  report_root = report_root,
  n_features = top_n_features
)
feature_vec <- feature_info$feature_vec
rank_file_used <- feature_info$rank_file_used

# 记录为字符串形式，便于元数据输出。
feature_str <- paste(feature_vec, collapse = "|")

# 固定三个数据集路径。
dataset_tbl <- tibble(
  dataset = c("train", "validation", "prevalidation"),
  file = c(
    file.path(data_dir, "01_ISMIO2501_train_tidy.xlsx"),
    file.path(data_dir, "02_ISMIO2501_validation_tidy.xlsx"),
    file.path(data_dir, "03_ISMIO2501prevalidation_tidy.xlsx")
  )
)

# 若训练集文件不存在则停止。
if (!file.exists(dataset_tbl$file[dataset_tbl$dataset == "train"])) {
  stop("未找到训练集文件：", dataset_tbl$file[dataset_tbl$dataset == "train"])
}

# 读取训练集原始数据。
train_raw <- read.xlsx(dataset_tbl$file[dataset_tbl$dataset == "train"], sheet = "tidy")

# 检查训练集必要列。
train_need_cols <- c("arms", "Event", "OS", feature_vec)
if (!all(train_need_cols %in% names(train_raw))) {
  miss_cols <- setdiff(train_need_cols, names(train_raw))
  stop("训练集缺少必要列：", paste(miss_cols, collapse = ", "))
}

# 自动识别 treated 标签；优先使用 TACE_TA。
treated_label <- if ("TACE_TA" %in% unique(as.character(train_raw$arms))) {
  "TACE_TA"
} else {
  sort(unique(as.character(train_raw$arms)))[2]
}

# 训练集中实际可用的 PS 变量。
ps_vec_train <- intersect(ps_feature_fixed, names(train_raw))

# 若训练集中可用 PS 变量太少则停止。
if (length(ps_vec_train) < 2) {
  stop("训练集中可用 PS 变量不足 2 个。")
}

# 构造训练集分析数据。
train_dat_full <- build_analysis_df(
  raw_df = train_raw,
  feature_vec = feature_vec,
  ps_vec = ps_vec_train,
  treated_label = treated_label
)

# 仅在训练集上做完整病例筛选后用于建模。
train_dat <- train_dat_full %>%
  filter(complete.cases(.))

# 若训练集样本不足或处理组单一则停止。
if (nrow(train_dat) < 30 || n_distinct(train_dat$W) < 2) {
  stop("训练集完整病例数不足或处理组单一，无法拟合模型。")
}

# 用训练集建立模型特征矩阵模板。
feature_blueprint <- build_design_blueprint(train_dat, feature_vec)

# 用训练集建立 PS 特征矩阵模板。
ps_blueprint <- build_design_blueprint(train_dat, ps_vec_train)

# 提取训练集模型矩阵。
X_train <- feature_blueprint$X_train

# 在训练集上估计 PS，并保留拟合对象。
ps_res <- estimate_ps_grf(train_dat, ps_blueprint)
ps_fit <- ps_res$ps_fit
ps_hat_train <- ps_res$ps_hat

# 用训练集拟合一次 surv_fl_grf。
fit_obj <- fit_surv_fl_grf_once(
  X_train = X_train,
  Y = train_dat$Y,
  W = train_dat$W,
  D = train_dat$D,
  t0 = t0,
  ps_hat = ps_hat_train
)

# 当前第9步患者级 CATE 列名。
cate_col_name <- "final_model_cate_surv_fl_grf_top24_rmst24"

# ------------------------------------------
# 8) 输出变量清单与运行元信息
# ------------------------------------------

# 保存最终使用的变量与 PS 变量。
write_csv(tibble(variable = feature_vec), file.path(out_dir, "01_final_top24_features.csv"))
write_csv(tibble(ps_variable = ps_feature_fixed), file.path(out_dir, "02_ps_variables_from_file.csv"))
write_csv(tibble(ps_variable = ps_vec_train), file.path(out_dir, "03_ps_variables_available_in_train.csv"))
write_csv(
  tibble(
    rank_file_used = rank_file_used,
    ps_file = ps_file,
    final_algorithm = algo_name,
    top_n_feature = top_n_features,
    rmst_horizon = t0,
    treated_label = treated_label,
    random_seed = 20260513,
    fit_strategy = "fit_once_on_train_then_predict_on_each_dataset",
    n_model_features = length(feature_vec),
    n_ps_variables_train = length(ps_vec_train)
  ),
  file.path(out_dir, "04_model_metadata.csv")
)

# ------------------------------------------
# 9) 将训练好的模型应用到三个数据集，并导出类似第7步的结果
# ------------------------------------------

# 初始化运行汇总容器。
run_meta <- tibble()

# 初始化结果对象列表。
scored_list <- list()

# 循环处理三个数据集。
for (i in seq_len(nrow(dataset_tbl))) {
  # 当前数据集名称。
  ds_name <- dataset_tbl$dataset[i]

  # 当前文件路径。
  ds_file <- dataset_tbl$file[i]

  # 若文件不存在，则直接记录并继续。
  if (!file.exists(ds_file)) {
    run_meta <- bind_rows(
      run_meta,
      tibble(
        dataset = ds_name,
        n_total = NA_integer_,
        n_scored = NA_integer_,
        n_unscored = NA_integer_,
        rmst_horizon = t0,
        ps_variable_n_current_dataset = NA_integer_,
        final_algorithm = algo_name,
        top_n_feature = top_n_features,
        treated_label = treated_label,
        random_seed = 20260513,
        status = "skip_file_not_found"
      )
    )
    next
  }

  # 读取原始数据。
  raw_df <- read.xlsx(ds_file, sheet = "tidy")

  # 评分并回填结果。
  scored_obj <- score_one_dataset(
    raw_df = raw_df,
    dataset_name = ds_name,
    treated_label = treated_label,
    feature_vec = feature_vec,
    ps_feature_fixed = ps_feature_fixed,
    feature_blueprint = feature_blueprint,
    ps_blueprint = ps_blueprint,
    ps_fit = ps_fit,
    fit_obj = fit_obj,
    t0 = t0,
    cate_col_name = cate_col_name
  )

  # 保存到列表。
  scored_list[[ds_name]] <- scored_obj

  # 记录运行元信息。
  run_meta <- bind_rows(
    run_meta,
    scored_obj$meta %>%
      mutate(
        final_algorithm = algo_name,
        top_n_feature = top_n_features,
        treated_label = treated_label,
        random_seed = 20260513,
        status = "ok"
      )
  )
}

# 写出总汇总表。
write_csv(run_meta, file.path(out_dir, "05_run_meta.csv"))

# 写出训练集：xlsx + csv。
if (!is.null(scored_list$train)) {
  write.xlsx(
    x = list(
      tidy = scored_list$train$data_with_cate,
      metadata = readr::read_csv(file.path(out_dir, "04_model_metadata.csv"), show_col_types = FALSE)
    ),
    file = file.path(out_dir, "06_train_with_cate.xlsx"),
    overwrite = TRUE
  )
  write_csv(scored_list$train$data_with_cate, file.path(out_dir, "06_train_with_cate.csv"))
  write_csv(scored_list$train$cate_only, file.path(out_dir, "09_train_cate_only.csv"))
}

# 写出 validation 集：xlsx + csv。
if (!is.null(scored_list$validation)) {
  write.xlsx(
    x = list(
      tidy = scored_list$validation$data_with_cate,
      metadata = readr::read_csv(file.path(out_dir, "04_model_metadata.csv"), show_col_types = FALSE)
    ),
    file = file.path(out_dir, "07_validation_with_cate.xlsx"),
    overwrite = TRUE
  )
  write_csv(scored_list$validation$data_with_cate, file.path(out_dir, "07_validation_with_cate.csv"))
  write_csv(scored_list$validation$cate_only, file.path(out_dir, "10_validation_cate_only.csv"))
}

# 写出 prevalidation 集：xlsx + csv。
if (!is.null(scored_list$prevalidation)) {
  write.xlsx(
    x = list(
      tidy = scored_list$prevalidation$data_with_cate,
      metadata = readr::read_csv(file.path(out_dir, "04_model_metadata.csv"), show_col_types = FALSE)
    ),
    file = file.path(out_dir, "08_prevalidation_with_cate.xlsx"),
    overwrite = TRUE
  )
  write_csv(scored_list$prevalidation$data_with_cate, file.path(out_dir, "08_prevalidation_with_cate.csv"))
  write_csv(scored_list$prevalidation$cate_only, file.path(out_dir, "11_prevalidation_cate_only.csv"))
}

# ------------------------------------------
# 10) 基于第9步新模型结果继续做类似第8步的10分位分析
# ------------------------------------------

# 训练集 decile-HR 结果。
train_hr <- analyze_one_dataset(
  data_input = scored_list$train$data_with_cate,
  dataset_name = "train",
  cate_col = cate_col_name,
  feature_vars = feature_vec,
  ps_vars = ps_feature_fixed
)

# 验证集 decile-HR 结果。
valid_hr <- analyze_one_dataset(
  data_input = scored_list$validation$data_with_cate,
  dataset_name = "validation",
  cate_col = cate_col_name,
  feature_vars = feature_vec,
  ps_vars = ps_feature_fixed
)

# 前验证集 decile-HR 结果。
prevalid_hr <- analyze_one_dataset(
  data_input = scored_list$prevalidation$data_with_cate,
  dataset_name = "prevalidation",
  cate_col = cate_col_name,
  feature_vars = feature_vec,
  ps_vars = ps_feature_fixed
)

# 生成“合并外部验证集”。
external_merged_raw <- bind_rows(
  scored_list$validation$data_with_cate,
  scored_list$prevalidation$data_with_cate
)

# 合并外部验证集 decile-HR 结果。
external_merged_hr <- analyze_one_dataset(
  data_input = external_merged_raw,
  dataset_name = "external_validation_merged",
  cate_col = cate_col_name,
  feature_vars = feature_vec,
  ps_vars = ps_feature_fixed
)

# 合并四套结果。
all_hr <- bind_rows(train_hr, valid_hr, prevalid_hr, external_merged_hr)

# 写出 decile 结果表。
write_csv(train_hr, file.path(out_dir, "12_train_cate_decile_hr.csv"))
write_csv(valid_hr, file.path(out_dir, "13_validation_cate_decile_hr.csv"))
write_csv(prevalid_hr, file.path(out_dir, "14_prevalidation_cate_decile_hr.csv"))
write_csv(external_merged_hr, file.path(out_dir, "15_external_validation_merged_cate_decile_hr.csv"))
write_csv(all_hr, file.path(out_dir, "16_all_datasets_cate_decile_hr.csv"))

# 绘制四套森林图。
plot_train <- plot_one_dataset_forest(train_hr, "train")
plot_valid <- plot_one_dataset_forest(valid_hr, "validation")
plot_prevalid <- plot_one_dataset_forest(prevalid_hr, "prevalidation")
plot_external_merged <- plot_one_dataset_forest(external_merged_hr, "external_validation_merged")

# 保存训练集森林图。
ggsave(
  filename = file.path(out_dir, "17_train_cate_decile_hr_forest.png"),
  plot = plot_train,
  width = 13.5,
  height = 8.8,
  dpi = 300
)
ggsave(
  filename = file.path(out_dir, "17_train_cate_decile_hr_forest.pdf"),
  plot = plot_train,
  width = 13.5,
  height = 8.8
)

# 保存 validation 森林图。
ggsave(
  filename = file.path(out_dir, "18_validation_cate_decile_hr_forest.png"),
  plot = plot_valid,
  width = 13.5,
  height = 8.8,
  dpi = 300
)
ggsave(
  filename = file.path(out_dir, "18_validation_cate_decile_hr_forest.pdf"),
  plot = plot_valid,
  width = 13.5,
  height = 8.8
)

# 保存 prevalidation 森林图。
ggsave(
  filename = file.path(out_dir, "19_prevalidation_cate_decile_hr_forest.png"),
  plot = plot_prevalid,
  width = 13.5,
  height = 8.8,
  dpi = 300
)
ggsave(
  filename = file.path(out_dir, "19_prevalidation_cate_decile_hr_forest.pdf"),
  plot = plot_prevalid,
  width = 13.5,
  height = 8.8
)

# 保存合并外部验证集森林图。
ggsave(
  filename = file.path(out_dir, "20_external_validation_merged_cate_decile_hr_forest.png"),
  plot = plot_external_merged,
  width = 13.5,
  height = 8.8,
  dpi = 300
)
ggsave(
  filename = file.path(out_dir, "20_external_validation_merged_cate_decile_hr_forest.pdf"),
  plot = plot_external_merged,
  width = 13.5,
  height = 8.8
)

# 控制台输出完成信息。
cat("第9步 surv_fl_grf 前24变量训练、外部验证、10分位HR与森林图全部完成。\n")
cat("最终模型算法：", algo_name, "\n")
cat("建模变量（前24）：", paste(feature_vec, collapse = ", "), "\n")
cat("PS文件：", ps_file, "\n")
cat("变量排序文件：", rank_file_used, "\n")
cat("输出目录：", out_dir, "\n")
