#!/usr/bin/env Rscript

# ============================================================
# 基于第7步最终模型，准备 Shiny 应用所需工件
# ------------------------------------------------------------
# 说明：
# 1. 治疗推荐仍基于最终 surv_fl_grf top12 模型预测的患者级 CATE
# 2. 生存曲线与 RMST 展示，额外使用同一批 top12 变量训练的
#    两个 treatment-specific survival forest 来近似展示
# 3. 所有工件写入 output/shiny_final_model_app/
# ============================================================

suppressPackageStartupMessages({
  if (!requireNamespace("pacman", quietly = TRUE)) {
    stop("缺少 pacman 包，请先安装 pacman。")
  }
  pacman::p_load(
    tidyverse,
    here,
    readr,
    openxlsx,
    grf,
    survlearners
  )
})

set.seed(20260521)

out_dir <- here::here("output", "shiny_final_model_app")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 1. 固定参数
# -----------------------------

data_dir <- here::here("data")
report_root <- here::here("output", "7-final_surv_fl_grf_top12_predict")
top_n_features <- 12L
t0 <- 24
surv_grid <- seq(0, t0, by = 0.25)

# -----------------------------
# 2. 复用的工具函数
# -----------------------------

locate_ps_file <- function() {
  ps_file_default <- file.path(data_dir, "PS变量最终确定.txt")
  ps_file_env <- Sys.getenv("PS_FEATURE_FILE", unset = "")

  ps_file <- if (nzchar(ps_file_env)) ps_file_env else ps_file_default

  if (!file.exists(ps_file)) {
    stop("未找到 PS 变量文件：", ps_file)
  }
  ps_file
}

extract_ps_vector_from_block <- function(block) {
  vars <- character()

  # 先尝试解析 c("x1","x2") 这种格式
  joined <- paste(block, collapse = "\n")
  c_match <- stringr::str_match(joined, "c\\((.*?)\\)")
  if (!is.na(c_match[1, 2])) {
    vars <- stringr::str_extract_all(c_match[1, 2], "\"[^\"]+\"|'[^']+'")[[1]]
    vars <- gsub("^\"|\"$|^'|'$", "", vars)
    vars <- trimws(vars)
    vars <- vars[nzchar(vars)]
  }

  # 若未解析到，则按逐行变量名解析
  if (length(vars) == 0) {
    plain <- trimws(block)
    plain <- plain[nzchar(plain)]
    plain <- plain[!grepl("^PS评分计算变量\\s*\\d+\\s*[:：]?$", plain)]
    plain <- sub("^\\d+\\s*[\\.|、\\)]\\s*", "", plain)
    plain <- trimws(plain)
    plain <- plain[nzchar(plain)]
    vars <- unique(plain)
  }

  vars
}

read_ps_feature_final_local <- function(ps_file, set_id = 1L) {
  lines <- readLines(ps_file, warn = FALSE, encoding = "UTF-8")
  idx <- grep("^\\s*PS评分计算变量\\s*\\d+\\s*[:：]?", lines)

  if (length(idx) > 0) {
    idx_end <- c(idx[-1] - 1L, length(lines))
    block_id <- which(seq_along(idx) == set_id)
    if (length(block_id) == 0) {
      stop("PS 文件中不存在 set_id = ", set_id)
    }
    block <- lines[(idx[block_id] + 1L):idx_end[block_id]]
    vars <- extract_ps_vector_from_block(block)
  } else {
    vars <- extract_ps_vector_from_block(lines)
  }

  vars <- unique(vars)
  if (length(vars) < 2) {
    stop("PS 变量读取为空或不足，请检查文件：", ps_file)
  }
  vars
}

get_top_feature_vec_by_qini <- function(report_root, n_features = 12L) {
  rank_file <- file.path(report_root, "01_final_top12_features.csv")
  if (!file.exists(rank_file)) {
    rank_file <- here::here("output", "2-rmst24_qini_rank_by_variable_train", "06_rmst24_qini_by_variable_ranked_train.csv")
  }
  if (!file.exists(rank_file)) {
    stop("未找到变量文件：", rank_file)
  }

  tab <- readr::read_csv(rank_file, show_col_types = FALSE)
  if ("variable" %in% names(tab) && nrow(tab) >= n_features) {
    feature_vec <- tab$variable[seq_len(n_features)]
  } else {
    top_tab <- tab %>%
      dplyr::filter(status == "ok", !is.na(variable), variable != "") %>%
      dplyr::slice_head(n = n_features)
    feature_vec <- top_tab$variable
  }

  if (length(feature_vec) < n_features) {
    stop("变量清单不足前 ", n_features, " 个。")
  }

  list(feature_vec = feature_vec, rank_file_used = rank_file)
}

build_design_blueprint <- function(dat, vars) {
  x_raw <- dat %>%
    dplyr::select(dplyr::all_of(vars)) %>%
    dplyr::mutate(dplyr::across(where(~ is.character(.x) || is.logical(.x)), as.factor))

  factor_levels <- purrr::map(
    x_raw %>% dplyr::select(where(is.factor)),
    levels
  )

  mm_formula <- stats::terms(~ . - 1, data = x_raw)
  X_train <- stats::model.matrix(mm_formula, data = x_raw)

  list(
    vars = vars,
    formula = mm_formula,
    factor_levels = factor_levels,
    colnames = colnames(X_train),
    X_train = X_train
  )
}

make_x_matrix_from_blueprint <- function(dat, blueprint) {
  x_raw <- dat %>%
    dplyr::select(dplyr::all_of(blueprint$vars))

  for (nm in names(blueprint$factor_levels)) {
    x_raw[[nm]] <- factor(as.character(x_raw[[nm]]), levels = blueprint$factor_levels[[nm]])
  }

  x_raw <- x_raw %>%
    dplyr::mutate(dplyr::across(where(~ is.character(.x) || is.logical(.x)), as.factor))

  X_now <- stats::model.matrix(blueprint$formula, data = x_raw)

  X_aligned <- matrix(
    0,
    nrow = nrow(X_now),
    ncol = length(blueprint$colnames),
    dimnames = list(NULL, blueprint$colnames)
  )

  common_cols <- intersect(colnames(X_now), blueprint$colnames)
  X_aligned[, common_cols] <- X_now[, common_cols, drop = FALSE]
  X_aligned
}

build_analysis_df <- function(raw_df, feature_vec, ps_vec, treated_label) {
  raw_df %>%
    dplyr::mutate(.row_id = dplyr::row_number()) %>%
    dplyr::transmute(
      .row_id = .row_id,
      W = as.numeric(as.character(arms) == treated_label),
      Y = as.numeric(OS),
      D = as.numeric(Event > 0),
      dplyr::across(dplyr::all_of(unique(c(feature_vec, ps_vec))))
    )
}

estimate_ps_grf <- function(dat, ps_blueprint) {
  X_ps <- make_x_matrix_from_blueprint(dat, ps_blueprint)

  ps_fit <- grf::regression_forest(
    X = X_ps,
    Y = dat$W,
    num.trees = 300,
    honesty = TRUE
  )

  ps_hat <- as.numeric(predict(ps_fit)$predictions)
  ps_hat <- pmin(pmax(ps_hat, 0.01), 0.99)

  list(ps_fit = ps_fit, ps_hat = ps_hat)
}

fit_surv_fl_grf_once <- function(X_train, Y, W, D, t0, ps_hat) {
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

predict_cate_on_dataset <- function(fit_obj, X_new) {
  as.numeric(predict(fit_obj, X_new))
}

get_mode_value <- function(x) {
  tb <- table(x, useNA = "no")
  names(tb)[which.max(tb)][1]
}

# -----------------------------
# 3. 读取训练集与元信息
# -----------------------------

ps_file <- locate_ps_file()
ps_available_file <- file.path(report_root, "03_ps_variables_available_in_train.csv")

ps_feature_fixed <- if (file.exists(ps_available_file)) {
  readr::read_csv(ps_available_file, show_col_types = FALSE)$ps_variable
} else {
  read_ps_feature_final_local(ps_file = ps_file, set_id = 1)
}

feature_info <- get_top_feature_vec_by_qini(report_root = report_root, n_features = top_n_features)
feature_vec <- feature_info$feature_vec

train_file <- file.path(data_dir, "01_ISMIO2501_train_tidy.xlsx")
train_raw <- openxlsx::read.xlsx(train_file, sheet = "tidy")

treated_label <- if ("TACE_TA" %in% unique(as.character(train_raw$arms))) {
  "TACE_TA"
} else {
  sort(unique(as.character(train_raw$arms)))[2]
}
control_label <- setdiff(sort(unique(as.character(train_raw$arms))), treated_label)[1]

ps_vec_train <- intersect(ps_feature_fixed, names(train_raw))
if (length(ps_vec_train) < 2) {
  stop("训练集中可用 PS 变量不足 2 个。")
}

train_dat_full <- build_analysis_df(
  raw_df = train_raw,
  feature_vec = feature_vec,
  ps_vec = ps_vec_train,
  treated_label = treated_label
)

train_dat <- train_dat_full %>%
  dplyr::filter(stats::complete.cases(.))

if (nrow(train_dat) < 30 || dplyr::n_distinct(train_dat$W) < 2) {
  stop("训练集完整病例数不足或处理组单一，无法拟合模型。")
}

feature_blueprint <- build_design_blueprint(train_dat, feature_vec)
ps_blueprint <- build_design_blueprint(train_dat, ps_vec_train)
X_train <- feature_blueprint$X_train

ps_res <- estimate_ps_grf(train_dat, ps_blueprint)
ps_fit <- ps_res$ps_fit
ps_hat_train <- ps_res$ps_hat

fit_obj <- fit_surv_fl_grf_once(
  X_train = X_train,
  Y = train_dat$Y,
  W = train_dat$W,
  D = train_dat$D,
  t0 = t0,
  ps_hat = ps_hat_train
)

# -----------------------------
# 4. 构建输入控件 schema
# -----------------------------

input_schema <- purrr::map(
  feature_vec,
  function(v) {
    x <- train_dat[[v]]
    if (is.numeric(x)) {
      list(
        name = v,
        input_type = "numeric",
        min = min(x, na.rm = TRUE),
        max = max(x, na.rm = TRUE),
        default = stats::median(x, na.rm = TRUE),
        step = signif((max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) / 100, 3)
      )
    } else {
      x_fac <- if (is.factor(x)) x else factor(as.character(x))
      list(
        name = v,
        input_type = "select",
        levels = levels(x_fac),
        default = get_mode_value(x_fac)
      )
    }
  }
)
names(input_schema) <- feature_vec

# -----------------------------
# 5. 仅保存可序列化工件与元数据
# -----------------------------

treated_idx <- which(train_dat$W == 1)
control_idx <- which(train_dat$W == 0)

artifact <- list(
  created_at = as.character(Sys.time()),
  random_seed = 20260521L,
  train_file = normalizePath(train_file, winslash = "/"),
  ps_file = normalizePath(ps_file, winslash = "/"),
  rank_file_used = normalizePath(feature_info$rank_file_used, winslash = "/"),
  t0 = t0,
  surv_grid = surv_grid,
  treated_label = treated_label,
  control_label = control_label,
  feature_vec = feature_vec,
  ps_vec_train = ps_vec_train,
  input_schema = input_schema,
  train_summary = tibble::tibble(
    metric = c("n_train_complete_case", "n_treated", "n_control", "rmst_horizon"),
    value = c(nrow(train_dat), length(treated_idx), length(control_idx), t0)
  )
)

saveRDS(artifact, file = file.path(out_dir, "final_model_shiny_artifact.rds"))
write_csv(tibble::tibble(variable = feature_vec), file.path(out_dir, "01_top12_features.csv"))
write_csv(tibble::tibble(ps_variable = ps_vec_train), file.path(out_dir, "02_ps_variables_train.csv"))
write_csv(
  tibble::tibble(
    item = c(
      "created_at", "train_file", "ps_file", "rank_file_used",
      "treated_label", "control_label", "rmst_horizon",
      "n_train_complete_case", "n_treated", "n_control"
    ),
    value = c(
      artifact$created_at,
      artifact$train_file,
      artifact$ps_file,
      artifact$rank_file_used,
      artifact$treated_label,
      artifact$control_label,
      artifact$t0,
      nrow(train_dat),
      length(treated_idx),
      length(control_idx)
    )
  ),
  file.path(out_dir, "03_artifact_metadata.csv")
)

capture.output(sessionInfo(), file = file.path(out_dir, "04_session_info.txt"))

message("Shiny 应用模型工件已生成：", file.path(out_dir, "final_model_shiny_artifact.rds"))
