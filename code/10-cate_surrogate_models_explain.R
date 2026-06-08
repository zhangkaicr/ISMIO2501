#!/usr/bin/env Rscript

# ============================================================
# 基于最终患者级 CATE 的 surrogate 多模型解释脚本（分数据集版）
# ------------------------------------------------------------
# 核心设计：
# 1. 读取第7步输出的 train / validation / prevalidation 患者级 CATE
# 2. 将 validation + prevalidation 合并为 external_merged
# 3. surrogate 模型一律只在训练集上拟合
# 4. 解释阶段分别在：
#    - 训练集 train
#    - 外部验证合并集 external_merged
#    上独立开展
# 5. 每个模型、每个数据集分别输出：
#    - DALEX 变量重要性表与图
#    - kernelshap + shapviz 的 SHAP beeswarm / bar 图
#    - 重要变量交互解释：
#      - SHAP dependence 图（双向）
#      - 基于 surrogate 预测面的双变量交互热图
# 6. 全部结果写入 output/explain/<model_name>/<split_name>/
# ============================================================

suppressPackageStartupMessages({
  if (!requireNamespace("pacman", quietly = TRUE)) {
    stop("缺少 pacman 包，请先安装 pacman 后再运行本脚本。")
  }
  pacman::p_load(
    tidyverse,
    here,
    DALEX,
    ingredients,
    kernelshap,
    shapviz,
    glmnet,
    xgboost,
    ranger,
    e1071,
    ggplot2
  )
})

# -----------------------------
# 一、全局参数
# -----------------------------

set.seed(20260521)

input_dir <- here::here("output", "7-final_surv_fl_grf_top12_predict")
output_root <- here::here("output", "explain")
cate_col <- "final_model_cate_surv_fl_grf_top12_rmst24"
top_n_feature <- 12L

# DALEX 变量重要性参数
vip_sample_n <- 1000L
vip_B <- 30L

# SHAP 参数
shap_explain_n <- 180L
shap_bg_n <- 120L
shap_seed <- 20260521L

# 交互分析参数
interaction_top_k <- 2L
interaction_grid_n <- 25L

dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 二、辅助函数
# -----------------------------

save_plot_dual <- function(plot_obj, png_path, pdf_path, width = 10, height = 7, dpi = 320) {
  ggplot2::ggsave(filename = png_path, plot = plot_obj, width = width, height = height, dpi = dpi)
  ggplot2::ggsave(filename = pdf_path, plot = plot_obj, width = width, height = height)
}

standardize_feature_types <- function(df) {
  df %>%
    dplyr::mutate(
      dplyr::across(where(is.character), as.factor),
      dplyr::across(where(is.logical), as.factor)
    )
}

write_csv_utf8 <- function(df, path) {
  if (file.exists(path)) {
    unlink(path, force = TRUE)
  }
  readr::write_csv(df, file = path)
}

sample_row_ids <- function(n_total, size_need, seed_use) {
  set.seed(seed_use)
  sample(seq_len(n_total), size = min(size_need, n_total), replace = FALSE)
}

build_design_blueprint <- function(raw_feature_df) {
  raw_feature_df <- standardize_feature_types(raw_feature_df)

  factor_levels <- list()
  factor_cols <- names(raw_feature_df)[vapply(raw_feature_df, is.factor, logical(1))]
  if (length(factor_cols) > 0) {
    for (nm in factor_cols) {
      factor_levels[[nm]] <- levels(raw_feature_df[[nm]])
    }
  }

  mm_formula <- stats::terms(~ . - 1, data = raw_feature_df)
  mm_train <- stats::model.matrix(mm_formula, data = raw_feature_df)

  list(
    feature_names = names(raw_feature_df),
    factor_levels = factor_levels,
    mm_formula = mm_formula,
    matrix_colnames = colnames(mm_train)
  )
}

prepare_raw_feature_df <- function(df, blueprint) {
  df <- df %>%
    dplyr::select(dplyr::all_of(blueprint$feature_names))

  df <- standardize_feature_types(df)

  if (length(blueprint$factor_levels) > 0) {
    for (nm in names(blueprint$factor_levels)) {
      df[[nm]] <- factor(as.character(df[[nm]]), levels = blueprint$factor_levels[[nm]])
    }
  }

  df
}

prepare_matrix_from_blueprint <- function(df, blueprint) {
  raw_df <- prepare_raw_feature_df(df, blueprint)
  mm_now <- stats::model.matrix(blueprint$mm_formula, data = raw_df)

  aligned <- matrix(
    0,
    nrow = nrow(mm_now),
    ncol = length(blueprint$matrix_colnames),
    dimnames = list(NULL, blueprint$matrix_colnames)
  )

  common_cols <- intersect(colnames(mm_now), blueprint$matrix_colnames)
  if (length(common_cols) > 0) {
    aligned[, common_cols] <- mm_now[, common_cols, drop = FALSE]
  }

  aligned
}

calc_reg_metrics <- function(y_true, y_pred) {
  tibble::tibble(
    n = length(y_true),
    rmse = sqrt(mean((y_true - y_pred)^2)),
    mae = mean(abs(y_true - y_pred)),
    r2 = 1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2),
    cor_pearson = suppressWarnings(stats::cor(y_true, y_pred, method = "pearson"))
  )
}

rmse_loss <- function(observed, predicted) {
  sqrt(mean((observed - predicted)^2))
}

make_mean_abs_shap_table <- function(shap_matrix) {
  tibble::tibble(
    variable = colnames(shap_matrix),
    mean_abs_shap = colMeans(abs(shap_matrix), na.rm = TRUE)
  ) %>%
    dplyr::arrange(dplyr::desc(mean_abs_shap))
}

clean_dalex_vip_df <- function(vip_obj) {
  vip_df <- as.data.frame(vip_obj)

  vip_df %>%
    dplyr::filter(!variable %in% c("_baseline_", "_full_model_")) %>%
    dplyr::group_by(variable, label) %>%
    dplyr::summarise(
      n_permutation = dplyr::n(),
      dropout_loss = mean(dropout_loss, na.rm = TRUE),
      dropout_loss_sd = stats::sd(dropout_loss, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dropout_loss)
}

predict_model_bundle <- function(model_bundle, newdata) {
  mm_new <- prepare_matrix_from_blueprint(newdata, model_bundle$blueprint)

  pred <- switch(
    model_bundle$model_type,
    linear_lm = {
      stats::predict(model_bundle$fit, newdata = as.data.frame(mm_new))
    },
    elastic_net = {
      as.numeric(stats::predict(model_bundle$fit, newx = mm_new, s = "lambda.min"))
    },
    random_forest = {
      as.numeric(stats::predict(model_bundle$fit, data = as.data.frame(mm_new))$predictions)
    },
    xgboost = {
      as.numeric(stats::predict(model_bundle$fit, newdata = mm_new))
    },
    svm_rbf = {
      as.numeric(stats::predict(model_bundle$fit, newdata = mm_new))
    },
    stop(sprintf("不支持的模型类型：%s", model_bundle$model_type))
  )

  pred
}

get_mode_value <- function(x) {
  tab <- table(x, useNA = "no")
  names(tab)[which.max(tab)][1]
}

make_reference_profile <- function(df) {
  ref_list <- vector("list", length = ncol(df))
  names(ref_list) <- names(df)

  for (nm in names(df)) {
    x <- df[[nm]]
    if (is.numeric(x)) {
      ref_list[[nm]] <- stats::median(x, na.rm = TRUE)
    } else if (is.factor(x)) {
      ref_list[[nm]] <- factor(get_mode_value(x), levels = levels(x))
    } else {
      ref_list[[nm]] <- get_mode_value(x)
    }
  }

  as.data.frame(ref_list, stringsAsFactors = FALSE) %>%
    standardize_feature_types()
}

get_grid_values <- function(x, n_grid = 25L) {
  if (is.numeric(x)) {
    vals <- unique(as.numeric(stats::quantile(x, probs = seq(0.05, 0.95, length.out = n_grid), na.rm = TRUE)))
    vals[order(vals)]
  } else if (is.factor(x)) {
    levels(x)
  } else {
    unique(as.character(x))
  }
}

make_interaction_surface <- function(model_bundle, reference_df, data_df, var1, var2, n_grid = 25L) {
  v1_vals <- get_grid_values(data_df[[var1]], n_grid = n_grid)
  v2_vals <- get_grid_values(data_df[[var2]], n_grid = n_grid)

  grid_df <- expand.grid(
    v1 = v1_vals,
    v2 = v2_vals,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  newdata <- reference_df[rep(1, nrow(grid_df)), , drop = FALSE]

  if (is.factor(data_df[[var1]])) {
    newdata[[var1]] <- factor(grid_df$v1, levels = levels(data_df[[var1]]))
  } else {
    newdata[[var1]] <- as.numeric(grid_df$v1)
  }

  if (is.factor(data_df[[var2]])) {
    newdata[[var2]] <- factor(grid_df$v2, levels = levels(data_df[[var2]]))
  } else {
    newdata[[var2]] <- as.numeric(grid_df$v2)
  }

  pred <- predict_model_bundle(model_bundle, newdata)

  tibble::tibble(
    var1 = var1,
    var2 = var2,
    var1_value = grid_df$v1,
    var2_value = grid_df$v2,
    pred_cate = pred
  )
}

plot_interaction_surface <- function(surface_df, data_df, var1, var2, title_text, subtitle_text) {
  var1_is_num <- is.numeric(data_df[[var1]])
  var2_is_num <- is.numeric(data_df[[var2]])

  plot_df <- surface_df

  if (!var1_is_num) {
    plot_df$var1_value <- factor(plot_df$var1_value, levels = unique(as.character(plot_df$var1_value)))
  } else {
    plot_df$var1_value <- as.numeric(plot_df$var1_value)
  }

  if (!var2_is_num) {
    plot_df$var2_value <- factor(plot_df$var2_value, levels = unique(as.character(plot_df$var2_value)))
  } else {
    plot_df$var2_value <- as.numeric(plot_df$var2_value)
  }

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = var1_value, y = var2_value, fill = pred_cate)
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c(option = "C") +
    ggplot2::labs(
      title = title_text,
      subtitle = subtitle_text,
      x = var1,
      y = var2,
      fill = "Predicted\nCATE"
    ) +
    ggplot2::theme_bw(base_size = 12)
}

safe_save_plot <- function(plot_expr, png_path, pdf_path, width = 10, height = 7) {
  tryCatch({
    plot_obj <- plot_expr
    save_plot_dual(plot_obj, png_path = png_path, pdf_path = pdf_path, width = width, height = height)
    TRUE
  }, error = function(e) {
    message(sprintf("保存图形失败：%s", conditionMessage(e)))
    FALSE
  })
}

# -----------------------------
# 三、读取输入并切分数据
# -----------------------------

feature_rank_df <- readr::read_csv(
  file.path(input_dir, "01_final_top12_features.csv"),
  show_col_types = FALSE
)
feature_vars <- feature_rank_df$variable

if (length(feature_vars) != top_n_feature) {
  stop(sprintf("读取到的 top12 变量数量不是 %d，请检查第7步输出。", top_n_feature))
}

train_df_raw <- readr::read_csv(
  file.path(input_dir, "06_train_with_cate.csv"),
  show_col_types = FALSE
) %>%
  dplyr::mutate(source_dataset = "train")

validation_df_raw <- readr::read_csv(
  file.path(input_dir, "07_validation_with_cate.csv"),
  show_col_types = FALSE
) %>%
  dplyr::mutate(source_dataset = "validation")

prevalidation_df_raw <- readr::read_csv(
  file.path(input_dir, "08_prevalidation_with_cate.csv"),
  show_col_types = FALSE
) %>%
  dplyr::mutate(source_dataset = "prevalidation")

external_merged_raw <- dplyr::bind_rows(validation_df_raw, prevalidation_df_raw) %>%
  dplyr::mutate(source_dataset = "external_merged")

make_analysis_df <- function(df, split_label) {
  df %>%
    dplyr::select(
      source_dataset,
      dplyr::any_of(c("raw_id", "patient_id")),
      dplyr::all_of(feature_vars),
      dplyr::all_of(cate_col)
    ) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::mutate(
      analysis_split = split_label,
      row_id_for_analysis = dplyr::row_number()
    )
}

train_df <- make_analysis_df(train_df_raw, "train")
external_merged_df <- make_analysis_df(external_merged_raw, "external_merged")

if (nrow(train_df) == 0 || nrow(external_merged_df) == 0) {
  stop("训练集或外部验证合并集在 complete-case 过滤后为空。")
}

x_train_raw <- train_df %>%
  dplyr::select(dplyr::all_of(feature_vars)) %>%
  standardize_feature_types()
y_train <- train_df[[cate_col]]

x_external_raw <- external_merged_df %>%
  dplyr::select(dplyr::all_of(feature_vars)) %>%
  standardize_feature_types()
y_external <- external_merged_df[[cate_col]]

blueprint <- build_design_blueprint(x_train_raw)
X_train <- prepare_matrix_from_blueprint(x_train_raw, blueprint)

split_data_list <- list(
  train = list(
    raw_df = x_train_raw,
    y = y_train,
    meta_df = train_df
  ),
  external_merged = list(
    raw_df = x_external_raw,
    y = y_external,
    meta_df = external_merged_df
  )
)

# -----------------------------
# 四、写出总元数据
# -----------------------------

dataset_summary_df <- dplyr::bind_rows(train_df, external_merged_df) %>%
  dplyr::group_by(analysis_split) %>%
  dplyr::summarise(
    n_used = dplyr::n(),
    cate_mean = mean(.data[[cate_col]], na.rm = TRUE),
    cate_sd = stats::sd(.data[[cate_col]], na.rm = TRUE),
    .groups = "drop"
  )

analysis_meta_df <- tibble::tibble(
  item = c(
    "input_dir",
    "cate_column",
    "random_seed",
    "shap_seed",
    "fit_data_scope",
    "explain_data_scope",
    "n_train_used",
    "n_external_merged_used",
    "n_feature_raw",
    "n_feature_matrix_train",
    "vip_sample_n",
    "vip_B",
    "shap_explain_n",
    "shap_bg_n",
    "interaction_top_k",
    "interaction_grid_n"
  ),
  value = c(
    normalizePath(input_dir, winslash = "/"),
    cate_col,
    "20260521",
    as.character(shap_seed),
    "train_only",
    "train_and_external_merged_separately",
    as.character(nrow(train_df)),
    as.character(nrow(external_merged_df)),
    as.character(length(feature_vars)),
    as.character(ncol(X_train)),
    as.character(vip_sample_n),
    as.character(vip_B),
    as.character(shap_explain_n),
    as.character(shap_bg_n),
    as.character(interaction_top_k),
    as.character(interaction_grid_n)
  )
)

write_csv_utf8(feature_rank_df, file.path(output_root, "00_top12_features.csv"))
write_csv_utf8(dataset_summary_df, file.path(output_root, "00_dataset_summary.csv"))
write_csv_utf8(analysis_meta_df, file.path(output_root, "00_analysis_metadata.csv"))
write_csv_utf8(train_df, file.path(output_root, "00_analysis_dataset_train.csv"))
write_csv_utf8(external_merged_df, file.path(output_root, "00_analysis_dataset_external_merged.csv"))
capture.output(sessionInfo(), file = file.path(output_root, "00_session_info.txt"))

# -----------------------------
# 五、定义 surrogate 模型
# -----------------------------

model_defs <- list(
  linear_lm = list(
    label = "Linear Regression (LM)",
    model_type = "linear_lm",
    fit_fun = function(X, y) {
      stats::lm(y ~ ., data = data.frame(y = y, as.data.frame(X)))
    },
    note = "线性回归基线模型，用于提供最直接的线性近似。"
  ),
  elastic_net = list(
    label = "Elastic Net (glmnet)",
    model_type = "elastic_net",
    fit_fun = function(X, y) {
      glmnet::cv.glmnet(
        x = X,
        y = y,
        family = "gaussian",
        alpha = 0.5,
        nfolds = 5,
        standardize = TRUE
      )
    },
    note = "带正则化的线性模型，适合处理中度共线性。"
  ),
  random_forest = list(
    label = "Random Forest (ranger)",
    model_type = "random_forest",
    fit_fun = function(X, y) {
      ranger::ranger(
        dependent.variable.name = "y",
        data = data.frame(y = y, as.data.frame(X)),
        num.trees = 800,
        mtry = max(2, floor(sqrt(ncol(X)))),
        min.node.size = 5,
        importance = "none",
        seed = 20260521
      )
    },
    note = "随机森林 surrogate，适合捕捉非线性与交互。"
  ),
  xgboost = list(
    label = "Gradient Boosting Tree (xgboost)",
    model_type = "xgboost",
    fit_fun = function(X, y) {
      xgboost::xgboost(
        x = X,
        y = y,
        objective = "reg:squarederror",
        nrounds = 350,
        learning_rate = 0.05,
        max_depth = 4,
        subsample = 0.8,
        colsample_bytree = 0.8,
        verbosity = 0
      )
    },
    note = "梯度提升树 surrogate，适合拟合复杂的 CATE 非线性结构。"
  ),
  svm_rbf = list(
    label = "Support Vector Regression (RBF)",
    model_type = "svm_rbf",
    fit_fun = function(X, y) {
      e1071::svm(
        x = X,
        y = y,
        type = "eps-regression",
        kernel = "radial",
        scale = TRUE
      )
    },
    note = "RBF 核支持向量回归 surrogate，适合平滑非线性映射。"
  )
)

# -----------------------------
# 六、逐模型处理
# -----------------------------

model_metric_list <- list()
model_top_vip_list <- list()
model_top_shap_list <- list()
interaction_summary_list <- list()
model_status_list <- list()

for (model_name in names(model_defs)) {
  model_def <- model_defs[[model_name]]
  model_dir <- file.path(output_root, model_name)
  dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

  message(sprintf("开始处理模型：%s", model_name))

  one_status <- tryCatch({
    # 1) 仅使用训练集拟合 surrogate 模型
    fit_obj <- model_def$fit_fun(X_train, y_train)

    model_bundle <- list(
      model_name = model_name,
      model_label = model_def$label,
      model_type = model_def$model_type,
      fit = fit_obj,
      blueprint = blueprint,
      feature_names = feature_vars,
      target_name = cate_col,
      note = model_def$note
    )

    # 2) 写出模型级元数据
    model_meta_df <- tibble::tibble(
      item = c(
        "model_name",
        "model_label",
        "model_type",
        "model_note",
        "fit_scope",
        "n_train_rows",
        "n_raw_features",
        "n_matrix_features"
      ),
      value = c(
        model_name,
        model_def$label,
        model_def$model_type,
        model_def$note,
        "train_only",
        as.character(nrow(x_train_raw)),
        as.character(length(feature_vars)),
        as.character(ncol(X_train))
      )
    )
    write_csv_utf8(model_meta_df, file.path(model_dir, "01_model_metadata.csv"))

    # 3) 分别在 train / external_merged 上评价与解释
    for (split_name in names(split_data_list)) {
      split_obj <- split_data_list[[split_name]]
      split_dir <- file.path(model_dir, split_name)
      dir.create(split_dir, recursive = TRUE, showWarnings = FALSE)

      x_split <- split_obj$raw_df
      y_split <- split_obj$y
      meta_split <- split_obj$meta_df

      # 3.1 surrogate 拟合表现
      pred_split <- predict_model_bundle(model_bundle, x_split)
      metric_df <- calc_reg_metrics(y_true = y_split, y_pred = pred_split) %>%
        dplyr::mutate(
          model_name = model_name,
          model_label = model_def$label,
          explain_split = split_name
        ) %>%
        dplyr::select(model_name, model_label, explain_split, dplyr::everything())

      write_csv_utf8(metric_df, file.path(split_dir, "01_model_fidelity_metrics.csv"))
      model_metric_list[[paste(model_name, split_name, sep = "__")]] <- metric_df

      # 3.2 DALEX explain + 变量重要性
      explainer_obj <- DALEX::explain(
        model = model_bundle,
        data = x_split,
        y = y_split,
        predict_function = function(m, newdata) predict_model_bundle(m, newdata),
        label = sprintf("%s [%s]", model_def$label, split_name),
        verbose = FALSE,
        precalculate = FALSE
      )

      set.seed(20260521 + match(split_name, names(split_data_list)))
      vip_obj <- DALEX::model_parts(
        explainer = explainer_obj,
        loss_function = rmse_loss,
        type = "variable_importance",
        N = min(vip_sample_n, nrow(x_split)),
        B = vip_B
      )

      vip_df <- clean_dalex_vip_df(vip_obj) %>%
        dplyr::mutate(
          model_name = model_name,
          explain_split = split_name,
          .before = 1
        )
      write_csv_utf8(vip_df, file.path(split_dir, "02_dalex_variable_importance.csv"))

      vip_plot <- plot(vip_obj) +
        ggplot2::ggtitle(
          label = sprintf("%s: DALEX Variable Importance", model_def$label),
          subtitle = sprintf("Explain split = %s", split_name)
        )
      save_plot_dual(
        plot_obj = vip_plot,
        png_path = file.path(split_dir, "03_dalex_variable_importance.png"),
        pdf_path = file.path(split_dir, "03_dalex_variable_importance.pdf"),
        width = 10,
        height = 7
      )

      model_top_vip_list[[paste(model_name, split_name, sep = "__")]] <- vip_df %>%
        dplyr::arrange(dropout_loss) %>%
        dplyr::slice_head(n = 5) %>%
        dplyr::mutate(rank_in_model = dplyr::row_number())

      # 3.3 SHAP：在当前 split 上分别抽解释样本与背景样本
      shap_explain_id <- sample_row_ids(
        n_total = nrow(x_split),
        size_need = shap_explain_n,
        seed_use = shap_seed + 10L * match(model_name, names(model_defs)) + match(split_name, names(split_data_list))
      )

      remaining_ids <- setdiff(seq_len(nrow(x_split)), shap_explain_id)
      if (length(remaining_ids) == 0) {
        remaining_ids <- seq_len(nrow(x_split))
      }

      shap_bg_id <- sample_row_ids(
        n_total = length(remaining_ids),
        size_need = shap_bg_n,
        seed_use = shap_seed + 100L + 10L * match(model_name, names(model_defs)) + match(split_name, names(split_data_list))
      )
      shap_bg_id <- remaining_ids[shap_bg_id]

      shap_x <- x_split[shap_explain_id, , drop = FALSE]
      shap_bg <- x_split[shap_bg_id, , drop = FALSE]

      shap_index_df <- meta_split %>%
        dplyr::slice(shap_explain_id) %>%
        dplyr::select(source_dataset, dplyr::any_of(c("raw_id", "patient_id")), row_id_for_analysis, dplyr::all_of(cate_col))
      shap_bg_index_df <- meta_split %>%
        dplyr::slice(shap_bg_id) %>%
        dplyr::select(source_dataset, dplyr::any_of(c("raw_id", "patient_id")), row_id_for_analysis, dplyr::all_of(cate_col))

      write_csv_utf8(shap_index_df, file.path(split_dir, "04_shap_explain_samples.csv"))
      write_csv_utf8(shap_bg_index_df, file.path(split_dir, "05_shap_background_samples.csv"))

      ks_obj <- kernelshap::kernelshap(
        object = model_bundle,
        X = shap_x,
        bg_X = shap_bg,
        pred_fun = function(object, newdata) predict_model_bundle(object, newdata),
        hybrid_degree = 1L,
        tol = 0.01,
        max_iter = 300L,
        verbose = TRUE,
        seed = shap_seed + 1000L + 10L * match(model_name, names(model_defs)) + match(split_name, names(split_data_list))
      )

      shap_matrix_df <- as.data.frame(ks_obj$S) %>%
        dplyr::mutate(shap_row_id = seq_len(nrow(.)), .before = 1)
      write_csv_utf8(shap_matrix_df, file.path(split_dir, "06_kernelshap_matrix.csv"))

      mean_abs_shap_df <- make_mean_abs_shap_table(ks_obj$S) %>%
        dplyr::mutate(
          model_name = model_name,
          explain_split = split_name,
          .before = 1
        )
      write_csv_utf8(mean_abs_shap_df, file.path(split_dir, "07_kernelshap_mean_abs.csv"))

      model_top_shap_list[[paste(model_name, split_name, sep = "__")]] <- mean_abs_shap_df %>%
        dplyr::slice_head(n = 5) %>%
        dplyr::mutate(rank_in_model = dplyr::row_number())

      sv_obj <- shapviz::shapviz(ks_obj)

      shap_beeswarm_plot <- shapviz::sv_importance(
        sv_obj,
        kind = "beeswarm",
        max_display = length(feature_vars),
        show_numbers = TRUE
      ) +
        ggplot2::ggtitle(
          label = sprintf("%s: SHAP Beeswarm", model_def$label),
          subtitle = sprintf("Explain split = %s", split_name)
        )
      save_plot_dual(
        plot_obj = shap_beeswarm_plot,
        png_path = file.path(split_dir, "08_kernelshap_beeswarm.png"),
        pdf_path = file.path(split_dir, "08_kernelshap_beeswarm.pdf"),
        width = 11,
        height = 7
      )

      shap_bar_plot <- shapviz::sv_importance(
        sv_obj,
        kind = "bar",
        max_display = length(feature_vars),
        show_numbers = TRUE
      ) +
        ggplot2::ggtitle(
          label = sprintf("%s: Mean |SHAP| Importance", model_def$label),
          subtitle = sprintf("Explain split = %s", split_name)
        )
      save_plot_dual(
        plot_obj = shap_bar_plot,
        png_path = file.path(split_dir, "09_kernelshap_importance_bar.png"),
        pdf_path = file.path(split_dir, "09_kernelshap_importance_bar.pdf"),
        width = 10,
        height = 7
      )

      # 3.4 交互解释：基于当前 split 的 top SHAP 变量选择前2个变量
      interaction_vars <- mean_abs_shap_df$variable[seq_len(min(interaction_top_k, nrow(mean_abs_shap_df)))]

      interaction_var_df <- tibble::tibble(
        model_name = model_name,
        explain_split = split_name,
        interaction_rank = seq_along(interaction_vars),
        variable = interaction_vars
      )
      write_csv_utf8(interaction_var_df, file.path(split_dir, "10_interaction_selected_variables.csv"))

      if (length(interaction_vars) >= 2) {
        var1 <- interaction_vars[1]
        var2 <- interaction_vars[2]

        interaction_summary_list[[paste(model_name, split_name, sep = "__")]] <- tibble::tibble(
          model_name = model_name,
          explain_split = split_name,
          var1 = var1,
          var2 = var2,
          selection_rule = "top2_mean_abs_shap"
        )

        # SHAP dependence：双向展示
        safe_save_plot(
          plot_expr = shapviz::sv_dependence(
            sv_obj,
            v = var1,
            color_var = var2
          ) +
            ggplot2::ggtitle(
              label = sprintf("%s: SHAP Dependence", model_def$label),
              subtitle = sprintf("%s colored by %s [%s]", var1, var2, split_name)
            ),
          png_path = file.path(split_dir, "11_shap_dependence_var1_by_var2.png"),
          pdf_path = file.path(split_dir, "11_shap_dependence_var1_by_var2.pdf"),
          width = 9,
          height = 7
        )

        safe_save_plot(
          plot_expr = shapviz::sv_dependence(
            sv_obj,
            v = var2,
            color_var = var1
          ) +
            ggplot2::ggtitle(
              label = sprintf("%s: SHAP Dependence", model_def$label),
              subtitle = sprintf("%s colored by %s [%s]", var2, var1, split_name)
            ),
          png_path = file.path(split_dir, "12_shap_dependence_var2_by_var1.png"),
          pdf_path = file.path(split_dir, "12_shap_dependence_var2_by_var1.pdf"),
          width = 9,
          height = 7
        )

        # surrogate 双变量交互面
        ref_profile <- make_reference_profile(x_split)
        surface_df <- make_interaction_surface(
          model_bundle = model_bundle,
          reference_df = ref_profile,
          data_df = x_split,
          var1 = var1,
          var2 = var2,
          n_grid = interaction_grid_n
        )
        write_csv_utf8(surface_df, file.path(split_dir, "13_interaction_surface.csv"))

        surface_plot <- plot_interaction_surface(
          surface_df = surface_df,
          data_df = x_split,
          var1 = var1,
          var2 = var2,
          title_text = sprintf("%s: Interaction Surface", model_def$label),
          subtitle_text = sprintf("%s x %s [%s]", var1, var2, split_name)
        )
        save_plot_dual(
          plot_obj = surface_plot,
          png_path = file.path(split_dir, "14_interaction_surface.png"),
          pdf_path = file.path(split_dir, "14_interaction_surface.pdf"),
          width = 9,
          height = 7
        )
      }
    }

    tibble::tibble(
      model_name = model_name,
      model_label = model_def$label,
      status = "ok",
      message = NA_character_
    )
  }, error = function(e) {
    tibble::tibble(
      model_name = model_name,
      model_label = model_def$label,
      status = "error",
      message = conditionMessage(e)
    )
  })

  model_status_list[[model_name]] <- one_status
}

# -----------------------------
# 七、总汇总
# -----------------------------

status_df <- dplyr::bind_rows(model_status_list)
write_csv_utf8(status_df, file.path(output_root, "00_model_run_status.csv"))

metric_summary_df <- dplyr::bind_rows(model_metric_list) %>%
  dplyr::arrange(explain_split, rmse, dplyr::desc(r2))
write_csv_utf8(metric_summary_df, file.path(output_root, "00_model_fidelity_summary.csv"))

top_vip_summary_df <- dplyr::bind_rows(model_top_vip_list)
write_csv_utf8(top_vip_summary_df, file.path(output_root, "00_top5_dalex_importance_summary.csv"))

top_shap_summary_df <- dplyr::bind_rows(model_top_shap_list)
write_csv_utf8(top_shap_summary_df, file.path(output_root, "00_top5_shap_importance_summary.csv"))

interaction_summary_df <- dplyr::bind_rows(interaction_summary_list)
write_csv_utf8(interaction_summary_df, file.path(output_root, "00_interaction_variable_pairs_summary.csv"))

# -----------------------------
# 八、生成中文 Markdown 报告
# -----------------------------

metric_lines <- c()
if (nrow(metric_summary_df) > 0) {
  for (split_name in unique(metric_summary_df$explain_split)) {
    split_df <- metric_summary_df %>%
      dplyr::filter(explain_split == split_name)
    metric_lines <- c(metric_lines, sprintf("- `%s`：", split_name))
    metric_lines <- c(
      metric_lines,
      apply(split_df, 1, function(x) {
        sprintf(
          "  - `%s`：RMSE = %.4f，MAE = %.4f，R2 = %.4f，Pearson r = %.4f",
          x[["model_name"]],
          as.numeric(x[["rmse"]]),
          as.numeric(x[["mae"]]),
          as.numeric(x[["r2"]]),
          as.numeric(x[["cor_pearson"]])
        )
      })
    )
  }
}

dalex_lines <- c()
if (nrow(top_vip_summary_df) > 0) {
  for (split_name in unique(top_vip_summary_df$explain_split)) {
    dalex_lines <- c(dalex_lines, sprintf("- `%s`：", split_name))
    for (mn in unique(top_vip_summary_df$model_name[top_vip_summary_df$explain_split == split_name])) {
      one_df <- top_vip_summary_df %>%
        dplyr::filter(model_name == mn, explain_split == split_name) %>%
        dplyr::arrange(rank_in_model)
      dalex_lines <- c(
        dalex_lines,
        sprintf("  - `%s`：%s", mn, paste(one_df$variable, collapse = " > "))
      )
    }
  }
}

shap_lines <- c()
if (nrow(top_shap_summary_df) > 0) {
  for (split_name in unique(top_shap_summary_df$explain_split)) {
    shap_lines <- c(shap_lines, sprintf("- `%s`：", split_name))
    for (mn in unique(top_shap_summary_df$model_name[top_shap_summary_df$explain_split == split_name])) {
      one_df <- top_shap_summary_df %>%
        dplyr::filter(model_name == mn, explain_split == split_name) %>%
        dplyr::arrange(rank_in_model)
      shap_lines <- c(
        shap_lines,
        sprintf("  - `%s`：%s", mn, paste(one_df$variable, collapse = " > "))
      )
    }
  }
}

interaction_lines <- c()
if (nrow(interaction_summary_df) > 0) {
  for (split_name in unique(interaction_summary_df$explain_split)) {
    interaction_lines <- c(interaction_lines, sprintf("- `%s`：", split_name))
    split_df <- interaction_summary_df %>%
      dplyr::filter(explain_split == split_name)
    for (i in seq_len(nrow(split_df))) {
      interaction_lines <- c(
        interaction_lines,
        sprintf(
          "  - `%s`：`%s × %s`",
          split_df$model_name[i],
          split_df$var1[i],
          split_df$var2[i]
        )
      )
    }
  }
}

report_lines <- c(
  "# 基于最终患者级 CATE 的 surrogate 多模型解释报告（分数据集版）",
  "",
  "## 1. 分析设计调整",
  "",
  "- 本轮分析已按最新要求调整为“训练集建模、训练集与外部验证合并集分别解释”的结构。",
  "- surrogate 模型仅使用训练集患者级 CATE 进行训练。",
  "- validation 与 prevalidation 合并为 `external_merged`，在解释阶段单独分析。",
  "- 这样做更符合机器学习解释的一般原则：训练集用于建模，外部验证集用于观察模型行为是否稳定。DALEX/EMA 也强调训练集与测试集的解释结果应进行对比。[^1]",
  "",
  "## 2. 数据来源",
  "",
  sprintf("- 输入目录：`%s`", normalizePath(input_dir, winslash = "/")),
  sprintf("- CATE 结果列：`%s`", cate_col),
  sprintf("- 训练集纳入样本量：`%d`", nrow(train_df)),
  sprintf("- 外部验证合并集纳入样本量：`%d`", nrow(external_merged_df)),
  "- surrogate 模型训练数据：`train`",
  "- surrogate 模型解释数据：`train` 与 `external_merged` 分开进行",
  "",
  "## 3. 使用的 top12 变量",
  "",
  paste0("- ", feature_vars),
  "",
  "## 4. surrogate 模型列表",
  "",
  "- `linear_lm`：线性回归基线模型",
  "- `elastic_net`：Elastic Net",
  "- `random_forest`：随机森林回归",
  "- `xgboost`：梯度提升树回归",
  "- `svm_rbf`：RBF 核支持向量回归",
  "",
  "## 5. 可重复性控制",
  "",
  "- 固定随机种子：`20260521`。",
  "- 固定 SHAP 种子：`20260521`。",
  "- 固定 top12 变量来源：第7步输出的 `01_final_top12_features.csv`。",
  "- 固定 surrogate 建模数据范围：仅训练集。",
  "- 固定解释数据范围：训练集与外部验证合并集分别处理。",
  "- 固定设计矩阵 blueprint：仅由训练集生成，再应用到外部验证合并集。",
  sprintf("- DALEX 变量重要性参数：`N = %d`，`B = %d`。", vip_sample_n, vip_B),
  sprintf("- SHAP 参数：解释样本数 `%d`，背景样本数 `%d`。", shap_explain_n, shap_bg_n),
  sprintf("- 交互解释变量选取规则：每个模型、每个数据集按 mean |SHAP| 取前 `%d` 个变量。", interaction_top_k),
  "",
  "## 6. surrogate 模型拟合表现",
  "",
  if (length(metric_lines) > 0) metric_lines else "- 本轮未成功生成模型拟合指标。",
  "",
  "## 7. DALEX 变量重要性概览",
  "",
  if (length(dalex_lines) > 0) dalex_lines else "- 本轮未成功生成 DALEX 变量重要性汇总。",
  "",
  "## 8. SHAP 平均绝对值排序概览",
  "",
  if (length(shap_lines) > 0) shap_lines else "- 本轮未成功生成 SHAP 汇总。",
  "",
  "## 9. 重要变量交互作用解释",
  "",
  "- 本轮已新增交互作用解释，思路是：先在每个模型、每个数据集内选出 SHAP 平均绝对值排名前2的变量，再对这两个变量做交互可视化。",
  "- 输出形式包括：",
  "- SHAP dependence 图：`var1` 由 `var2` 着色，以及反向图。",
  "- surrogate 预测交互热图：固定其他变量在参考值，绘制 `var1 × var2` 对预测 CATE 的联合影响面。",
  if (length(interaction_lines) > 0) interaction_lines else "- 本轮未成功生成交互变量组合汇总。",
  "",
  "## 10. 输出目录说明",
  "",
  "- 根目录：`output/explain/`",
  "- 模型级目录：`output/explain/<model_name>/`",
  "- 数据集级目录：`output/explain/<model_name>/<split_name>/`",
  "- 其中 `<split_name>` 目前包括：`train` 与 `external_merged`",
  "- 每个数据集子目录中包含：",
  "- surrogate 拟合指标",
  "- DALEX 变量重要性结果表与图",
  "- SHAP beeswarm / bar 图及原始 SHAP 数值",
  "- 交互变量选择表",
  "- SHAP dependence 图",
  "- surrogate 双变量交互热图",
  "",
  "## 11. 当前结果如何使用",
  "",
  "- 若目标是寻找“稳定解释变量”，应优先比较 train 与 external_merged 中都靠前的变量。",
  "- 若目标是寻找“稳定交互结构”，应优先查看 train 与 external_merged 中重复出现的变量对。",
  "- 若某模型在训练集拟合很强，但在 external_merged 上明显下降，则该模型的解释结果需要更谨慎。",
  "",
  "## 12. 后续建议",
  "",
  "- 对当前拟合效果最好的 `random_forest` 与 `xgboost`，优先深入阅读交互图。",
  "- 后续可补充代表性高/低 CATE 患者的 `waterfall` 或 `break_down` 个案解释。",
  "- 也可进一步加上 ALE / PDP 曲线，补充主效应与交互效应的连续变化趋势。",
  "",
  "[^1]: DALEX/EMA 体系强调可在训练集与测试集分别开展模型探索；若模型具有可泛化性，两者行为应相似，若差异明显则需关注分布漂移或模型外推问题。"
)

writeLines(report_lines, con = file.path(output_root, "00_cate_surrogate_explain_report.md"))

message("全部 surrogate 分数据集解释分析已完成。")
