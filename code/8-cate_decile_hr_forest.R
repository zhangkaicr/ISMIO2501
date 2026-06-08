# ==========================================
# 脚本名称：8-cate_decile_hr_forest.R
# 核心用途：
# 1) 读取第7步生成的训练集/验证集/前验证集 CATE 结果；
# 2) 按患者 CATE 从高到低做10等分；
# 3) 在每个等分内分别计算 OW、IPW 与未调整 HR；
# 4) 输出每个数据集的 HR/95%CI/P 值结果表；
# 5) 绘制每个数据集的森林图，并将 X 轴设为 log 坐标。
# ==========================================

# 加载 pacman，统一依赖管理。
library(pacman, help, pos = 2, lib.loc = NULL)
# 加载脚本所需依赖。
p_load(tidyverse, openxlsx, here, survival)

# 固定随机种子，提升结果可复现性。
set.seed(20260513)

# 加载 PS 加权 HR 函数。
source(here("4-ps_weighted_hr_function.R"))

# -----------------------------
# 0. 路径与辅助函数
# -----------------------------

# 解析当前脚本路径，便于按脚本名自动创建 output 子目录。
get_current_script_path <- function(default_path = here("8-cate_decile_hr_forest.R")) {
  # 获取完整命令行参数。
  args_full <- commandArgs(trailingOnly = FALSE)
  # 提取 --file=xxx 参数。
  file_arg <- args_full[stringr::str_detect(args_full, "^--file=")]
  # 若存在脚本路径，则优先使用该路径。
  if (length(file_arg) > 0) {
    return(normalizePath(stringr::str_remove(file_arg[1], "^--file="), winslash = "/", mustWork = FALSE))
  }
  # 否则回退为默认脚本路径。
  normalizePath(default_path, winslash = "/", mustWork = FALSE)
}

# 根据脚本名创建 output/脚本名 目录。
build_output_dir <- function(script_path) {
  # 获取脚本主名。
  script_stem <- tools::file_path_sans_ext(basename(script_path))
  # 生成输出目录。
  out_dir <- here("output", script_stem)
  # 若目录不存在则递归创建。
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  # 返回输出目录。
  out_dir
}

# 解析 PS变量最终确定.txt，兼容“分组文本格式”和“R向量格式”。
parse_ps_groups <- function(ps_file) {
  # 检查文件是否存在。
  if (!file.exists(ps_file)) {
    stop(paste0("未找到PS变量文件：", ps_file))
  }

  # 读取原始文本行。
  lines_raw <- readr::read_lines(ps_file, lazy = FALSE)
  # 去掉首尾空白。
  lines_trim <- stringr::str_trim(lines_raw)
  # 识别“PS评分计算变量X：”标题行。
  header_idx <- which(stringr::str_detect(lines_trim, "^PS评分计算变量\\d+：$"))

  # 若识别到分组标题，则按分组格式解析。
  if (length(header_idx) > 0) {
    return(
      purrr::map_dfr(seq_along(header_idx), function(i) {
        # 当前组起始位置。
        start_idx <- header_idx[i]
        # 当前组结束位置。
        end_idx <- if (i < length(header_idx)) header_idx[i + 1] - 1 else length(lines_trim)
        # 当前组名称。
        group_name <- lines_trim[start_idx]
        # 当前组正文。
        block_lines <- lines_trim[seq.int(start_idx + 1, end_idx)]
        # 变量定义行。
        variable_lines <- block_lines[stringr::str_detect(block_lines, "^\\d+\\.[^[:space:]].*$")]

        # 若当前组无变量，则返回空表。
        if (length(variable_lines) == 0) {
          return(tibble())
        }

        # 返回解析结果。
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

  # 若未识别到分组标题，则尝试解析 R 向量格式。
  quoted_vars <- stringr::str_match_all(paste(lines_raw, collapse = "\n"), '"([^"]+)"')[[1]]
  variable_values <- quoted_vars[, 2]

  # 若提取到变量，则按固定组返回。
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

  # 若两种格式都无法识别，则报错。
  stop("PS变量文件格式无法识别；请使用“PS评分计算变量X：”分组格式或R向量格式。")
}

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
    data_input = NULL,
    data_file,
    dataset_name,
    cate_col,
    feature_vars,
    ps_vars,
    time_col = "Y",
    event_col = "D",
    treat_col = "W"
) {
  # 若直接传入数据框，则优先使用；否则从文件读取。
  if (!is.null(data_input)) {
    dat <- tibble::as_tibble(data_input)
  } else {
    # 读取追加了 CATE 的数据表。
    dat <- readr::read_csv(data_file, show_col_types = FALSE)
  }

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

# -----------------------------
# 1. 固定输入输出
# -----------------------------

# 获取当前脚本路径。
script_path <- get_current_script_path()
# 创建当前脚本独立输出目录。
out_dir <- build_output_dir(script_path)

# 指定第7步输出目录。
input_dir <- here("output", "7-final_surv_fl_grf_top12_predict")
if (!dir.exists(input_dir)) {
  stop("未找到目录：output/7-final_surv_fl_grf_top12_predict，请先完成第7步最终模型预测。")
}

# 指定3个输入文件。
train_file <- file.path(input_dir, "06_train_with_cate.csv")
valid_file <- file.path(input_dir, "07_validation_with_cate.csv")
prevalid_file <- file.path(input_dir, "08_prevalidation_with_cate.csv")

# 指定第7步输出的前12变量文件。
feature_file <- file.path(input_dir, "01_final_top12_features.csv")
# 指定 PS 变量文件。
ps_file <- here("data", "PS变量最终确定.txt")

# 检查输入文件是否存在。
required_files <- c(train_file, valid_file, prevalid_file, feature_file, ps_file)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(paste0("以下输入文件不存在：", paste(missing_files, collapse = "; ")))
}

# 固定第7步生成的 CATE 列名。
cate_col <- "final_model_cate_surv_fl_grf_top12_rmst24"

# 读取最终模型前12变量。
feature_vars <- readr::read_csv(feature_file, show_col_types = FALSE) %>%
  pull(variable)

# 解析固定 PS 变量。
ps_group_table <- parse_ps_groups(ps_file)
ps_group_name <- unique(ps_group_table$group_name)[1]
ps_vars <- ps_group_table %>%
  filter(group_name == ps_group_name) %>%
  arrange(order_id) %>%
  pull(variable)

# 保存本次使用的变量信息。
write_csv(tibble(variable = feature_vars), file.path(out_dir, "01_final_top12_features.csv"))
write_csv(ps_group_table, file.path(out_dir, "02_ps_groups_long.csv"))
write_csv(tibble(variable = ps_vars), file.path(out_dir, "03_ps_variables_used.csv"))

# -----------------------------
# 2. 逐数据集计算 decile HR
# -----------------------------

# 训练集 decile-HR 结果。
train_hr <- analyze_one_dataset(
  data_file = train_file,
  dataset_name = "train",
  cate_col = cate_col,
  feature_vars = feature_vars,
  ps_vars = ps_vars
)

# 验证集 decile-HR 结果。
valid_hr <- analyze_one_dataset(
  data_file = valid_file,
  dataset_name = "validation",
  cate_col = cate_col,
  feature_vars = feature_vars,
  ps_vars = ps_vars
)

# 前验证集 decile-HR 结果。
prevalid_hr <- analyze_one_dataset(
  data_file = prevalid_file,
  dataset_name = "prevalidation",
  cate_col = cate_col,
  feature_vars = feature_vars,
  ps_vars = ps_vars
)

# 读取两个外部验证集原表并做按列并集拼接，生成“合并外部验证集”。
valid_with_cate_raw <- readr::read_csv(valid_file, show_col_types = FALSE)
prevalid_with_cate_raw <- readr::read_csv(prevalid_file, show_col_types = FALSE)
external_merged_raw <- bind_rows(valid_with_cate_raw, prevalid_with_cate_raw)

# 合并外部验证集 decile-HR 结果。
external_merged_hr <- analyze_one_dataset(
  data_input = external_merged_raw,
  data_file = NULL,
  dataset_name = "external_validation_merged",
  cate_col = cate_col,
  feature_vars = feature_vars,
  ps_vars = ps_vars
)

# 合并四套结果。
all_hr <- bind_rows(train_hr, valid_hr, prevalid_hr, external_merged_hr)

# -----------------------------
# 3. 绘图并写出结果
# -----------------------------

# 保存运行元信息。
run_meta <- tibble(
  script_path = script_path,
  output_dir = out_dir,
  input_dir = normalizePath(input_dir, winslash = "/", mustWork = FALSE),
  cate_column = cate_col,
  top_feature_n = length(feature_vars),
  ps_variable_n = length(ps_vars)
)
write_csv(run_meta, file.path(out_dir, "04_run_meta.csv"))

# 写出三套结果表和总表。
write_csv(train_hr, file.path(out_dir, "05_train_cate_decile_hr.csv"))
write_csv(valid_hr, file.path(out_dir, "06_validation_cate_decile_hr.csv"))
write_csv(prevalid_hr, file.path(out_dir, "07_prevalidation_cate_decile_hr.csv"))
write_csv(external_merged_hr, file.path(out_dir, "08_external_validation_merged_cate_decile_hr.csv"))
write_csv(all_hr, file.path(out_dir, "09_all_datasets_cate_decile_hr.csv"))

# 生成四个数据集森林图。
plot_train <- plot_one_dataset_forest(train_hr, "Train")
plot_valid <- plot_one_dataset_forest(valid_hr, "Validation")
plot_prevalid <- plot_one_dataset_forest(prevalid_hr, "Prevalidation")
plot_external_merged <- plot_one_dataset_forest(external_merged_hr, "External Validation Merged")

# 输出 PNG 与 PDF，便于查看和论文使用。
ggsave(
  filename = file.path(out_dir, "09_train_cate_decile_hr_forest.png"),
  plot = plot_train,
  width = 13.5,
  height = 8.8,
  dpi = 300
)
ggsave(
  filename = file.path(out_dir, "09_train_cate_decile_hr_forest.pdf"),
  plot = plot_train,
  width = 13.5,
  height = 8.8
)

ggsave(
  filename = file.path(out_dir, "10_validation_cate_decile_hr_forest.png"),
  plot = plot_valid,
  width = 13.5,
  height = 8.8,
  dpi = 300
)
ggsave(
  filename = file.path(out_dir, "10_validation_cate_decile_hr_forest.pdf"),
  plot = plot_valid,
  width = 13.5,
  height = 8.8
)

ggsave(
  filename = file.path(out_dir, "11_prevalidation_cate_decile_hr_forest.png"),
  plot = plot_prevalid,
  width = 13.5,
  height = 8.8,
  dpi = 300
)
ggsave(
  filename = file.path(out_dir, "11_prevalidation_cate_decile_hr_forest.pdf"),
  plot = plot_prevalid,
  width = 13.5,
  height = 8.8
)

ggsave(
  filename = file.path(out_dir, "12_external_validation_merged_cate_decile_hr_forest.png"),
  plot = plot_external_merged,
  width = 13.5,
  height = 8.8,
  dpi = 300
)
ggsave(
  filename = file.path(out_dir, "12_external_validation_merged_cate_decile_hr_forest.pdf"),
  plot = plot_external_merged,
  width = 13.5,
  height = 8.8
)

# 控制台输出结果预览。
cat("Decile-wise HR results (first 12 rows):\n")
print(all_hr %>% slice_head(n = 12))
cat("\n输出目录：", out_dir, "\n")
