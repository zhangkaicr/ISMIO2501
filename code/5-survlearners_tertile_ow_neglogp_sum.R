# ==========================================
# 脚本名称：5-survlearners_tertile_ow_neglogp_sum.R
# 核心用途：
# 1) 读取 survlearners 迭代得到的患者级 CATE；
# 2) 对每个 算法×step（特征集）按 CATE 排序并三等分；
# 3) 在每个三等分组内进行 OW 加权 Cox HR 及 P 值估计；
# 4) 输出每个 算法×step 的 3个分组 -log10(P) 及其总和。
#
# 使用方法（项目根目录）：
# Rscript --version
# Rscript 5-survlearners_tertile_ow_neglogp_sum.R
#
# 输出文件：
# output/5-survlearners_tertile_ow_neglogp_sum/06_survlearners_tertile_ow_hr_p_results.csv
# output/5-survlearners_tertile_ow_neglogp_sum/07_survlearners_tertile_ow_neglogp_sum.csv
# ==========================================

# 加载 pacman，统一依赖管理。
library(pacman, help, pos = 2, lib.loc = NULL)
# 加载脚本依赖：数据处理、读写、路径工具。
p_load(tidyverse, openxlsx, here)

# 固定随机种子，确保复现。
set.seed(20260423)

# -----------------------------
# 0A. 路径与PS解析辅助函数
# -----------------------------

# 解析 Rscript 调用时的当前脚本路径，便于按脚本名创建输出目录。
get_current_script_path <- function(default_path = here("5-survlearners_tertile_ow_neglogp_sum.R")) {
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

# 根据脚本名创建 output/脚本名 目录。
build_output_dir <- function(script_path) {
  # 仅保留脚本主名。
  script_stem <- tools::file_path_sans_ext(basename(script_path))
  # 生成输出目录。
  out_dir <- here("output", script_stem)
  # 若目录不存在则递归创建。
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

# 加载此前定义的“PS加权HR函数”脚本（内含 calc_weighted_hr_ps）。
source(here("4-ps_weighted_hr_function.R"))

# -----------------------------
# 1. 路径与数据读取
# -----------------------------

# 获取当前脚本路径并创建独立输出目录。
script_path <- get_current_script_path()
out_dir <- build_output_dir(script_path)
# 定义 3 号脚本输出目录。
input_dir <- here("output", "3-survlearners_recursive_feature_growth")
if (!dir.exists(input_dir)) {
  stop("未找到目录：output/3-survlearners_recursive_feature_growth，请先完成第3步 survlearners 递归特征分析。")
}

# 指定 PS 变量文件路径并解析。
ps_file_path <- here("data", "PS变量最终确定.txt")
ps_group_table <- parse_ps_groups(ps_file_path)
write_csv(ps_group_table, file.path(out_dir, "01_ps_groups_long.csv"))

# 默认使用解析结果中的第一个变量组；若用户传入环境变量则优先使用指定组。
ps_group_name_default <- unique(ps_group_table$group_name)[1]
ps_group_name <- Sys.getenv("PS_GROUP_NAME", unset = ps_group_name_default)
ps_feature_selected <- ps_group_table %>%
  filter(group_name == ps_group_name) %>%
  arrange(order_id)
if (nrow(ps_feature_selected) == 0) {
  stop(paste0("未在PS变量文件中找到指定变量组：", ps_group_name))
}
write_csv(ps_feature_selected, file.path(out_dir, "02_ps_group_selected.csv"))

# 患者级 CATE 输入文件。
cate_file <- file.path(input_dir, "08_survlearners_recursive_feature_growth_all_algorithms_patient_cate.csv")
# step级摘要输入文件（含 used_features）。
summary_file <- file.path(input_dir, "07_survlearners_recursive_feature_growth_all_algorithms.csv")

# 读取患者级 CATE 数据。
cate_df <- readr::read_csv(cate_file, show_col_types = FALSE)
# 读取 step 级摘要数据（含 used_features 字段）。
step_df <- readr::read_csv(summary_file, show_col_types = FALSE)

# 读取原始训练数据（按当前项目口径使用 train01 的 tidy 工作表）。
raw_df <- read.xlsx(here("data", "01_ISMIO2501_train_tidy.xlsx"), sheet = "tidy")

# -----------------------------
# 2. 基础预处理
# -----------------------------

# 检查原始数据关键字段。
need_cols <- c("arms", "Event", "OS")
miss_cols <- setdiff(need_cols, names(raw_df))
if (length(miss_cols) > 0) {
  stop(paste0("原始数据缺少关键字段：", paste(miss_cols, collapse = ", ")))
}

# 统一 patient_id 生成规则（与 3-survlearners 脚本一致）。
raw_df <- raw_df %>%
  mutate(
    patient_id = if ("raw_id" %in% names(.)) as.character(raw_id) else as.character(row_number())
  )

# 定义 treated 组：优先 TACE_TA，否则取排序后第二组。
if ("TACE_TA" %in% unique(as.character(raw_df$arms))) {
  treated_label <- "TACE_TA"
} else {
  treated_label <- sort(unique(as.character(raw_df$arms)))[2]
}

# 构建二值处理变量（1=treated, 0=control），并整理结局字段。
raw_df <- raw_df %>%
  mutate(
    W_bin = as.numeric(as.character(arms) == treated_label),
    Y_time = as.numeric(OS),
    D_event = as.numeric(Event > 0)
  )

# 仅保留当前数据中可用的固定PS变量。
ps_feature_fixed <- intersect(ps_feature_selected$variable, names(raw_df))
if (length(ps_feature_fixed) < 2) {
  stop("固定PS变量在当前数据中可用列不足，无法进行OW-HR计算。")
}
write_csv(
  tibble(variable = ps_feature_fixed),
  file.path(out_dir, "03_ps_variables_available_in_train.csv")
)

# 过滤 step 级结果为有效状态，提取每个算法×step 对应的特征集字符串。
step_feature_map <- step_df %>%
  filter(status == "ok") %>%
  select(algorithm, step, used_features) %>%
  distinct()

# 合并患者级 CATE 与特征集映射。
cate_long <- cate_df %>%
  filter(status == "ok", in_model == 1, !is.na(cate)) %>%
  inner_join(step_feature_map, by = c("algorithm", "step"))

# -----------------------------
# 3. 定义三等分函数
# -----------------------------

# 定义函数：按 CATE 从高到低排序后切成3组（1=高CATE组，3=低CATE组）。
assign_tertile_by_rank <- function(x) {
  # 处理全缺失情况。
  if (all(is.na(x))) return(rep(NA_integer_, length(x)))
  # 用 rank(method="first") 打破并列，确保可切分。
  rk <- rank(-x, ties.method = "first", na.last = "keep")
  # 将排序后的秩切成 3 组。
  grp <- dplyr::ntile(rk, 3)
  as.integer(grp)
}

# -----------------------------
# 4. 主循环：算法×step×tertile 计算 OW-HR
# -----------------------------

# 初始化“分组三行明细结果”容器。
tertile_hr_result <- tibble()

# 枚举每个 算法×step 组合。
combo_tbl <- cate_long %>%
  select(algorithm, step, used_features) %>%
  distinct() %>%
  arrange(algorithm, step)

# 保存本次实际进入 tertile-OW 计算的组合列表。
write_csv(combo_tbl, file.path(out_dir, "04_algorithm_step_combinations.csv"))

# 循环计算。
for (i in seq_len(nrow(combo_tbl))) {
  # 当前算法、step、特征集字符串。
  algo_i <- combo_tbl$algorithm[i]
  step_i <- combo_tbl$step[i]
  feat_str_i <- combo_tbl$used_features[i]

  # 当前组合的患者 CATE。
  cate_i <- cate_long %>%
    filter(algorithm == algo_i, step == step_i) %>%
    transmute(
      patient_id = as.character(patient_id),
      cate = cate
    )

  # 为当前组合按 CATE 排序并三等分。
  cate_i <- cate_i %>%
    mutate(
      tertile = assign_tertile_by_rank(cate)
    )

  # 与原始数据合并，得到用于 HR 计算的数据。
  dat_i <- raw_df %>%
    inner_join(cate_i, by = "patient_id")

  # 解析当前 step 对应特征集合。
  feat_vec <- strsplit(feat_str_i, split = "|", fixed = TRUE)[[1]]
  # 仅保留在原始数据中真实存在的特征，避免报错。
  feat_vec <- intersect(feat_vec, names(dat_i))
  # 仅保留当前数据中存在的PS固定变量。
  ps_vec <- intersect(ps_feature_fixed, names(dat_i))

  # 若特征集为空，当前组合直接标记失败并写入3行占位。
  if (length(feat_vec) == 0) {
    tertile_hr_result <- bind_rows(
      tertile_hr_result,
      tibble(
        algorithm = algo_i,
        step = step_i,
        used_features = feat_str_i,
        tertile = 1:3,
        n = NA_integer_,
        n_treated = NA_integer_,
        n_control = NA_integer_,
        hr = NA_real_,
        lcl95 = NA_real_,
        ucl95 = NA_real_,
        p_value = NA_real_,
        neg_log10_p = NA_real_,
        status = "error_empty_feature_set"
      )
    )
    next
  }

  # 分别在三个三等分内进行 OW 加权 HR 估计。
  for (g in 1:3) {
    # 当前三等分子集。
    sub_g <- dat_i %>%
      filter(tertile == g)

    # 若当前组样本或治疗组信息不足，记录 NA。
    if (nrow(sub_g) < 30 || dplyr::n_distinct(sub_g$W_bin) < 2) {
      tertile_hr_result <- bind_rows(
        tertile_hr_result,
        tibble(
          algorithm = algo_i,
          step = step_i,
          used_features = feat_str_i,
          tertile = g,
          n = nrow(sub_g),
          n_treated = sum(sub_g$W_bin == 1, na.rm = TRUE),
          n_control = sum(sub_g$W_bin == 0, na.rm = TRUE),
          hr = NA_real_,
          lcl95 = NA_real_,
          ucl95 = NA_real_,
          p_value = NA_real_,
          neg_log10_p = NA_real_,
          status = "skip_low_sample_or_single_arm"
        )
      )
      next
    }

    # 调用已构建函数：仅使用 OW 方法计算 HR。
    fit_g <- tryCatch(
      calc_weighted_hr_ps(
        data = sub_g,
        time_col = "Y_time",
        event_col = "D_event",
        treat_col = "W_bin",
        covariate_cols = feat_vec,
        ps_covariate_cols = ps_vec,
        methods = c("OW")
      ),
      error = function(e) e
    )

    # 若函数失败，记录错误状态。
    if (inherits(fit_g, "error")) {
      tertile_hr_result <- bind_rows(
        tertile_hr_result,
        tibble(
          algorithm = algo_i,
          step = step_i,
          used_features = feat_str_i,
          tertile = g,
          n = nrow(sub_g),
          n_treated = sum(sub_g$W_bin == 1, na.rm = TRUE),
          n_control = sum(sub_g$W_bin == 0, na.rm = TRUE),
          hr = NA_real_,
          lcl95 = NA_real_,
          ucl95 = NA_real_,
          p_value = NA_real_,
          neg_log10_p = NA_real_,
          status = paste0("error: ", conditionMessage(fit_g))
        )
      )
      next
    }

    # 提取 OW 行结果（处理组相对对照组，方向由 W_bin=1 相对 W_bin=0 保证一致）。
    ow_row <- fit_g$hr_table %>%
      filter(method == "OW") %>%
      slice_head(n = 1)

    # 计算 -log10(P)，并处理极小 p 的数值下溢。
    p_use <- ow_row$p_value
    neg_log10_p <- ifelse(is.na(p_use), NA_real_, -log10(pmax(p_use, 1e-300)))

    # 写入当前三等分结果。
    tertile_hr_result <- bind_rows(
      tertile_hr_result,
      tibble(
        algorithm = algo_i,
        step = step_i,
        used_features = feat_str_i,
        tertile = g,
        n = as.integer(ow_row$n),
        n_treated = as.integer(ow_row$n_treated),
        n_control = as.integer(ow_row$n_control),
        hr = as.numeric(ow_row$hr),
        lcl95 = as.numeric(ow_row$lcl95),
        ucl95 = as.numeric(ow_row$ucl95),
        p_value = as.numeric(ow_row$p_value),
        neg_log10_p = as.numeric(neg_log10_p),
        status = "ok"
      )
    )
  }

  # 控制台打印进度。
  cat(sprintf("完成 %d/%d: %s step=%d\n", i, nrow(combo_tbl), algo_i, step_i))
}

# -----------------------------
# 5. 结果聚合与写出
# -----------------------------

# 汇总每个 算法×step 的三组 -log10(P) 总和。
neglogp_sum_df <- tertile_hr_result %>%
  group_by(algorithm, step, used_features) %>%
  summarise(
    neg_log10_p_t1 = neg_log10_p[tertile == 1] %>% dplyr::first(),
    neg_log10_p_t2 = neg_log10_p[tertile == 2] %>% dplyr::first(),
    neg_log10_p_t3 = neg_log10_p[tertile == 3] %>% dplyr::first(),
    neg_log10_p_sum = sum(neg_log10_p, na.rm = FALSE),
    neg_log10_p_sum_na_rm = sum(if_else(status == "ok", neg_log10_p, NA_real_), na.rm = TRUE),
    n_ok_tertile = sum(status == "ok"),
    .groups = "drop"
  ) %>%
  arrange(desc(neg_log10_p_sum))

# 保存本次运行元信息。
run_meta <- tibble(
  script_path = script_path,
  output_dir = out_dir,
  input_dir = normalizePath(input_dir, winslash = "/", mustWork = FALSE),
  cate_file = normalizePath(cate_file, winslash = "/", mustWork = FALSE),
  summary_file = normalizePath(summary_file, winslash = "/", mustWork = FALSE),
  data_file = normalizePath(here("data", "01_ISMIO2501_train_tidy.xlsx"), winslash = "/", mustWork = FALSE),
  ps_file = normalizePath(ps_file_path, winslash = "/", mustWork = FALSE),
  ps_group_name = ps_group_name,
  combo_n = nrow(combo_tbl),
  tertile_row_n = nrow(tertile_hr_result)
)
write_csv(run_meta, file.path(out_dir, "05_run_meta.csv"))

# 写出三等分明细结果。
write_csv(
  tertile_hr_result,
  file.path(out_dir, "06_survlearners_tertile_ow_hr_p_results.csv")
)

# 写出每个 算法×step 的三组 -log10(P) 总和结果。
write_csv(
  neglogp_sum_df,
  file.path(out_dir, "07_survlearners_tertile_ow_neglogp_sum.csv")
)

# 控制台输出结果预览。
cat("三等分 OW-HR 结果（前10）：\n")
print(tertile_hr_result %>% slice_head(n = 10))
cat("\n三组 -log10(P) 总和结果（前10）：\n")
print(neglogp_sum_df %>% slice_head(n = 10))
cat("\n输出目录：", out_dir, "\n")
