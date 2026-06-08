# ==========================================
# 脚本名称：6-hr_trend_screening.R
# 核心用途：
# 1) 读取第5步输出的 tertile OW-HR 结果；
# 2) 将每个 算法×step 的三个 tertile 指标整理为宽表；
# 3) 依据 HR 趋势计算“趋势评分”；
# 4) 输出用于后续筛选最优算法及特征组合的趋势评分表。
#
# 使用方法（项目根目录）：
# Rscript --version
# Rscript 6-hr_trend_screening.R
#
# 输出文件：
# output/6-hr_trend_screening/01_run_meta.csv
# output/6-hr_trend_screening/02_hr_tertile_wide.csv
# output/6-hr_trend_screening/03_hr_trend_scored.csv
# output/6-hr_trend_screening/04_hr_trend_top_candidates.csv
# ==========================================

# 加载 pacman，统一依赖管理。
library(pacman, help, pos = 2, lib.loc = NULL)
# 加载脚本依赖：数据处理与路径管理。
p_load(tidyverse, here)

# 固定随机种子，确保复现。
set.seed(20260423)

# -----------------------------
# 0. 路径辅助函数
# -----------------------------

# 解析 Rscript 调用时的当前脚本路径，便于按脚本名创建输出目录。
get_current_script_path <- function(default_path = here("6-hr_trend_screening.R")) {
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

# -----------------------------
# 1. 读取第5步结果
# -----------------------------

# 获取当前脚本路径并创建输出目录。
script_path <- get_current_script_path()
out_dir <- build_output_dir(script_path)

# 定义第5步输出目录。
input_dir <- here("output", "5-survlearners_tertile_ow_neglogp_sum")
if (!dir.exists(input_dir)) {
  stop("未找到目录：output/5-survlearners_tertile_ow_neglogp_sum，请先完成第5步计算。")
}

# 指定第5步的两个核心输入文件。
hr_file <- file.path(input_dir, "06_survlearners_tertile_ow_hr_p_results.csv")
sum_file <- file.path(input_dir, "07_survlearners_tertile_ow_neglogp_sum.csv")

# 读取 tertile 级 OW-HR 明细表。
hr_df <- readr::read_csv(hr_file, show_col_types = FALSE)
# 读取 algorithm×step 的 -log10(P) 汇总表。
sum_df <- readr::read_csv(sum_file, show_col_types = FALSE)

# -----------------------------
# 2. 基础检查与整理
# -----------------------------

# 检查 HR 明细表关键字段。
need_hr_cols <- c(
  "algorithm", "step", "used_features", "tertile",
  "hr", "lcl95", "ucl95", "p_value", "neg_log10_p", "status"
)
miss_hr_cols <- setdiff(need_hr_cols, names(hr_df))
if (length(miss_hr_cols) > 0) {
  stop(paste0("HR明细表缺少必要列：", paste(miss_hr_cols, collapse = ", ")))
}

# 检查汇总表关键字段。
need_sum_cols <- c(
  "algorithm", "step", "used_features",
  "neg_log10_p_sum", "neg_log10_p_sum_na_rm", "n_ok_tertile"
)
miss_sum_cols <- setdiff(need_sum_cols, names(sum_df))
if (length(miss_sum_cols) > 0) {
  stop(paste0("sum-log10p汇总表缺少必要列：", paste(miss_sum_cols, collapse = ", ")))
}

# 将 tertile 明细表转为宽表，便于后续直接比较 t1/t2/t3 的 HR 趋势。
hr_wide <- hr_df %>%
  mutate(
    tertile = as.integer(tertile)
  ) %>%
  arrange(algorithm, step, tertile) %>%
  pivot_wider(
    id_cols = c(algorithm, step, used_features),
    names_from = tertile,
    values_from = c(n, n_treated, n_control, hr, lcl95, ucl95, p_value, neg_log10_p, status),
    names_glue = "{.value}_t{tertile}"
  )

# -----------------------------
# 3. 定义趋势评分
# -----------------------------

# 趋势规则说明：
# - 规则1：第1三等分 HR < 1
# - 规则2：HR_t2 > HR_t1
# - 规则3：HR_t3 > HR_t2
# 其中：
# - trend_score_rule：三条规则满足的条数（0-3）
# - trend_margin_sum：满足规则时对应的“幅度奖励”
# - trend_score：规则分 + 幅度分
# - trend_pass_strict：严格满足“t1<1 且 HR 严格递增”的标志
trend_scored <- hr_wide %>%
  left_join(
    sum_df %>%
      select(algorithm, step, used_features, neg_log10_p_sum, neg_log10_p_sum_na_rm, n_ok_tertile),
    by = c("algorithm", "step", "used_features")
  ) %>%
  mutate(
    # 三个 tertile 是否都成功完成 OW-HR 计算。
    all_tertile_ok = n_ok_tertile == 3,
    # 第1组 HR 是否小于1。
    hr_t1_lt_1 = !is.na(hr_t1) & hr_t1 < 1,
    # 第2组 HR 是否高于第1组。
    hr_t2_gt_t1 = !is.na(hr_t1) & !is.na(hr_t2) & hr_t2 > hr_t1,
    # 第3组 HR 是否高于第2组。
    hr_t3_gt_t2 = !is.na(hr_t2) & !is.na(hr_t3) & hr_t3 > hr_t2,
    # 是否满足严格递增。
    hr_strict_increasing = hr_t2_gt_t1 & hr_t3_gt_t2,
    # 是否同时满足“t1<1 且严格递增”。
    trend_pass_strict = all_tertile_ok & hr_t1_lt_1 & hr_strict_increasing,
    # 规则分：三条规则满足几条。
    trend_score_rule = as.integer(hr_t1_lt_1) + as.integer(hr_t2_gt_t1) + as.integer(hr_t3_gt_t2),
    # 幅度分1：t1 距离 1 有多大下降。
    trend_margin_t1 = if_else(hr_t1_lt_1, 1 - hr_t1, 0),
    # 幅度分2：t2 相对 t1 增加多少。
    trend_margin_12 = if_else(hr_t2_gt_t1, hr_t2 - hr_t1, 0),
    # 幅度分3：t3 相对 t2 增加多少。
    trend_margin_23 = if_else(hr_t3_gt_t2, hr_t3 - hr_t2, 0),
    # 总幅度分。
    trend_margin_sum = trend_margin_t1 + trend_margin_12 + trend_margin_23,
    # 综合趋势评分：规则分 + 幅度分。
    trend_score = trend_score_rule + trend_margin_sum
  ) %>%
  arrange(
    desc(trend_pass_strict),
    desc(trend_score_rule),
    desc(trend_score),
    desc(neg_log10_p_sum_na_rm)
  )

# 选出“严格通过趋势规则”的候选集合，并按趋势分与sum-log10p排序。
top_candidates <- trend_scored %>%
  filter(trend_pass_strict) %>%
  arrange(
    desc(trend_score),
    desc(neg_log10_p_sum_na_rm),
    algorithm,
    step
  )

# -----------------------------
# 4. 写出结果
# -----------------------------

# 保存本次运行元信息。
run_meta <- tibble(
  script_path = script_path,
  output_dir = out_dir,
  input_dir = normalizePath(input_dir, winslash = "/", mustWork = FALSE),
  hr_file = normalizePath(hr_file, winslash = "/", mustWork = FALSE),
  sum_file = normalizePath(sum_file, winslash = "/", mustWork = FALSE),
  total_combo_n = nrow(hr_wide),
  strict_pass_n = nrow(top_candidates)
)
write_csv(run_meta, file.path(out_dir, "01_run_meta.csv"))

# 写出宽表版本，便于人工直接查看每个 tertile 的 HR/CI/P。
write_csv(hr_wide, file.path(out_dir, "02_hr_tertile_wide.csv"))

# 写出带趋势评分的总表。
write_csv(trend_scored, file.path(out_dir, "03_hr_trend_scored.csv"))

# 写出严格通过趋势规则的候选表。
write_csv(top_candidates, file.path(out_dir, "04_hr_trend_top_candidates.csv"))

# 控制台输出结果预览。
cat("趋势评分总表（前10）：\n")
print(trend_scored %>% slice_head(n = 10))
cat("\n严格通过趋势规则的候选（前10）：\n")
print(top_candidates %>% slice_head(n = 10))
cat("\n输出目录：", out_dir, "\n")
