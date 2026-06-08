# ==========================================
# 脚本名称：1-rmst_hte_qini_fun.R
# 核心用途：
# 1) 定义一个可复用函数：基于 RMST 目标计算 HTE 的 Qini 值；
# 2) 生成可控的模拟生存数据；
# 3) 运行示例并输出 Qini 曲线点与 Qini 标量指标。
#
# 使用方法（在项目根目录）：
# Rscript --version
# Rscript 1-rmst_hte_qini_fun.R
# ==========================================

# 加载 pacman，便于统一管理 R 包依赖。
library(pacman, help, pos = 2, lib.loc = NULL)
# 加载本脚本依赖：tidyverse 负责数据流，grf 负责因果生存森林。
p_load(tidyverse, grf)

# 固定随机种子，保证示例结果可复现。
set.seed(20260423)

# ---------------------------------------------------------
# 函数1：计算 RMST-HTE 的 Qini（曲线点 + 标量）
# ---------------------------------------------------------
#' 计算 RMST 目标下 HTE 的 Qini 指标
#'
#' @param data         数据框，必须包含生存时间、事件、处理及协变量列
#' @param time_col     生存时间列名（字符串）
#' @param event_col    事件指示列名（字符串，1=事件发生，0=删失）
#' @param treatment_col 处理变量列名（字符串，1=处理组，0=对照组）
#' @param covariate_cols 协变量列名向量
#' @param ps_covariate_cols 倾向评分模型变量列名向量（可选）；若为NULL则使用 covariate_cols
#' @param horizon_prob RMST 截断时间分位数（0-1 之间）
#' @param q_grid       Qini 曲线分位点网格
#' @param train_ratio  训练集比例（用于训练-评估分离）
#' @param num_trees_ps PS 森林树数
#' @param num_trees_csf CSF 森林树数
#' @param qini_boot_R  grf 内部 QINI 标量估计的 bootstrap 次数
#'
#' @return 一个列表，包含：
#' - qini_scalar：grf 的 QINI 标量估计及标准误
#' - qini_curve_points：按 q 的 Qini 曲线点
#' - tau_eval：评估集个体化效应预测值
#' - dr_score_eval：评估集 DR score
#' - meta：样本量、horizon 等元信息
compute_rmst_hte_qini <- function(
    data,
    time_col,
    event_col,
    treatment_col,
    covariate_cols,
    ps_covariate_cols = NULL,
    horizon_prob = 0.7,
    q_grid = seq(0.1, 1, by = 0.1),
    train_ratio = 0.7,
    num_trees_ps = 500,
    num_trees_csf = 1200,
    qini_boot_R = 100
) {
  # 基础参数检查：horizon 分位必须在 (0,1)。
  if (!is.numeric(horizon_prob) || !is.finite(horizon_prob) || horizon_prob <= 0 || horizon_prob >= 1) {
    stop("horizon_prob 必须是 (0,1) 内的数值。")
  }
  # 基础参数检查：训练集比例必须在 (0,1)。
  if (!is.numeric(train_ratio) || !is.finite(train_ratio) || train_ratio <= 0 || train_ratio >= 1) {
    stop("train_ratio 必须是 (0,1) 内的数值。")
  }

  # 若未单独指定PS变量，则默认使用建模协变量。
  if (is.null(ps_covariate_cols)) {
    ps_covariate_cols <- covariate_cols
  }

  # 汇总所有必需字段，便于统一检查缺失。
  required_cols <- c(time_col, event_col, treatment_col, covariate_cols, ps_covariate_cols)
  # 找出数据中缺失的字段。
  missing_cols <- setdiff(required_cols, names(data))
  # 若缺少必要列，则停止执行。
  if (length(missing_cols) > 0) {
    stop(paste0("输入数据缺少必要列：", paste(missing_cols, collapse = ", ")))
  }

  # 仅保留本次分析所需字段，降低后续处理复杂度。
  df <- data %>%
    select(all_of(required_cols)) %>%
    mutate(
      # 统一生存时间为数值。
      .Y = as.numeric(.data[[time_col]]),
      # 统一事件为 0/1 数值。
      .D = as.numeric(.data[[event_col]] > 0),
      # 统一处理为 0/1 数值。
      .W = as.numeric(.data[[treatment_col]] > 0)
    ) %>%
    # 去除关键变量缺失，避免建模报错。
    filter(!is.na(.Y), !is.na(.D), !is.na(.W))

  # 若处理变量不是二值，直接报错。
  if (!all(df$.W %in% c(0, 1))) {
    stop("treatment_col 必须可转换为二值变量（0/1）。")
  }

  # 构建协变量数据框。
  x_raw <- df %>%
    select(all_of(covariate_cols))
  # 将字符/逻辑列转因子，以便 model.matrix 做 one-hot 展开。
  x_raw <- x_raw %>%
    mutate(across(where(~ is.character(.x) || is.logical(.x)), as.factor))
  # 保留协变量完整样本，保证输入 grf 的 X 无缺失。
  cc_idx <- complete.cases(x_raw)
  # 对齐分析主表。
  df <- df[cc_idx, , drop = FALSE]
  # 对齐协变量表。
  x_raw <- x_raw[cc_idx, , drop = FALSE]

  # 使用无截距公式展开为数值设计矩阵。
  x_terms <- terms(~ . - 1, data = x_raw)
  # 生成矩阵 X（grf 要求数值矩阵）。
  X <- model.matrix(x_terms, data = x_raw)

  # 记录样本量。
  n <- nrow(X)
  # 若样本量过小，则提示需要更大数据。
  if (n < 80) {
    stop("样本量过小（<80），建议增大样本后再进行 RMST-HTE Qini 评估。")
  }

  # 按给定分位数定义 RMST 的 horizon。
  horizon <- as.numeric(stats::quantile(df$.Y, probs = horizon_prob, na.rm = TRUE))

  # 随机划分训练/评估索引，实现训练-评估分离。
  idx_train <- sample.int(n, size = floor(train_ratio * n), replace = FALSE)
  # 评估集索引为其余样本。
  idx_eval <- setdiff(seq_len(n), idx_train)
  # 若划分后任一子集过小，给出报错提示。
  if (length(idx_train) < 50 || length(idx_eval) < 30) {
    stop("训练集或评估集样本过小，请调大样本量或调整 train_ratio。")
  }

  # 提取训练集矩阵。
  X_train <- X[idx_train, , drop = FALSE]
  # 提取评估集矩阵。
  X_eval <- X[idx_eval, , drop = FALSE]
  # 提取训练集生存时间。
  Y_train <- df$.Y[idx_train]
  # 提取评估集生存时间。
  Y_eval <- df$.Y[idx_eval]
  # 提取训练集处理变量。
  W_train <- df$.W[idx_train]
  # 提取评估集处理变量。
  W_eval <- df$.W[idx_eval]
  # 提取训练集事件变量。
  D_train <- df$.D[idx_train]
  # 提取评估集事件变量。
  D_eval <- df$.D[idx_eval]

  # 用PS变量单独构建设计矩阵并拆分训练/评估。
  x_ps_raw <- df %>%
    select(all_of(ps_covariate_cols)) %>%
    mutate(across(where(~ is.character(.x) || is.logical(.x)), as.factor))
  x_ps_terms <- terms(~ . - 1, data = x_ps_raw)
  X_ps <- model.matrix(x_ps_terms, data = x_ps_raw)
  X_ps_train <- X_ps[idx_train, , drop = FALSE]
  X_ps_eval <- X_ps[idx_eval, , drop = FALSE]

  # 在训练集拟合 PS 模型，得到观测性场景下的 W.hat。
  ps_forest <- regression_forest(
    X = X_ps_train,
    Y = W_train,
    num.trees = num_trees_ps,
    honesty = TRUE
  )
  # 预测训练集 PS，并做截断以减少极端值影响。
  ps_train <- pmin(pmax(predict(ps_forest, X_ps_train)$predictions, 0.01), 0.99)
  # 预测评估集 PS，并做同样截断。
  ps_eval <- pmin(pmax(predict(ps_forest, X_ps_eval)$predictions, 0.01), 0.99)

  # 训练集拟合 CSF：用于学习 tau 排序规则。
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

  # 评估集拟合 CSF：用于计算正交化评分（DR score），避免同样本偏乐观。
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

  # 用训练集模型在评估集生成优先级分数（tau_hat）。
  tau_eval <- as.numeric(predict(csf_train, X_eval)$predictions)
  # 在评估集提取 DR score（用于构建 Qini 曲线）。
  dr_score_eval <- as.numeric(get_scores(csf_eval))

  # 计算 grf 内置的 QINI 标量估计（带标准误）。
  qini_scalar_raw <- rank_average_treatment_effect(
    forest = csf_eval,
    priorities = tau_eval,
    target = "QINI",
    q = q_grid,
    R = qini_boot_R
  )
  # 整理 QINI 标量结果，便于后续打印与存储。
  qini_scalar <- tibble(
    metric = "QINI",
    estimate = as.numeric(qini_scalar_raw$estimate[1]),
    std_err = as.numeric(qini_scalar_raw$std.err[1]),
    z_value = estimate / std_err,
    p_value = 2 * pnorm(-abs(z_value))
  )

  # 按 tau 从高到低排序，模拟“优先给高获益人群治疗”。
  ord <- order(tau_eval, decreasing = TRUE)
  # 排序后的 DR score。
  s_ord <- dr_score_eval[ord]
  # 计算排序后累计和。
  cum_s <- cumsum(s_ord)
  # 计算总体平均分数，作为随机策略基线斜率。
  mu_s <- mean(s_ord)
  # 记录评估集样本量。
  n_eval <- length(s_ord)

  # 逐个 q 计算 Qini 曲线点：targeted cumulative gain - random gain。
  qini_curve_points <- map_dfr(q_grid, function(qi) {
    # 计算当前 q 对应的目标样本数。
    m <- max(1L, floor(qi * n_eval))
    # 计算“按预测排序干预”的累计增益。
    gain_targeted <- cum_s[m] / n_eval
    # 计算“随机干预”的期望增益。
    gain_random <- qi * mu_s
    # 返回当前 q 的曲线点。
    tibble(
      q = as.numeric(qi),
      qini = as.numeric(gain_targeted - gain_random)
    )
  })

  # 返回结构化结果，便于下游直接使用。
  list(
    qini_scalar = qini_scalar,
    qini_curve_points = qini_curve_points,
    tau_eval = tau_eval,
    dr_score_eval = dr_score_eval,
    meta = tibble(
      n_total = n,
      n_train = length(idx_train),
      n_eval = length(idx_eval),
      horizon = horizon,
      horizon_prob = horizon_prob
    )
  )
}

# # ---------------------------------------------------------
# # 函数2：生成带异质性治疗效应的模拟生存数据
# # ---------------------------------------------------------
# #' 生成用于 RMST-HTE Qini 演示的模拟数据
# #'
# #' @param n 样本量
# #' @return 数据框：包含生存时间、删失、处理、协变量与真实异质性效应
# generate_demo_survival_data <- function(n = 1200) {
#   # 生成连续协变量 x1（标准正态）。
#   x1 <- rnorm(n, mean = 0, sd = 1)
#   # 生成连续协变量 x2（标准正态）。
#   x2 <- rnorm(n, mean = 0, sd = 1)
#   # 生成二分类协变量 x3。
#   x3 <- rbinom(n, size = 1, prob = 0.45)
#   # 生成三分类协变量 x4。
#   x4 <- sample(c("A", "B", "C"), size = n, replace = TRUE, prob = c(0.4, 0.35, 0.25))

#   # 构造“真实异质性获益”函数：x1 高、x3=1 时治疗更有益。
#   true_benefit <- 0.35 * x1 + 0.30 * x3 - 0.20 * (x4 == "C")
#   # 构造观测性场景下的真实倾向评分（非随机分配）。
#   ps_true <- plogis(0.2 + 0.4 * x2 - 0.35 * x3 + 0.2 * (x4 == "B"))
#   # 按倾向评分生成处理指示。
#   W <- rbinom(n, size = 1, prob = ps_true)

#   # 构造基线风险（对数风险线性预测子）。
#   lp_base <- -0.35 + 0.55 * x2 + 0.30 * (x4 == "C")
#   # 将治疗获益映射到风险比：获益越高，处理后事件风险下降越明显。
#   lp_treat <- lp_base - W * true_benefit
#   # 转换为事件指数分布的风险率。
#   hazard <- exp(lp_treat)

#   # 生成潜在事件时间（指数分布，便于演示）。
#   T_event <- rexp(n, rate = hazard)
#   # 生成删失时间（控制删失比例）。
#   C_censor <- rexp(n, rate = 0.18)

#   # 观察到的时间为事件与删失时间较小者。
#   Y <- pmin(T_event, C_censor)
#   # 事件指示：事件时间不大于删失时间记为 1。
#   D <- as.numeric(T_event <= C_censor)

#   # 输出模拟数据。
#   tibble(
#     time = Y,
#     event = D,
#     treatment = W,
#     x1 = x1,
#     x2 = x2,
#     x3 = x3,
#     x4 = x4,
#     true_benefit = true_benefit,
#     ps_true = ps_true
#   )
# }

# # ---------------------------------------------------------
# # 示例运行：生成测试数据并计算 Qini
# # ---------------------------------------------------------

# # 生成模拟生存数据。
# demo_df <- generate_demo_survival_data(n = 1200)

# # 调用函数，计算 RMST-HTE Qini。
# res <- compute_rmst_hte_qini(
#   data = demo_df,
#   time_col = "time",
#   event_col = "event",
#   treatment_col = "treatment",
#   covariate_cols = c("x1", "x2", "x3", "x4"),
#   horizon_prob = 0.7,
#   q_grid = seq(0.1, 1, by = 0.1),
#   train_ratio = 0.7,
#   num_trees_ps = 500,
#   num_trees_csf = 1200,
#   qini_boot_R = 100
# )

# # 打印元信息（样本量与 horizon）。
# cat("==== 元信息 ====\n")
# print(res$meta)

# # 打印 QINI 标量指标（estimate / std_err / p_value）。
# cat("==== QINI 标量 ====\n")
# print(res$qini_scalar)

# # 打印 QINI 曲线点（前10行）。
# cat("==== QINI 曲线点（前10行） ====\n")
# print(res$qini_curve_points %>% slice_head(n = 10))

# # 绘制并展示 QINI 曲线（若在交互环境可见；脚本环境仅用于快速检查）。
# qini_plot <- ggplot(res$qini_curve_points, aes(x = q, y = qini)) +
#   geom_hline(yintercept = 0, linetype = 2, color = "grey60") +
#   geom_line(color = "#1E8449", linewidth = 1.1) +
#   geom_point(color = "#1E8449", size = 2) +
#   labs(
#     # 图标题使用 ASCII 字符，避免某些终端图形设备的中文编码警告。
#     title = "RMST-HTE Qini Curve (Simulated Data)",
#     x = "Top-q treated fraction",
#     y = "Qini(q)"
#   ) +
#   theme_minimal()

# # 输出图对象（在 Rscript 下会触发一次绘图设备渲染检查）。
# print(qini_plot)
