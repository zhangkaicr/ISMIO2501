# ==========================================
# 脚本名称：4-ps_weighted_hr_function.R
# 核心用途：
# 1) 在观察性研究中，基于倾向性评分（PS）构建多种加权算法；
# 2) 计算各加权算法下组间 Cox HR；
# 3) 输出 HR 点估计、95%CI 与 P 值。
#
# 说明：
# - 本脚本只定义函数，不主动运行分析。
# - 推荐在主脚本中 source() 后调用。
# ==========================================

# 加载包管理器 pacman，便于统一依赖加载。
library(pacman, help, pos = 2, lib.loc = NULL)
# 加载函数所需包：tidyverse 用于数据流，survival 用于 Cox 模型。
p_load(tidyverse, survival)

# ---------------------------------------------------------
# 函数1：根据 PS 构建不同加权算法的权重
# ---------------------------------------------------------
#' 构建观察性研究常用 PS 权重
#'
#' @param w 二值处理指示（0/1）
#' @param ps 倾向性评分向量，范围应在 (0,1)
#' @param methods 权重方法向量，可选：
#'   "IPW", "sIPW", "ATT", "ATC", "OW", "MW"
#'
#' @return tibble，每列是对应方法的权重
make_ps_weights <- function(
    w,
    ps,
    methods = c("IPW", "sIPW", "ATT", "ATC", "OW", "MW")
) {
  # 计算样本中处理组比例，用于稳定化权重。
  p_treat <- mean(w == 1, na.rm = TRUE)

  # 初始化输出列表，后续按方法逐个写入。
  weight_list <- list()

  # 遍历方法向量，构建各方法权重。
  for (m in methods) {
    # IPW（ATE）：标准逆概率加权。
    if (m == "IPW") {
      weight_list[[m]] <- if_else(w == 1, 1 / ps, 1 / (1 - ps))
    }
    # sIPW（稳定化 ATE）：在 IPW 基础上加入边际处理概率，降低方差。
    if (m == "sIPW") {
      weight_list[[m]] <- if_else(w == 1, p_treat / ps, (1 - p_treat) / (1 - ps))
    }
    # ATT：处理组权重为1，对照组按 e/(1-e) 重加权，目标是 ATT。
    if (m == "ATT") {
      weight_list[[m]] <- if_else(w == 1, 1, ps / (1 - ps))
    }
    # ATC：对照组权重为1，处理组按 (1-e)/e 重加权，目标是 ATC。
    if (m == "ATC") {
      weight_list[[m]] <- if_else(w == 1, (1 - ps) / ps, 1)
    }
    # OW（Overlap Weight）：强调共同支持区域，减少极端权重影响。
    if (m == "OW") {
      weight_list[[m]] <- if_else(w == 1, 1 - ps, ps)
    }
    # MW（Matching Weight）：近似匹配思想，降低尾部样本影响。
    if (m == "MW") {
      weight_list[[m]] <- pmin(ps, 1 - ps) / if_else(w == 1, ps, 1 - ps)
    }
  }

  # 合并为 tibble 返回。
  tibble::as_tibble(weight_list)
}

# ---------------------------------------------------------
# 函数2：多加权算法下的 HR 估计主函数
# ---------------------------------------------------------
#' 计算多种加权算法下的组间 HR（观察性研究）
#'
#' @param data 输入数据框
#' @param time_col 生存时间列名（字符串）
#' @param event_col 事件列名（字符串，>0 视为事件）
#' @param treat_col 处理列名（字符串，>0 视为处理组）
#' @param covariate_cols 混杂协变量列名向量（用于估计 PS）
#' @param ps_covariate_cols 用于PS估计的协变量列名向量（可选）；
#'   若为NULL则默认使用 covariate_cols
#' @param ps 已知 PS 向量（可选）；若为 NULL 则函数内部用 logistic 回归估计
#' @param ps_trim PS 截断区间，默认 c(0.01, 0.99)
#' @param methods 权重方法向量，默认 c("IPW","sIPW","ATT","ATC","OW","MW")
#' @param robust_se Cox 模型是否使用稳健方差，默认 TRUE
#'
#' @return list：
#' - hr_table：各算法 HR 结果表
#' - analysis_data：建模数据（含 PS 与权重）
calc_weighted_hr_ps <- function(
    data,
    time_col,
    event_col,
    treat_col,
    covariate_cols,
    ps_covariate_cols = NULL,
    ps = NULL,
    ps_trim = c(0.01, 0.99),
    methods = c("IPW", "sIPW", "ATT", "ATC", "OW", "MW"),
    robust_se = TRUE
) {
  # 检查 PS 截断区间参数长度是否正确。
  if (length(ps_trim) != 2) {
    stop("ps_trim 必须是长度为2的数值向量，例如 c(0.01, 0.99)。")
  }
  # 检查 PS 截断区间顺序是否合法。
  if (!is.finite(ps_trim[1]) || !is.finite(ps_trim[2]) || ps_trim[1] <= 0 || ps_trim[2] >= 1 || ps_trim[1] >= ps_trim[2]) {
    stop("ps_trim 必须满足 0 < lower < upper < 1。")
  }

  # 若未单独指定PS变量，则默认复用 covariate_cols。
  if (is.null(ps_covariate_cols)) {
    ps_covariate_cols <- covariate_cols
  }

  # 汇总必需字段并检查缺失。
  required_cols <- c(time_col, event_col, treat_col, unique(c(covariate_cols, ps_covariate_cols)))
  miss_cols <- setdiff(required_cols, names(data))
  if (length(miss_cols) > 0) {
    stop(paste0("输入数据缺少必要列：", paste(miss_cols, collapse = ", ")))
  }

  # 提取分析字段并统一变量类型。
  df <- data %>%
    select(all_of(required_cols)) %>%
    mutate(
      .time = as.numeric(.data[[time_col]]),
      .event = as.numeric(.data[[event_col]] > 0),
      .treat = as.numeric(.data[[treat_col]] > 0)
    )

  # 若用户未传入 PS，则内部估计 PS。
  if (is.null(ps)) {
    # 构建用于 PS 模型的协变量数据框。
    x_df <- df %>%
      select(all_of(ps_covariate_cols)) %>%
      # 字符与逻辑转因子，便于 glm 自动展开。
      mutate(across(where(~ is.character(.x) || is.logical(.x)), as.factor))

    # 拼接处理变量用于 logistic 回归拟合。
    ps_df <- bind_cols(tibble(.treat = df$.treat), x_df)

    # 过滤完整案例，避免 PS 拟合失败。
    cc <- complete.cases(ps_df, df$.time, df$.event)
    df <- df[cc, , drop = FALSE]
    ps_df <- ps_df[cc, , drop = FALSE]

    # 拟合 logistic 回归估计 PS。
    ps_fit <- glm(
      formula = .treat ~ .,
      data = ps_df,
      family = binomial()
    )

    # 预测 PS 并转为数值向量。
    ps_hat <- as.numeric(predict(ps_fit, type = "response"))
  } else {
    # 若用户传入 PS，先检查长度是否匹配。
    if (length(ps) != nrow(df)) {
      stop("传入的 ps 长度必须等于 data 行数。")
    }

    # 合并 PS 后再做完整案例过滤（time/event/treat/ps）。
    df <- df %>%
      mutate(.ps_in = as.numeric(ps))
    cc <- complete.cases(df)
    df <- df[cc, , drop = FALSE]
    ps_hat <- as.numeric(df$.ps_in)
  }

  # 检查处理变量是否包含两组。
  if (dplyr::n_distinct(df$.treat) < 2) {
    stop("处理变量仅包含单组，无法估计组间 HR。")
  }

  # 截断 PS，减少极端值导致的权重爆炸。
  ps_hat <- pmin(pmax(ps_hat, ps_trim[1]), ps_trim[2])

  # 构建各算法权重矩阵。
  w_tbl <- make_ps_weights(
    w = df$.treat,
    ps = ps_hat,
    methods = methods
  )

  # 合并到分析数据框。
  df_analysis <- bind_cols(
    df %>% select(.time, .event, .treat),
    tibble(ps = ps_hat),
    w_tbl
  )

  # 初始化 HR 结果容器。
  hr_table <- tibble()

  # 按方法逐一拟合加权 Cox 模型。
  for (m in methods) {
    # 若该方法列不存在则跳过。
    if (!(m %in% names(df_analysis))) next

    # 提取当前方法权重。
    w_cur <- df_analysis[[m]]

    # 拟合加权 Cox（默认稳健方差）。
    fit <- tryCatch(
      coxph(
        Surv(.time, .event) ~ .treat,
        data = df_analysis,
        weights = w_cur,
        robust = robust_se
      ),
      error = function(e) NULL
    )

    # 拟合失败时记录 NA。
    if (is.null(fit)) {
      hr_table <- bind_rows(
        hr_table,
        tibble(
          method = m,
          n = nrow(df_analysis),
          n_treated = sum(df_analysis$.treat == 1),
          n_control = sum(df_analysis$.treat == 0),
          hr = NA_real_,
          lcl95 = NA_real_,
          ucl95 = NA_real_,
          p_value = NA_real_
        )
      )
      next
    }

    # 提取模型摘要。
    sm <- summary(fit)
    # 提取 log(HR)。
    beta <- as.numeric(sm$coefficients[".treat", "coef"])
    # 优先使用 robust se。
    se_col <- if ("robust se" %in% colnames(sm$coefficients)) "robust se" else "se(coef)"
    se <- as.numeric(sm$coefficients[".treat", se_col])

    # 计算 HR、95%CI 与 p 值。
    hr <- exp(beta)
    lcl <- exp(beta - 1.96 * se)
    ucl <- exp(beta + 1.96 * se)
    z <- beta / se
    p <- 2 * pnorm(-abs(z))

    # 追加结果行。
    hr_table <- bind_rows(
      hr_table,
      tibble(
        method = m,
        n = nrow(df_analysis),
        n_treated = sum(df_analysis$.treat == 1),
        n_control = sum(df_analysis$.treat == 0),
        hr = hr,
        lcl95 = lcl,
        ucl95 = ucl,
        p_value = p
      )
    )
  }

  # 按方法返回结果。
  list(
    hr_table = hr_table,
    analysis_data = df_analysis
  )
}

# ---------------------------------------------------------
# 示例（注释状态，可按需取消注释）
# ---------------------------------------------------------
# source("4-ps_weighted_hr_function.R")
# dat <- openxlsx::read.xlsx(here::here("data", "01_ISMIO2501_train_tidy.xlsx"), sheet = "tidy")
# dat <- dat %>% mutate(W_bin = as.numeric(as.character(arms) == "TACE_TA"))
# res <- calc_weighted_hr_ps(
#   data = dat,
#   time_col = "OS",
#   event_col = "Event",
#   treat_col = "W_bin",
#   covariate_cols = c("age_grade", "BCLC", "Capsule_appearance")
# )
# print(res$hr_table)
