# ==========================================
# 脚本名称：generate_test_data.R
# 核心用途：
#   生成用于演示测试的模拟数据，确保：
#   1) 数据格式与原项目完全兼容
#   2) 不覆盖或修改原项目的任何原始业务数据
#   3) 仅用于演示场景，数据规模适当缩小以加快演示速度
#
# 使用方法（在 demo 目录）：
# Rscript --version
# Rscript generate_test_data.R
# ==========================================

# 加载 pacman，便于统一管理 R 包依赖
library(pacman, help, pos = 2, lib.loc = NULL)
# 加载依赖包：tidyverse 用于数据处理，openxlsx 用于写 Excel
p_load(tidyverse, openxlsx)

# 固定随机种子，保证测试数据可复现
set.seed(20260605)

# ---------------------------------------------------------
# 1. 定义临床变量集合（与原项目 PS变量最终确定.txt 一致）
# ---------------------------------------------------------

# 定义 PS 评分计算变量组（模拟原项目的变量定义）
ps_variables <- c(
  "age_grade",        # 年龄分级
  "BCLC",             # BCLC 分期
  "Capsule_appearance", # 包膜表现
  "Tumor_number",     # 肿瘤数目
  "Tumor_size",       # 肿瘤大小
  "AFP",              # 甲胎蛋白
  "ALT",              # 谷丙转氨酶
  "AST",              # 谷草转氨酶
  "ALB",              # 白蛋白
  "TBIL",             # 总胆红素
  "PLT",              # 血小板
  "INR"               # 国际标准化比值
)

# 定义额外临床变量（用于变量重要性排序）
extra_variables <- c(
  "Gender",           # 性别
  "HBV_DNA",          # HBV DNA
  "HCV",              # HCV 感染
  "Cirrhosis",        # 肝硬化
  "Vascular_invasion", # 血管侵犯
  "Lymph_node",       # 淋巴结转移
  "Child_Pugh",       # Child-Pugh 分级
  "ECOG",             # ECOG 评分
  "Previous_treatment", # 既往治疗
  "TACE_type",        # TACE 类型
  "Drug_regimen",     # 药物方案
  "Response_evaluation" # 疗效评价
)

# 合并所有变量
all_variables <- c(ps_variables, extra_variables)

# ---------------------------------------------------------
# 2. 定义模拟数据生成函数
# ---------------------------------------------------------

#' 生成模拟的 HCC TACE 临床数据
#'
#' @param n 样本量
#' @param dataset_name 数据集名称（用于日志）
#' @param treated_prob 处理组比例
#'
#' @return tibble：包含模拟的临床数据
generate_simulated_clinical_data <- function(n, dataset_name, treated_prob = 0.55) {
  
  cat(sprintf("正在生成 %s 数据集（n = %d）...\n", dataset_name, n))
  
  # 生成患者 ID
  raw_id <- paste0("P", sprintf("%04d", seq_len(n)))
  
  # 生成治疗分组（arms）：TACE+TA vs TACE alone
  arms <- sample(
    c("TACE_TA", "TACE_alone"),
    size = n,
    replace = TRUE,
    prob = c(treated_prob, 1 - treated_prob)
  )
  
  # 生成年龄分级（分类变量）
  age_grade <- sample(
    c("<=50", "51-60", "61-70", ">70"),
    size = n,
    replace = TRUE,
    prob = c(0.15, 0.30, 0.35, 0.20)
  )
  
  # 生成 BCLC 分期（有序分类变量）
  BCLC <- sample(
    c("A", "B", "C"),
    size = n,
    replace = TRUE,
    prob = c(0.25, 0.45, 0.30)
  )
  
  # 生成包膜表现（分类变量）
  Capsule_appearance <- sample(
    c("Complete", "Incomplete", "Absent"),
    size = n,
    replace = TRUE,
    prob = c(0.30, 0.40, 0.30)
  )
  
  # 生成肿瘤数目（分类变量）
  Tumor_number <- sample(
    c("Single", "2-3", ">=4"),
    size = n,
    replace = TRUE,
    prob = c(0.40, 0.35, 0.25)
  )
  
  # 生成肿瘤大小（连续变量，单位 cm）
  Tumor_size <- round(rnorm(n, mean = 5.5, sd = 2.5), 1)
  Tumor_size <- pmax(1.0, pmin(Tumor_size, 15.0))  # 截断到合理范围
  
  # 生成 AFP（连续变量，单位 ng/mL，对数正态分布）
  AFP <- round(exp(rnorm(n, mean = 4.5, sd = 1.2)), 0)
  AFP <- pmax(1, AFP)
  
  # 生成 ALT（连续变量，单位 U/L）
  ALT <- round(rnorm(n, mean = 45, sd = 25), 0)
  ALT <- pmax(5, ALT)
  
  # 生成 AST（连续变量，单位 U/L）
  AST <- round(rnorm(n, mean = 50, sd = 30), 0)
  AST <- pmax(5, AST)
  
  # 生成 ALB（连续变量，单位 g/L）
  ALB <- round(rnorm(n, mean = 38, sd = 5), 1)
  ALB <- pmax(20, pmin(ALB, 55))
  
  # 生成 TBIL（连续变量，单位 μmol/L）
  TBIL <- round(rnorm(n, mean = 18, sd = 10), 1)
  TBIL <- pmax(3, TBIL)
  
  # 生成 PLT（连续变量，单位 ×10^9/L）
  PLT <- round(rnorm(n, mean = 150, sd = 60), 0)
  PLT <- pmax(30, PLT)
  
  # 生成 INR（连续变量）
  INR <- round(rnorm(n, mean = 1.1, sd = 0.2), 2)
  INR <- pmax(0.8, pmin(INR, 2.0))
  
  # 生成性别（分类变量）
  Gender <- sample(
    c("Male", "Female"),
    size = n,
    replace = TRUE,
    prob = c(0.75, 0.25)
  )
  
  # 生成 HBV DNA（分类变量）
  HBV_DNA <- sample(
    c("Negative", "Low", "High"),
    size = n,
    replace = TRUE,
    prob = c(0.20, 0.40, 0.40)
  )
  
  # 生成 HCV 感染（二分类变量）
  HCV <- sample(c(0, 1), size = n, replace = TRUE, prob = c(0.85, 0.15))
  
  # 生成肝硬化（二分类变量）
  Cirrhosis <- sample(c(0, 1), size = n, replace = TRUE, prob = c(0.40, 0.60))
  
  # 生成血管侵犯（分类变量）
  Vascular_invasion <- sample(
    c("None", "Micro", "Macro"),
    size = n,
    replace = TRUE,
    prob = c(0.50, 0.30, 0.20)
  )
  
  # 生成淋巴结转移（二分类变量）
  Lymph_node <- sample(c(0, 1), size = n, replace = TRUE, prob = c(0.80, 0.20))
  
  # 生成 Child-Pugh 分级（有序分类变量）
  Child_Pugh <- sample(
    c("A", "B"),
    size = n,
    replace = TRUE,
    prob = c(0.70, 0.30)
  )
  
  # 生成 ECOG 评分（有序分类变量）
  ECOG <- sample(
    c("0", "1", "2"),
    size = n,
    replace = TRUE,
    prob = c(0.50, 0.35, 0.15)
  )
  
  # 生成既往治疗（分类变量）
  Previous_treatment <- sample(
    c("None", "Surgery", "RFA", "TACE"),
    size = n,
    replace = TRUE,
    prob = c(0.40, 0.20, 0.20, 0.20)
  )
  
  # 生成 TACE 类型（分类变量）
  TACE_type <- sample(
    c("Conventional", "Drug_eluting"),
    size = n,
    replace = TRUE,
    prob = c(0.55, 0.45)
  )
  
  # 生成药物方案（分类变量）
  Drug_regimen <- sample(
    c("Epirubicin", "Doxorubicin", "Cisplatin"),
    size = n,
    replace = TRUE,
    prob = c(0.40, 0.35, 0.25)
  )
  
  # 生成疗效评价（分类变量）
  Response_evaluation <- sample(
    c("CR", "PR", "SD", "PD"),
    size = n,
    replace = TRUE,
    prob = c(0.10, 0.30, 0.35, 0.25)
  )
  
  # 构造"真实异质性获益"函数：用于生成模拟的生存时间
  # 这个函数决定了不同患者对治疗的反应差异
  true_benefit <- (
    0.30 * as.numeric(BCLC == "A") +
    0.20 * as.numeric(Tumor_number == "Single") +
    0.15 * (Tumor_size - mean(Tumor_size)) / sd(Tumor_size) +
    0.10 * as.numeric(Capsule_appearance == "Complete") -
    0.15 * as.numeric(Vascular_invasion == "Macro") -
    0.10 * as.numeric(Child_Pugh == "B")
  )
  
  # 构造基线风险（对数风险线性预测子）
  lp_base <- (
    -2.5 +
    0.40 * as.numeric(BCLC == "C") +
    0.30 * (Tumor_size - mean(Tumor_size)) / sd(Tumor_size) +
    0.25 * as.numeric(Vascular_invasion == "Macro") +
    0.20 * as.numeric(Lymph_node == 1) +
    0.15 * as.numeric(Child_Pugh == "B") +
    0.10 * log(AFP / 100 + 1)
  )
  
  # 将治疗获益映射到风险比
  W <- as.numeric(arms == "TACE_TA")
  lp_treat <- lp_base - W * true_benefit
  
  # 转换为事件风险率
  hazard <- exp(lp_treat)
  
  # 生成潜在事件时间（Weibull 分布）
  T_event <- rweibull(n, shape = 1.2, scale = 1 / hazard)
  
  # 生成删失时间（控制删失比例约 30-40%）
  C_censor <- rexp(n, rate = 0.03)
  
  # 观察到的 OS 时间
  OS <- round(pmin(T_event, C_censor), 1)
  
  # OS 事件指示
  Event <- as.numeric(T_event <= C_censor)
  
  # 生成 PFS 时间（通常比 OS 短）
  T_pfs <- T_event * runif(n, min = 0.4, max = 0.8)
  C_pfs <- rexp(n, rate = 0.05)
  PFS <- round(pmin(T_pfs, C_pfs), 1)
  PFS_Event <- as.numeric(T_pfs <= C_pfs)
  
  # 组装最终数据框
  df <- tibble(
    raw_id = raw_id,
    arms = arms,
    Event = Event,
    OS = OS,
    PFS_Event = PFS_Event,
    PFS = PFS,
    age_grade = age_grade,
    BCLC = BCLC,
    Capsule_appearance = Capsule_appearance,
    Tumor_number = Tumor_number,
    Tumor_size = Tumor_size,
    AFP = AFP,
    ALT = ALT,
    AST = AST,
    ALB = ALB,
    TBIL = TBIL,
    PLT = PLT,
    INR = INR,
    Gender = Gender,
    HBV_DNA = HBV_DNA,
    HCV = HCV,
    Cirrhosis = Cirrhosis,
    Vascular_invasion = Vascular_invasion,
    Lymph_node = Lymph_node,
    Child_Pugh = Child_Pugh,
    ECOG = ECOG,
    Previous_treatment = Previous_treatment,
    TACE_type = TACE_type,
    Drug_regimen = Drug_regimen,
    Response_evaluation = Response_evaluation,
    true_benefit = round(true_benefit, 4)
  )
  
  cat(sprintf("  %s 数据集生成完成：n = %d，事件率 = %.1f%%\n", 
              dataset_name, nrow(df), mean(df$Event) * 100))
  
  return(df)
}

# ---------------------------------------------------------
# 3. 生成三个数据集
# ---------------------------------------------------------

# 训练集（样本量 300，比原项目小以加快演示速度）
df_train <- generate_simulated_clinical_data(
  n = 300,
  dataset_name = "训练集",
  treated_prob = 0.55
)

# 验证集（样本量 200）
df_validation <- generate_simulated_clinical_data(
  n = 200,
  dataset_name = "验证集",
  treated_prob = 0.50
)

# 前验证集（样本量 150）
df_prevalidation <- generate_simulated_clinical_data(
  n = 150,
  dataset_name = "前验证集",
  treated_prob = 0.52
)

# ---------------------------------------------------------
# 4. 写出 Excel 文件（与原项目格式完全兼容）
# ---------------------------------------------------------

# 定义输出目录
output_dir <- here::here("test_data")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 写出训练集
write.xlsx(
  x = list(tidy = df_train),
  file = file.path(output_dir, "01_ISMIO2501_train_tidy.xlsx"),
  overwrite = TRUE
)
cat("训练集文件已保存：01_ISMIO2501_train_tidy.xlsx\n")

# 写出验证集
write.xlsx(
  x = list(tidy = df_validation),
  file = file.path(output_dir, "02_ISMIO2501_validation_tidy.xlsx"),
  overwrite = TRUE
)
cat("验证集文件已保存：02_ISMIO2501_validation_tidy.xlsx\n")

# 写出前验证集
write.xlsx(
  x = list(tidy = df_prevalidation),
  file = file.path(output_dir, "03_ISMIO2501prevalidation_tidy.xlsx"),
  overwrite = TRUE
)
cat("前验证集文件已保存：03_ISMIO2501prevalidation_tidy.xlsx\n")

# ---------------------------------------------------------
# 5. 生成 PS 变量文件（与原项目格式完全兼容）
# ---------------------------------------------------------

# 构建 PS 变量文件内容
ps_file_content <- c(
  "PS评分计算变量1：",
  paste0(seq_along(ps_variables), ".", ps_variables),
  "",
  "PS评分计算变量2：",
  paste0(seq_along(ps_variables[1:8]), ".", ps_variables[1:8])
)

# 写出 PS 变量文件
writeLines(
  ps_file_content,
  con = file.path(output_dir, "PS变量最终确定.txt"),
  useBytes = TRUE
)
cat("PS变量文件已保存：PS变量最终确定.txt\n")

# ---------------------------------------------------------
# 6. 输出数据汇总信息
# ---------------------------------------------------------

cat("\n========================================\n")
cat("测试数据生成完成！\n")
cat("========================================\n")
cat(sprintf("训练集样本量：%d\n", nrow(df_train)))
cat(sprintf("验证集样本量：%d\n", nrow(df_validation)))
cat(sprintf("前验证集样本量：%d\n", nrow(df_prevalidation)))
cat(sprintf("总样本量：%d\n", nrow(df_train) + nrow(df_validation) + nrow(df_prevalidation)))
cat(sprintf("PS变量数量：%d\n", length(ps_variables)))
cat(sprintf("总临床变量数量：%d\n", ncol(df_train) - 6))  # 减去 ID、arms、Event、OS、PFS_Event、PFS
cat("========================================\n")
cat("输出目录：", normalizePath(output_dir, winslash = "/"), "\n")
