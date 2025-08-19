################################################################################
# 🏥 85세 이상 FWLS 환자 CT 진단 유용성 연구
# Step 1: 데이터 전처리 및 탐색적 분석 (실제 데이터용)
################################################################################

# ==============================================================================
# 🔧 패키지 로드
# ==============================================================================


packages_needed <- c("tidyverse", "readxl", "tableone", "naniar", 
                     "VIM", "corrplot", "gtsummary", "lubridate")

for(pkg in packages_needed) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

cat("🏥 85세 이상 FWLS 환자 CT 진단 유용성 연구 시작\n\n")

# ==============================================================================
# 📁 데이터 로드
# ==============================================================================
data_dir <- "/Users/youjinlee/Library/Mobile Documents/com~apple~CloudDocs/My R/Fever c claude/raw fever data"


# 실제 엑셀 파일들 읽기

base_data  <- read_excel(file.path(data_dir, "base_result.xlsx"))
nurse_data <- read_excel(file.path(data_dir, "nurse.xlsx"))
lab_data   <- read_excel(file.path(data_dir, "fever_lab.xlsx"))
ct_data    <- read_excel(file.path(data_dir, "ct.xlsx"))



# ==============================================================================
# 🧹 데이터 전처리
# ==============================================================================

# 기본정보 데이터 정리
base_data_clean <- base_data %>%
  mutate(
    등록번호 = as.character(등록번호),
    나이 = as.numeric(str_extract(as.character(나이), "\\d+")),
    성별 = case_when(
      성별 %in% c("M", "남", "남성") ~ "M",
      성별 %in% c("F", "여", "여성") ~ "F",
      TRUE ~ as.character(성별)
    ),
    # 기저질환 0/1 변환
    고혈압 = ifelse(고혈압 == "+", 1, 0),
    당뇨 = ifelse(당뇨 == "+", 1, 0),
    심질환 = ifelse(심질환 == "+", 1, 0),
    신질환 = ifelse(신질환 == "+", 1, 0),
    호흡기질환 = ifelse(호흡기질환 == "+", 1, 0),
    뇌혈관질환 = ifelse(뇌혈관질환 == "+", 1, 0),
    Neoplasm = ifelse(Neoplasm == "+", 1, 0)
  ) %>%
  filter(나이 >= 85)  # 85세 이상만

# 다른 데이터들도 등록번호 정리 및 필터링
nurse_data_clean <- nurse_data %>%
  mutate(등록번호 = as.character(등록번호)) %>%
  filter(등록번호 %in% base_data_clean$등록번호)

lab_data_clean <- lab_data %>%
  mutate(등록번호 = as.character(등록번호)) %>%
  filter(등록번호 %in% base_data_clean$등록번호)

ct_data_clean <- ct_data %>%
  mutate(등록번호 = as.character(등록번호)) %>%
  filter(등록번호 %in% base_data_clean$등록번호)

cat("✅ 데이터 전처리 완료\n")
cat("  - 85세 이상 환자:", nrow(base_data_clean), "명\n")
cat("  - 평균 나이:", round(mean(base_data_clean$나이, na.rm = TRUE), 1), "세\n\n")

# ==============================================================================
# 🔍 데이터 품질 점검
# ==============================================================================

check_missing <- function(data, name) {
  missing_count <- sum(is.na(data))
  if(missing_count > 0) {
    cat("⚠️", name, "결측값:", missing_count, "개\n")
  }
}

check_missing(base_data_clean, "기본정보")
check_missing(nurse_data_clean, "간호기록") 
check_missing(lab_data_clean, "검사결과")
check_missing(ct_data_clean, "CT결과")



################################################################################
# 📊 전체 데이터 결측값 분석 (논문용)
################################################################################

# ==============================================================================
# 🔍 결측값 분석 함수 (간결 버전)
# ==============================================================================

# analyze_missing_simple() 함수는 이터 안에 결측값(빈 칸, NA)이 얼마나 있는지 분석해서 보기 좋게 요약
analyze_missing_simple <- function(data, data_name) {
  
  # 기본 정보
  n_rows <- nrow(data)
  n_cols <- ncol(data)
  
  # 각 열별 결측값 계산
  missing_summary <- data %>%
    summarise_all(~sum(is.na(.))) %>%
    pivot_longer(everything(), names_to = "변수", values_to = "결측값") %>%
    mutate(결측률 = round(결측값 / n_rows * 100, 1)) %>%
    filter(결측값 > 0) %>%  # 결측값이 있는 변수만
    arrange(desc(결측값))
  
  # 결과 출력
  cat("📊", data_name, "데이터 (", n_rows, "행 ×", n_cols, "열)\n")
  
  if(nrow(missing_summary) == 0) {
    cat("   ✅ 결측값 없음\n\n")
    return(invisible(NULL))
  }
  
  # 심각한 결측값만 표시 (20% 이상)
  severe_missing <- missing_summary %>% filter(결측률 >= 20)
  moderate_missing <- missing_summary %>% filter(결측률 >= 5 & 결측률 < 20)
  minor_missing <- missing_summary %>% filter(결측률 < 5)
  
  if(nrow(severe_missing) > 0) {
    cat("   🚨 심각한 결측 (≥20%):", nrow(severe_missing), "개 변수\n")
    for(i in 1:min(30, nrow(severe_missing))) {  # 최대 30개만
      cat("      -", severe_missing$변수[i], ":", severe_missing$결측률[i], "%\n")
    }
    if(nrow(severe_missing) > 3) cat("      - 외", nrow(severe_missing)-3, "개 변수\n")
  }
  
  if(nrow(moderate_missing) > 0) {
    cat("   ⚠️ 보통 결측 (5-19%):", nrow(moderate_missing), "개 변수\n")
  }
  
  if(nrow(minor_missing) > 0) {
    cat("   ✅ 경미한 결측 (<5%):", nrow(minor_missing), "개 변수\n")
  }
  
  # 완전한 케이스 비율
  complete_rate <- round(sum(complete.cases(data)) / n_rows * 100, 1)
  cat("   📋 완전한 케이스:", complete_rate, "%\n\n")
  
  return(missing_summary)
}

# ==============================================================================
# 📊 전체 데이터셋 결측값 분석
# ==============================================================================

cat("🔍 전체 데이터 결측값 분석 (논문용 요약)\n")
cat(strrep("=", 50), "\n\n")

# 각 데이터셋 분석
base_missing <- analyze_missing_simple(base_data_clean, "기본정보")
nurse_missing <- analyze_missing_simple(nurse_data_clean, "간호기록")
lab_missing <- analyze_missing_simple(lab_data_clean, "검사결과")
ct_missing <- analyze_missing_simple(ct_data_clean, "CT결과")

# ==============================================================================
# 📋 핵심 결측값 변수 식별 (논문 작성용)
# ==============================================================================

cat("💡 논문 분석을 위한 핵심 권장사항:\n")
cat(strrep("-", 40), "\n")

# 모든 결측값 정보 통합
all_missing <- list(
  "기본정보" = base_missing,
  "간호기록" = nurse_missing, 
  "검사결과" = lab_missing,
  "CT결과" = ct_missing
)

# 심각한 결측값 변수들 식별
critical_vars <- c()
exclude_vars <- c()

for(dataset_name in names(all_missing)) {
  missing_data <- all_missing[[dataset_name]]
  if(!is.null(missing_data) && nrow(missing_data) > 0) {
    
    # 50% 이상 결측: 분석에서 제외 권장
    severe <- missing_data %>% filter(결측률 >= 50)
    if(nrow(severe) > 0) {
      exclude_vars <- c(exclude_vars, paste0(severe$변수, " (", dataset_name, ")"))
    }
    
    # 20-49% 결측: 주의 필요
    moderate <- missing_data %>% filter(결측률 >= 20 & 결측률 < 50)
    if(nrow(moderate) > 0) {
      critical_vars <- c(critical_vars, paste0(moderate$변수, " (", dataset_name, ")"))
    }
  }
}

# 권장사항 출력
if(length(exclude_vars) > 0) {
  cat("\n🚫 분석 제외 권장 변수 (≥50% 결측):\n")
  for(i in 1:min(5, length(exclude_vars))) {
    cat("   -", exclude_vars[i], "\n")
  }
  if(length(exclude_vars) > 5) cat("   - 외", length(exclude_vars)-5, "개 변수\n")
}

if(length(critical_vars) > 0) {
  cat("\n⚠️ 주의 필요 변수 (20-49% 결측):\n")
  for(i in 1:min(5, length(critical_vars))) {
    cat("   -", critical_vars[i], "\n")
  }
  if(length(critical_vars) > 5) cat("   - 외", length(critical_vars)-5, "개 변수\n")
}

if(length(exclude_vars) == 0 && length(critical_vars) == 0) {
  cat("\n✅ 모든 변수가 분석 가능한 수준입니다.\n")
}

# ==============================================================================
# 📊 데이터셋별 품질 점수 (논문용)
# ==============================================================================

cat("\n📊 데이터셋 품질 평가:\n")
cat(strrep("-", 30), "\n")

quality_assessment <- data.frame(
  데이터셋 = c("기본정보", "간호기록", "검사결과", "CT결과"),
  환자수 = c(nrow(base_data_clean), nrow(nurse_data_clean), 
          nrow(lab_data_clean), nrow(ct_data_clean)),
  완전케이스비율 = c(
    round(sum(complete.cases(base_data_clean)) / nrow(base_data_clean) * 100, 1),
    round(sum(complete.cases(nurse_data_clean)) / nrow(nurse_data_clean) * 100, 1),
    round(sum(complete.cases(lab_data_clean)) / nrow(lab_data_clean) * 100, 1),
    round(sum(complete.cases(ct_data_clean)) / nrow(ct_data_clean) * 100, 1)
  ),
  품질등급 = c(
    ifelse(sum(complete.cases(base_data_clean)) / nrow(base_data_clean) >= 0.8, "우수", 
           ifelse(sum(complete.cases(base_data_clean)) / nrow(base_data_clean) >= 0.6, "양호", "개선필요")),
    ifelse(sum(complete.cases(nurse_data_clean)) / nrow(nurse_data_clean) >= 0.8, "우수", 
           ifelse(sum(complete.cases(nurse_data_clean)) / nrow(nurse_data_clean) >= 0.6, "양호", "개선필요")),
    ifelse(sum(complete.cases(lab_data_clean)) / nrow(lab_data_clean) >= 0.8, "우수", 
           ifelse(sum(complete.cases(lab_data_clean)) / nrow(lab_data_clean) >= 0.6, "양호", "개선필요")),
    ifelse(sum(complete.cases(ct_data_clean)) / nrow(ct_data_clean) >= 0.8, "우수", 
           ifelse(sum(complete.cases(ct_data_clean)) / nrow(ct_data_clean) >= 0.6, "양호", "개선필요"))
  )
)

print(quality_assessment)

# ==============================================================================
# 💾 결과 저장 (논문 작성용)
# ==============================================================================

cat("\n💾 분석 결과 저장:\n")

# 논문용 결측값 요약
missing_summary_for_paper <- list(
  exclude_variables = exclude_vars,
  attention_variables = critical_vars,
  quality_assessment = quality_assessment,
  analysis_date = Sys.Date()
)

saveRDS(missing_summary_for_paper, "missing_analysis_summary.rds")

cat("   ✅ missing_analysis_summary.rds 저장 완료\n")
cat("   📋 논문 Methods 섹션에서 결측값 처리 방법 기술 시 참고\n\n")

# ==============================================================================
# 🎯 논문 작성 가이드
# ==============================================================================

cat("📝 논문 작성 시 결측값 기술 예시:\n")
cat(strrep("-", 40), "\n")

total_excluded <- length(exclude_vars)
total_attention <- length(critical_vars)

if(total_excluded > 0) {
  cat("Methods 섹션:\n")
  cat("\"Variables with >50% missing data (n=", total_excluded, ") were excluded from analysis.\"\n\n")
}

if(total_attention > 0) {
  cat("Limitations 섹션:\n") 
  cat("\"", total_attention, " variables had 20-49% missing data, which may affect result interpretation.\"\n\n")
}

if(total_excluded == 0 && total_attention == 0) {
  cat("Methods 섹션:\n")
  cat("\"All variables had <20% missing data and were included in the analysis.\"\n\n")
}

cat("✅ 결측값 분석 완료! 논문 작성 시 위 가이드를 참고하세요.\n")

# ==============================================================================
# 📊 기본 통계
# ==============================================================================

cat("📊 85세 이상 환자 기본 특성:\n")

# 나이 및 성별
age_stats <- base_data_clean %>%
  summarise(
    n = n(),
    mean_age = round(mean(나이, na.rm = TRUE), 1),
    median_age = median(나이, na.rm = TRUE),
    range = paste(min(나이, na.rm = TRUE), "-", max(나이, na.rm = TRUE))
  )

gender_table <- table(base_data_clean$성별)

cat("  - 총 환자:", age_stats$n, "명\n")
cat("  - 평균 나이:", age_stats$mean_age, "세 (범위:", age_stats$range, ")\n")
cat("  - 남성:", gender_table["M"], "명 (", round(gender_table["M"]/sum(gender_table)*100, 1), "%)\n")
cat("  - 여성:", gender_table["F"], "명 (", round(gender_table["F"]/sum(gender_table)*100, 1), "%)\n\n")

# 기저질환 요약
comorbidity_summary <- base_data_clean %>%
  summarise(
    고혈압 = sum(고혈압 == 1, na.rm = TRUE),
    당뇨 = sum(당뇨 == 1, na.rm = TRUE),
    심질환 = sum(심질환 == 1, na.rm = TRUE),
    신질환 = sum(신질환 == 1, na.rm = TRUE),
    호흡기질환 = sum(호흡기질환 == 1, na.rm = TRUE),
    뇌혈관질환 = sum(뇌혈관질환 == 1, na.rm = TRUE),
    악성종양 = sum(Neoplasm == 1, na.rm = TRUE)
  )

total_patients <- nrow(base_data_clean)

cat("📊 주요 기저질환 (상위 5개):\n")


# 1. 데이터프레임 → 벡터 변환
comorbidity_vector <- unlist(comorbidity_summary)

# 2. 백분율 계산
comorbidity_pct <- round(comorbidity_vector / total_patients * 100, 1)

# 3. 정렬 (높은 순서대로)
top_comorbidities <- sort(comorbidity_pct, decreasing = TRUE)[1:5]

# 4. 출력

for(i in 1:length(top_comorbidities)) {
  disease <- names(top_comorbidities)[i]
  count <- comorbidity_summary[[disease]]
  pct <- top_comorbidities[i]
  cat("  ", i, ".", disease, ":", count, "명 (", pct, "%)\n")
}

# ==============================================================================
# 🏥 CT 시행 현황
# ==============================================================================

ct_patients <- base_data_clean %>%
  filter(등록번호 %in% ct_data_clean$등록번호)

cat("\n📊 CT 시행 현황:\n")
cat("  - CT 시행:", nrow(ct_patients), "명 /", nrow(base_data_clean), "명 (",
    round(nrow(ct_patients)/nrow(base_data_clean)*100, 1), "%)\n")

if(nrow(ct_patients) > 0) {
  ct_age_diff <- round(mean(ct_patients$나이, na.rm = TRUE) - mean(base_data_clean$나이, na.rm = TRUE), 1)
  if(abs(ct_age_diff) > 0.5) {
    cat("  - CT군 평균나이:", round(mean(ct_patients$나이, na.rm = TRUE), 1), 
        "세 (전체보다", ifelse(ct_age_diff > 0, "+", ""), ct_age_diff, "세)\n")
  }
}

# ==============================================================================
# 💾 데이터 저장
# ==============================================================================

cleaned_data_list <- list(
  base_data = base_data_clean,
  nurse_data = nurse_data_clean,
  lab_data = lab_data_clean,
  ct_data = ct_data_clean,
  summary = list(
    total_patients = nrow(base_data_clean),
    ct_patients = nrow(ct_patients),
    processing_date = Sys.Date()
  )
)

saveRDS(cleaned_data_list, "cleaned_fwls_data.rds")

cat("\n✅ 전처리 완료!\n")
cat("💾 저장파일: cleaned_fwls_data.rds\n")
cat("🎯 다음단계: 데이터 병합 및 통계분석\n\n")

# 현재 메모리의 주요 객체 안내
cat("📋 생성된 R 객체:\n")
cat("  - base_data_clean (", nrow(base_data_clean), "명)\n")
cat("  - nurse_data_clean (", nrow(nurse_data_clean), "건)\n")  
cat("  - lab_data_clean (", nrow(lab_data_clean), "건)\n")
cat("  - ct_data_clean (", nrow(ct_data_clean), "건)\n")
cat("  - cleaned_data_list (전체 데이터)\n")