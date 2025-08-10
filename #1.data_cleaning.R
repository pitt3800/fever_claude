################################################################################
# 85세 이상 FWLS 환자의 CT 진단 유용성 연구
# 데이터 전처리 - 벡터화 오류 수정 버전
################################################################################

# 필요한 라이브러리 로드
library(tidyverse)
library(readxl)
library(lubridate)
library(tableone)

################################################################################
# 1. 데이터 구조 확인 함수 (경로 반영)
################################################################################

check_data_structure <- function() {
  cat("========== 데이터 구조 확인 ==========\n\n")
  
  # 데이터 경로 고정
  data_dir <- "/Users/youjinlee/Library/Mobile Documents/com~apple~CloudDocs/My R/Fever c claude"
  
  # 각 파일의 컬럼명 확인
  base_data <- read_excel(file.path(data_dir, "base_result_s.xlsx"))
  nurse_data <- read_excel(file.path(data_dir, "nurse_s.xlsx"))
  lab_data <- read_excel(file.path(data_dir, "fever_lab_s.xlsx"))
  ct_data <- read_excel(file.path(data_dir, "ct_s.xlsx"))
  
  cat("1. base_result_s.xlsx 컬럼명:\n")
  print(names(base_data))
  cat("\n데이터 크기:", nrow(base_data), "행,", ncol(base_data), "열\n")
  
  # 기저질환 컬럼의 unique 값 확인
  if("고혈압" %in% names(base_data)) {
    cat("\n고혈압 컬럼의 고유값:", unique(base_data$고혈압), "\n")
  }
  if("당뇨" %in% names(base_data)) {
    cat("당뇨 컬럼의 고유값:", unique(base_data$당뇨), "\n")
  }
  
  cat("\n----------------------------------------\n")
  
  return(list(base = base_data, nurse = nurse_data, 
              lab = lab_data, ct = ct_data))
}

################################################################################
# 2. 데이터 로드 함수 (경로 반영)
################################################################################

load_data <- function() {
  
  cat("데이터 로드 중...\n")
  
  # 데이터 경로 고정
  data_dir <- "/Users/youjinlee/Library/Mobile Documents/com~apple~CloudDocs/My R/Fever c claude"
  
  # 엑셀 파일 읽기
  base_data <- read_excel(file.path(data_dir, "base_result_s.xlsx"))
  nurse_data <- read_excel(file.path(data_dir, "nurse_s.xlsx"))
  lab_data <- read_excel(file.path(data_dir, "fever_lab_s.xlsx"))
  ct_data <- read_excel(file.path(data_dir, "ct_s.xlsx"))
  
  # 데이터 타입 정리
  base_data <- base_data %>%
    mutate(
      등록번호 = as.character(등록번호),
      나이 = as.numeric(gsub("[^0-9]", "", as.character(나이))),
      내원일자 = as.character(내원일자),
      성별 = as.factor(성별)
    )
  
  nurse_data <- nurse_data %>%
    mutate(등록번호 = as.character(등록번호))
  
  lab_data <- lab_data %>%
    mutate(등록번호 = as.character(등록번호))
  
  ct_data <- ct_data %>%
    mutate(등록번호 = as.character(등록번호))
  
  cat("데이터 로드 완료\n")
  
  return(list(
    base = base_data,
    nurse = nurse_data,
    lab = lab_data,
    ct = ct_data
  ))
}
