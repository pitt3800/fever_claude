################################################################################
# 85세 이상 FWLS 환자의 CT 진단 유용성 연구
# 통계 분석 및 예측 모델 구축
################################################################################

library(tidyverse)
library(pROC)
library(glmnet)
library(caret)
library(rms)
library(ResourceSelection)
library(DynNom)
library(rmda)

# 데이터 로드
data <- readRDS("processed_fwls_data.rds")

################################################################################
# 1. 단변량 분석
################################################################################

univariate_analysis <- function(data) {
  
  # CT 검사를 받은 환자만 분석
  data_ct <- data %>% filter(CT_performed == 1)
  
  # 분석할 변수 목록
  variables <- c("나이", "성별", "HTN", "DM", "Heart_disease", 
                 "Kidney_disease", "Lung_disease", "CVA", "Malignancy", "CCI")
  
  # 결과 저장용 데이터프레임
  univariate_results <- data.frame()
  
  for (var in variables) {
    if (var %in% c("나이", "CCI")) {
      # 연속형 변수: 로지스틱 회귀
      formula <- as.formula(paste("CT_positive ~", var))
      model <- glm(formula, data = data_ct, family = binomial)
      
      # 결과 추출
      coef_summary <- summary(model)$coefficients[2, ]
      OR <- exp(coef_summary[1])
      CI_lower <- exp(coef_summary[1] - 1.96 * coef_summary[2])
      CI_upper <- exp(coef_summary[1] + 1.96 * coef_summary[2])
      p_value <- coef_summary[4]
      
    } else {
      # 범주형 변수: 카이제곱 검정
      if (var == "성별") {
        data_ct[[var]] <- as.factor(data_ct[[var]])
      }
      
      # 2x2 테이블 생성
      tab <- table(data_ct[[var]], data_ct$CT_positive)
      
      # Fisher's exact test (작은 샘플 고려)
      test_result <- fisher.test(tab)
      OR <- test_result$estimate
      CI_lower <- test_result$conf.int[1]
      CI_upper <- test_result$conf.int[2]
      p_value <- test_result$p.value
    }
    
    # 결과 저장
    univariate_results <- rbind(
      univariate_results,
      data.frame(
        Variable = var,
        OR = round(OR, 2),
        CI_95 = paste0(round(CI_lower, 2), "-", round(CI_upper, 2)),
        P_value = ifelse(p_value < 0.001, "<0.001", round(p_value, 3))
      )
    )
  }
  
  return(univariate_results)
}

################################################################################
# 2. 다변량 로지스틱 회귀 분석
################################################################################

multivariate_analysis <- function(data) {
  
  # CT 검사를 받은 환자만
  data_ct <- data %>% 
    filter(CT_performed == 1) %>%
    drop_na(CT_positive)  # 결측치 제거
  
  # 2.1 전체 모델
  full_model <- glm(
    CT_positive ~ 나이 + 성별 + HTN + DM + Heart_disease + 
      Kidney_disease + Lung_disease + CVA + Malignancy,
    data = data_ct,
    family = binomial
  )
  
  # 2.2 단계적 변수 선택 (Stepwise selection)
  step_model <- step(full_model, direction = "both", trace = 0)
  
  # 2.3 최종 모델 결과
  final_model_summary <- summary(step_model)
  
  # 2.4 모델 성능 평가
  # 예측 확률
  predicted_probs <- predict(step_model, type = "response")
  
  # ROC 곡선
  roc_obj <- roc(data_ct$CT_positive, predicted_probs)
  auc_value <- auc(roc_obj)
  
  # 최적 cutoff 찾기 (Youden's index)
  coords_best <- coords(roc_obj, "best", method = "youden")
  
  # Hosmer-Lemeshow test (calibration)
  hl_test <- hoslem.test(data_ct$CT_positive, predicted_probs, g = 10)
  
  # 결과 정리
  results <- list(
    model = step_model,
    summary = final_model_summary,
    auc = auc_value,
    cutoff = coords_best,
    calibration = hl_test,
    roc_obj = roc_obj
  )
  
  return(results)
}

################################################################################
# 3. 예측 모델 및 점수 체계 개발
################################################################################

develop_prediction_score <- function(data, model) {
  
  data_ct <- data %>% 
    filter(CT_performed == 1) %>%
    drop_na(CT_positive)
  
  # 3.1 회귀계수를 점수로 변환
  coefs <- coef(model)
  
  # 점수 체계 생성 (회귀계수를 0-10 스케일로 변환)
  score_weights <- round(coefs[-1] * 2, 0)  # Intercept 제외
  score_weights[score_weights < 0] <- 0  # 음수는 0으로
  
  # 3.2 각 환자의 점수 계산
  calculate_score <- function(row) {
    score <- 0
    for (var in names(score_weights)) {
      if (var %in% names(row)) {
        score <- score + score_weights[var] * as.numeric(row[var])
      }
    }
    return(score)
  }
  
  data_ct$prediction_score <- apply(data_ct, 1, calculate_score)
  
  # 3.3 점수별 CT 양성률
  score_performance <- data_ct %>%
    mutate(score_group = cut(prediction_score, 
                             breaks = quantile(prediction_score, probs = seq(0, 1, 0.25)),
                             include.lowest = TRUE)) %>%
    group_by(score_group) %>%
    summarise(
      n = n(),
      CT_positive_rate = mean(CT_positive) * 100,
      Mortality_rate = mean(Mortality_30d, na.rm = TRUE) * 100
    )
  
  return(list(
    weights = score_weights,
    performance = score_performance,
    data_with_scores = data_ct
  ))
}

################################################################################
# 4. 부트스트랩 검증
################################################################################

bootstrap_validation <- function(data, n_bootstrap = 1000) {
  
  data_ct <- data %>% 
    filter(CT_performed == 1) %>%
    drop_na(CT_positive)
  
  auc_values <- numeric(n_bootstrap)
  
  set.seed(123)  # 재현성을 위한 시드 설정
  
  for (i in 1:n_bootstrap) {
    # 부트스트랩 샘플 생성
    boot_indices <- sample(1:nrow(data_ct), replace = TRUE)
    boot_data <- data_ct[boot_indices, ]
    
    # 모델 적합
    boot_model <- glm(
      CT_positive ~ 나이 + CCI + HTN + DM,
      data = boot_data,
      family = binomial
    )
    
    # AUC 계산
    boot_pred <- predict(boot_model, newdata = data_ct, type = "response")
    boot_roc <- roc(data_ct$CT_positive, boot_pred, quiet = TRUE)
    auc_values[i] <- auc(boot_roc)
  }
  
  # 95% 신뢰구간
  ci_95 <- quantile(auc_values, probs = c(0.025, 0.975))
  
  return(list(
    mean_auc = mean(auc_values),
    ci_95 = ci_95,
    auc_distribution = auc_values
  ))
}

################################################################################
# 5. 시각화 함수
################################################################################

create_plots <- function(mv_results, score_results) {
  
  # 5.1 ROC 곡선
  pdf("roc_curve.pdf", width = 8, height = 6)
  plot(mv_results$roc_obj, 
       main = "ROC Curve for CT Positive Findings",
       col = "blue", lwd = 2)
  text(0.6, 0.2, paste0("AUC = ", round(mv_results$auc, 3)), cex = 1.2)
  dev.off()
  
  # 5.2 Calibration plot
  pdf("calibration_plot.pdf", width = 8, height = 6)
  data_for_cal <- score_results$data_with_scores
  
  # 예측 확률을 10분위로 나누기
  data_for_cal$prob_decile <- cut(
    predict(mv_results$model, type = "response"),
    breaks = quantile(predict(mv_results$model, type = "response"), 
                      probs = seq(0, 1, 0.1)),
    include.lowest = TRUE
  )
  
  cal_data <- data_for_cal %>%
    group_by(prob_decile) %>%
    summarise(
      predicted = mean(predict(mv_results$model, type = "response")),
      observed = mean(CT_positive)
    )
  
  plot(cal_data$predicted, cal_data$observed,
       xlim = c(0, 1), ylim = c(0, 1),
       xlab = "Predicted Probability",
       ylab = "Observed Proportion",
       main = "Calibration Plot",
       pch = 16, col = "blue", cex = 1.5)
  abline(0, 1, lty = 2, col = "red")
  dev.off()
  
  # 5.3 Forest plot for multivariate analysis
  pdf("forest_plot.pdf", width = 10, height = 8)
  
  # 오즈비와 신뢰구간 추출
  coef_summary <- summary(mv_results$model)$coefficients[-1, ]  # Intercept 제외
  OR <- exp(coef_summary[, 1])
  CI_lower <- exp(coef_summary[, 1] - 1.96 * coef_summary[, 2])
  CI_upper <- exp(coef_summary[, 1] + 1.96 * coef_summary[, 2])
  
  forest_data <- data.frame(
    Variable = rownames(coef_summary),
    OR = OR,
    CI_lower = CI_lower,
    CI_upper = CI_upper
  )
  
  # Forest plot 그리기
  par(mar = c(5, 10, 4, 2))
  plot(forest_data$OR, 1:nrow(forest_data),
       xlim = c(0.1, 10), ylim = c(0, nrow(forest_data) + 1),
       xlab = "Odds Ratio (95% CI)", ylab = "",
       yaxt = "n", log = "x",
       pch = 16, cex = 1.5,
       main = "Forest Plot: Predictors of Positive CT Findings")
  
  # 신뢰구간 추가
  for (i in 1:nrow(forest_data)) {
    lines(c(forest_data$CI_lower[i], forest_data$CI_upper[i]), c(i, i), lwd = 2)
  }
  
  # 참조선 (OR = 1)
  abline(v = 1, lty = 2, col = "gray")
  
  # 변수명 추가
  axis(2, at = 1:nrow(forest_data), labels = forest_data$Variable, las = 1)
  
  dev.off()
  
  cat("플롯이 생성되었습니다:\n")
  cat("- roc_curve.pdf\n")
  cat("- calibration_plot.pdf\n")
  cat("- forest_plot.pdf\n")
}

################################################################################
# 6. 메인 실행
################################################################################

cat("\n=== 통계 분석 시작 ===\n")

# 1. 단변량 분석
cat("\n1. 단변량 분석 결과:\n")
univariate_results <- univariate_analysis(data)
print(univariate_results)

# 2. 다변량 분석
cat("\n2. 다변량 로지스틱 회귀 분석:\n")
mv_results <- multivariate_analysis(data)
print(mv_results$summary)
cat("\nAUC:", round(mv_results$auc, 3), "\n")
cat("최적 Cutoff:", round(mv_results$cutoff$threshold, 3), "\n")
cat("민감도:", round(mv_results$cutoff$sensitivity, 3), "\n")
cat("특이도:", round(mv_results$cutoff$specificity, 3), "\n")

# 3. 예측 점수 개발
cat("\n3. 예측 점수 체계:\n")
score_results <- develop_prediction_score(data, mv_results$model)
cat("\n점수 가중치:\n")
print(score_results$weights)
cat("\n점수별 성능:\n")
print(score_results$performance)

# 4. 부트스트랩 검증
cat("\n4. 부트스트랩 검증 (1000 iterations):\n")
boot_results <- bootstrap_validation(data)
cat("평균 AUC:", round(boot_results$mean_auc, 3), "\n")
cat("95% CI:", round(boot_results$ci_95[1], 3), "-", 
    round(boot_results$ci_95[2], 3), "\n")

# 5. 시각화
cat("\n5. 시각화 생성 중...\n")
create_plots(mv_results, score_results)

# 6. 결과 저장
saveRDS(list(
  univariate = univariate_results,
  multivariate = mv_results,
  score_system = score_results,
  bootstrap = boot_results
), "analysis_results.rds")

cat("\n=== 분석 완료 ===\n")
cat("결과가 analysis_results.rds에 저장되었습니다.\n")