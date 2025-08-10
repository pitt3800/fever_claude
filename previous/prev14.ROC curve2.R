###2. rco()를 이용하는 방법

library(pROC)
library(MASS)

data("Pima.te")
summary(Pima.te)   # 데이터의 구조 및 요약 정보를 살펴봅니다.

glu_roc <- roc(Pima.te$type, Pima.te$glu);glu_roc   # "glu" 변수에 대한 ROC를 계산하여 value로 저장합니다.


#.각변수별로 AUC를 구하는 table
Diag_DF <- data.frame(Attribute=c(colnames(Pima.te)[1:7]), AUC=NA)   # AUC 계산을 위한 데이터 프레임을 생성합니다. 
for(i in 1:nrow(Diag_DF)){
  roc_result <- roc(Pima.te$type, Pima.te[,as.character(Diag_DF$Attribute[i])])   
  # 확진 결과에 대한 데이터(type)와 진단 방법에 대한 후보 변수를 입력하여 AUC를 계산합니다. 
  Diag_DF[i,'AUC'] <- roc_result$auc   # AUC 값을 입력합니다.
}
Diag_DF <- Diag_DF[order(-Diag_DF$AUC),]   # AUC 값을 오름차순 정렬합니다.
Diag_DF   # 결과를 확인해보면 "glu" 변수가 가장 좋은 성능임을 확인 할 수 있습니다.

View(Pima.te)

#.tiff("ROC_Curve.tiff")   # 그래프 저장 형식과 파일명을 설정합니다.
a <-plot.roc(glu_roc,   # roc를 계산한 value를 입력합니다.
             col="red",   # 선의 색상을 설정합니다.
             print.auc=TRUE,   # auc 값을 출력하도록 설정합니다.
             max.auc.polygon=TRUE,   # auc의 최대 면적을 출력하도록 설정합니다.
             print.thres=TRUE, print.thres.pch=19, print.thres.col = "red",   # 기준치(cut-off value)에 대한 출력, 포인트, 색상을 설정합니다.
             auc.polygon=TRUE, auc.polygon.col="#D1F2EB")   # 선 아래 면적에 대한 출력, 색상을 설정합니다. 
#.dev.off()   # 저장해줍니다. 

#..smooth graph 그리기

#.tiff("ROC_Curve_Smooth.tiff")   # 그래프 저장 형식과 파일명을 설정합니다.
b <-plot.roc(smooth(glu_roc),   # smooth 함수를 적용합니다.
             col="red",   # 선의 색상을 설정합니다
             print.auc=TRUE,   # auc 값을 출력하도록 설정합니다.
             max.auc.polygon=TRUE,    # auc의 최대 면적을 출력하도록 설정합니다.
             auc.polygon=TRUE, auc.polygon.col="#D1F2EB")   # 선 아래 면적에 대한 출력, 색상을 설정합니다. 
# dev.off()   # 저장해줍니다. 



#.비교하는 ROC curve 그리기

#.tiff("ROC_Curve_Compare.tiff")   # 그래프 저장 형식과 파일명을 설정합니다.
plot.roc(glu_roc,   # roc를 계산한 value를 입력합니다.
         col="red",   # 선의 색상을 설정합니다.
         print.auc=TRUE, print.auc.adj=c(2.5,-8),    # auc 값을 출력하도록 설정합니다. 출력하는 글자의 위치를 지정합니다.
         max.auc.polygon=TRUE,   # auc의 최대 면적을 출력하도록 설정합니다.
         print.thres=TRUE, print.thres.pch=19, print.thres.col = "red", print.thres.adj=c(0.3,-1.2),    # 기준치(cut-off value)에 대한 출력, 포인트, 색상 위치를 설정합니다.
         auc.polygon=TRUE, auc.polygon.col="#D1F2EB")   # 선 아래 면적에 대한 출력, 색상을 설정합니다. 
plot.roc(ped_roc,   # 비교 변수의 roc를 계산한 value를 입력합니다.
         add=TRUE,   # 기본 그래프에 추가할 수 있도록 설정합니다.
         col="blue",   # 선의 색상을 설정합니다.
         print.auc=TRUE, print.auc.adj=c(1.11,1.2),    # auc 값을 출력하도록 설정합니다. 출력하는 글자의 위치를 지정합니다.
         print.thres=TRUE, print.thres.pch=19, print.thres.col = "blue", print.thres.adj=c(-0.085,1.1))    # 기준치(cut-off value)에 대한 출력, 포인트, 색상 위치를 설정합니다.
legend("bottomright",   # legend의 위치를 우측 하단으로 설정합니다.
       legend=c("glucose", "pedigree"),   # legend의 명칭을 설정합니다.
       col=c("red", "blue"), lwd=2)   # 색상과 라인 두께를 설정합니다.
#dev.off()   # 저장해줍니다. 
#.[출처] [R] pROC 패키지로 ROC Curve 그리기|작성자 베어베어스



