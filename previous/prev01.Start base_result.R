
library(multipleROC)
library(pROC)
library(readxl)
library(dplyr)
library(tidyr)
library("ggplot2")
library("ggthemes")
library(lubridate)
library(stringr)
library(moonBook)
library(multipleROC)
require(survival)
library(survminer)
# library(webr) 
library(ggplot2)
library(reshape)
library(tidyverse) 
library(readxl)
library(gridExtra)
library(RColorBrewer)
library(scales)
library(car)
library(gapminder)
library(webr)
library(stringr)

#.map_dfr

# rm(list=ls())

#.base_result 정

base_result<-read_excel(path="base_result.xlsx", sheet="Sheet1",col_names = TRUE)
base_result$N_ID <-paste(base_result$내원일자,base_result$등록번호,sep='_')
base_result = base_result[-which(duplicated(base_result$N_ID)),]
base_result <-select(base_result,N_ID,1:28)
base_result <-arrange(base_result,N_ID)
nrow(base_result)


colnames(base_result)

base_result_colnames <-as.data.frame(colnames(base_result))
# write.csv(base_result_colnames, "base_result_colnames.csv")
base_result_colnames<-read.csv("base_result_colnames.csv", header=T ,stringsAsFactors=FALSE)

names(base_result)<-base_result_colnames$X.1


#.class 바꾸기
variableNames = c("Visit_day", "expire_day","DC_day")
base_result[ , variableNames] = lapply(base_result[ , variableNames], ymd)

base_result$AGE <-as.numeric(base_result$AGE)

base_result$sex <-factor(base_result$sex)

#.NA 값을 일단 처리해야 논리 연산에서 오류안나..전체 DF에 대하여 NA를 특정 문자로 바꾸면 날짜 형식에서는 오류가 나
base_result[,7:23][is.na(base_result[,7:23])] <- "unknown"


#.기저질환에 +면 1 -면 0 NA 이거나 ? 면 unknown


#.데이터 프레임 전체에서 특정값을 특정 변수로 바꾸는 방법은 일단 없어 보이네..ifelese를 ㅆ는 것이 str_replace 보다 짧게 할 수 있음

base_result$hepatic <- ifelse(base_result$hepatic =="+", "1", ifelse(base_result$hepatic == "-", "0", "unknown"))
base_result$HTN <- ifelse(base_result$HTN =="+", "1", ifelse(base_result$HTN == "-", "0", "unknown"))
base_result$DM <- ifelse(base_result$DM =="+", "1", ifelse(base_result$DM == "-", "0", "unknown"))
base_result$Cardiac <- ifelse(base_result$Cardiac =="+", "1", ifelse(base_result$Cardiac == "-", "0", "unknown"))
base_result$Renal <- ifelse(base_result$Renal =="+", "1", ifelse(base_result$Renal == "-", "0", "unknown"))
base_result$cerebral <- ifelse(base_result$cerebral =="+", "1", ifelse(base_result$cerebral == "-", "0", "unknown"))
base_result$Cancer <- ifelse(base_result$Cancer =="+", "1", ifelse(base_result$Cancer == "-", "0", "unknown"))


# base_result$HTN <-str_replace(base_result$HTN, "\\+", "1")
# base_result$HTN <-str_replace(base_result$HTN, "\\-", "0")
# base_result$HTN <-str_replace(base_result$HTN, "\\?", "unknown")

str(base_result)

variableNames = c("hepatic", "HTN", "DM","Cardiac","Renal","cerebral","Cancer")
base_result[ , variableNames] = lapply(base_result[ , variableNames], factor)
unique(base_result$HTN)

nrow(base_result)
colnames(base_result)

write.csv(base_result, "base_result.csv")


# ct 하고 base_result 붙인 것은 ##02.Start CT 에서



