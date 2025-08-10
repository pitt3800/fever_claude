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


rm(list=ls())
DF <-read.csv("DF_fever.csv", header=T ,stringsAsFactors=FALSE)
# DF <-read.csv("DF_fever_ABGA.csv", header=T ,stringsAsFactors=FALSE) # ABGA 만 환자군만 selection
DF <-select(DF,-X)


nrow(DF)
str(DF)
colnames(DF)

#.class 바꾸기

v_numeric <-c(4,11:48)
DF[ , v_numeric] = lapply(DF[ ,v_numeric], as.numeric)
#.NA를 해결하기 위해 예를 들어 BST NA는 'no bst'로 바꾸었는데 이 BST 열을 as.numeric 하면
#'no bst'는 숫자로 바뀔수 없어 대신 NA 가되고 그럼 as.numeric function이 돌아가질 않네.


v_date <-c("visit_D","onset_D","expire_day")
DF[ , v_date] = lapply(DF[ ,v_date], ymd)

DF$sex<-factor(DF$sex ,order=TRUE,levels=c("F","M"),labels=c("F","M"))
DF$mental <-factor(DF$mental,order=TRUE,levels=c("1","2","3"),labels=c("A","P","V"))


v_factor <-c("HTN","DM","Cardiac","Renal","Pulmonary","cerebral","Cancer","fever","nursinghome")
DF[ , v_factor] = lapply(DF[ ,v_factor], as.factor)


#.baseline characteristics
#.통계분석 전에 미리 해야..

colnames(DF)
str(DF)

write.csv(DF, "final DF.csv")

DF1 <-select(DF,fever,4,5,7:57,62,63,64,65,68) #.DF1 은 mytable 돌릴 열
str(DF1)
colnames(DF1)
nrow(DF1)

a <-mytable(fever~.,data=DF1,show.total=TRUE);a

mycsv(mytable(fever~.,data=DF1),file='test1.csv')


t.test(SBP~fever, data=DF, var.equal=TRUE) 
#.즉 mytable로 돌리는 것이나 같아.


as.data.frame(table(DF1$abdomen_code)) %>%
  arrange(desc(Freq))













# mytable(Dx~.,data=acs)
# str(acs)


## 중요한 변수 수치로 알아봐..

DF %>% group_by(fever) %>% numSummary(CRP)
DF %>% group_by(fever) %>% summarize(mean(AGE))
DF %>% group_by(fever) %>% numSummaryTable(AGE)
DF %>% group_by(fever) %>% numSummary(AGE,CRP)


#.DF1$sex <-factor(DF1$sex) logistic에서는 numeric 이어야 하고 그래프 그릴때는 factor 이어야 하고..

## 중요한 변수 그림으로 알아봐.

moonBook::densityplot(CRP~fever,data=DF)

ggplot(data=DF)+geom_density(aes(x=AGE),alpha=0.5) 

densityplot(AGE~fever,data=DF)

ggplot(data=DF)+geom_boxplot(aes(x=fever,y=AGE),alpha=0.5) 

ggplot(data = DF, mapping = aes(x=AGE)) + 
  
  geom_histogram(bins=5,binwidth=2)  +# bins 는 x 축을 몇개로 나룰것인가,binwidth 는 한 컬럼의 크기
  



ggplot(data=DF, aes(x=fever,y= AGE,group=fever))+
  geom_boxplot(fill='yellow',color='red',width=0.3,alpha=0.1,outlier.color = 'red',outlier.shape = 2)+
  stat_summary(fun="mean", geom="point", shape=22, size=3, fill="blue")

ggplot(data=DF, aes(x=sex,y= AGE))+
  geom_boxplot(fill='yellow',color='red',width=0.5,alpha=0.1,outlier.color = 'red',outlier.shape = 2)+
  geom_jitter(width=0.05)
stat_summary(fun="mean", geom="point", shape=22, size=3, fill="blue")

ggplot(data = DF, mapping = aes(x=AGE)) + 
  geom_histogram(bins=400,binwidth=1)

DF %>% ggplot(aes(x=sex, y= AGE)) + 
  geom_bar(fun = "mean", stat = "summary")

DF %>% ggplot(aes(x= mental)) + geom_bar(stat='count', fill="gray",col="yellow")

ggplot(data=DF)+geom_density(aes(x=AGE,fill=sex),alpha=0.5,) +
  scale_fill_brewer(palette = "Set3")


DF$Chest_interpretation2








#  아래 부분은 참고만 이미 위에서 해봤으니깐..
# #.그래프 제대로 그릴려면 sex를 factor로 DF1$sex <-factor(DF1$sex)  다시 numeric로 DF1$sex <-as.numeric(DF1$sex)
# DF$sex <-factor(DF$sex)
# DF$sex = relevel(DF$sex, ref = "0")
# DF$sex =factor(DF$sex,labels=c("F","M"))
# DF$sex <-as.numeric(DF$sex)
# 
# ggplot(data=DF)+geom_boxplot(aes(x=sex,y=AGE),alpha=0.5) 
# moonBook::densityplot(AGE~sex,data=DF)
# 
# ggplot(data=DF, aes(x=sex,y= AGE))+
#   geom_boxplot(fill='yellow',color='red',width=0.5,alpha=0.1,outlier.color = 'red',outlier.shape = 2)+
#   geom_jitter(width=0.05)
# stat_summary(fun="mean", geom="point", shape=22, size=3, fill="blue")
# 
# ggplot(data = DF, mapping = aes(x=AGE)) + 
#   geom_histogram(bins=400,binwidth=1)  # bins 는 x 축을 몇개로 나룰것인가,binwidth 는 한 컬럼의 크기
#   
#   DF %>% ggplot(aes(x=sex, y= AGE)) + 
#   geom_bar(fun = "mean", stat = "summary")
# 
# DF %>% ggplot(aes(x= mental)) + geom_bar(stat='count', fill="gray",col="yellow")
# 
# 
# ggplot(data=DF)+geom_density(aes(x=AGE,fill=sex),alpha=0.5,) +
#   scale_fill_brewer(palette = "Set3")


#.명목변수 모자이크 그래프 그려봐
result=table(DF1$sex,DF1$intubation);result
mosaicplot(result ,color=c("tan1","firebrick2","blue"),ylab="intubation",xlab="Sex")