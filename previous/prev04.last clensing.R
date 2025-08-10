#..DF_final 불러오고 class 변경 필요한 열만 선택해서 통계 돌릴 준비해야..

# 
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

DF_final <-read.csv("DF_final.csv", header=T )

DF <-DF_final

table(DF$nursinghome) #175

#.필요없거나 중복된 열 제거

colnames(DF)
DF <-select(DF,-1,-135)



ncol(DF)
nrow(DF)

#.일단 covid 환자 정리해야..

DF_covid <-select(DF,1,61:69)

colnames(DF_covid)

DF_covid[is.na(DF_covid)] <- "Negative" #.data frame 전체에서 NA -> negative로

DF_covid$covid19 <-"Negative"

which(DF_covid[4]=="Positive")

which(DF_covid[5]=="Positive")

which(DF_covid[7]=="Positive")

which(DF_covid[10]=="Positive")

covid <-c(335,591,720,771,787,796,797,845,807,834,839,855,865)

DF_covid$covid19[covid] <-"Positive"

aa <-filter(DF_covid,covid19=="Positive")
nrow(aa) #.aa covid19=="Positive" 13명

colnames(DF_covid)

DF_covid <-select(DF_covid,1,11)

filter(DF_covid,covid19=="Positive")


#.DF에서 covid 관련된 열 제거
colnames(DF)
DF <-select(DF,-(61:69) )


DF <-full_join(DF, DF_covid, by ="N_ID") #.이건 covid 관련 처방을 정리한 DF
str(DF)
colnames(DF)
DF$covid19

#.NA를 해결해야. 정말 필요한 열만 추려야해 NA 해결하는거 힘드니깐..
#.DF 중 na 가 유의미하게 많은 열은 아예 제거
DFcolname <-as.data.frame(colSums(is.na(DF)));DFcolname
names(DFcolname) <-c("n")

DFcolname$code_names <-rownames(DFcolname);DFcolname
DFcolname$ID <-c(1:nrow(DFcolname))
DFcolname <-arrange(DFcolname,desc(n));DFcolname
colnames(DFcolname)

#.cancer_d는 138명인데 이걸 다 날릴 수 없으니 정리
DF$Cancer_d[is.na(DF$Cancer_d)] <- "unknown"

#.Chest_interpretation2,AP_interpretqation2 는 NA라고 해서 날릴 수 없으니 

DF$Chest_interpretation2[is.na(DF$Chest_interpretation2)] <- "no fever focus"
DF$AP_interpretation2 [is.na(DF$AP_interpretation2)] <- "no fever focus"

#.각 열별 NA 개수
a <-colSums(is.na(DF))
as.data.frame(a) %>%
   arrange(desc(a))

#.분석을 위해 임으로 다음 열중 NA는  정상 수치로 변경
DF$pH.ABGA [is.na(DF$pH.ABGA)] <-7.40
DF$Base.Excess[is.na(DF$Base.Excess )] <-0
DF$Bicarbonate [is.na(DF$Bicarbonate )] <-24
DF$BST[is.na(DF$BST)] <-100
DF$trans[is.na(DF$trans)] <-"no trans"
DF$expire_day[is.na(DF$expire_day)] <-"no expire"
DF$Dx_dc [is.na(DF$Dx_dc )] <-"no DC Dx"
DF$DC_state[is.na(DF$DC_state)] <-"no DC_state"
DF$adm_depart[is.na(DF$adm_depart)] <-"no adm_depart"
DF$ward[is.na(DF$ward)] <-"no ward"

#.ABGA,base,bicaronate 다 있는 열만 추리기위해..나중에 분석할때 의미 있는지 확인위해
# DF$pH.ABGA [is.na(DF$pH.ABGA)] <-"g"
# DF$Base.Excess[is.na(DF$Base.Excess )] <-"gg"
# DF$Bicarbonate [is.na(DF$Bicarbonate )] <-"ggg"



#.다시 위 코드 돌려

DFcolname <-as.data.frame(colSums(is.na(DF)));DFcolname
names(DFcolname) <-c("n")
DFcolname$code_names <-rownames(DFcolname);DFcolname
DFcolname$ID <-c(1:nrow(DFcolname))
DFcolname <-arrange(DFcolname,desc(n));DFcolname

DFcolname$ID2 <-c(1:nrow(DFcolname));DFcolname



#DFcolname$ID2 31까지 DF에서 제거, ID2는 결측값 많은 순서대로 번호 부여한 것, ID 는 DF에서 열 번호

a <-filter(DFcolname,ID2 %in% 1:31) # a 는 DFcolnames 에서  결측값이 너무 많은  열 WBC.Stick. 까지 이 열은 열을 없애야

b <-a$ID;b # b는 DF 에서 결측값 많이 있은 열을 제거하기위해 DF 열 이름에 대한 번호
b <-as.vector(b);b

# write.csv(na , "na.csv")
#.DFcolname 확인 후 필요 NA 있으면서 필요 없는 열 제거..하나라도 NA 있는 것 추려..

DF <-DF[,-b]
colnames(DF)
ncol(DF)
nrow(DF)
str(DF)
#. DF는 결측값이 많아 열을 아예 삭제할 열 제거한 데이터 프레임.
#. DF에서 여전히 남아 있는 NA 해결해야...

DF_na <-as.data.frame(colSums(is.na(DF)));DF_na #.각 열의 NA 개수

names(DF_na) <-c("n")

DF_na <-arrange(DF_na,desc(n));DF_na

#.E-Eosino 같은 열 이름은 filter가 안돼서 

DF <-dplyr::rename(DF,Eosino="E.Eosino",Lymp="E.Lymph",Mono="E.Mono",Neutro="E.Neutro")

nrow(DF)
colnames(DF)



DF_na <-na.omit(DF) #.na 있는 환자는  삭제해버릴까?
colnames(DF_na)
nrow(DF_na) #24명 빠졌다..

#.DF_na 로 분석하는게 낫겠어..
DF <-DF_na

str(DF)




#.열 이름의 복잡하게 되어 있으니 이거 변경해야..

a <-as.data.frame(colnames(DF));a
colnames(DF)
nrow(DF)
str(DF)

# write.csv(a , "DF_col.csv")
DF_col_yj<-read.csv("DF_col_yj.csv", header=T ,stringsAsFactors=FALSE)
colnames(DF_col_yj)
names(DF) <-DF_col_yj$colnames

#.여기 DF 가 정말 최종..이제 분석만 하면 돼
str(DF)

DF_na <-as.data.frame(colSums(is.na(DF)));DF_na

#.기저질환 정리해야하는데..

as.data.frame(table(DF$hepatic)) %>%
   arrange(desc(Freq))

as.data.frame(table(DF$HTN)) %>%
  arrange(desc(Freq))

as.data.frame(table(DF$Cancer)) %>%
  arrange(desc(Freq))

as.data.frame(table(DF$OP)) %>%
  arrange(desc(Freq))

as.data.frame(table(DF$Cardiac)) %>%
  arrange(desc(Freq))

as.data.frame(table(DF$Renal)) %>%
  arrange(desc(Freq))

as.data.frame(table(DF$Pulmonary)) %>%
  arrange(desc(Freq))

as.data.frame(table(DF$DM))%>%
  arrange(desc(Freq))

as.data.frame(table(DF$cerebral)) %>%
                arrange(desc(Freq))

colnames(DF)


##. 필요한 열만 추출
DF<-select(DF,1,2,3,4,10,11,19,20,21,22:29,38:68,70,72,73,75,77,79,81,83,85,87,89,91:99,6,7,16,30,31,69,71)
colnames(DF)

colnames(DF)

a <-c(46:54) #.기저질환 관련 열

DF <-as.data.frame(lapply(DF , function(DF) if(is.character(DF )|is.factor(DF )) gsub("unknown", "0", DF ) else(DF )))
DF <-as.data.frame(lapply(DF , function(DF) if(is.character(DF )|is.factor(DF )) gsub("-", "0", DF ) else(DF )))
DF <-as.data.frame(lapply(DF , function(DF) if(is.character(DF )|is.factor(DF )) gsub("\\+", "1", DF ) else(DF )))
DF <-as.data.frame(lapply(DF , function(DF) if(is.character(DF )|is.factor(DF )) gsub("\\?", "0", DF ) else(DF )))

DF[,46:54]

str(DF)
colnames(DF)

DF$Chest_interpretation2

#.CT에서 fever focus 여부 판단 열 'fever' 만들기

DF$fever <-NA

for(i in 1:nrow(DF))
{ 
  if(  (DF$Chest_interpretation2[i]=="no fever focus") & (DF$AP_interpretation2[i]=="no fever focus") ){ DF$fever[i] <-"no"  }
  
  else{DF$fever[i] <-"yes" }
}

colnames(DF)
table(DF$fever)

#.expire date, 응급실 퇴원 결과, 병원 퇴원 결과, CT 판독별 진단명, 퇴실시 진단명  icu 여부 추가해야.증상발현시간
#.csv 로 넘겨서 다시 불러와야..
#.Dx_adm:ER 퇴실시 즉, 입원하거나 귀가하나 시 진단명으로 dx 와 거의 같아..왜 다를때가 있는지는 모르겠어
#.응급실 퇴실시의 진단명이 중요하니깐. CT 소견보다 Dx_adm 으로 진단명 분류해야..

add <-select(DF,N_ID,name,Chest_interpretation2,AP_interpretation2,ward,Dx_adm,Dx_dc,DC_state,onset_D,onset_H,expire_day,route,trans,visit_by)
# write.csv(add, "add.csv")
nrow(add)

Dx_adm_table <-as.data.frame(table(add$Dx_adm)) %>%
  arrange(desc(Freq));Dx_adm_table
# write.csv(Dx_adm_table , "Dx EM.csv") 

add$Dx_adm_code <-NA
Dx_class<-read.csv("Dx classification.csv", header=T ,stringsAsFactors=FALSE)
Dx_class <-select(Dx_class,-X)


for(i in 1:nrow(add))
{

if( add$Dx_adm[i] %in% Dx_class[,1] ) {add$Dx_adm_code[i] <-colnames(Dx_class[1])}
  else if (add$Dx_adm[i] %in% Dx_class[,2] ) {add$Dx_adm_code[i] <-colnames(Dx_class[2])}
  else if (add$Dx_adm[i] %in% Dx_class[,3] ) {add$Dx_adm_code[i] <-colnames(Dx_class[3])}
  else if (add$Dx_adm[i] %in% Dx_class[,4] ) {add$Dx_adm_code[i] <-colnames(Dx_class[4])}
  else if (add$Dx_adm[i] %in% Dx_class[,5] ) {add$Dx_adm_code[i] <-colnames(Dx_class[5])}
  else if (add$Dx_adm[i] %in% Dx_class[,6] ) {add$Dx_adm_code[i] <-colnames(Dx_class[6])}
  else if (add$Dx_adm[i] %in% Dx_class[,7] ) {add$Dx_adm_code[i] <-colnames(Dx_class[7])}
  else if (add$Dx_adm[i] %in% Dx_class[,8] ) {add$Dx_adm_code[i] <-colnames(Dx_class[8])}
  else if (add$Dx_adm[i] %in% Dx_class[,9] ) {add$Dx_adm_code[i] <-colnames(Dx_class[9])}
else{add$Dx_adm_code[i] <-"etc"}      
}


Dx_dc_table<-as.data.frame(table(add$Dx_dc)) %>%
  arrange(desc(Freq));Dx_dc_table
# write.csv(Dx_dc_table , "Dx DC.csv")
add$D_dc_code <-NA

for(i in 1:nrow(add))
{
  
  if( add$Dx_dc[i] %in% Dx_class[,1] ) {add$Dx_dc_code[i] <-colnames(Dx_class[1])}
  else if (add$Dx_dc[i] %in% Dx_class[,2] ) {add$Dx_dc_code[i] <-colnames(Dx_class[2])}
  else if (add$Dx_dc[i] %in% Dx_class[,3] ) {add$Dx_dc_code[i] <-colnames(Dx_class[3])}
  else if (add$Dx_dc[i] %in% Dx_class[,4] ) {add$Dx_dc_code[i] <-colnames(Dx_class[4])}
  else if (add$Dx_dc[i] %in% Dx_class[,5] ) {add$Dx_dc_code[i] <-colnames(Dx_class[5])}
  else if (add$Dx_dc[i] %in% Dx_class[,6] ) {add$Dx_dc_code[i] <-colnames(Dx_class[6])}
  else if (add$Dx_dc[i] %in% Dx_class[,7] ) {add$Dx_dc_code[i] <-colnames(Dx_class[7])}
  else if (add$Dx_dc[i] %in% Dx_class[,8] ) {add$Dx_dc_code[i] <-colnames(Dx_class[8])}
  else if (add$Dx_dc[i] %in% Dx_class[,9] ) {add$Dx_dc_code[i] <-colnames(Dx_class[9])}
  else{add$Dx_dc_code[i] <-"etc"}      
}


DC_state_table <-as.data.frame(table(add$DC_state)) %>%
  arrange(desc(Freq));DC_state_table

add$DC_state <-gsub("no DC_state","EM_DC", x=add$DC_state)
add$DC_state <-gsub("전원 (1,2차)", "1차 전원",  x=add$DC_state)
add$DC_state <-gsub("전원 (3차)","3차전원", x=add$DC_state)

#.no ward,no_DC_state,no Dx_DC 는 다 EM 귀가


#.CT 에  대한 코드 구기
Dx_class_CT<-read.csv("Dx claasification_CT.csv", header=T ,stringsAsFactors=FALSE)
add$chest_code <-NA
add$abdome_code <-NA

chest_table <-as.data.frame(table(add$Chest_interpretation2)) %>%
  arrange(desc(Freq));chest_table
write.csv(chest_table , "chest_table.csv")

colnames(Dx_class_CT)

#.no fever focus는 그대로 놔두어야..

for(i in 1:nrow(add))
{
  if      (add$Chest_interpretation2[i] %in% Dx_class_CT[,1] ) {add$chest_code[i] <-colnames(Dx_class_CT[1])}
  else if (add$Chest_interpretation2[i] %in% Dx_class_CT[,2] ) {add$chest_code[i] <-colnames(Dx_class_CT[2])}
  else if (add$Chest_interpretation2[i] %in% Dx_class_CT[,3] ) {add$chest_code[i] <-colnames(Dx_class_CT[3])}
  else if (add$Chest_interpretation2[i] %in% Dx_class_CT[,4] ) {add$chest_code[i] <-colnames(Dx_class_CT[4])}
  else if (add$Chest_interpretation2[i] %in% Dx_class_CT[,5] ) {add$chest_code[i] <-colnames(Dx_class_CT[5])}
  else if (add$Chest_interpretation2[i] %in% Dx_class_CT[,6] ) {add$chest_code[i] <-colnames(Dx_class_CT[6])}
  else if (add$Chest_interpretation2[i] %in% Dx_class_CT[,7] ) {add$chest_code[i] <-colnames(Dx_class_CT[7])}
  else if (add$Chest_interpretation2[i] %in% Dx_class_CT[,8] ) {add$chest_code[i] <-colnames(Dx_class_CT[8])}
  else{add$chest_code[i] <-"etc"}      
}

as.data.frame(table(add$chest_code)) %>%
  arrange(desc(Freq))

 abdomen_table <-as.data.frame(table(add$AP_interpretation2)) %>%
  arrange(desc(Freq));abdomen_table
 write.csv(abdomen_table , "abdomen_table.csv")
 

 for(i in 1:nrow(add))
 {
   if      (add$AP_interpretation2[i] %in% Dx_class_CT[,1] ) {add$abdomen_code[i] <-colnames(Dx_class_CT[1])}
   else if (add$AP_interpretation2[i] %in% Dx_class_CT[,2] ) {add$abdomen_code[i] <-colnames(Dx_class_CT[2])}
   else if (add$AP_interpretation2[i] %in% Dx_class_CT[,3] ) {add$abdomen_code[i] <-colnames(Dx_class_CT[3])}
   else if (add$AP_interpretation2[i] %in% Dx_class_CT[,4] ) {add$abdomen_code[i] <-colnames(Dx_class_CT[4])}
   else if (add$AP_interpretation2[i] %in% Dx_class_CT[,5] ) {add$abdomen_code[i] <-colnames(Dx_class_CT[5])}
   else if (add$AP_interpretation2[i] %in% Dx_class_CT[,6] ) {add$abdomen_code[i] <-colnames(Dx_class_CT[6])}
   else if (add$AP_interpretation2[i] %in% Dx_class_CT[,7] ) {add$abdomen_code[i] <-colnames(Dx_class_CT[7])}
   else if (add$AP_interpretation2[i] %in% Dx_class_CT[,8] ) {add$abdomen_code[i] <-colnames(Dx_class_CT[8])}
   else{add$abdomen_code[i] <-"etc"}      
 }
 

as.data.frame(table(add$abdomen_code)) %>%
   arrange(desc(Freq))
 
# #.APN, colitis 가 etc로 구분되고 있어 이거 찾아야해..
# a <-select(add,AP_interpretation2,abdomen_code)
# compare <-write.csv(a,"compare.csv")
# 
# add$AP_interpretation2[641]
# Dx_class_CT[,4]
# add$AP_interpretation2[17]
# Dx_class_CT[,5]
# add$AP_interpretation2[305]
# Dx_class_CT[,4]
# add$AP_interpretation2[334]
# Dx_class_CT[,4]
# add$AP_interpretation2[716]
# Dx_class_CT[,5]
# add$AP_interpretation2[17]

ward <-ward_table <-as.data.frame(table(add$ward)) %>%
   arrange(desc(Freq));ward
 
ward_code <-NA # 앞자리 21,23,24 이면 ICU

icu <-c("21","23","24")
 
add$ward <- gsub("no ward", "EM_DC", add$ward)
 
for(i in 1:nrow(add))
{ 
  if(str_sub(add$ward[i],1,2) %in% icu  ){ add$ward_code[i] <-"ICU" }
  else if(str_sub(add$ward[i],1,2) =="EM"){ add$ward_code[i] <-"EM_DC" }
  else{ add$ward_code[i] <-"GW"}
}

colnames(add)

#.증상 발생시간 구하기 N_ID에서 내원날짜, onset_D 서로 빼기
 
#.기본 expire_day 는 날짜가 8자리가 아니야..년도 뒤에 0 추가 & 달 뒤에 0 추가 됨
#."no expire" 를 2222 0 01 0 01 로 바꾸고 후에 expired_day 열에서 5,8 번째 자리 0 제거
#.select(filter(add,expire_day !="no expire"),N_ID,expire_day)

add$expire_day <-gsub("no expire","2222001001",add$expire_day)

add$expire_day <- str_c( str_sub(add$expire_day,1,4) ,str_sub(add$expire_day,6,7) ,str_sub(add$expire_day,9,10),sep = "")

add$visit_D <-str_sub(add$N_ID,1,8)
vv= c("expire_day","onset_D","visit_D")
add[ , vv] = lapply(add[ , vv], ymd)

add$sx_duration <-NA
add$sx_duration <-as.numeric( add$visit_D-add$onset_D )

expire_day_table <-as.data.frame(table(add$expire_day)) %>%
  arrange(desc(Freq));expire_day_table

#.add 하고 DF 하고 합쳐야..

colnames(add)
colnames(DF)

add <-select(add,-2,-3,-4)

# add <-select(DF,N_ID,ward,Dx_adm,Dx_dc,DC_state,onset_D,onset_H,expire_day,route,trans,visit_by,Dx_adm_code,Dx_dc_code,chest_code)


add <-select(add,N_ID,ward,Dx_adm,Dx_dc,DC_state,onset_D,onset_H,expire_day,route,trans,visit_by,Dx_adm_code,Dx_dc_code,chest_code,abdomen_code,ward_code,visit_D,sx_duration)
DF <-select(DF,-ward,-Dx_adm,-Dx_dc,-DC_state,-onset_D,-onset_H,-expire_day,-route,-trans,-visit_by)


DF_fever <-full_join(add, DF, by = "N_ID")
colnames(DF_fever)
#.분석에 필요한 열만 추려서 csv 로
str(DF_fever)
DF_fever <-select(DF_fever,3,19,20,21,12,4,13,14,15,22:60,63:71,5,6,8,17,18,75:76,77,83,2,16,83)


write.csv(DF_fever,"DF_fever.csv")
colnames(DF_fever)

# write.csv(DF,"DF_fever_ABGA.csv")

# DF_ct <-select(DF,1,2,3,44,45,83,84)
# write.csv(DF_ct, "DF_ct.csv")

####### 여기까지가 data cleanising########################

#..결측값의 처리, 논문의 의도에 맞게 
# ABGA를 안한 경우는 필요 없는 경우가 대부분일 것으로 추정하기 때문에 정상 기본 값을 넣어줘야해..
# 
# DF_A_lab$"Base Excess"[is.na(DF_A_lab$"Base Excess")] <- 0
# DF_A_lab$Bicarbonate[is.na(DF_A_lab$Bicarbonate)] <- 24
# DF_A_lab$Hb[is.na(DF_A_lab$Hb)] <- 12
# DF_A_lab$O2Saturation[is.na(DF_A_lab$O2Saturation)] <- 99
# DF_A_lab$pCO2[is.na(DF_A_lab$pCO2)] <- 40
# DF_A_lab$ "pH ABGA"[is.na(DF_A_lab$ "pH ABGA")] <- 7.4
# DF_A_lab$Platelet[is.na(DF_A_lab$Platelet)] <- 500000
# DF_A_lab$PO2[is.na(DF_A_lab$PO2)] <- 75
# DF_A_lab$RBC[is.na(DF_A_lab$RBC)] <- 4
# DF_A_lab$WBC[is.na(DF_A_lab$WBC)] <- 6
# 
# 
# DD<-bind_cols(DF_A_info,DF_A_lab)