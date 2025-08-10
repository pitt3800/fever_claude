#install.packages("webr") #.numSummary(height) 
#install.packages("tidyverse")
#install.packages("devtools")
# devtools::install_github("tidyverse/purrr") # ggplot2, purrr, tibble, dplyr, tidyr, stringer readr, farcats 같이 설치 돼.. 이것 설치 하고 또 설치하면 오류나기도....
#install.packages("purr")
#install.packages("purrr")
#install.packages("reshape")
#install.packages("gridExtra")
# install.packages("gapminder")
# install.packages("plyr")
# library(gapminder)
#library(dplyr)
#.library(purr)
#.library(purrr)


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


#.map_dfr

rm(list=ls())

##.데이터 불러온 다음 필요한 열만 추린다음 열 이름도 바꾸고 ,class 도 바꿔.

DFraw <-read_excel(path="fever.xlsx")
colnames(DFraw)

#.DFlab의 '결과'가를  DF_A에 넣을때 DF_A의 해당되는 열도 같은 class 여야해..

DFraw$AGE <-as.numeric(DFraw$AGE)
DFraw$결과 <-as.numeric(DFraw$결과)
DFraw$GEND <-as.factor(DFraw$GEND ) 
DFraw$의식상태 <-factor(DFraw$의식상태,order=TRUE,levels=c("A","P","V"),labels=c("A","P","V"))
DFraw$구역 <-as.factor(DFraw$구역 )
DFraw$입원과 <-as.factor(DFraw$입원과)
DFraw$입원병동 <-as.factor(DFraw$입원병동)

# DFlab$결과 <-as.numeric(DFlab$결과)
# 
# DF_A[ , c(24:33)] = lapply(DF_A[ , c(24:33)], as.numeric)
# 
# str(DFraw)


##.우선 명단만 있는 DF 정리 중복 데이터 처리

DF<-DFraw %>%
  select(-처방코드,-코드명,-결과)
 
D <-as.data.frame( table(DF$IDNO) )
D <-D[D$Freq>1,]#.D는 코드명에 따라 Freq가 달라질듯

# 다른 날 방문한 같은 ID 환자는 서로 다른 case로 처리 ,나중에 for 구문을 쓰면서 lab 변수 채울때 반복 내원한 환자의
# 경우 등록번호만으로 채우면 오류나므로 내원일자 + 등록번호 로 새로운 ID 만들어,
# DFlab에서 같은날짜에 같은 코드명은 하나만 남게 정리
#    또 같은 날 두번 lab 한 환자도 오류 for를 쓰면 오류 날 수 있어 이에 대해서는 코드+ 코드명으로 새로운 변수 만들어 
#    lab에서 코드는 다르나 코드명이 같은 경우 ex UA에서 WBC, blood에서 WBC..

DF <-mutate(DF,new_ID =paste(IDNO,내원일,sep="-"))

nrow(DF) #197344

DF = DF[!duplicated(DF[,"new_ID"]),]



DF <-select(DF,new_ID,"IDNO","PNME","AGE","GEND","의식상태","HIBP","LOBP","맥박","호흡수", "체온","SPO2",
            "구역","입원과","입원병동","내원일","퇴실일","입원일","퇴원일", "응급퇴실 진단명","퇴원상병명", 
            "퇴원상태", "사망일")

# names(DF)[names(DF)=="IDNO"] <- c("ID") 


DF <- rename(DF,ID=IDNO,name=PNME,sex=GEND,mental_status=의식상태,SBP=HIBP,DBP=LOBP,HR=맥박,RR=호흡수,
             BT=체온,SPO2=SPO2,location_EM=구역,Depart=입원과, 
             Ward=입원병동,Visit_day=내원일,DC_day= 퇴실일,EM_Dx="응급퇴실 진단명", Dc_dx=퇴원상병명, expire_day=사망일)



str(DF)




###

DFlab <-select(DFraw,IDNO,PNME,내원일,처방코드,코드명, 결과)

#.DFlab 에서 필요한 lab 값을 을 DF에 붙이기 위해 DF 열값으로 들어갈 필요한 lab 이름 추출-
# 하루에 두번 한 경우가  있어 duplicated로 제거해서 하나만 남겨야해
# duplicated은 맨 처음값만 남기기 때문에  이를 고려하여 날짜별로 정렬을 해야해..

#.코드명이 검체에 따라 같을 수 있어 처방코드+코드명으로 새로운 변수 만들고
#.unique 를 이용해 필요한 코드명 찾아서 CSV 만들고 number에서 필요없는 것 삭제 후 다시 불러와.


DFlab <-mutate(DFlab,new =paste(코드명,처방코드,sep="!"))

code_name <-unique(DFlab$new)
  
code_name <-data.frame(code_name)    

code_name <-separate(code_name,code_name,into=c("code","codename"),sep="!")

code_name <-mutate(code_name,new=paste(codename,code,sep="!"))


# write.csv(code_name, "code_name.csv") #.number에서 필요없는 것 삭제하고 다시 불러와 나중에 다시 자세히 해봐

code_name <-read.csv("code_name.csv", header=T ,stringsAsFactors=FALSE)



a <-as.vector(code_name$new)
b <-1:10

c <-data.frame(a,b)

d <-spread(c,a,b)

DF_A <-bind_rows(DF,d)

str(DF_A)

#.DFlab의 '결과'가를  DF_A에 넣을때 DF_A의 해당되는 열도 같은 class 여야해..
DF_A[ , c(24:33)] = lapply(DF_A[ , c(24:33)], as.numeric)


#. DF 에 code_name 넣고 DF_A 만들고 DF_A 와 DFlab 을 조합해서 for 구문으로 채워넣어야해...

DFlab <-mutate(DFlab, new_ID=paste(IDNO,내원일,sep="-"))

DFlab = DFlab[!duplicated(DFlab[,c('new_ID','new')]),]


nrow(DFlab)





for(i in 1:nrow(DF_A)){
  for(j in 24:33){ 
    
    if(
       colnames(DF_A[j]) %in% DFlab$new[which(DFlab$new_ID==DF_A$new_ID[i])]
       )
    
    {
      DF_A[i,j] <-DFlab$결과[
                            which( 
                              (DFlab$new_ID==DF_A$new_ID[i]) & (DFlab$new==colnames(DF_A)[j]) 
                                 )
                            ]  
      }
    else {DF_A[i,j] <- NA}
      
      
  
  }
}

#.테이블 채웠으니 exclusion도 하고..이름도 바꾸고 class 도 바꾸고 열도 순서에 맞게하고 

DF_A <-DF_A %>% filter(AGE>65)


j_name <-colnames(DF_A[,c(24:33)])

j_name <-as.data.frame(j_name)

j_name <-j_name %>% separate(j_name, into=c("code_name", "code"), sep="!")

DF_A_lab <-DF_A[,c(24:33)]
DF_A_info <-DF_A[,c(1:23)]


names(DF_A_lab)<-j_name$code_name



str(DF_A_lab)



#..결측값의 처리, 논문의 의도에 맞게 
  # ABGA를 안한 경우는 필요 없는 경우가 대부분일 것으로 추정하기 때문에 정상 기본 값을 넣어줘야해..

DF_A_lab$"Base Excess"[is.na(DF_A_lab$"Base Excess")] <- 0
DF_A_lab$Bicarbonate[is.na(DF_A_lab$Bicarbonate)] <- 24
DF_A_lab$Hb[is.na(DF_A_lab$Hb)] <- 12
DF_A_lab$O2Saturation[is.na(DF_A_lab$O2Saturation)] <- 99
DF_A_lab$pCO2[is.na(DF_A_lab$pCO2)] <- 40
DF_A_lab$ "pH ABGA"[is.na(DF_A_lab$ "pH ABGA")] <- 7.4
DF_A_lab$Platelet[is.na(DF_A_lab$Platelet)] <- 500000
DF_A_lab$PO2[is.na(DF_A_lab$PO2)] <- 75
DF_A_lab$RBC[is.na(DF_A_lab$RBC)] <- 4
DF_A_lab$WBC[is.na(DF_A_lab$WBC)] <- 6


DD<-bind_cols(DF_A_info,DF_A_lab)
