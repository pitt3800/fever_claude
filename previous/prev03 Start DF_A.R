# install.packages("webr") #.numSummary(height)
# install.packages("tidyverse")
# install.packages("devtools")
# devtools::install_github("tidyverse/purrr") # ggplot2, purrr, tibble, dplyr, tidyr, stringer readr, farcats 같이 설치 돼.. 이것 설치 하고 또 설치하면 오류나기도....
# install.packages("purr")
# install.packages("purrr")
# install.packages("reshape")
# install.packages("gridExtra")
# install.packages("gapminder")
# install.packages("plyr")
# library(gapminder)
# install.packages("remotes")
# remotes::install_github("cardiomoon/multipleROC")

# 
# library(multipleROC)
# library(pROC)
# library(readxl)
# library(dplyr)
# library(tidyr)
# library("ggplot2")
# library("ggthemes")
# library(lubridate)
# library(stringr)
# library(moonBook)
# library(multipleROC)
# require(survival)
# library(survminer)
# # library(webr)
# library(ggplot2)
# library(reshape)
# library(tidyverse)
# library(readxl)
# library(gridExtra)
# library(RColorBrewer)
# library(scales)
# library(car)
# library(gapminder)
# library(webr)
# library(stringr)

#.map_dfr

# rm(list=ls())

##.데이터 불러온 다음 필요한 열만 추린다음 열 이름도 바꾸고 ,class 도 바꿔.
##.DF:lab 결과 없는 data frame, DFlab: ID 하고 lab 값만 있는 data frame, 
## DF_A: DF에 lab 변수명을 열로 만들고 DFlab을 이용해서 for 구문으로 lab 값을 채우기

#.fever 에 대해서는 EMR.R에서 including criter에 해당하는 DF 구해서 fever.xlsx 로 구했어.. DF는 구할 필요 없어..
##.우선 lab 결과 없는  DF 만들기- 중복 데이터 처리



DF <-read.csv("fever.csv", header=T ,stringsAsFactors=FALSE)

#.DF 정리

DF <-select(DF,-X)

# 필요한 열만 선택- 열이름만 있는 CSV 파일 불러서 선택하는 걸로

info<-read.csv("emr_info.csv", header=T ,stringsAsFactors=FALSE)
DF <-select(DF,info$info)
colnames(DF) <-info$X

DF$N_ID <-paste(DF$Visit_day,DF$ID,sep='_')

DF = DF[-which(duplicated(DF$N_ID)),] #. 같은 날 두번 온 환자 제외
DF <-arrange(DF,N_ID)
nrow(DF) #2035
DF <-select(DF,N_ID,2:44)

str(DF)



#1 다른 날 방문한 같은 ID 환자는 서로 다른 case로 처리해야 ,나중에 for 구문을 쓰면서 lab에 대한  변수 채울때 반복 내원한 환자의
#경우 등록번호만으로 채울때 오류남
#. 그래서 내원일자와 ID를 조합한 새로운 ID로 만들고 duplicate로 중복 case 제거(하루에 두번 내원환자의 경우는 통계적으로 제거해야하니깐
#. 그리고 이걸로 for 구문 돌리면서 lab 채우면 됨..

#.rename은 다른 package에서도 쓰이므로 dplyr::rename 써줘야...


##.DFlab 정리

# DFlab에서 같은날짜에 같은 코드명은 하나만 남게 정리 
#. 엑셀에서 코드는 다르나 코드명이 같은 경우 ex UA에서 WBC, blood에서 WBC.. 제거해야하고 코드+ 코드명으로 새로운 코드명 만들어서 for 구문 둘려야하고 
#  또 같은 날 두번 lab 한 환자도  for를 쓰면 오류 날 수 있어 이에 대해서는 코드 + 내원일로  으로 새로운 변수 만들고 duplicate로 제거해야.




DFlab<-read_excel(path="feverlab.xlsx", sheet="Sheet1",col_names = TRUE)
DFlab$N_ID <-paste(DFlab$내원일,DFlab$IDNO,sep='_')
DFlab <-select(DFlab,N_ID,1:27)
DFlab <-arrange(DFlab,N_ID)
nrow(DFlab) #442200

colnames(DFlab)
# write.csv(colnames(DFlab), "DFlab_colnames.csv")
DFlab_colnames<-read.csv("DFlab_colnames.csv", header=T ,stringsAsFactors=FALSE)

colnames(DFlab)<-DFlab_colnames$X.1

# DFlab 에서 필요한 lab 값을 을 DF에 열로 붙이기 위해 DF 열값으로 들어갈 필요한 lab 이름 추출-
#하루에 두번 한 경우가  있어 duplicated로 제거해서 하나만 남겨야해
# duplicated은 맨 처음값만 남기기 때문에  이를 고려하여 날짜별로 정렬을 해야해..

DFlab = DFlab[!duplicated(DFlab[,c("N_ID","code")]),]


#.result 값이 NA 일경우 for 구문 안돌아가..이 환자는 그냥 사망 또는 lab 없이 입원한 것 같아..EMR 확인 필요
#.이환자는 DFlab 과 DF에서 다 제외해야..

sum(is.na(DFlab$result))
result_na <-DFlab[is.na(DFlab$result),]

nrow(DFlab) #130012

DFlab <-filter(DFlab,!(N_ID %in% result_na$N_ID) )

DF <-filter(DF,!(N_ID %in% result_na$N_ID) )


#.코드명이 검체에 따라 같을 수 있어 처방코드+코드명으로 새로운 변수 만들고
#.unique 를 이용해 필요한 코드명 찾아서 CSV 만들고 number에서 필요없는 것 삭제 후 다시 불러와.

DFlab <-mutate(DFlab,new_code =paste(code_name,code,sep="!"))

code_name <-unique(DFlab$new_code)
  
code_name <-data.frame(code_name)    

code_name <-separate(code_name,code_name,into=c("code","codename"),sep="!")

# write.csv(code_name, "code_name.csv") #.number에서 필요없는 것 삭제하고 다시 불러와 나중에 다시 자세히 해봐

code_name <-read.csv("code_name.csv", header=T ,stringsAsFactors=FALSE)

code_name$new_code <-paste(code_name$code,code_name$codename,sep="!")

#DF_A 만들기
# 

a <-as.vector(code_name$new_code)
b <-"10000"

c <-data.frame(a,b)

d <-spread(c,a,b);d

DF_A <-bind_cols(DF,d)

str(DF_A)

#. DF 에 code_name을 열로  넣고 DF_A 만들고 DF_A 와 DFlab 을 조합해서 for 구문으로 채워넣어야해...


nrow(DFlab)

# DFlab$new_code==colnames(DF_A) 는 서로 같네...
# DF_A의 lab 값을 DFlab을 참조해서 채우려면 DF_A의 ID 와 DF_lab의 ID 가 같은 값중에서 DFlab$new_code 하고
# colnames(DF_A) 같을때 해당 lab 값의  결과를  


# DF_A[i,j] 의 값은  
# DF_A$N_ID 하고 DFlab$N_ID 같고  DF_A colnames 하고  DFlab$new_code 같을 때 DFlab$결과 값이야.. 
# 그럼 if 아래 코딩에서 if 부분 없어도 되는 것 아냐..이전 dyspen에서는 ER 내원시 모든 lab 값을 받았기 때문에 DF_A에
# 포함되는 lab 값인지 확인해야하기 때문에 이게 있어야할 것 같으나 없어도 돼네...

#.두 데이터 참고 해서 칸 채울때 반드시 해당 셀 class가 값아야해..

for(i in 1:nrow(DF_A)){
  for(j in 45:108){

    if(
       colnames(DF_A[j]) %in% DFlab$new_code[which(DFlab$N_ID==DF_A$N_ID[i])]
       )

    {
      DF_A[i,j] <-DFlab$result[
                            which(
                              (DFlab$N_ID==DF_A$N_ID[i]) & (DFlab$new_code==colnames(DF_A)[j])
                                 )
                            ]
    }
    else {DF_A[i,j] <- NA}



                   }
                        }



# 
# for(i in 1:nrow(DF_A)){
#   for(j in 45:108){ 
#     
#       DF_A[i,j] <-DFlab$result[
#         which( 
#                (DFlab$N_ID==DF_A$N_ID[i]) & (DFlab$new_code==colnames(DF_A)[j]) 
#              )
#                              ]  
#    
# 
#                     }
#                         }

# 이렇게 if가 없으면 오류가 나 왜냐면 어떤 환자에서  amylase 검사를 안나가는 경우 위 코드로 돌리면 DF_A[i,j]에 해당하는 DFlab$result 에 amylase 값이 없어 오류가 나


#.합쳐진 lab이름 원래 대로 간단히: DF_A를 DF_A_info: 정보만 있는, DF_A_lab: lab만으로 나누어서 DF_A_lab의 변수명을 names() 이용해서
#.변환시킨 후 다시 합쳐

colnames(DF_A)
joined_name <-colnames(DF_A[,c(45:108)])

joined_name <-as.data.frame(joined_name)

joined_name <-joined_name %>% separate(joined_name, into=c("code_name", "code"), sep="!")

new_name <-joined_name$code_name

DF_A_info <-DF_A[,c(1:44)]
DF_A_lab <-DF_A[,c(45:108)]

names(DF_A_lab) <-new_name

DF_A <-bind_cols(DF_A_info,DF_A_lab)

str(DF_A)

write.csv(DF_A, "DF_A.csv")


