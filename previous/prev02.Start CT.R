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



ct <-read_excel(path="ct.xlsx", sheet="Sheet1",col_names = TRUE)
ct$N_ID <-paste(ct$내원일자,ct$등록번호,sep='_')
ct <-select(ct,N_ID,1:9)
ct <-arrange(ct,N_ID)



#.ct.xlsx 정리

ct <-dplyr::rename(ct,ID="등록번호",Visit_day="내원일자",Visit_time="내원시간",name= "환자명", sex="성별",AGE="나이",ct_code ="처방코드",ct_name="처방명",interpretation="판독결과")

#vlength(unique(ct$환자명)) #1253 CT는 한번이라도 찍은 사람
#.이렇게 하면 안돼지 왜냐면 3년동안 날짜를 달리해서 재내원한 환자가 있기 때문

length(unique(ct$N_ID)) #.1474 반복 내원한 환자 포함하여 한번이라도 내원한 환자
ct$number <-rownames(ct) #.나중에 3번 4번 CT 찍은 환자 열번호로 제거하기 위해 

#.chest CT & APCT 2번 이상 찍은 사람만 select 
ct_table<-data.frame(table(ct$N_ID)) ;ct_table  # 환자별 CT 찍은 횟수

names(ct_table) <-c("N_ID","freq");ct_table

ct_table2 <- data.frame(table(ct_table$freq));ct_table2#.CT 시행 횟수별 환자수
names(ct_table2) <-c("N_ID","freq");ct_table2 #.CT 시행 횟수별 환자수 CT 2번 찍은 환자  891

ct2_ID <-filter(ct_table,freq>1);ct2_ID #.CT 두번 이상 시행한  내원한 환자의 N_ID만  있는 환자 ,
#여기서 생각해야할 것은 chest CT만 두번 APCT만 두번 찍은 환자가 있다는 것.20200806_15607605은 제외해야 왜냐면 이환자는 chest CT만 두번 찍었어,20200813_14839810 이환자는 AP만 두번 찍었어
#.이 ID는 chest CT, ab CT 나누어서 ID 비교해서 찾음
ct2_ID <-filter(ct2_ID,N_ID !="20200806_15607605")
ct2_ID <-filter(ct2_ID,N_ID !="20200813_14839810")





table(ct2_ID$freq) # 2번 이상 시행한 환자의 시행 횟수별 환자수

ct2 <-filter(ct,N_ID %in% ct2_ID$N_ID);ct2 #.2번 이상 찍은 환자 DF
ct_table2 <- data.frame(table(ct2$N_ID));ct_table2
as.data.frame(table(ct_table2$Freq))

nrow(ct2) # 1797

#.한번 내원시 CT 3번 이상 찍은 환자 select
ct3_ID <-filter(ct_table,freq>2);ct3_ID
#.non-e, e 두번 찍은 경우이네.. 이 환자들은 ct 확인 후 non-e CT는제외해야..하나하나 확인해야할 듯.
names(ct3_ID) <-c("N_ID","freq");ct3_ID

ct3 <-filter(ct2,N_ID %in%  ct3_ID$N_ID);ct3

#.필요없는 CT 를 rownames로 제거하기 위해 ct$number <-rownames(ct) 를 앞의 코드에 추가함
#.ct_ct3를 write 해서 필요 없는 것 확인 

# write.csv(ct3, "ct3 .csv")


d <-c(578,1150,1824,2042,2256,2259,2262)

ct_final <-filter(ct2,!(number %in% d)) 
nrow(ct_final)
#.ct_final 은 ct.csv에서 CT 2번 이상 찍은 환자 중에서 3번 이상 찍은 환자의 CT 제외한 CT 판독소견


nrow(ct_final) # 1790명...2번씩 찍었으니깐 1792/2= 895 이게 최종 including 환자 수

#.long data를 short data로 즉 각 환자별 CT 에 관련된행이 2개니깐 이걸 열로 
#.CT chest 관련 행 filter, CT abomen 관련행 filter  두 DF에서 열 합치기

ct_final_chest<-filter(ct_final,str_detect(ct_name,"CHEST"));ct_final_chest
ct_final_chest <-arrange(ct_final_chest,N_ID)

ct_final_abdomen<-filter(ct_final,!str_detect(ct_name,"CHEST"));ct_final_abdomen
ct_final_abdomen<-arrange(ct_final_abdomen,N_ID)

write.csv( ct_final_chest, "chest .csv")
write.csv( ct_final_abdomen, "abdomen .csv")



ct_final_chest <-dplyr::rename(ct_final_chest,Chest_CT=ct_name,Chest_interpretation=interpretation)
ct_final_abdomen<-dplyr::rename(ct_final_abdomen,AP_CT=ct_name,AP_interpretation=interpretation)

ct_final <-cbind(ct_final_chest,select(ct_final_abdomen,9:11) );ct_final

#.결국 ct_final 에 있는 N_ID가 가장 최종의 including cretiera
#.DF에서건, base_result에서건,DFlab에서건 여기에 포한된 것만 추리면 돼...

# write.csv(select(ct_final,Chest_interpretation,AP_interpretation) , "interpretation.csv")

#.CT 판독 소견을 추려야해...ex> pneumonia, utir

nrow(ct_final)

ct<-ct_final 
nrow(ct)

#열이름 number 이 2개 있으면 아래서 filter 쓸때 오류나니깐 지워
colnames(ct)
ct <-select(ct,1:10,12,13)

ct$Chest_interpretation2 <-NA
ct$AP_interpretation2 <-NA


#.이렇게 c() 로 묽으면 안되네

# for(i in 1:nrow(ct))
# { 
#   if( str_detect(ct$Chest_interpretation[i], c("pneumonia","consolidatin", "GGO","centrilobular opacity"," infectious bronchiolitis" ))){ct$Chest_interpretation2[i] <-"pneumonia " }
#   else{ ct$Chest_interpretation2[i] <-"11"}
# }



# 이렇게 하면 대소문자 구분해서 소문자만 나오네
# for(i in 1:nrow(ct))
# {
#   if( str_detect(ct$Chest_interpretation[i], "pneumonia")|
#       str_detect(ct$Chest_interpretation[i], "consolidation"))
#     {ct$Chest_interpretation2[i] <-"pneumonia " }
#   else{ ct$Chest_interpretation2[i] <-"22"}
# }

for(i in 1:nrow(ct))
{
  if( str_detect(ct$Chest_interpretation[i], fixed("pneumonia",ignore_case = TRUE))|
      str_detect(ct$Chest_interpretation[i], fixed("consoli",ignore_case = TRUE))|
      str_detect(ct$Chest_interpretation[i], fixed("GGO",ignore_case = TRUE))|
      str_detect(ct$Chest_interpretation[i], fixed("centrilobular opacity",ignore_case = TRUE))|
      str_detect(ct$Chest_interpretation[i], fixed("infectious bronchiolitis",ignore_case = TRUE))
  )
  {ct$Chest_interpretation2[i] <-"pneumonia" }
  else if (str_detect(ct$Chest_interpretation[i], fixed("aortitis",ignore_case = TRUE)))
  {ct$Chest_interpretation2[i] <-"aortitis" }
  else if (str_detect(ct$Chest_interpretation[i], fixed("infection",ignore_case = TRUE)))
  {ct$Chest_interpretation2[i] <-"infection"}
  
  else if (str_detect(ct$Chest_interpretation[i], fixed("cavitary lesion",ignore_case = TRUE)))
  {ct$Chest_interpretation2[i] <-"cavitary lesion"}
  else if (str_detect(ct$Chest_interpretation[i], fixed("empyema",ignore_case = TRUE)))
  {ct$Chest_interpretation2[i] <-"empyema"}
  else if (str_detect(ct$Chest_interpretation[i], fixed("Active Tbc",ignore_case = TRUE)))
  {ct$Chest_interpretation2[i] <-"Active Tbc"}
  else if (str_detect(ct$Chest_interpretation[i], fixed("tbc pericarditis",ignore_case = TRUE)))
  {ct$Chest_interpretation2[i] <-"tbc pericarditis"}
  else if (str_detect(ct$Chest_interpretation[i], fixed("Joint fluid collection",ignore_case = TRUE)))
  {ct$Chest_interpretation2[i] <-"Joint fluid collection"}
  else if (str_detect(ct$Chest_interpretation[i], fixed("Necrotizing pneumonioa",ignore_case = TRUE)))
  {ct$Chest_interpretation2[i] <-"Necrotizing pneumonioa"}
  else if (str_detect(ct$Chest_interpretation[i], fixed("Osteomyelitis",ignore_case = TRUE)))
  {ct$Chest_interpretation2[i] <-"Osteomyelitis"}
  else if (str_detect(ct$Chest_interpretation[i], fixed("bronchopneuomnia",ignore_case = TRUE)))
  {ct$Chest_interpretation2[i] <-"bronchopneuomnia"}
  else if (str_detect(ct$Chest_interpretation[i], fixed("R/O Reactivation of Tb",ignore_case = TRUE)))
  {ct$Chest_interpretation2[i] <-"R/O Reactivation of Tb"}
  
  else{ ct$Chest_interpretation2[i] <-NA}
}


#.판독 소견에 "No visible pneumonia" 가 있어 이거 해결해야..
#.ct1 "No visible pneumonia"  있는 것 ct0 나머지 둘 합쳐

colnames(ct)

ct1 <-filter(ct,str_detect(Chest_interpretation, fixed("No visible pneumonia",ignore_case = TRUE)))
nrow(ct1)

ct1$Chest_interpretation2 <-"NA"

ct0 <-filter(ct,!(N_ID %in% ct1$N_ID))

nrow(ct0)
nrow(ct)
nrow(ct1)

ct <-rbind(ct0,ct1)


for(i in 1:nrow(ct))
{
  if( str_detect(ct$AP_interpretation[i], fixed("cholecystitis",ignore_case = TRUE)))
  {ct$AP_interpretation2[i] <-"cholecytitis" }
  else if (str_detect(ct$AP_interpretation[i], fixed("APN",ignore_case = TRUE)))
  {ct$AP_interpretation2[i] <-"APN" }
  else if (str_detect(ct$AP_interpretation[i], fixed("appendicitis",ignore_case = TRUE)))
  {ct$AP_interpretation2[i] <-"appendicitis" }
  else if (str_detect(ct$AP_interpretation[i], fixed("perinephric",ignore_case = TRUE)))
  {ct$AP_interpretation2[i] <-"APN" }
  else if (str_detect(ct$AP_interpretation[i], fixed("colitis",ignore_case = TRUE)))
  {ct$AP_interpretation2[i] <-"colitis" }
  else if (str_detect(ct$AP_interpretation[i], fixed("pericholecystic",ignore_case = TRUE)))
  {ct$AP_interpretation2[i] <-"cholecytitis" }
  else if (str_detect(ct$AP_interpretation[i], fixed("ileitis",ignore_case = TRUE)))
  {ct$AP_interpretation2[i] <-"colitis" }
  else if (str_detect(ct$AP_interpretation[i], fixed("pyelitis",ignore_case = TRUE)))
  {ct$AP_interpretation2[i] <-"APN" }
  else if (str_detect(ct$AP_interpretation[i], fixed("uti",ignore_case = TRUE)))
  {ct$AP_interpretation2[i] <-"uti" }
  else if (str_detect(ct$AP_interpretation[i], fixed("liver abscess",ignore_case = TRUE)))
  {ct$AP_interpretation2[i] <-"liver abscess" }
  else if (str_detect(ct$AP_interpretation[i], fixed("Peritonitis",ignore_case = TRUE)))
  {ct$AP_interpretation2[i] <-"uti" }
  else if (str_detect(ct$AP_interpretation[i], fixed("liver abscess",ignore_case = TRUE)))
  {ct$AP_interpretation2[i] <-"liver abscess" }
  else if (str_detect(ct$AP_interpretation[i], fixed("ureteritis",ignore_case = TRUE)))
  {ct$AP_interpretation2[i] <-"uti" }
  else if (str_detect(ct$AP_interpretation[i], fixed("cystitis",ignore_case = TRUE)))
  {ct$AP_interpretation2[i] <-"uti" }
  else if (str_detect(ct$AP_interpretation[i], fixed("Pericarditis",ignore_case = TRUE)))
  {ct$AP_interpretation2[i] <-"Pericarditis" }
  else if (str_detect(ct$AP_interpretation[i], fixed("Peritonitis",ignore_case = TRUE)))
  {ct$AP_interpretation2[i] <-"Peritonitis" }
  else if (str_detect(ct$AP_interpretation[i], fixed("cellulitis",ignore_case = TRUE)))
  {ct$AP_interpretation2[i] <-"cellulitis" }
  else if (str_detect(ct$AP_interpretation[i], fixed("Fournier gangrene",ignore_case = TRUE)))
  {ct$AP_interpretation2[i] <-"Fournier gangrene" }
  else if (str_detect(ct$AP_interpretation[i], fixed("Prostatitis",ignore_case = TRUE)))
  {ct$AP_interpretation2[i] <-"Prostatitis" }
  else if (str_detect(ct$AP_interpretation[i], fixed("cholangitis",ignore_case = TRUE)))
  {ct$AP_interpretation2[i] <-"cholangitis" }
  else if (str_detect(ct$AP_interpretation[i], fixed("enterocolits",ignore_case = TRUE)))
  {ct$AP_interpretation2[i] <-"colitis" }
  else if (str_detect(ct$AP_interpretation[i], fixed("Bile duct obstruction",ignore_case = TRUE)))
  {ct$AP_interpretation2[i] <-"biliary" }
  else if (str_detect(ct$AP_interpretation[i], fixed("pyelonephritis",ignore_case = TRUE)))
  {ct$AP_interpretation2[i] <-"APN" }
  else if (str_detect(ct$AP_interpretation[i], fixed("CBD with secondary biliary tree dilatation",ignore_case = TRUE)))
  {ct$AP_interpretation2[i] <-"biliary" }
  else if (str_detect(ct$AP_interpretation[i], fixed("diffuse biliary tree dilatation",ignore_case = TRUE)))
  {ct$AP_interpretation2[i] <-"biliary" }
  
  
  else{ ct$AP_interpretation2[i] <-NA}
}




# str_detect 쓰면 너무 노가대대 같으니깐.. 다시 grepl 써서 역시 찾는 단어는 하나밖에 안되네..

# chest<-read.csv("chest.csv", header=T ,stringsAsFactors=FALSE, fileEncoding="euc-kr")
# 
# for(j in 1:ncol(chest))
# {
#   
#   for(i in 1:nrow(ct))
#   {
#     
#        if(grepl(chest[,j] ,ct$Chest_interpretation[i],ignore.case=TRUE))
#        {ct$Chest_interpretation2[i] <-colnames(chest)[j]}
#        else{ct$Chest_interpretation2[i] <-777}
#     
#   }
#   
# }


#.이제부터 CT interpretation2 를 채워야해...

ct <-select(ct,N_ID,AGE,name,Chest_interpretation,Chest_interpretation2,AP_interpretation,AP_interpretation2)
# write.csv(aa, "yj_CT_interpretation.csv") #."yj_CT_interpretation.csv" 은 위 코딩으로 Chest_interpretation2,AP_interpretation2 를 간단히 완성한 것

nrow(ct)

# ct_no <-filter(ct, (is.na(Chest_interpretation2)) & (is.na(AP_interpretation2)))

# nrow(ct_no)
# write.csv(ct_no,"ct_no.csv") #.이렇게 내보내서 불러와서 interpretation3 만들었어
ct_no<-read.csv("ct_no yj_edit.csv", header=T ,stringsAsFactors=FALSE)
#.ct_no는 이전에 만든 파일이어서 이전 변수명이라 새롭게 변수명 바꾸어야해..

ct_no <-dplyr::rename(ct_no,N_ID=new_ID,name=환자명)
nrow(ct_no)
ct_no$N_ID2 <-str_split(ct_no$N_ID,"-") #.이건 list 야..
ct_no$Visit_day <-NA
ct_no$ID <-NA


for(i in 1:nrow(ct_no))
{ 
  ct_no$ID[i] <-ct_no$N_ID2[[c(i,1)]]
}

for(i in 1:nrow(ct_no))
{ 
  ct_no$Visit_day[i] <-ct_no$N_ID2[[c(i,2)]]
}

ct_no$N_ID <-paste(ct_no$Visit_day,ct_no$ID,sep="_")

colnames(ct_no)
ct_no <-select(ct_no,N_ID,4,5,7,8);ct_no
nrow(ct_no)

ct_y <-filter(ct,!(N_ID %in% ct_no$N_ID))
colnames(ct_y)
ct_y <-select(ct_y,1,4:7)
nrow(ct_y)

ct <-rbind(ct_no, ct_y)
nrow(ct_no)

#.ct를 pneumonia, no 등으로 나누어서 나중에 다시 채워넣어야.

# ct_pneumonia <-filter(ct ,Chest_interpretation2 =="pneumonia")
# nrow(ct_pneumonia)
# write.csv(ct_pneumonia, "ct_pneumonia.csv")



write.csv(ct,"ct.csv")
arrange(as.data.frame(table(ct$Chest_interpretation2)),Freq)
arrange(as.data.frame(table(ct$AP_interpretation2)),Freq)


ct_no <-filter(ct,(is.na(Chest_interpretation2)) & (is.na(AP_interpretation2)) )
nrow(ct_no)

ct_y <-filter(ct,!(is.na(Chest_interpretation2)) | !(is.na(AP_interpretation2)) )
nrow(ct_y)

nrow(ct)

ct <-arrange(ct,N_ID,desc=TRUE)
colnames(ct)

write.csv(ct,"ct_final.csv")


# #. 일단 이것이 ct 관련 최종 버젼 여기에대가 base_result & DF lab 붙여야...

base_result<-read.csv("base_result.csv", header=T ,stringsAsFactors=FALSE)



base_result_ct <-filter(base_result,N_ID %in% ct$N_ID)
nrow(base_result_ct)

base_result_ct <-arrange(base_result_ct,N_ID,desc=TRUE)

colnames(base_result_ct)

#.ct 하고 base_result_ct 동일성 여부 판단해보자..

identical(ct$N_ID, base_result_ct$N_ID)


ct_base_result_ct <-cbind(ct,base_result_ct);ct_base_result_ct
colnames(ct_base_result_ct)
ct_base_result_ct <-select(ct_base_result_ct,-6,-7,-8,-9,-10,-11,-12)

write.csv(ct_base_result_ct , "ct_base_result_ct.csv")


#.여기서 covid 제외해야...




data.frame(table(ct$AP_interpretation2))

data.frame(table(ct$Chest_interpretation2))
#.X ray 에서 명확한 pbneumonia 있으면 제외해야..

