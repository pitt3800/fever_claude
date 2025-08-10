


DF<-read_excel(path="nurse.xlsx", sheet="Sheet1",col_names = TRUE)

#.DF 정리


#.DF: EMR 통해 환자 filtering
#.DF_A : DF 에 DF_lab 을 통해 lab 값 포함
#.DF_A_ct: DF_A중  ct 두번찍은 사람만 filtering
#.base_result: DF 환자에 대해 기타 정보
#.base_result_ct: base_result 중  ct 두번 찍은 사람만 filtering

#.ct: CT 두번 찍은 사람
#.ct_base_resulrt_ct: base_result_ct에 ct 판독소견 포함
#.DF_nurse_ct 는 CT 두번 찍은 사람의 간호특이사항
colnames(DF)

DF <-select(DF,1,2,3,7,8,9)

names(DF) <-c("ID","name","Visit_day","Procedure","etc","Note")

DF$N_ID <-paste(DF$Visit_day,DF$ID,sep='_')

DF = DF[-which(duplicated(DF$N_ID)),] #. 같은 날 두번 온 환자 제외
DF <-arrange(DF,N_ID)
nrow(DF) #2035

str(DF)
colnames(DF)
DF <-select(DF, 7,1,2,4:6)


DF_nurse_ct <-filter(DF,N_ID %in% ct$N_ID)
DF_nurse_ct <-arrange(DF_nurse_ct,N_ID,name)

nrow(DF_nurse_ct)
str(DF_nurse_ct)

#.NA 처리

DF_nurse_ct[is.na(DF_nurse_ct)] <-"aa"
colSums(is.na(DF_nurse_ct))

#.요양원 있는 행 찾아야..

a <-which(grepl("요양",DF_nurse_ct$Procedure))
b <-which(grepl("요양",DF_nurse_ct$Note))
c <-which(grepl("요양",DF_nurse_ct$etc))

d <-c(a,b,c)
length(d)
d <-unique(d)
d <-sort(d);d

DF_nurse_ct$nursinghome <-NA

DF_nurse_ct$nursinghome[d] <-1
DF_nurse_ct$nursinghome[is.na(DF_nurse_ct$nursinghome)] <-0

table(DF_nurse_ct$nursinghome)

colnames(DF_nurse_ct)
DF_nurse_ct <-select(DF_nurse_ct,-3,-4)

write.csv(DF_nurse_ct, "nurse.csv")



