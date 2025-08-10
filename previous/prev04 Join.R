rm(list=ls())

ct <-read.csv("ct_final.csv", header=T ,stringsAsFactors=FALSE)
colnames(ct)
ct <-select(ct,-X)


ct_base_result_ct <-read.csv("ct_base_result_ct.csv", header=T ,stringsAsFactors=FALSE)
colnames(ct_base_result_ct)
ct_base_result_ct <-select(ct_base_result_ct,-X)


DF_A<-read.csv("DF_A.csv", header=T ,stringsAsFactors=FALSE)
colnames(DF_A)
DF_A <-select(DF_A,-1)

DF_nurse_ct<-read.csv("nurse.csv", header=T ,stringsAsFactors=FALSE)
str(DF_nurse_ct)
colnames(DF_nurse_ct)
DF_nurse_ct <-select(DF_nurse_ct,-X)



#. 이제 CT 찍은 환자만 filtering

#.DF: EMR 통해 환자 filtering
#.DF_A : DF 에 DF_lab 을 통해 lab 값 포함
#.DF_A_ct: DF_A중  ct 두번찍은 사람만 filtering
#.base_result: DF 환자에 대해 기타 정보
#.base_result_ct: base_result 중  ct 두번 찍은 사람만 filtering
#.DF_nurse_ct 는 CT 두번 찍은 사람의 간호특이사항

#.ct: CT 두번 찍은 사람
#.ct_base_resulrt_ct: base_result_ct에 ct 판독소견 포함


DF_A_ct <-filter(DF_A,N_ID %in% ct$N_ID)
DF_A_ct <-arrange(DF_A_ct,N_ID,name)

DF_nurse_ct <-arrange(DF_nurse_ct,N_ID)

nrow(DF_A_ct)

nrow(DF_nurse_ct)

# colnames(base_result_ct)
# 
# 
# ct_base_result_ct<-full_join(ct, base_result_ct, by = "N_ID")
ct_base_result_ct <-arrange(ct_base_result_ct,N_ID)

#.세 데이터 프레임 비교 해보자.. ->같네...
# write.csv(DF_A_ct$N_ID , "DF_A_ct_ID .csv")
# write.csv(ct_base_result_ct$N_ID , "ct_base_result_ct.csv")
# write.csv(DF_nurse_ct$N_ID , "DF_nurse_ct.csv")


#DF_A_ct & ct_base_result_ct 합칠때 중복 되는 열을 미리 제거할 필요

colnames(ct_base_result_ct)


#.DF_A_ct 하고 합칠 때 중복되는 열제거

colnames(DF_A_ct)

colnames(ct_base_result_ct)

DF_final <-full_join(DF_A_ct, ct_base_result_ct, by ="N_ID")

#.DF_nurse_ct 도 추가로 합쳐 , 중복되는 열 제거

colnames(DF_nurse_ct)



DF_final <-full_join(DF_final, DF_nurse_ct , by ="N_ID")

colnames(DF_final)
nrow(DF_final)

#.DF_final 은 DF,DF_lab을 합친 DF_A, base_result와 ct를 통합한, base_result_ct, 그리고 ct를 합친 최종 file

write.csv(DF_final, "DF_final.csv")


