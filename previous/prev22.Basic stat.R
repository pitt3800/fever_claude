##연속 변수 간의 두군의 평균 비교

#1.연속변수 인지.
moonBook::densityplot(AGE~sex,data=DF)
#2.정규분포인지...

shapiro.test(DF$age[DF$intubation=="intubation"])
shapiro.test(DF$age[DF$intubation=="normal"]) #.정규분포를 이루지 않네..age 생성시 rnorm()으로 만들었지만 변경 하면서 정규성 잃은 듯.
#3.등분산인지.
var.test(age~intubation,data=DF) #.등분산은 하네..그러나 의미 없지. 이미 정규분포를 하지 않으니

#1,#2,#3에 따라 t.text() or wilcox.test

t.test(age~intubation, data=DF, var.equal=TRUE) #여기서는 정규분포하지 않으므로 wilcox.test

wilcox.test(age~intubation,data=DF) #.p_value:6.271e-07 두 그룹간 차이가 있네..

##..명목 변수간의 독립여부  확인

result=table(DF$sex,DF$intubation);result
#.보기 편하게 sex를 factor로 바꿔봐..
DF$sex <-factor(DF$sex ,labels=c("F","M"))

chisq.test(result)

#.baseline characteristics
mytable(sex~.,data=DF,show.total=TRUE)


data(acs)
rm(list=ls())

mytable(Dx~LDLC,data=acs)
mytable(Dx~LDLC+DM,data=acs)
mytable(Dx~.,data=acs)
mytable(Dx+sex~.,data=acs)

#.Sys.setlocale(category="LC_ALL",locale="UTF-8")
# method=1 : 정규분포로 가정하여 anova를 실시
# method=2 : 정규분포 하지 않는 것으로 간주하여 Kruskal-Wallis test를 실시
# method=3 :잔차의 정규성 검정을 통해 anova 또는 K-W test를 실시
# 정규분포를 하는경우 데이터는 평균과 표준편차로 표시되고, 정규분포하지 않는 경우 중앙값과 사분위값으로 표시된다.