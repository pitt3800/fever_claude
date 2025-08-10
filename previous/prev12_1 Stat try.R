###.환자군 달리해서 통계 돌리기..DF2 는 DF1 에서 이상치 제거

#1.요양원 환자만..selection
DF_eldery <-filter(DF2,nursinghome=="1")
nrow(DF_eldery)

DF_eldery <-select(DF_eldery ,-nursinghome)

a<-mytable(fever~.,data=DF_eldery,show.total=TRUE);a
mycsv(a,file="baseline table_eldery.csv")

str(DF_eldery)
str(DF2)
colnames(DF_eldery)
colnames(DF2)


## 일단 모델 만들어..
result <-glm(fever~.,family=binomial,data=DF_eldery)
summary(result) 

#.이제 만든 glm에서 어떤 변수를 넣어야 최선이 되는지 모델을 선택해야..
reduced.model=step(result)
#.#.AIC 값이 떨어지면서 높아지기 전에 멈춰
#.모든 변수를 넣는 것 보다 의미 없는 변수는 없는 모델이 좋으니깐..step() 의 기본은 backward 인듯
summary(reduced.model)

# 다중 공선성 있는지 확인하기 위해 VIF 필요
# library(car)

vif(reduced.model)
sqrt(vif(reduced.model)) #다중 공선성 있는 것 (2~3 이상? )인 것 다 제거하고 다시 만들어

fit <-glm(fever~ AGE+mental+BT+Albumin+Alk.Phosphatase+ANC+Cholesterol+Neutro+PCT+Platelet+Potassium+Protein+PT.INR+Cardiac+Renal+Pulmonary+OP+Cancer+insurance,family=binomial,data=DF1)
reduced.model=step(fit)


vif(reduced.model)
sqrt(vif(reduced.model))

#.실제에서는 이런 p_value와 회귀 계수를 넣는 것보다.. 대부분 odd ratio를 통해 1을 중심으로 
# 얼마나 많이 벗어낫는지를 표현해

# odd ratio구하기위해

extractOR(reduced.model)

ORplot(reduced.model,type=2,show.OR = FALSE, show.CI=TRUE,lwd=2,col=c("darkblue","red"),main="Plot of OR")






#1.ABGA 시행한 환자만..selection

##04.last.clensing 에서 ABGA 안한환자를 전처리함
# DF$pH.ABGA [is.na(DF$pH.ABGA)] <-0.0
# DF$Base.Excess[is.na(DF$Base.Excess )] <-0.0
# DF$Bicarbonate [is.na(DF$Bicarbonate )] <-0.0

a <-mytable(fever~.,data=DF1,show.total=TRUE);a

mycsv(a,file="baseline table_ABGA.csv")

str(DF1)
nrow(DF1)

DF1 <-filter(DF1,Bicarbonate != 0.0)
DF$Bicarbonate



## 일단 모델 만들어..
result <-glm(fever~.,family=binomial,data=DF1)
summary(result) 

#.이제 만든 glm에서 어떤 변수를 넣어야 최선이 되는지 모델을 선택해야..
reduced.model=step(result)
#.#.AIC 값이 떨어지면서 높아지기 전에 멈춰
#.모든 변수를 넣는 것 보다 의미 없는 변수는 없는 모델이 좋으니깐..step() 의 기본은 backward 인듯
summary(reduced.model)

#.reduced.model에서 의미 있는 변수 중에서 다중 공선성 있는 변수 제거


# 다중 공선성 있는지 확인하기 위해 VIF 필요
# library(car)

vif(reduced.model)
sqrt(vif(reduced.model)) #다중 공선성 있는 것 (2~3 이상? )인 것 다 제거하고 다시 만들어

#.pH,covid19는 OR 구할때 너 큰  값이 나오고 그  그래프가 이상하게 나와서 뺏음
# fit <-glm(fever~pH.ABGA+covid19+sex+DBP+HR+SPO2+Albumin+Alk.Phosphatase+ANC+Bicarbonate+Chloride+Cholesterol+Creatinine+CRP+Neutro+Hb+PCT+Cancer,family=binomial,data=DF1)

fit <-glm(fever~sex+DBP+HR+SPO2+Albumin+Alk.Phosphatase+ANC+Bicarbonate+Chloride+Cholesterol+Creatinine+CRP+Neutro+Hb+PCT+Cancer,family=binomial,data=DF1)
summary(fit) 
reduced.model=step(fit)
summary(reduced.model)

vif(reduced.model)
sqrt(vif(reduced.model))

#.실제에서는 이런 p_value와 회귀 계수를 넣는 것보다.. 대부분 odd ratio를 통해 1을 중심으로 
# 얼마나 많이 벗어낫는지를 표현해

# odd ratio구하기위해

extractOR(reduced.model)

ORplot(reduced.model,type=2,show.OR = FALSE, show.CI=TRUE,lwd=2,col=c("darkblue","red"),main="Plot of OR")

#.이상치 확인 
par(mfrow=c(1,1))
influencePlot(reduced.model,id.method = "identify") #. 이걸하면 r 이판단한 이상치 리스트 보여줘

influencePlot(reduced.model, id=list(method="identify"))

a <-c(678,113,109,429,796,215,577,833,210,856,646)

#.이상치 빼고 해보자..

DF2 <-filter(DF1,!rownames(DF1)%in% a)

result <-glm(fever~.,family=binomial,data=DF1)
summary(result) 

reduced.model=step(result)
summary(reduced.model)

nrow(DF1)
nrow(DF2)


fit2 <-glm(fever~sex+DBP+HR+SPO2+Albumin+Alk.Phosphatase+ANC+Bicarbonate+Chloride+Cholesterol+Creatinine+CRP+Neutro+Hb+PCT+Cancer,family=binomial,data=DF2)
reduced.model2=step(fit2)

extractOR(reduced.model2)

ORplot(reduced.model2,type=2,show.OR = FALSE, show.CI=TRUE,lwd=2,col=c("darkblue","red"),main="Plot of OR")


#.37 로지스틱 회귀분석 R에서 따라하기 1부 # 과산포를 확인해야만 최종 모델을 선택할 수 있어..

fit <-glm(fever~ route+SPO2+PCT+Hb+Cholesterol+HR+Neutro+sex+DBP+CRP+Albumin+ANC+Bicarbonate+Cancer+Alk.Phosphatase+Creatinine+Chloride,family=binomial,data=DF2)

fit.od  <-glm(fever~ route+SPO2+PCT+Hb+Cholesterol+HR+Neutro+sex+DBP+CRP+Albumin+ANC+Bicarbonate+Cancer+Alk.Phosphatase+Creatinine+Chloride,family=quasibinomial,data=DF2)

pchisq(summary(fit.od)$dispersion*fit$df.residual,fit$df.residual,lower=F) # 나온값이 0.05보다 크다면 과산포는 없다.family=binomial 를 넣어야..
















#.70대이상 별 의미 없네

DF3 <-filter(DF2,AGE>0)
nrow(DF3)

fit3 <-glm(fever~ route+SPO2+PCT+Hb+Cholesterol+HR+Neutro+sex+DBP+CRP+Albumin+ANC+Bicarbonate+Cancer+Alk.Phosphatase+Creatinine+Chloride,family=binomial,data=DF3)
reduced.model3=step(fit3)

extractOR(reduced.model3)

ORplot(reduced.model3,type=2,show.OR = FALSE, show.CI=TRUE,lwd=2,col=c("darkblue","red"),main="Plot of OR")
