#################Poissoing regression
#.로지스틱 회귀분석이 질병의 위험인자에 대한 연구이고 ,
#.Cox 비례위험모형이 생존율의 위험인자에 대한 연구라면 
#.포아송 회귀분석은 질병의 발생률과 발생률에 영향을 미치는 인자에 대한 연구이다.


data(breslow.dat,package="robust") 
summary(breslow.dat)
#.sumY 투약 이후 간질 발생률이 얼마나 되나?

#포아송에서 과산포 확인해야.. 
# install.packages("qcc")
library(qcc)
qcc.overdispersion.test(breslow.dat$sumY,type="poisson")  #sumY는 투약이후 간질 발생률이 얼마나 되는지.
#p 값이 0에 가깝기 때문에 과산포가 있다. 


fit =glm(sumY~Base+ Age+Trt,family=quasipoisson,data=breslow.dat) #과산포 없으면 poisson
summary(fit) # Trtprogabid 효과가 없네.. 

extractOR(fit)
extractOR(fit,digits=3)

ORplot(fit,type=2,show.CI=TRUE, main="Plot for Quasi")