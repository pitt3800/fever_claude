#########################-Survival analysis#############
require(survival)
data("colon")


colon <-na.omit(colon)
colon$TS <-Surv(colon$time,colon$status==1)#.TS라는 변수 생성 status==1 사망. 생존환자는 time에 +표시


fit <-survfit(TS ~rx,data=colon)
plot(fit)
plot(fit,col=1:3,lty=1:3)
legend("topright",legend=levels(colon$rx),col=1:3,lty=1:3)


#####Cumarative hazard start################################################################# 
# 특정질환의 유병율 실험군과 대조군 보여줄때는 뒤집어야 좋아
plot(fit,col=1:3,lty=1:3,mark.time=FALSE)

legend("topleft",legend=levels(colon$rx),col=1:3,lty=1:3)

#fun="cumhaz" 그래프 뒤집어라 
plot(fit,col=1:3,lty=1:3,fun="cumhaz",mark.time=FALSE)
######Cumarative hazard end


######.다른 방법 ggsurvplot 이용

#.내 자료

KM = survfit(Surv(DF$time,DF$status==1) ~ degree, data = DF)
summary(KM)
ggsurvplot(KM)

ggsurvplot(KM,
           conf.int = TRUE, # 신뢰구간 표현 여부
           risk.table = TRUE, # 테이블 표시 여부
           risk.table.height = 0.4, # 테이블 높이 설정
           ggtheme = theme_bw(), # 데이터 테마 설정
           palette = c("#2E9FDF","red","blue"), 
           font.x = c(10), # x축 제목 크기 설정
           font.y = c(10), # y축 제목 크기 설정
           font.tickslab = c(10), # 축 값 크기 설정
           surv.median.line = "hv", # 50% 생존지점 표시,내 자료에서는 50%까지 떨어지지 안항.
           break.time.by = 1000,
           xlim = c(0,5000)
)



#####Log-rank test -생존곡선을 통계적으로 비교하는 방법, 귀무가설: 모든시점에서 생존곡선들은 차이가 없다.

survdiff(TS~rx,data=colon)
survdiff(Surv(time, status==1)~rx,data=colon)

#### #cox Regression:Log-rank test를 통해 차이가 있는지 여부를 확인했다면 어떤 것끼리 차이가 있는지.확인해기 위해 
# 셀제는 p_value를 구하지 않고 95% HR 로 표현한다.
out =coxph(Surv(time, status==1)~rx,data=colon)
summary(out)

####exp(coef) exp(-coef) lower .95 upper .95
#.rxLev        0.9500      1.053    0.8141    1.1085
#.rxLev+5FU    0.6367      1.571    0.5383    0.7531
#.rxLev는 1을 포함하므로 의미 없다. 그래프에서도 나타나고 있네..


##### Hazard rations of all idividual variables, 마지막 
colon$TS <-Surv(colon$time,colon$status==1)
out =coxph(colon$TS~rx,data=colon) #=out =coxph(Surv(time, status==1)~rx,data=colon)
summary(out)


attach(colon) #.나중에 colon 안서도 돼..

out=mycph(TS~.,data=colon)  #.moonbook관심없는 변수"-"로  제거 후 여러 변수의 HR 정리해서 나와..
out

out2=coxph(TS~.-id-study-time-status-etype,data=colon) #.out 결과값을 다 보여줄 필요 없기 때문에 의미 있는 것만 추리기 위해
final=step(out2,direction="backward")
HRplot(final,type=2,show.CI=TRUE)
