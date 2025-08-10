##.ROC curve 그리기 https://github.com/cardiomoon/multipleROC
###1. multipleROC를 이용하는 방법




DF1 %>% group_by(intubation) %>% numSummaryTable(age)

x=multipleROC(intubation~total_MEWS,data=DF1) #roc 그래프와 AUC, sen,spec등 표현 수 있어.
table(DF1$total_MEWS>=2,DF1$intubation)

table(DF1$EWS,DF1$intubation)

#.The optimal cutoff value 
length(unique(DF1$total_MEWS)) #.모든 EWS에 대하여 아래에서 민감도 특이도 구하기 위하여...

calSens=function(x,y){
  newx=sort(unique(x))
  completeTable=function(res){
    if(nrow(res)==1){
      res1=matrix(c(0,0),nrow=1)
      temp=setdiff(c("TRUE","FALSE"),attr(res,"dimnames")[[1]][1])
      if(temp=="FALSE") res=rbind(res1,res)
      else res=rbind(res,res1)
      res
    }
    res
  }
  
  getSens=function(cut){
    res=table(x>=cut,y)
    res=completeTable(res)
    sens=res[2,2]/sum(res[,2])
    spec=res[1,1]/sum(res[,1])
    ppv=res[2,2]/sum(res[2,])
    npv=res[1,1]/sum(res[1,])
    data.frame(x=cut,sens=sens,spec=spec,fpr=1-spec,ppv=ppv,npv=npv,sum=sens+spec)
  }
  map_dfr(newx,function(cut){getSens(cut)})
}
result=calSens(DF1$total_MEWS,DF1$intubation)
result

longdf <- result %>% pivot_longer(cols=sens:spec,names_to = "rate") #transform the data in long from.
ggplot(data=longdf,aes(x=x,y=value,color=rate))+geom_line() #.optimal cutoff value 에 대한 그래프

result[which.max(result$sum),]
#.민감도가 높아지면 특이도가 낮아지고 반대로 특이도가 높아지면 민감도가 낮아진다. 최적의 절사점은 
#.민감도와 특이도의 합이 가장 높은 점이라고 할 수 있다.x 가 13일때

result=result[order(result$sens),];result
ggplot(data=result,aes(x=fpr,y=sens))+geom_line() # fpr=false positive rate 별의미 없는 듯.



#.Multiple ROC curves

a=multipleROC(dizz~EWS,data=DF,plot=FALSE) # plot=FALSE 하면 정보없이 그래프만.
b=multipleROC(dizz~age,data=DF,plot=FALSE)
c=multipleROC(dizz~RR,data=DF,plot=FALSE)
d=multipleROC(dizz~Lactic_acid ,data=DF,plot=FALSE)

plot_ROC(list(a,b,c,d),show.eta=FALSE,show.sens=FALSE)

plot_ROC(list(a,b,c,d),facet=TRUE) #  facetted plot

plot_ROC(list(a,b,c,d))+facet_grid(no~.) #.ggplot2 이용


# Models with multiple predictors

multipleROC(dizz~EWS+RR+age+WBC,data=DF)


fit=glm(dizz~EWS+RR+age+WBC,data=DF,family=binomial)#. 위 그래프는 이 glm에서 나왔다..
fit2=glm(dizz~EWS,data=DF,family=binomial)


fit3=glm(male~height+weight+age,data=radial,family=binomial)

#.Automatic stepwise backward elimination and final model selection
final=step(fit,trace=0)
summary(final)




#.두 ROC 비교
anova(fit,fit2,test="Chisq")

#. comparing two ROC
DF$dizz <-factor(DF$dizz)
DF$dizz <-factor(DF$dizz,labels=c(0,1)) #.정상이 기준값임을 명시해줘야...
DF$dizz <-relevel(DF$dizz,ref="normal") #.정상이 기준값임을 명시해줘야...
DF$dizz <-as.numeric(DF$dizz)


step_ROC(dizz~EWS+RR+age+WBC,data=DF) 
#.단계적 회귀를 통해 초기모형과 최종모형을 만들고 두 모형의 ROC 곡선을 그려주고 두 AUC 값을 비교
#.내 자료는 안돼..


