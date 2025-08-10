DF<-read.csv("dyspneacopdlab.csv", header=T ,stringsAsFactors=FALSE, fileEncoding="euc-kr")
str(DF)
nrow(DF)

unique(DF$코드명)
