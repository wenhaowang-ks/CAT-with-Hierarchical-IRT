path <-"C:\\Users\\wenhaowang\\Documents\\Publication\\HieraticalIRTAdaptiveTest2016\\revised6.2017\\Result_changeMulti"
setwd(path)
library(XLConnect)
library(dplyr)
library(reshape2)
##########simple model#################
for (method in c('multi','localmulti','uni')){
  bias=NULL
  cor=NULL
  RMSE=NULL
  for (condition in c('1','2','3','4','5')){
    data<-read.table(file=paste0(method,'5_condition',condition,'.txt'))
    bresult<-c(mean(data$V3-data$V8),mean(data$V4-data$V9),mean(data$V5-data$V10),mean(data$V6-data$V11),mean(data$V7-data$V12))
    rresult<-c(sqrt(mean((data$V3-data$V8)^2)),sqrt(mean((data$V4-data$V9)^2)),sqrt(mean((data$V5-data$V10)^2)),sqrt(mean((data$V6-data$V11)^2)),sqrt(mean((data$V7-data$V12)^2)))
    cresult<-c(cor(data$V3,data$V8),cor(data$V4,data$V9),cor(data$V5,data$V10),cor(data$V6,data$V11),cor(data$V7,data$V12))
    
    bias<-rbind(bias,bresult)
    RMSE<-rbind(RMSE,rresult)
    cor<-rbind(cor,cresult)
  }
  
  bias<-as.data.frame(bias)
  RMSE<-as.data.frame(RMSE)
  cor<-as.data.frame(cor)
  
  bias<-cbind(bias,seq(1,5))
  RMSE<-cbind(RMSE,seq(1,5))
  cor<-cbind(cor,seq(1,5))
  
  colnames(bias)<-c(paste0('bias',seq(1,5)),'condition')
  colnames(cor)<-c(paste0('cor',seq(1,5)),'condition')
  colnames(RMSE)<-c(paste0('mrmse',seq(1,5)),'condition')
  
  writeWorksheetToFile("Bias.xls",bias,sheet=paste0(method,'5'))
  writeWorksheetToFile("RMSE.xls",RMSE,sheet=paste0(method,'5'))
  writeWorksheetToFile("Cor.xls",cor,sheet=paste0(method,'5'))
}

##########complex model#################
for (method in c('multi','localmulti','uni')){
  bias=NULL
  cor=NULL
  RMSE=NULL
  for (condition in c('1','2','3','4','5')){
    data<-read.table(file=paste0(method,'13_condition',condition,'.txt'))
    bresult<-c(mean(data$V3-data$V16),mean(data$V4-data$V17),mean(data$V5-data$V18),mean(data$V6-data$V19),
              mean(data$V7-data$V20),mean(data$V8-data$V21),mean(data$V9-data$V22),mean(data$V10-data$V23),
              mean(data$V11-data$V24),mean(data$V12-data$V25),mean(data$V13-data$V26),
              mean(data$V14-data$V27),mean(data$V15-data$V28))
    rresult<-c(sqrt(mean((data$V3-data$V16)^2)),sqrt(mean((data$V4-data$V17)^2)),sqrt(mean((data$V5-data$V18)^2)),
               sqrt(mean((data$V6-data$V19)^2)),sqrt(mean((data$V7-data$V20)^2)),sqrt(mean((data$V8-data$V21)^2)),
               sqrt(mean((data$V9-data$V22)^2)),sqrt(mean((data$V10-data$V23)^2)),
               sqrt(mean((data$V11-data$V24)^2)),sqrt(mean((data$V12-data$V25)^2)),sqrt(mean((data$V13-data$V26)^2)),
               sqrt(mean((data$V14-data$V27)^2)),sqrt(mean((data$V15-data$V28)^2)))
    cresult<-c(cor(data$V3,data$V16),cor(data$V4,data$V17),cor(data$V5,data$V18),cor(data$V6,data$V19),
               cor(data$V7,data$V20),cor(data$V8,data$V21),cor(data$V9,data$V22),cor(data$V10,data$V23),
               cor(data$V11,data$V24),cor(data$V12,data$V25),cor(data$V13,data$V26),
               cor(data$V14,data$V27),cor(data$V15,data$V28))
    
    bias<-rbind(bias,bresult)
    RMSE<-rbind(RMSE,rresult)
    cor<-rbind(cor,cresult)
  }
  
  bias<-as.data.frame(bias)
  RMSE<-as.data.frame(RMSE)
  cor<-as.data.frame(cor)
  
  bias<-cbind(bias,seq(1,5))
  RMSE<-cbind(RMSE,seq(1,5))
  cor<-cbind(cor,seq(1,5))
  
  colnames(bias)<-c(paste0('bias',seq(1,13)),'condition')
  colnames(cor)<-c(paste0('cor',seq(1,13)),'condition')
  colnames(RMSE)<-c(paste0('mrmse',seq(1,13)),'condition')
  
  writeWorksheetToFile("Bias.xls",bias,sheet=paste0(method,'13'))
  writeWorksheetToFile("RMSE.xls",RMSE,sheet=paste0(method,'13'))
  writeWorksheetToFile("Cor.xls",cor,sheet=paste0(method,'13'))
}
###################time##################
test=NULL
rep=NULL
for (method in c('multi','localmulti','uni')){
  data<-read.table(file=paste0(method,'5_time.txt'),fill=TRUE)
  t<-data%>%filter(is.na(V5)&is.na(V3)==F)%>%select(tl=V1,time=V4)%>%mutate(method=method,model='Simple')
  r<-data%>%filter(is.na(V5)&is.na(V3))%>%select(tl=V1,time=V2)%>%mutate(method=method,model='Simple')
  test<-rbind(test,t)
  rep<-rbind(rep,r)
  data<-read.table(file=paste0(method,'13_time.txt'),fill=TRUE)
  t<-data%>%filter(is.na(V5)&is.na(V3)==F)%>%select(tl=V1,time=V4)%>%mutate(method=method,model='Complex')
  r<-data%>%filter(is.na(V5)&is.na(V3))%>%select(tl=V1,time=V2)%>%mutate(method=method,model='Complex')
  test<-rbind(test,t)
  rep<-rbind(rep,r)
}
test2<-dcast(test, tl + model ~ method,value.var='time')%>%arrange(desc(model),tl)
rep2<-dcast(rep, tl + model ~ method,value.var='time')%>%arrange(desc(model),tl)
writeWorksheetToFile("Time.xls",test2,sheet="Test")
writeWorksheetToFile("Time.xls",rep2,sheet="Replication")
