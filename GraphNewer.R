path <- "C:\\Users\\wenhaowang\\Documents\\Publication\\HieraticalIRTAdaptiveTest2016\\revised6.2017\\Result_changeMulti"
setwd(path)
outpath<- "C:\\Users\\wenhaowang\\Documents\\Publication\\HieraticalIRTAdaptiveTest2016\\revised6.2017\\Analysis_changeMulti\\"
library(XLConnect)
library(ggplot2)
library(dplyr)
library(reshape2)
############RMSE############
wb<-loadWorkbook("RMSE.xls")
all4<-NULL
for (dim in c('5','13')){
  all<-NULL
  model=ifelse(dim=='5','Simple','Complex')
  for (method in c('multi','localmulti','uni')){
    data<-readWorksheet(wb, sheet=paste0(method,dim))
    if (dim=='5'){
      data<-data%>%mutate(g=mrmse1,s=mrmse2,sg=mrmse3,t=(mrmse4+mrmse5)/2)}else{
        data<-data%>%mutate(g=mrmse1,s=(mrmse2+mrmse3+mrmse4)/3,sg=(mrmse5+mrmse6+mrmse7)/3,
                            t=(mrmse8+mrmse9+mrmse10+mrmse11+mrmse12+mrmse13)/6)
      }
    data<-data%>%select(condition,g,s,sg,t)%>%
      mutate(method=ifelse(method=='multi','M',ifelse(method=='localmulti','LM','U')))%>%
      mutate(model=model)
    all<-rbind(all,data)
  }
  all<-all%>%mutate(testlength=condition*18)%>%select(-condition)
  all2<-melt(all,id.vars=c('method','model','testlength'))%>%mutate(name=(paste0(method,testlength)))%>%
    mutate(factor=ifelse(variable=='g','G',ifelse(variable=='s','S',ifelse(variable=='sg','SwT','T'))))
  all4<-rbind(all2,all4)
}
all4<-all4%>%mutate(factor2=paste0(model,' H-IRT model ',factor))

all4$factor2=factor(as.factor(all4$factor2),levels(as.factor(all4$factor2))[c(1,3,2,4,5,7,6,8)])

  pdf(paste0(outpath,"RMSEGraph.pdf"),width=9,height=4)
  p<-ggplot(all4,aes(x=testlength,y=value,linetype=method,shape=method))+geom_line()+geom_point(size=1.5)+
      xlab("Test Length")+ylab('RMSE')+scale_x_continuous(breaks=seq(18,90,18))+labs(linetype='Method',shape='Method')+theme_bw()+
     facet_wrap(~factor2,nrow=2)
    print(p)
    dev.off()


############Bias############
wb<-loadWorkbook("Bias.xls")
all4<-NULL
for (dim in c('5','13')){
  all<-NULL
  model=ifelse(dim=='5','Simple','Complex')
  for (method in c('multi','localmulti','uni')){
    data<-readWorksheet(wb, sheet=paste0(method,dim))
    if (dim=='5'){
      data<-data%>%mutate(g=bias1,s=bias2,sg=bias3,t=(bias4+bias5)/2)}else{
        data<-data%>%mutate(g=bias1,s=(bias2+bias3+bias4)/3,sg=(bias5+bias6+bias7)/3,
                            t=(bias8+bias9+bias10+bias11+bias12+bias13)/6)
      }
    data<-data%>%select(condition,g,s,sg,t)%>%
      mutate(method=ifelse(method=='multi','M',ifelse(method=='localmulti','LM','U')))%>%
      mutate(model=model)
    all<-rbind(all,data)
  }
  all<-all%>%mutate(testlength=condition*18)%>%select(-condition)
  all2<-melt(all,id.vars=c('method','model','testlength'))%>%mutate(name=(paste0(method,testlength)))%>%
    mutate(factor=ifelse(variable=='g','G',ifelse(variable=='s','S',ifelse(variable=='sg','SwT','T'))))
  all4<-rbind(all2,all4)
}
all4<-all4%>%mutate(factor2=paste0(model,' H-IRT model ',factor))
all4$factor2=factor(as.factor(all4$factor2),levels(as.factor(all4$factor2))[c(1,3,2,4,5,7,6,8)])

  pdf(paste0(outpath,"Bias.pdf"),width=9,height=4)
  p<-ggplot(all4,aes(x=testlength,y=value,linetype=method,shape=method))+geom_line()+geom_point(size=1.5)+
    xlab("Test Length")+ylab('Bias')+scale_x_continuous(breaks=seq(18,90,18))+labs(linetype='Method',shape='Method')+theme_bw()+
    facet_wrap(~factor2,nrow=2)
  print(p)
  dev.off()



############Correlation############
wb<-loadWorkbook("Cor.xls")
all4<-NULL
for (dim in c('5','13')){
  all<-NULL
  model=ifelse(dim=='5','Simple','Complex')
  for (method in c('multi','localmulti','uni')){
    data<-readWorksheet(wb, sheet=paste0(method,dim))
    if (dim=='5'){
      data<-data%>%mutate(g=cor1,s=cor2,sg=cor3,t=tanh((atanh(cor4)+atanh(cor5))/2))}else{
        data<-data%>%mutate(g=cor1,s=tanh((atanh(cor2)+atanh(cor3)+atanh(cor4))/3),sg=tanh((atanh(cor5)+atanh(cor6)+atanh(cor7))/3),
                            t=tanh((atanh(cor8)+atanh(cor9)+atanh(cor10)+atanh(cor11)+atanh(cor12)+atanh(cor13))/6))
      }
    data<-data%>%select(condition,g,s,sg,t)%>%
      mutate(method=ifelse(method=='multi','M',ifelse(method=='localmulti','LM','U')))%>%
      mutate(model=model)
    all<-rbind(all,data)
  }
  all<-all%>%mutate(testlength=condition*18)%>%select(-condition)
  all2<-melt(all,id.vars=c('method','model','testlength'))%>%mutate(name=(paste0(method,testlength)))%>%
    mutate(factor=ifelse(variable=='g','G',ifelse(variable=='s','S',ifelse(variable=='sg','SwT','T'))))
  all4<-rbind(all2,all4)
} 
all4<-all4%>%mutate(factor2=paste0(model,' H-IRT model ',factor))
all4$factor2=factor(as.factor(all4$factor2),levels(as.factor(all4$factor2))[c(1,3,2,4,5,7,6,8)])

pdf(paste0(outpath,"CorGraph.pdf"),width=9,height=4)
  p<-ggplot(all4,aes(x=testlength,y=value,linetype=method,shape=method))+geom_line()+geom_point(size=1.5)+
    xlab("Test Length")+ylab('Correlation')+scale_x_continuous(breaks=seq(18,90,18))+labs(linetype='Method',shape='Method')+theme_bw()+
    facet_wrap(~factor2,nrow=2)
  print(p)
  dev.off()

