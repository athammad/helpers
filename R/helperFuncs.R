mlMetrics<-function(model,training=NULL,testing=NULL){
  #train
  trainr<-list(
    trainRes<-predict(model,training[,-1]),
    r2=caret::R2(trainRes,training[,1]),
    rmspe=MLmetrics::RMSPE(trainRes ,training[,1]),
    mape=MLmetrics::MAPE(trainRes ,training[,1]))
  
  names(trainr)[1]<-"trainRes"
  #Test
  testr<-list(
    testRes<-predict(model,testing[,-1]),
    r2=caret::R2(testRes,testing[,1]),
    rmspe=MLmetrics::RMSPE(testRes ,testing[,1]),
    mape=MLmetrics::MAPE(testRes ,testing[,1]))
  
  names(testr)[1]<-"testRes"
  #----------------------------------#
  Mylist<-list(trainr,testr)
  names(Mylist)<-c("trainr","testr")
  return(Mylist)
  
}


mdensity<-function(data,main=paste(names(data),collapse = ' vs '),...){
  ##combines multiple density plots together when given a list
  df=data.frame();
  for(n in names(data)){
    idf=data.frame(x=data[[n]],label=rep(n,length(data[[n]])))
    df=rbind(df,idf)
  }
  lattice::densityplot(~x,data=df,groups = label,plot.points = F, ref = T, auto.key = list(space = "right"),main=main,...)
}


shpPlot<-function(shpData,varName,virdisColor="C",...){
ggplot() + 
  geom_sf(data =shpData , aes_string(fill = varName)) +
  viridis::scale_fill_viridis(option=virdisColor) + theme_void()}

