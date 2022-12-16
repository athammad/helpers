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


shpPlot<-function(shpData,varName,virdisColor=NULL,scaleColor=NULL,...){
  p<-ggplot() + 
    geom_sf(data =shpData , aes_string(fill = varName),...) + theme_void()
  
  if(!is.null(virdisColor)){
    p +viridis::scale_fill_viridis(option=virdisColor)
  }else{
  
    p +scale_fill_gradient(low = scaleColor[1], high = scaleColor[2], na.value = NA)
  }
  
}


lMerge<-function(dt=NULL,newNames=NULL){
mergedData = Reduce(function(...) merge(..., all = TRUE), dt)

if(!is.null(newNames)){
setnames(mergedData,newNames) 
}
return(mergedData)
}
                    
                    

plotGPS<-function(dt=NULL,LAT="Latitude", LON="Longitude",...){
  
  
  #Get the world map country border points
  library(maps)
  library(ggplot2)
  world_map <- map_data("world")
  
  
  #Creat a base plot with gpplot2
  p <- ggplot() + coord_fixed() +
    xlab("") + ylab("")
  
  #Add map to base plot
  base_world_messy <- p + geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
                                       colour="darkolivegreen3", fill="darkolivegreen3")
  
  
  
  
  cleanup <- 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_rect(fill = 'white', colour = 'white'), 
          axis.line = element_line(colour = "white"), legend.position="none",
          axis.ticks=element_blank(), axis.text.x=element_blank(),
          axis.text.y=element_blank())
  
  base_world <- base_world_messy + cleanup
  
  
  map_data <- 
    base_world +
    geom_point(data=dt, 
               aes_string(x=LON, y=LAT), colour="Blue", 
               fill="Light Blue",pch=21, size=5, alpha=I(0.7))
  
  return(map_data)
  
  
}
                    
                    
# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2country = function(points)
{  
  library(sp)
  library(rworldxtra)
  library(rworldmap)
  #countriesSP <- getMap(resolution='low')
  countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # convert our list of points to a SpatialPoints object
  
  # pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  
  #setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  # return the ADMIN names of each country
  #indices$ADMIN  
  indices$ISO3 # returns the ISO3 code 
  #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
}

                    
getMachineInfo<-function(){
  SYS<-paste(Sys.info()["sysname"],"System") 
  CORES<-parallel::detectCores()
  RAM<-system("lsmem | grep 'Total online memory'", intern = TRUE)
  
  cat(crayon::cyan(paste0(crayon::bold(SYS),"\n","CORES: ",CORES,"\n",RAM)))
}
 
                    
                    
shashPlot<-function(mu = 0.01, sigma = 0.08,nu = 1.2, tau = 0.1,PROB=NULL,color="red"){
  
  ggplot(data = data.frame(x = c(-0, 1)), aes(x)) +
    stat_function(fun = gamlss.dist::dSHASH, n = 10001, 
                  args = list(mu = mu, sigma = sigma, nu = nu, tau = tau)
                  ) +
    geom_vline(xintercept=PROB, size=1.5, color=color)+
    theme_minimal()
  
}                 
                    
                    
                    
