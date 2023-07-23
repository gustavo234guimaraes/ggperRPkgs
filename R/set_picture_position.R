


set_picture_position<-function(data=NULL,fig.path=NULL,fig.x=NULL,fig.y=NULL,fig.z=NULL,
                               runparallel=TRUE,nclust="auto"){
  
  if(!is.null(data)){
    if(sum(c("path","x","y")%in%names(data))<3){
      stop("The data.frame data need to have the columns path<character>, x<numeric> and y<numeric>")
    }
    if("sf"%in%class(data)){
      data<-data %>% 
        st_drop_geometry() %>% 
        as.data.frame()
    }
  }else{
    data<-data.frame(
      path=fig.path,
      x=fig.x,
      y=fig.y
    )
    if(!is.null(fig.z)){data$z<-fig.z}
  }
  
  
  
    
    if(runparallel){
      if(nclust=="auto"){
        cl1<-makeCluster(trunc(0.5*detectCores(logical = TRUE)))
      }else{
        cl1<-makeCluster(ncluster)
      }
      
      parApply(cl=cl1,data,1,function(x){
        exiftoolr::exif_call(args = na.exclude(c(paste0('-GPSLongitude="',x["x"],'"'),
                                                 paste0('-GPSLatitude="',x["Y"],'"'),
                                                 ifelse(x["x"]<0,'-GPSLongitudeRef="W"',NA),
                                                 ifelse(x["x"]<0,'-GPSLatitudeRef="S"',NA),
                                                 ifelse("z"%in%names(x),paste0('-GPSAltitude="',x["z"],'"'),NA))),
                             path = x["path"])
        return(TRUE)
      })
      
    }else{
      apply(data,1,function(x){
        exiftoolr::exif_call(args = na.exclude(c(paste0('-GPSLongitude="',x["x"],'"'),
                                                 paste0('-GPSLatitude="',x["y"],'"'),
                                                 ifelse(x["x"]<0,'-GPSLongitudeRef="W"',NA),
                                                 ifelse(x["y"]<0,'-GPSLatitudeRef="S"',NA),
                                                 ifelse("z"%in%names(x),paste0('-GPSAltitude="',x["z"],'"'),NA))),
                             path = x["path"])
        return(TRUE)
      })
  }
  
  
  
}











