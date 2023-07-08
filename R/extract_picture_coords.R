
extract_picture_coords<-function(fig.path=NULL,dir=getwd(),
                                 fig.pattern=".JPG",export.path="same",
                                 save.geometry = TRUE, fig.crs=4326){
  
  if("try-error"%in%invisible(class(try(exif_version(),silent=TRUE)))){
    cat("Installing exiftool")
    exiftoolr::install_exiftool()
  }
  
  
  if(!is.null(fig.path)){
    exif_read(path = fig.path, tags = c("filename","*GPS*")) %>% 
      return()
  }
  
  
  files<-list.files(path = dir,pattern =  fig.pattern,recursive = T,full.names = T)
  system2("chcp 65001")
  pictures_infos<-exif_read(path = files, tags = c("filename","*GPS*"))
  
  pictures_infos<-pictures_infos %>% 
    filter(!is.na(GPSLongitude)) %>% 
    filter(!is.na(GPSLatitude))
  
  if(sum(str_detect(pictures_infos$SourceFile,"FRONTAL"))>0&sum(str_detect(pictures_infos$SourceFile,"TRASEIRA"))>0){
    pictures_infos$PicturePosition<-ifelse(str_detect(str_to_upper(pictures_infos$SourceFile),"FRONTAL"),
                                           "Frontal","Traseira")
  }
  
  if(export.path=="same"){
    export.path<-dir
  }
  
  pictures_infos %>% 
    fwrite(paste0(export.path,"/pontos_fotos.csv"))
  if(save.geometry){
    pictures_infos %>% 
      st_as_sf(coords=c("GPSLongitude","GPSLatitude"),crs=fig.crs) %>% 
      st_transform(31984) %>% 
      st_write(paste0(export.path,"/pontos_fotos.geojson"),append=F)
    
    return(paste0(export.path,"/pontos_fotos",c(".csv",".geojson")))
  }else{
    return(paste0(export.path,"/pontos_fotos.csv"))
  }
  
}






