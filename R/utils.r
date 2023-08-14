
set_col_names<-function(x,names){
  names(x)<-names
  return(x)
  }


get_osmconverter_path<-function(){
  ggper_path<-.libPaths()[lapply(.libPaths(), function(x){
    dir.exists(paste0(x,"/ggperRPkgs"))
  }) %>% unlist()]
  if(dir.exists(paste0(ggper_path,"/ggperRPkgs/data"))){
    ggper_path<-paste0(ggper_path,"/ggperRPkgs/data")
  }else{
    ggper_path<-paste0(ggper_path,"/ggperRPkgs/data")
    dir.create(ggper_path)
  }
  osmconvert_path<-list.files(
    path = ggper_path,
    pattern = "osmconvert.exe",
    full.names = T
  )
  if(length(osmconvert_path)==0){
    tempzip<-tempfile(fileext = ".zip")
    GET("https://drive.google.com/uc?export=download&id=18LbLDRVqlS-KQGEK2gMNbxMQd4dJa94H",
        write_disk(tempzip,overwrite = T))
    utils::unzip(tempzip,exdir = ggper_path)
    osmconvert_path<-list.files(
      path = ggper_path,
      pattern = "osmconvert.exe",
      full.names = T
    )
  }
  return(osmconvert_path)
}
