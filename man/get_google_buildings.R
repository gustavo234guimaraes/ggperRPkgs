



get_google_buildings<-function(layer=NULL,output.format="csv"){
  if(output.format%in%c("df","sf")==FALSE){
    stop('Parameter output.format it must be "df" or "sf"')
  }
  if(is.character(feature)){
    cat("Getting OSM bbox",sep = "\n")
    feature<-osmdata::getbb(place_name = feature,format_out = "sf_polygon") %>% 
      unique.data.frame() %>% st_as_sf()
    if(nrow(feature)>1){
      cat("The OSM API returns more than 1 polygons","Do you wish to continue","1.Yes","2.No",sep = "\n")
      choice<-readline(prompt = "Choice: ")
      while (choice%in%c("1","2")==FALSE) {
        cat("Not find this option",sep = "\n")
        cat("The OSM API returns more than 1 polygons","Do you wish to continue","1.Yes","2.No",sep = "\n")
        choice<-readline(prompt = "Choice: ")
      }
      if(choice=="2"){
        stop("Execution stoped")
      }
    }
    
    cat(" ",sep = "\n")
    cat("Using: ")
    print(feature)
  }else{
    if(st_geometry_type(feature,by_geometry = F)%in%c("POLYGON","MULTIPOLYGON")==FALSE){
      cat("Getting layer bbox")
      feature<-st_as_sfc(st_bbox(feature))
      cat("Using:")
      print(feature)
    }
  }
  
  
  metadata<-rjson::fromJSON(file = "https://sites.research.google/open-buildings/tiles.geojson")
  
  metadata<-lapply(metadata$features, function(x){
    x$geometry$coordinates
    a<-rbindlist(x$geometry$coordinates) %>% t()
    a<-apply(a, 1, function(x){paste0(x[1],' ',x[2])})
    data.frame(wkt=paste0("Polygon((",paste0(a,collapse = ", "),"))"),tile_id=x$properties$tile_id,
               tile_url=x$properties$tile_url,size_mb=x$properties$size_mb) %>% 
      return()
  }) %>% rbindlist()
  
  metadata<-st_as_sf(metadata,wkt="wkt",crs=4326,remove=F)
  filter<-st_filter(metadata,feature)
  for (i in 1:nrow(filter)) {
    cat(paste0("Downloading tile ",i," of ",nrow(filter),"  Size: ",filter$size_mb[i],"mb"),sep = '\n')
    filegz<-tempfile(fileext = ".csv.gz")
    httr::GET(url=filter$tile_url[i],
              httr::write_disk(filegz))
    filecsv<-tempfile(fileext = ".csv")
    R.utils::gunzip(filegz,destname=filecsv)
    if(i==1){
      buildings<-fread(filecsv)
    }else{
      buildings<-bind_rows(
        buildings,
        fread(filecsv)
      )
    }
  }
  box<-st_bbox(st_buffer(feature,2000))
  if(output.format=="df"){
    filter(buildings,latitude>min(box[c(2,4)])&latitude<max(box[c(2,4)])) %>% 
      filter(longitude>min(box[c(1,3)])&longitude<max(box[c(1,3)])) %>%
      st_as_sf(wkt="geometry",crs=4326) %>% 
      st_filter(feature) %>% 
      mutate(wkt=st_as_text(geometry)) %>% 
      st_drop_geometry() %>% 
      as.data.frame() %>% 
      return()
  }else{
    filter(buildings,latitude>min(box[c(2,4)])&latitude<max(box[c(2,4)])) %>% 
      filter(longitude>min(box[c(1,3)])&longitude<max(box[c(1,3)])) %>%
      st_as_sf(wkt="geometry",crs=4326) %>% 
      st_filter(feature) %>% 
      return()
  }
  
    
  
  
  
  
  
}

