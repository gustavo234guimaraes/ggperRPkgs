

download_google_buildings<-function(layer,destiny_file=NULL){
  output.format<-str_sub(destiny_file,start=str_locate(destiny_file,"\\.")[,1]+1)
  if(output.format%in%c("csv","geojson")==FALSE){
    stop('Output format it must be ".csv" or ".geojson"')
  }
  if(is.null(destiny_file)){
    destiny_file<-tempfile(fileext = output.format)
  }
  if(is.character(layer)){
    cat("Getting OSM bbox",sep = "\n")
    feature<-osmdata::getbb(place_name = layer,format_out = "sf_polygon")
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
  }
  
  if(st_geometry_type(layer,by_geometry = F)%in%c("POLYGON","MULTIPOLYGON")==FALSE){
    cat("Getting layer bbox")
    feature<-st_as_sfc(st_bbox(feature))
    cat("Using:")
    print(feature)
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
    cat(paste0("Downloading tile ",i," of ",nrow(filter),"  Size: ",filter$size_mb[i],"mb"),sep="\n")
    filegz<-tempfile(fileext = ".csv.gz")
    httr::GET(url=filter$tile_url[i],
              httr::write_disk(filegz))
    filecsv<-tempfile(fileext = ".csv")
    R.utils::gunzip(filegz,destname=filecsv)
    cat("Writing file ",i, " of ",nrow(filter))
    if(output.format=="csv"){
      fread(filecsv) %>% 
        st_as_sf(wkt="geometry",crs=4326) %>% 
        st_filter(feature) %>% 
        mutate(wkt=st_as_text(geometry)) %>% 
        st_drop_geometry() %>% 
        as.data.frame() %>% 
        fwrite(destiny_file,append=TRUE)
    }else{
      fread(filecsv) %>% 
      st_as_sf(wkt="geometry",crs=4326) %>% 
        st_filter(feature) %>% 
        st_write(destiny_file,append=TRUE)
    }
    
    
  }
  
}

