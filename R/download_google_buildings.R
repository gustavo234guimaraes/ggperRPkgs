

download_google_buildings<-function(layer,destiny_file=NULL){
  output.format<-str_sub(destiny_file,start=str_locate(destiny_file,"\\.")[,1]+1)
  if(output.format%in%c("csv","txt","geojson","kml","shp","gpkg")==FALSE){
    stop('Invalid output format, see details')
  }
  if(is.null(destiny_file)){
    destiny_file<-tempfile(fileext = output.format)
  }
  if(is.character(layer)){
    cat("Getting OSM bbox",sep = "\n")
    feature<-osmdata::getbb(place_name = layer,format_out = "sf_polygon") %>% 
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
    cat("Using: ",sep = "\n")
    print(feature)
  }else{
    if(st_geometry_type(layer,by_geometry = F)%in%c("POLYGON","MULTIPOLYGON")==FALSE){
      cat("Getting layer bbox",sep = "\n")
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
    cat(paste0("Downloading tile ",i," of ",nrow(filter),"  Size: ",filter$size_mb[i],"mb"),sep="\n")
    filegz<-tempfile(fileext = ".csv.gz")
    httr::GET(url=filter$tile_url[i],
              httr::write_disk(filegz),progress(type = "down"),config(max_recv_speed_large = filter$size_mb*1000))
    filecsv<-tempfile(fileext = ".csv")
    R.utils::gunzip(filegz,destname=filecsv)
    cat(paste0("Writing file ",i, " of ",nrow(filter)),sep = "\n")
    cat("\n")
    box<-st_bbox(st_buffer(feature,2000))
    if(memuse::Sys.meminfo()$totalram@size<16&file.info(filecsv)$size>4000000|memuse::Sys.meminfo()$totalram@size<7&file.info(filecsv)$size>2500000){
      if(memuse::Sys.meminfo()$totalram@size<7){
        maxlines=1000000
      }else{
        maxlines=3000000
      }
      nlines<-length(count.fields(filecsv))
      cat("This building data is too large, the reading process will splitted",sep="\n")
      nlines<-length(count.fields(filecsv))
      skips<-trunc(nlines/maxlines)
      skips<-cumsum(rep(maxlines,skips))
      skips<-c(0,skips)
      for (i in 1:length(skips)) {
        cat(paste0("Processing step ",i," of ",length(skips)),sep = "\n")
        if(output.format%in%c("csv","txt")){
          result<-fread(filecsv,skip = skips[i], nrows = maxlines,
                        col.names = c("latitude","longitude","area_in_meters",
                                      "confidence","geometry","full_plus_code")) %>% 
            dplyr::filter('latitude'>min(box[c(2,4)])&'latitude'<max(box[c(2,4)])) %>% 
            dplyr::filter('longitude'>min(box[c(1,3)])&'longitude'<max(box[c(1,3)])) %>% 
            st_as_sf(wkt="geometry",crs=4326) %>% 
            st_filter(feature) %>% 
            mutate(wkt=st_as_text(geometry)) %>% 
            st_drop_geometry() %>% 
            as.data.frame()
          if(nrow(result)>0){
            fwrite(result,destiny_file,append=TRUE)
          }
            
        }else{
          result<-fread(filecsv,skip = skips[i],nrows = maxlines,
                        col.names = c("latitude","longitude","area_in_meters",
                                      "confidence","geometry","full_plus_code"))
          result<-dplyr::filter(result, 'latitude' > min(box[c(2,4)]) & 'latitude' < max(box[c(2,4)])) %>% 
            dplyr::filter('longitude'>min(box[c(1,3)])&'longitude'<max(box[c(1,3)])) %>% 
            st_as_sf(wkt="geometry",crs=4326) %>% 
            st_filter(feature)
          if(nrow(result)>0){
            st_write(result,destiny_file,append=TRUE)
          }
            
        }
        
        
      }
    }else{
      if(output.format%in%c("csv","txt")){
        fread(filecsv) %>% 
          dplyr::filter('latitude'>min(box[c(2,4)])&'latitude'<max(box[c(2,4)])) %>% 
          dplyr::filter('longitude'>min(box[c(1,3)])&'longitude'<max(box[c(1,3)])) %>% 
          st_as_sf(wkt="geometry",crs=4326) %>% 
          st_filter(feature) %>% 
          mutate(wkt=st_as_text(geometry)) %>% 
          st_drop_geometry() %>% 
          as.data.frame() %>% 
          fwrite(destiny_file,append=TRUE)
      }else{
        fread(filecsv) %>% 
          dplyr::filter('latitude'>min(box[c(2,4)])&'latitude'<max(box[c(2,4)])) %>% 
          dplyr::filter('longitude'>min(box[c(1,3)])&'longitude'<max(box[c(1,3)])) %>% 
          st_as_sf(wkt="geometry",crs=4326) %>% 
          st_filter(feature) %>% 
          st_write(destiny_file,append=TRUE)
      }
    }
    
    
    
  }
  cat("Done!",sep = "\n")
}

