
download_pbf<-function(place,dest.path=getwd(),fname="extraction"){
  if(is.character(place)){
    place<-osmdata::getbb(place_name = place,format_out = "sf_polygon",limit = 1) %>% 
      st_bbox()
  }else{
    if(("sf"%in%class(place)|"bbox"%in%class(place))==FALSE){
      stop("The place format must be a character, bbox or a sf")
      return(NULL)
    }
    if("sf"%in%class(place)){
      place<-st_bbox(place)
    }
  }
  
  extracted_pbf<-oe_download(
    file_url = oe_match(
      place=place
    )$url,
    download_directory = tempdir()
  )
  
  system2(command = get_osmconverter_path(),
          args = paste0(extracted_pbf,' -b=',paste0(place,collapse = ","),' -o=',paste0(dest.path,"\\",fname,".osm.pbf")),
          invisible = FALSE,wait = TRUE)
  
}





