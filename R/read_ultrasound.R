read_ultrasound<-function(path,xmin,xmax){
  r<-raster(path)
  r_df<-raster::as.data.frame(r,xy=T)
  r_df$image_path<-path
  names(r_df)[3]<-"reflect"
  r_df<-r_df %>%
    filter(x>xmin,x<xmax) %>%
    group_by(image_path,y) %>%
    summarise(mean=mean(reflect))
  return(r_df)
}
