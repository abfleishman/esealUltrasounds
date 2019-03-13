#' Read Ultra Sound and flatten  time series
#'
#' Read in a PNG ultrasound file made with a XX ultrasound divice in to r and
#' filter for the selected region of the image and collapse the time axis by
#' taking a mean value for every row of pixels.
#'
#' @param path path to a png file
#' @param xmin the left x coordinant of the analysis selection
#' @param xmax the right x coordinant of the analysis selection
#' @export
#' @return a data frame with image path, y, and mean reflectivity
#' @importFrom raster raster as.data.frame
#' @importFrom dplyr filter group_by summarise
#' @importFrom magrittr %>%
read_ultrasound<-function(path,xmin,xmax){
  r<-raster(path)
  r_df<-raster::as.data.frame(r,xy=T)
  r_df$image_path<-path
  names(r_df)[3]<-"reflect"
  r_df<-r_df %>%
    filter(x>xmin,x<xmax) %>%
    group_by(image_path,y) %>%
    summarise(mean=mean(reflect),
              mean_norm=scale(mean,center = T,scale = T),
              sd = sd(reflect),
              median = median(reflect),
              min = min(reflect),
              max = max(reflect))
  return(r_df)
}
