#' Get the Coordinants of Peaks
#'
#' Get the coordinants of local maximums
#'
#' @param df a data.frame
#' @return a data.frame with the coordinants on peaks
#' @export

get_peaks<-function(df){
  temp<-df[df$image_path==paths[i],]
  temp$drv1_smoothed2[is.na(temp$drv1_smoothed2)]<-0
  maxs<-localMaxima(temp$drv1_smoothed2)
  peaks<-data.frame(drv1_smoothed2=temp$drv1_smoothed2[maxs],
                    cms=temp$cms[maxs])
  return(peaks)
}
