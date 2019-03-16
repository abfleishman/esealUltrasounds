#' Get the Coordinants of Peaks
#'
#' Get the coordinants of local maximums
#'
#' @param df a data.frame
#' @return a data.frame with the coordinants on peaks
#' @export

get_peaks<-function(df){
  df$drv1_smoothed2[is.na(df$drv1_smoothed2)]<-0
  maxs<-localMaxima(df$drv1_smoothed2)
  mins<-localMaxima(-df$drv1_smoothed2)
  peaks<-rbind(data.frame(drv1_smoothed2=df$drv1_smoothed2[maxs],
                    cms=df$cms[maxs], type="max",stringsAsFactors = F),
               data.frame(drv1_smoothed2=df$drv1_smoothed2[mins],
                          cms=df$cms[mins], type="min",stringsAsFactors = F))
  return(peaks)
}
