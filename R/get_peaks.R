#' Get the Coordinants of Peaks
#'
#' Get the coordinants of local maximums
#'
#' @param df a data.frame
#' @return a data.frame with the coordinants on peaks
#' @export

get_peaks<-function(df, dist_col,peaks_col){
  df[is.na(df[[peaks_col]]),peaks_col]<-0
  maxs<-local_maxima(df[[peaks_col]])
  mins<-local_maxima(-df[[peaks_col]])
  peaks<-rbind(data.frame(peaks_col=df[maxs,peaks_col],
                          dist_col=df[maxs,dist_col], type="max",
                          stringsAsFactors = F),
               data.frame(peaks_col=df[mins,peaks_col],
                          dist_col=df[mins,dist_col], type="min",
                          stringsAsFactors = F))
  return(peaks)
}
