#' Choose Peaks
#'
#' Pick the peaks that have the largest AUC,
#'
#' @param df a data.frame
#' @param xs a data.frame
#' @param dist_col column name of the distance column
#' @param peaks_col column name of the smoothed data
#' @return a data.frame with the coordinants on peaks
#' @importFrom dplyr lag group_by summarise filter mutate row_number arrange bind_rows left_join
#' @importFrom data.table setDT foverlaps
#' @importFrom magrittr %>%
#'
#' @export

choose_peaks<-function(df,xs,dist_col = "cms", peaks_col = "drv1_smoothed3"){
  peaks<-area_under_peaks(df, dist_col,peaks_col )
  peaks$cms_diff<-abs(peaks$cms-xs$cms)

chosen_peaks<-peaks[peaks$AUC==max(peaks$AUC,na.rm=T)|peaks[[peaks_col]]==max(peaks[[peaks_col]],na.rm=T)|peaks$cms_diff==min(peaks$cms_diff,na.rm=T),]
chosen_peaks<-chosen_peaks[!is.na(chosen_peaks$cms),]
return(chosen_peaks)
}
