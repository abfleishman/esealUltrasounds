#' Get the Area Under Peaks
#'
#' Get the area under peaks of local maximums
#'
#' @param df a data.frame
#' @return a data.frame with the coordinants on peaks
#' @importFrom dplyr lag group_by summarise filter mutate row_number arrange bind_rows left_join
#' @importFrom data.table setDT foverlaps
#' @importFrom magrittr %>%
#'
#' @export

# get the peaks for for plotting
area_under_peaks<-function(df,dist_col,peaks_col){
  empty_row<-data.frame(col= 0 ,  col2=0, type="min",stringsAsFactors = F)
  names(empty_row)[1:2]<-c(dist_col,peaks_col)

  peaks<-get_peaks(df, dist_col, peaks_col)   %>%
    bind_rows(empty_row) %>%
    arrange_(paste("-",dist_col)) %>%
    mutate(id=row_number())

  integ_breaks<-peaks[peaks$type=="min",c(dist_col,"id")]
  names(integ_breaks)[1]<-"start"
  integ_breaks$end<-lag(integ_breaks$start)
  integ_breaks<-integ_breaks[!is.na(integ_breaks$end),]
  setDT(integ_breaks,key = c("start","end"))

  df$dist2<-df[[dist_col]]
  setDT(df)

  temp<-foverlaps(df,y = integ_breaks,by.x=c(dist_col,"dist2"))

  temp1<-temp %>%
    filter(!is.na(id)) %>%
    mutate(id=id-1) %>%
    group_by(id,start,end) %>%
    summarise(AUC=round(sum(mean),2))

  maxs<-peaks %>%
    left_join(temp1, by = "id")

  return(maxs)
}


