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
area_under_peaks<-function(df){
  # df<-df %>% mutate(drv1_smoothed2=rollmean(drv1_smoothed2,k=30,fill = "NA"))
  peaks<-get_peaks(df) %>%
    # filter(abs(cms)>1,abs(drv1_smoothed2)>max(abs(drv1_smoothed2))*.1) %>%
    bind_rows(data.frame(drv1_smoothed2 = 0 ,  cms=0, type="min",stringsAsFactors = F))%>%
    arrange(-cms) %>%
    # select(-drv1_smoothed2) %>%
    mutate(id=row_number())
  # %>% filter(type=="min")

  integ_breaks<-peaks[peaks$type=="min",c("cms","id")]
  names(integ_breaks)[1]<-"start"
  integ_breaks$end<-lag(integ_breaks$start)
  integ_breaks<-integ_breaks[!is.na(integ_breaks$end),]
  setDT(integ_breaks,key = c("start","end"))

  df$cms2<-df$cms
  setDT(df)

  temp<-foverlaps(df,y = integ_breaks,by.x=c("cms","cms2"))

  temp1<-temp %>%
    filter(!is.na(id)) %>%
    mutate(id=id-1) %>%
    group_by(id,start,end) %>%
    summarise(AUC=round(sum(mean),2))

  maxs<-peaks %>%
    left_join(temp1, by = "id")

  return(maxs)
}


