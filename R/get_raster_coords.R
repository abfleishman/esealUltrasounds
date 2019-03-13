#' Get Analysis Selection from a list of Rasters
#'
#' Create a data frome with the analysis coordinants for each ultrasound image. this is an interactive plot that a user must click twice to select/create an alanysis selection comprised of coordinants along the x axis indicating the min and max that will be averaged/flattend by read ultra sound.  This is required before running read_ultrasound.
#'
#' @param raster_obj list object with rasters read in with raster_obj = purrr::map(paths_to_rasters,raster)
#' @export
#' @return a data frame with with analysis selection x,y coordinants
#' @importFrom graphics plot


get_raster_coords<-function(raster_obj){
  print(raster_obj@file@name)
  dev.new(noRStudioGD = T)
  plot(raster_obj)
  pt1<-click()
  pt2<-click()
  good_coords<-data.frame(
    image_path=raster_obj@file@name,
    xmin=min(c(pt1[1],pt2[1])),
    xmax=max(c(pt1[1],pt2[1])),
    ymin=min(c(pt1[2],pt2[2])),
    ymax=max(c(pt1[2],pt2[2]))
  )
  return(good_coords)
}
