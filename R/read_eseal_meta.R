library(XML)
library(purrr)
read_eseal_meta<-function(image_dir){
  data <- xmlParse(paste0(image_dir,"/metadata.xml"))
  xml_data <- xmlToList(data)
  eseal_meta<-data.frame(width = map_chr(1:length(xml_data[["images"]]),function(x)xml_data[["images"]][[x]][["imagesize-primary-base"]][["width"]]),
                         height = map_chr(1:length(xml_data[["images"]]),function(x)xml_data[["images"]][[x]][["imagesize-primary-base"]][["height"]]),
                         y_res = map_chr(1:length(xml_data[["images"]]),function(x)xml_data[["images"]][[x]][["y-units-primary-base"]][["resolution"]]),
                         y_units = map_chr(1:length(xml_data[["images"]]),function(x)xml_data[["images"]][[x]][["y-units-primary-base"]][["unit"]]),
                         px_aspect_ratio = map_chr(1:length(xml_data[["images"]]),function(x)xml_data[["images"]][[x]][["graph-pixel-aspect-ratio"]][["value"]]),
                         imagefile = map_chr(1:length(xml_data[["images"]]),function(x)xml_data[["images"]][[x]][[".attrs"]][["imagefile"]]),
                         imagename = map_chr(1:length(xml_data[["images"]]),function(x)xml_data[["images"]][[x]][[".attrs"]][["imagename"]]),
                         createtime = map_chr(1:length(xml_data[["images"]]),function(x)xml_data[["images"]][[x]][[".attrs"]][["createtime"]]),
                         familyname = xml_data[["patient"]][["familyname"]],
                         givenname = xml_data[["patient"]][["givenname"]],
                         image_dir = image_dir)

  return(eseal_meta)
}
