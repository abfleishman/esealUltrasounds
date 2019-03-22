library(data.table)
library(magick)
library(raster)
library(tidyverse)
library(patchwork)
library(zoo)
library(quantmod)
require(XML)
library(esealUltrasounds)

# TO DO:
# try a cut off of 30,000 to start (no peak below ): could set all vals below to 0
## normalize raster, then calculate everything?
## interactive choice between peaks (peak a vs peak b)
## tag depth

# List Paths to PNGs
paths=list.files("/git_repos/esealUltrasounds",pattern = ".png",full.names = T,recursive = T)
paths<-as.character(paths)
# list all the image dirs
image_dirs<-as.character(unique(dirname(paths)))

# read metadata
eseal_meta<-map_dfr(image_dirs,read_eseal_meta)

# load all the rasters in to a list
rasters<-map(paths,raster)

# # what if we transform the raster
# rat<-rasters[[i]]
# crs(rat)<-CRS("+proj=utm +zone=33 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")
# par(mfrow=c(3,3))
# y <- focal(rasters[[i]], w=matrix(1, 51, 1), mean) # this is smoothing the y by 51 pixels
# plot(y)
# plot(terrain(rat,opt = "slope",unit = "tangent"))
# plot(terrain(rat,opt = "aspect",unit = "degrees"))
# plot(terrain(rat,opt = "TPI",unit = "degrees"))
# plot(terrain(rat,opt = "TRI",unit = "degrees"))
# plot(terrain(rat,opt = "roughness",unit = "degrees"))
# plot(terrain(rat,opt = "flowdir",unit = "degrees"))




# # Detect horizontal lines -------------------------------------------------
#
# # get image
# img <- image_read(paths[2])
# print(image_info(img))
#
# # define horizontal and vertical Sobel kernel
# Shoriz <- matrix(c(2, 3, 2, 0, 0, 0, -2, -3, -2), nrow = 3)
# Svert <- t(Shoriz)
#
# # get horizontal edges
# imgV <- image_convolve(img, Svert,iterations = 2)
#
# hm<-imgV %>% image_canny(geometry = "1x10+80%+99%")
#
# a<-raster::as.raster(hm)
# a[a!="#ffffffff"]<-0
# a[a=="#ffffffff"]<-1
#
# b<- matrix(data = as.numeric(a),nrow = dim(a)[2],ncol = dim(a)[1])
# plot(t(b) %>% raster)
# b %>% rowSums() %>% plot
# plot(a)


# ne ----------------------------------------------------------------------


# plot and get the analysis sections
eseal_coords<-map_dfr(rasters,get_raster_coords)
save(eseal_coords,file = "data/eseal_coords.rdata")
# read rasters and flatten over analysis section (time dim)
us_dat<-pmap_dfr(list(path=eseal_coords$image_path,xmin=eseal_coords$xmin,xmax=eseal_coords$xmax),read_ultrasound)

# add im_dir, im_name and join with metadata and coords
us_dat <-us_dat %>% mutate(image_dir = dirname(image_path),imagefile = basename(image_path)) %>%
  left_join(eseal_meta) %>%
  left_join(eseal_coords) %>%
  select(image_path, image_dir, imagefile, createtime, familyname,givenname, imagename,everything())

# prep dist axis smooth the mean, and then smoothe the 1st deriv twice
dat<-us_dat %>%
  group_by(image_path) %>%
  mutate(cms=((y/as.numeric(as.character(y_res)))*100)-max((y/as.numeric(as.character(y_res)))*100),
         mean = rollmean(x = mean,k=2,fill = NA), # k= 50 was a bit arbitrary
         drv1 =  mean-lag(mean),
         drv2 =  drv1-lag(drv1),
         drv1_smoothed = rollmean(x = drv1, k=50,fill = NA), # k= 50 was a bit arbitrary
         drv1_smoothed2 = rollmean(x =drv1_smoothed,k=50,fill=NA,align = "center"),
         drv1_smoothed3 = rollmean(x =drv1_smoothed2,k=30,fill=NA,align = "center"),
         drv2_smoothed = rollmean(x = drv2, k=50,fill = NA), # k= 50 was a bit arbitrary
         drv2_smoothed2 = rollmean(x =drv2_smoothed,k=50,fill=NA,align = "center"))

dat %>%
  group_by(image_path) %>%
  filter(drv1_smoothed3==max(drv1_smoothed3,na.rm = T))

# make four panel plots
pdf('plots.pdf',width = 8,height = 8,onefile = T)
for(i in 1:length(paths)){
  # get the coords for the analysis selection
  xs<-dat %>% ungroup %>% filter(image_path==paths[i]) %>%
    select(xmin,xmax,ymin,ymax,y_res) %>%
    distinct() %>%
    mutate(cms=(mean(c(ymin,ymax))/as.numeric(as.character(y_res)))-(973.5/as.numeric(as.character(y_res)))*100)
  if(nrow(xs)==0) next
  if(is.na(xs$cms)|nrow(xs)==0) next
  # get the raster ad a long data.frame
  hm<-as.data.frame(rasters[[i]],xy=T)
  names(hm)[3]<-"reflect"

  # add cms and create a normalized val
  hm<-hm %>%    mutate(cms=((y/as.numeric(as.character(xs$y_res)))*100)-max((y/as.numeric(as.character(xs$y_res)))*100),
                       ref_norm = scale(reflect,center = T,scale = T))
  xs$cms<-((mean(c(xs$ymin,xs$ymax))/as.numeric(as.character(xs$y_res)))*100)-(max(hm$y)/as.numeric(as.character(xs$y_res))*100)
  peaks<-area_under_peaks(dat %>% filter(image_path==paths[i]),
                          dist_col = "cms",peaks_col = "drv1_smoothed3")
  chosen_ones<-peaks %>%
    mutate(cms_diff=abs(cms-xs$cms)) %>%
    filter(AUC==max(AUC,na.rm=T)|drv1_smoothed3==max(drv1_smoothed3,na.rm=T)|cms_diff==min(cms_diff))

  plot(
    ggplot()+
      geom_raster(data=hm,aes(x,cms,fill=sqrt(reflect)))+
      geom_vline(xintercept = c(xs$xmin,xs$xmax ),col="red",size = 1,linetype=2)+
      geom_path(data=dat %>% filter(image_path==paths[i]),
                aes(drv1_smoothed2,cms),col="grey80",show.legend = T)+
      geom_point(data = peaks,aes(x=drv1_smoothed3,y=cms,col=type))+
      geom_point(data = peaks,aes(x=AUC/10000,y=cms),col="red",pch=6)+
      geom_point(data = chosen_ones,aes(x=drv1_smoothed3,y=cms),col="green")+
      scale_fill_gradient(low = "black",high = "white")+
      # scale_fill_viridis_c(option = "plasma" )+
      scale_y_continuous(breaks = seq(0,-9,-.5))+
      theme_bw()  +
      # geom_segment(x=xs$xmin,xend=xs$xmax,y=peaks$x+w,yend=peaks$x+w,col="green",linetype=4)+

      ggplot(data=dat %>% filter(image_path==paths[i]) ,aes(col=imagename))+
      facet_wrap(~givenname+imagename,scales = "free")+
      geom_path(aes(cms,mean),show.legend = F)+
      coord_flip()+
      scale_x_continuous(breaks =  seq(0,-9,-.5))+
      labs(x="Depth (cm)",y="Mean Reflectivity")+theme_bw()+

      plot_layout(nrow=1,widths = c(3,1)))
}
dev.off()

# can we set it up to let a user pick one of the peaks?
getGraphicsEvent

library(plotly)
ggplotly(
  ggplot(data=dat %>% filter(image_path==paths[i]) ,aes(col=imagename))+
    facet_wrap(~givenname+imagename,scales = "free")+
    geom_path(aes(y,smoothed2),show.legend = F)+
    coord_flip()+
    labs(x="Depth (Pixels)",y="Mean Reflectivity")+theme_bw()
)
