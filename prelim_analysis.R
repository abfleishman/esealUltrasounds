library(magick)
library(raster)
library(tidyverse)
library(patchwork)
library(zoo)
library(quantmod)
require(XML)
library(esealUltrasounds)
# try a cut off of 30,000 to start (no peak below )
## normalize
## interactive choice between peaks (peak a vs peak b)
## integrate under the peaks somehow
## tag depth
##
## List Paths
paths=list.files("/git_repos/esealUltrasounds",pattern = ".png",full.names = T,recursive = T)

# list all the image dirs (need to make this generic)
image_dirs<-list.dirs("/git_repos/esealUltrasounds",full.names = T)
image_dirs<-image_dirs[str_detect(image_dirs,"Session_.*")][1]
paths=list.files(image_dirs,pattern = ".png",full.names = T,recursive = T)

# read metadata
eseal_meta<-map_dfr(image_dirs,read_eseal_meta)

# load all the rasters
rasters<-map(paths,raster)

# plot and get the analysis sections
eseal_coords<-map_dfr(rasters,get_raster_coords)

# read rasters and flatten over analysis section (time dim)
us_dat<-pmap_dfr(list(path=eseal_coords$image_path,xmin=eseal_coords$xmin,xmax=eseal_coords$xmax),read_ultrasound)

# add im_dir, im_name and join with metadata and coords
us_dat <-us_dat %>% mutate(image_dir = dirname(image_path),imagefile = basename(image_path)) %>%
  left_join(eseal_meta) %>%
  left_join(eseal_coords)

# prep dist axis smooth the mean, and then smoothe the 1st deriv twice
dat<-us_dat %>%
  group_by(image_path) %>%
  mutate(cms=((y/as.numeric(as.character(y_res)))*100)-max((y/as.numeric(as.character(y_res)))*100),
         mean = rollmean(x = mean,k=10,fill = NA), # k= 50 was a bit arbitrary
         drv1 =  mean-lag(mean),
         drv2 =  drv1-lag(drv1),
         drv1_smoothed = rollmean(x = drv1, k=50,fill = NA), # k= 50 was a bit arbitrary
         drv1_smoothed2 = rollmean(x =drv1_smoothed,k=50,fill=NA,align = "center"),
         drv2_smoothed = rollmean(x = drv2, k=50,fill = NA), # k= 50 was a bit arbitrary
         drv2_smoothed2 = rollmean(x =drv2_smoothed,k=50,fill=NA,align = "center"))


ggplot()+
   geom_raster(data=hm,aes(x,cms,fill=ref_norm))+
  geom_vline(xintercept = c(xs$xmin,xs$xmax ),col="red",size = 1,linetype=2)+
    geom_path(data=dat %>% filter(image_path==paths[i]),
            aes(drv1_smoothed2,cms),col="black",show.legend = T)+
  scale_fill_viridis_c(option = "plasma" )+
  geom_point(data = peaks,aes(x=drv1_smoothed2,y=cms))


i=1
plot(rasters[[i]])
lines(dat$drv1_smoothed[dat$image_path==paths[i]],dat$y[dat$image_path==paths[i]])

# make four panel plots
pdf('plots.pdf',width = 8,height = 8,onefile = T)
for(i in 1:length(paths)){
  xs<-dat %>% filter(image_path==paths[i]) %>% select(xmin,xmax,y_res) %>% distinct()

  hm<-as.data.frame(rasters[[i]],xy=T)
  names(hm)[3]<-"reflect"

  hm<-hm %>%    mutate(cms=((y/as.numeric(as.character(xs$y_res)))*100)-max((y/as.numeric(as.character(xs$y_res)))*100),
                       ref_norm = scale(reflect,center = T,scale = T))
  peaks<-get_peaks(dat %>% filter(image_path==paths[i]))

  plot(
    ggplot()+
      geom_raster(data=hm,aes(x,cms,fill=ref_norm))+
      geom_vline(xintercept = c(xs$xmin,xs$xmax ),col="red",size = 1,linetype=2)+
      geom_path(data=dat %>% filter(image_path==paths[i]),
                aes(drv1_smoothed2,cms),col="grey80",show.legend = T)+
      geom_point(data = peaks,aes(x=drv1_smoothed2,y=cms),col="red")+
      scale_fill_viridis_c(option = "plasma" )+
      scale_y_continuous(breaks = seq(0,-9,-.5))+
      theme_bw()  +
      # geom_segment(x=xs$xmin,xend=xs$xmax,y=peaks$x+w,yend=peaks$x+w,col="green",linetype=4)+

      ggplot(data=dat %>% filter(image_path==paths[i]) ,aes(col=imagename))+
      facet_wrap(~givenname+imagename,scales = "free")+
      geom_path(aes(cms,mean),show.legend = F)+
      coord_flip()+
      scale_x_continuous(breaks =  seq(0,-9,-.5))+
      labs(x="Depth (cm)",y="Mean Reflectivity")+theme_bw()+plot_layout(nrow=1,widths = c(3,1)))
}
# can we set it up to let a user pick one of the peaks?
getGraphicsEvent
dev.off()
library(plotly)
ggplotly(
  ggplot(data=dat %>% filter(image_path==paths[i]) ,aes(col=imagename))+
    facet_wrap(~givenname+imagename,scales = "free")+
    geom_path(aes(y,smoothed2),show.legend = F)+
    coord_flip()+
    labs(x="Depth (Pixels)",y="Mean Reflectivity")+theme_bw()
)
