library(magick)
library(raster)
library(tidyverse)
library(zoo)

# depth axis or raster
# try a cut off of 30,00 to start (no peak below )
## normalize
## interactive choice between peaks
## integrate under the peaks somehow
## code to pull out metadata to Rachel
## tag depth
paths=list.files("/git_repos/eseal_ultrasounds",pattern = ".png",full.names = T,recursive = T)

require(XML)
image_dir<-"/git_repos/eseal_ultrasounds/Session_15AF2D4E-DE65-3440-BCB7-74671FAAC012#00_0A"

image_dirs<-list.dirs("/git_repos/eseal_ultrasounds",full.names = T)[43:44]
eseal_meta<-map_dfr(image_dirs,read_eseal_meta)

rasters<-map(paths,raster)
eseal_coords<-map_dfr(rasters,get_raster_coords)

us_dat<-pmap_dfr(list(path=eseal_coords$image_path,xmin=eseal_coords$xmin,xmax=eseal_coords$xmax),read_ultrasound)

us_dat <-us_dat %>% mutate(image_dir = dirname(image_path),imagefile = basename(image_path)) %>%
  left_join(eseal_meta) %>%
  left_join(eseal_coords)

head(us_dat)
table(us_dat$imagename,us_dat$image_dir)
head(dat)
dat<-us_dat %>%
  group_by(image_path) %>%
  mutate(cms=((y/as.numeric(as.character(y_res)))*100)-max((y/as.numeric(as.character(y_res)))*100),
         mean = rollmean(x = mean,k=10,fill = NA), # k= 50 was a bit arbitrary
         smoothed = rollmean(x = mean-lag(mean),k=50,fill = NA), # k= 50 was a bit arbitrary
         smoothed2 = rollmean(x =smoothed,k=50,fill=NA,align = "center"))

ggplot(data=dat %>% filter(image_path==paths[i]) ,aes(col=imagename))+
  geom_path(aes(cms,mean),col="gray80",show.legend = F)+
  facet_wrap(~givenname+imagename,scales = "free")+
  geom_path(aes(cms,smoothed2),col="red",alpha=.5,show.legend = F)+
  coord_flip()+
  labs(x="Depth (Pixels)",y="Mean Reflectivity")+theme_bw()
library(quantmod)
findPeaks(hm)
# dat %>% filter(y<750) %>% filter(smoothed2 == max(smoothed2,na.rm=T)) %>% mutate(y=y-max(dat$y))
library(patchwork)

i=1
plot(x=hm$y[findPeaks(hm$reflect, thresh=100)],y=hm$reflect[findPeaks(hm$reflect, thresh=100)])
library(zoo)

argmax <- function(x, y, w=1, ...) {
  n <- length(y)
  y.smooth <- loess(y ~ x)$fitted
  y.max <- rollapply(zoo(y.smooth), 2*w+1, max, align="center")
  delta <- y.max - y.smooth[-c(1:w, n+1-1:w)]
  i.max <- which(delta <= 0) + w
  list(x=x[i.max], i=i.max, y.hat=y.smooth)
}

test <- function(x,y,w, span) {
  peaks <- argmax(x, y, w=w, span=span)

  plot(x, y, cex=0.75, col="Gray", main=paste("w = ", w, ", span = ", span, sep=""))
  lines(x, peaks$y.hat,  lwd=2) #$
  y.min <- min(y)
  sapply(peaks$i, function(i) lines(c(x[i],x[i]), c(y.min, peaks$y.hat[i]),
                                    col="Red", lty=2))
  points(x[peaks$i], peaks$y.hat[peaks$i], col="Red", pch=19, cex=1.25)
}

peaks <- argmax(x, y, w=w, span=span)
peaks$y.hat[peaks$i]
peaks$x
test(x = x,y = y,w = 10,span = .01)
x=dat %>% filter(image_path==paths[i],!is.na(smoothed2)) %>% pull(y)
y=dat %>% filter(image_path==paths[i],!is.na(smoothed2)) %>% pull(smoothed2)
test(x,y,w = 100,span = .001)
ggplot(hm,aes(x,y,fill=reflect))+
       geom_raster(interpolate = T) +
       scale_fill_viridis_c(option = "plasma" )+
       geom_vline(xintercept = c(xs$xmin,xs$xmax ),col="red",size = 1,linetype=2)+
       theme_bw()+geom_segment(x=xs$xmin,xend=xs$xmax,y=peaks$x+50,yend=peaks$x+50,col="green",linetype=4)

w=100
span = 0.01
pdf('plots.pdf',width = 8,height = 8,onefile = T)
for(i in 1:length(paths)){
  hm<-as.data.frame(rasters[[i]],xy=T)
  names(hm)[3]<-"reflect"
  xs<-dat %>% filter(image_path==paths[i]) %>% select(xmin,xmax) %>% distinct()
  # x=dat %>% filter(image_path==paths[i],!is.na(smoothed2)) %>% pull(y)
  # y=dat %>% filter(image_path==paths[i],!is.na(smoothed2)) %>% pull(mean)
  # peaks <- argmax(x, y, w=w, span=span)
  plot(ggplot(hm,aes(x,y,fill=reflect))+
    geom_raster(interpolate = T) +
    scale_fill_viridis_c(option = "plasma" )+
    geom_vline(xintercept = c(xs$xmin,xs$xmax ),col="red",size = 1,linetype=2)+
    theme_bw()+
      # geom_segment(x=xs$xmin,xend=xs$xmax,y=peaks$x+w,yend=peaks$x+w,col="green",linetype=4)+

    ggplot(data=dat %>% filter(image_path==paths[i]) ,aes(col=imagename))+
    facet_wrap(~givenname+imagename,scales = "free")+
    geom_path(aes(cms,smoothed),show.legend = F)+
    coord_flip()+
    labs(x="Depth (cm)",y="Mean Reflectivity")+
    theme_bw()+
    ggplot(data=dat %>% filter(image_path==paths[i]) ,aes(col=imagename))+
    facet_wrap(~givenname+imagename,scales = "free")+
    geom_path(aes(cms,mean),show.legend = F)+
    coord_flip()+
    labs(x="Depth (cm)",y="Mean Reflectivity")+theme_bw()+
    ggplot(data=dat %>% filter(image_path==paths[i]) ,aes(col=imagename))+
    facet_wrap(~givenname+imagename,scales = "free")+
    geom_path(aes(cms,smoothed2),show.legend = F)+
    coord_flip()+
    labs(x="Depth (cm)",y="Mean Reflectivity")+theme_bw()+plot_layout(ncol = 2))
}
dev.off()
library(plotly)
ggplotly(
ggplot(data=dat %>% filter(image_path==paths[i]) ,aes(col=imagename))+
  facet_wrap(~givenname+imagename,scales = "free")+
  geom_path(aes(y,smoothed2),show.legend = F)+
  coord_flip()+
  labs(x="Depth (Pixels)",y="Mean Reflectivity")+theme_bw()
)
