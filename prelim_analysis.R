library(magick)
library(raster)
library(tidyverse)
library(zoo)
r<-raster("C:/Users/ConservationMetrics/Desktop/Session_15AF2D4E-DE65-3440-BCB7-74671FAAC012#00_0A/image000000008.png")

plot(r)

r_fort<-raster::as.data.frame(r,xy=T)

head(r_fort)

dat<-r_fort %>% 
  # filter(x>334,x<513) %>% #pick a time range to work with
  group_by(y) %>% 
  rename(reflect=image000000008) %>% # need to do something here so that it is generalized
  summarise(mean=mean(reflect)) %>%
  ungroup %>% 
  mutate(smoothed = rollmean(x = mean-lag(mean),k=50,fill = NA), # k= 50 was a bit arbitrary
         smoothed2 = rollmean(x =smoothed,k=50,fill=NA,align = "center")) 

dat %>% filter(y<750) %>% filter(smoothed2 == max(smoothed2,na.rm=T)) %>% mutate(y=y-max(dat$y))
  
ggplot(data=dat,aes(y,smoothed2))+
  geom_path()+
  coord_flip()+
  labs(x="Depth (Pixels)",y="Mean Reflectivity")
max(r_fort$x)
