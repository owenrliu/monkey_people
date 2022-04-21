# Explore transect location datasets from Maria Voigt
library(tidyverse)
library(sf)
library(here)
library(rnaturalearth)
library(lubridate)

#background map
bg <- ne_countries('large',country=c("Indonesia","Malaysia"),returnclass = 'sf')

ggplot(bg)+
  geom_sf()

# Import data
dat <- list.files(here('data'),full.names = T) %>% map(read_csv)

#### first dataset
dat1 <- pluck(dat,1)
glimpse(dat1)

# spatialize and make lines
dat1_lines <- pmap_df(list(dat1$long_start,dat1$lat_start,dat1$long_end,dat1$lat_end),function(x1,y1,x2,y2){
  matrix(c(x1,x2,y1,y2),ncol=2) %>% st_linestring() %>% st_sfc() %>% st_set_crs(4326) %>% st_as_sf
})
dat1_sf <- dat1 %>% bind_cols(dat1_lines) %>% st_sf(sf_column_name = "x")
bbox <- st_bbox(dat1_sf)+c(-1,-1,1,1)

dat1_plot <- ggplot()+
  geom_sf(data=bg,aes(fill=name_sort))+
  geom_sf(data=dat1_sf,aes(color=factor(year)))+
  # scale_fill_manual(values=c("#e69f00","#2271B2"))+
  scale_fill_manual(values=c("#e69f00","white"))+
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))+
  # xlim(bbox[1],bbox[3])+ylim(bbox[2],bbox[4])+
  labs(color="Year",fill="Country")
dat1_plot

#### second dataset
dat2 <- pluck(dat,2)
glimpse(dat2)

# NOTE: lots of NAs here, mostly for the "end of transect" pieces. Maybe these are points, not lines? remove them for now though
dat2 <- dat2 %>% drop_na()
# spatialize and make lines
dat2_lines <- pmap_df(list(dat2$long_start,dat2$lat_start,dat2$long_end,dat2$lat_end),function(x1,y1,x2,y2){
  matrix(c(x1,x2,y1,y2),ncol=2) %>% st_linestring() %>% st_sfc() %>% st_set_crs(4326) %>% st_as_sf
})
dat2_sf <- dat2 %>% bind_cols(dat2_lines) %>% st_sf(sf_column_name = "x")
bbox <- st_bbox(dat2_sf)+c(-1,-1,1,1)

dat2_plot <- ggplot()+
  geom_sf(data=bg,aes(fill=name_sort))+
  geom_sf(data=dat2_sf,aes(color=factor(year)),size=2)+
  # scale_fill_manual(values=c("#e69f00","#2271B2"))+
  scale_fill_manual(values=c("#e69f00","white"))+
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))+
  # xlim(bbox[1],bbox[3])+ylim(bbox[2],bbox[4])+
  labs(color="Year",fill="Country")
dat2_plot

#### third dataset
dat3 <- pluck(dat,3)
glimpse(dat3)

# NOTE: this data in UTM zone 47N
dat3 <- dat3 %>%
  # fix dates
  mutate(date1=dmy_hm(date_start)) %>% 
  mutate(date2=dmy_hms(date_start)) %>% 
  mutate(date3=dmy(date_start)) %>% 
  mutate(date=coalesce(date1,date2)) %>% 
  mutate(date=coalesce(date,date3)) %>% 
  mutate(year=year(date))

# spatialize and make lines
dat3_lines <- pmap_df(list(dat3$X_start,dat3$Y_start,dat3$X_end,dat3$Y_end),function(x1,y1,x2,y2){
  matrix(c(x1,x2,y1,y2),ncol=2) %>% st_linestring() %>% st_sfc() %>% st_set_crs(32647) %>% st_as_sf
})
dat3_sf <- dat3 %>% bind_cols(dat3_lines) %>% st_sf(sf_column_name = "x") %>% 
  st_transform(4326) %>% 
  filter(st_is_valid(.))
  
bbox <- st_bbox(dat3_sf)+c(-1,-1,1,1)

dat3_plot <- ggplot()+
  geom_sf(data=bg,aes(fill=name_sort))+
  geom_sf(data=dat3_sf,aes(color=factor(year)),size=2)+
  # scale_fill_manual(values=c("#e69f00","#2271B2"))+
  scale_fill_manual(values=c("#e69f00","white"))+
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))+
  # xlim(bbox[1],bbox[3])+ylim(bbox[2],bbox[4])+
  labs(color="Year",fill="Country")
dat3_plot

# all plots
library(cowplot)
datplot <- plot_grid(dat1_plot,dat2_plot,dat3_plot,nrow=2,ncol=2)
datplot
