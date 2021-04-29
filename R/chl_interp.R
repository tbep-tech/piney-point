
library(tidyverse)
library(lubridate)
library(sf)
library(gstat)
library(sp)
library(raster)
library(mapview)
library(leaflet)
library(stars)
library(tbeptools)
library(ggmap)
library(extrafont)

data(rswqdat)
data(rsstatloc)
data(segmask)

# prj <- "+proj=longlat +datum=WGS84 +no_defs"
prj <- '+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

# non-bay stations
nonbay <- c('BH01', 'P Port 2', 'P Port 3', 'PM Out', '20120409-01', 'PPC41', 'P Port 4', 'PMB01')

# station locations
locs <- rsstatloc %>%
  dplyr::select(station) %>% 
  st_transform(crs = prj)

# transform segmask
segmask <- segmask %>% 
  st_transform(crs = prj)

# chl ltb targets
chltrgs <- c(4.6, 5.1)

# chl levels  
levs <- c('< 4.6, below management target', '4.6 - 5.1, above management target', '> 5.1, above regulatory threshold')
tmlevs <- c('During discharge (Mar 30 - Apr 9)', 'After discharge (Apr 10 to Present)')

# current chl data
chldat <- rswqdat %>%
  filter(!station %in% nonbay) %>%
  filter(source %in% 'fldep') %>% 
  filter(var %in% 'chla') %>%
  dplyr::select(station, date, var, val) %>%
  inner_join(locs, ., by = 'station') %>%
  mutate(
    # date = floor_date(date, unit = 'week'),
    date = case_when(
      date < as.Date('2021-04-10') ~ tmlevs[1],
      T ~ tmlevs[2]
    ),
    date = factor(date, levels = tmlevs),
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  )

cols <- RColorBrewer::brewer.pal(3, 'Blues')

# chldat bbox
bbox <- chldat %>%
  st_bbox %>%
  st_as_sfc() %>%
  st_as_sf() %>%
  as_Spatial()

# empty grid for interpolation
grd <- as.data.frame(spsample(bbox, "regular", n = 200000))
names(grd) <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd) <- TRUE  # Create SpatialPixel object
fullgrid(grd) <- TRUE  # Create SpatialGrid object
proj4string(grd) <- prj

# interpolate by date
ests <- chldat %>% 
  group_by(date) %>% 
  # filter(date == as.Date('2021-04-04')) %>% 
  nest %>% 
  mutate(
    mp = purrr::map(data, function(x){
      
      wkspa <- x %>%
        as_Spatial
      
      # add crs to grid
      proj4string(wkspa) <- prj
      
      # interpolate
      interp <- gstat::idw(log(val) ~ 1, wkspa, newdata = grd, idp = 1) %>%
        raster %>%
        mask(segmask) %>% 
        st_as_stars
      
      interp_poly <- interp %>% 
        dplyr::select(val = var1.pred) %>% 
        mutate(
          val = exp(val), 
          cat = case_when(
            val < chltrgs[1] ~ levs[1],
            val >= chltrgs[1] & val < chltrgs[2] ~ levs[2],
            val >= chltrgs[2] ~ levs[3], 
            T ~ NA_character_
          ), 
          cat = factor(cat, levels = levs)
        ) %>% 
        st_as_sf %>% 
        dplyr::select(cat) %>% 
        group_by(cat) %>%
        summarise(.groups = 'drop')
      
      out <- interp_poly
      
      return(out) 
      
    })
  ) %>% 
  dplyr::select(-data) %>% 
  unnest('mp') %>% 
  ungroup() %>% 
  st_as_sf() 

pall <- ggplot() +
  geom_sf(data = ests, aes(fill = cat), inherit.aes = F, color = NA) + 
  scale_fill_manual('Chlorophyll estimate (ug/L)', values = cols) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~date) + 
  theme_bw() + 
  theme(
    panel.background = element_rect(fill = 'lightgrey'),
    plot.background = element_rect(fill = 'transparent', colour = NA),
    legend.background = element_rect(fill = 'transparent', colour = NA),
    strip.background = element_blank(), 
    strip.text = element_text(size = 12, colour = 'white'),
    axis.title = element_blank(), 
    legend.position = 'top',
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    text = element_text(colour = 'white')
  ) + 
  guides(fill = guide_legend(direction = 'vertical', title.position = 'left'))
pall

png('figure/chl-all.png', width = 6, height = 4.5, units = 'in', res = 400, family = 'Lato', bg = 'transparent')
print(pall)
dev.off()

for(i in levels(ests$date)){
    
  toplo <- ests %>% 
    filter(date %in% i)
    
  pi <- ggplot() +
    geom_sf(data = toplo, aes(fill = cat), inherit.aes = F, color = NA) + 
    scale_fill_manual('Chlorophyll estimate (ug/L)', values = cols, drop = F) +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) +
    facet_wrap(~date) + 
    theme_bw() + 
    theme(
      panel.background = element_rect(fill = 'lightgrey'),
      legend.background = element_rect(fill = 'transparent', colour = NA),
      plot.background = element_rect(fill = 'transparent', colour = NA),
      strip.background = element_blank(), 
      strip.text = element_text(size = 14, colour = 'white'),
      axis.title = element_blank(), 
      legend.position = 'top',
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      axis.text = element_blank(), 
      axis.ticks = element_blank(), 
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 10),
      text = element_text(colour = 'white')
    ) + 
    guides(fill = guide_legend(direction = 'vertical', title.position = 'left'))
  
  png(paste0('figure/', i, '.png'), width = 4, height = 5, units = 'in', res = 400, family = 'Lato', bg = 'transparent')
  print(pi)
  dev.off()

}
