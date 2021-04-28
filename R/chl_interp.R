
library(tidyverse)
library(lubridate)
library(sf)
library(gstat)
library(sp)
library(raster)
library(mapview)
library(leaflet)
library(stars)
library(ggmap)
library(extrafont)

data(rswqdat)
data(rsstatloc)
data(segmask)

prj <- "+proj=longlat +datum=WGS84 +no_defs"

# non-bay stations
nonbay <- c('BH01', 'P Port 2', 'P Port 3', 'PM Out', '20120409-01', 'PPC41', 'P Port 4', 'PMB01')

# station locations
locs <- rsstatloc %>%
  dplyr::select(station)

# chl ltb targets
chltrgs <- targets %>% 
  filter(bay_segment == 'LTB') %>% 
  dplyr::select(chla_target, chla_thresh)
  
# chl levels  
levs <- c('< 4.6', '4.6 - 5.1, above management target', '> 5.1, above regulatory threshold')

# current chl data
chldat <- rswqdat %>%
  filter(!station %in% nonbay) %>%
  filter(var %in% 'chla') %>%
  dplyr::select(station, date, var, val) %>%
  inner_join(locs, ., by = 'station') %>%
  mutate(
    date = floor_date(date, unit = 'week')
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
  nest %>% 
  mutate(
    mp = purrr::map(data, function(x){
      
      wkspa <- x %>%
        as_Spatial

      # add crs to grid
      proj4string(wkspa) <- prj
      
      # interpolate
      interp <- suppressMessages(gstat::idw(val ~ 1, wkspa, newdata = grd, idp = 2.0)) %>%
        raster %>%
        mask(segmask) %>% 
        st_as_stars
      
      interp_poly <- interp %>% 
        dplyr::select(val = var1.pred) %>% 
        mutate(
          cat = case_when(
            val < chltrgs[['chla_target']] ~ levs[1],
            val >= chltrgs[['chla_target']] & val < chltrgs[['chla_thresh']] ~ levs[2],
            val >= chltrgs[['chla_thresh']] ~ levs[3], 
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
  st_as_sf() %>% 
  mutate(
    datelb = paste('Week of', format(date, '%b %d')),
    datelb = factor(datelb, levels = unique(datelb))
  )

##
# plots

# # basemap
# dat_ext <- unname(st_bbox(chldat))
# bsmap1 <- get_stamenmap(bbox = dat_ext, maptype = 'terrain-background', zoom = 12)
# 
# # change opacity of basemap
# mapatt <- attributes(bsmap1)
# bsmap1_transparent <- matrix(adjustcolor(bsmap1, 
#                                          alpha.f = 0.9), 
#                              nrow = nrow(bsmap1))
# attributes(bsmap1_transparent) <- mapatt


# ggmap(bsmap1) +
pall <- ggplot() +
  geom_sf(data = ests, aes(fill = cat), inherit.aes = F, color = NA) + 
  scale_fill_manual('Chlorophyll estimate (ug/L)', values = cols) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~datelb) + 
  theme_bw() + 
  theme(
    panel.background = element_rect(fill = 'lightgrey'),
    strip.background = element_blank(), 
    strip.text = element_text(size = 14),
    axis.title = element_blank(), 
    legend.position = 'top',
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  ) + 
  guides(fill = guide_legend(direction = 'vertical', title.position = 'left'))

tiff('figure/chl-all.tif', width = 6, height = 7, units = 'in', compression = 'lzw', res = 400, family = 'Lato')
print(pall)
dev.off()

for(i in levels(ests$datelb)){
    
  toplo <- ests %>% 
    filter(datelb %in% i)
    
  pi <- ggplot() +
    geom_sf(data = toplo, aes(fill = cat), inherit.aes = F, color = NA) + 
    scale_fill_manual('Chlorophyll estimate (ug/L)', values = cols, drop = F) +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) +
    facet_wrap(~datelb) + 
    theme_bw() + 
    theme(
      panel.background = element_rect(fill = 'lightgrey'),
      strip.background = element_blank(), 
      strip.text = element_text(size = 14),
      axis.title = element_blank(), 
      legend.position = 'top',
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      axis.text = element_blank(), 
      axis.ticks = element_blank(), 
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 10)
    ) + 
    guides(fill = guide_legend(direction = 'vertical', title.position = 'left'))
  
  tiff(paste0('figure/', i, '.tif'), width = 4, height = 5, units = 'in', compression = 'lzw', res = 400, family = 'Lato')
  print(pi)
  dev.off()

}
