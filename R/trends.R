
library(tidyverse)
library(lubridate)
library(mapview)
library(sf)
library(stars)
library(raster)
library(googlesheets4)
library(googledrive)
library(tbeptools)

data(rswqdat)
data(bswqdat)
data(bsstatloc)
data(rsstatloc)
data(ppseg)
data(segmask)

source('R/funcs.R')

loadfonts(device = 'pdf', quiet = T)
if(Sys.info()[1] == 'Windows')
  loadfonts(device = 'win', quiet = T)

# segments
ppseg <- ppseg %>% 
  rename(area = Name) %>% 
  group_by(area) %>% 
  summarise() %>% 
  st_buffer(dist = 0.0001) %>% 
  st_buffer(dist = -0.0001) %>% 
  mutate(
    area = factor(area)
  )

cols <- c("#E16A86", "#50A315", "#009ADE")
names(cols) <- levels(ppseg$area)

nonbay <- c('BH01', 'P Port 2', 'P Port 3', 'PM Out', '20120409-01', 'PPC41', 'P Port 4', 'PMB01', 'NGS-S Pond')

vr <- 'tn'

##
# wq data

# monitoring data
rswqtmp <- rswqdat %>% 
  filter(var == vr) %>% 
  filter(!station %in% nonbay) %>% 
  inner_join(rsstatloc, ., by = c('station', 'source')) %>% 
  st_intersection(ppseg) %>% 
  st_set_geometry(NULL) %>% 
  select(-qual, -bswqstation, -nrmrng, -source, -source_lng, -uni, -lbunis, -comment) %>% 
  mutate(
    date = floor_date(date, unit = 'week'), 
    fillcl = factor(area, levels = levels(area), labels = cols), 
    fillcl = as.character(fillcl)
  ) %>% 
  group_by(date, var, lbs, area, fillcl) %>% 
  summarise(
    midv = median(val, na.rm = T),
    maxv = quantile(val, 0.95, na.rm = T),
    minv = quantile(val, 0.05, na.rm = T)
  )

# baseline data
bswqtmp <- bswqdat %>% 
  select(-source, -uni) %>% 
  filter(var == vr) %>% 
  filter(yr > 2005 & mo %in% c(3, 4, 5)) %>% 
  inner_join(bsstatloc, ., by = 'station') %>% 
  st_intersection(ppseg) %>% 
  st_set_geometry(NULL) %>% 
  group_by(mo, var, area) %>% 
  summarise(   
    avev = mean(val, na.rm = T), 
    stdv = sd(val, na.rm = T), 
    .groups = 'drop'
  ) %>%
  left_join(parms, by = 'var') %>% 
  mutate(
    avev = round(avev, sigdig), 
    stdv = round(stdv, sigdig), 
    minv = avev - stdv, 
    minv = pmax(0, minv),
    maxv = avev + stdv,
    lbunis = gsub('^.*\\s(\\(.*\\))$', '\\1', lbs), 
    lbunis = gsub('pH', '', lbunis), 
    datestr= paste0('2021-', mo, '-01'), 
    datestr = ymd(datestr), 
    dateend = ceiling_date(datestr, unit = 'month')
  )

# legend only
pleg <- ggplot() + 
  geom_rect(data = toplo2, aes(xmin = datestr, xmax = dateend, ymin = minv, ymax = maxv, fill = 'Monthly baseline (mean +/- 1 sd)'), alpha = 0.2) +
  scale_fill_manual(NULL, values = 'blue') +
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = 'top', 
  )
pleg <- g_legend(pleg)

fl <- 'figure/synth_leg.png'
png(fl, family = 'Lato', res = 200, height = 2.5, width = 6, units = 'in')
grid::grid.newpage()
grid::grid.draw(pleg)
dev.off()

ylab <- unique(rswqtmp$lbs)

flts <- levels(rswqtmp$area)
for(flt in flts){
  
  toplo1 <- rswqtmp %>% 
    filter(area %in% flt)
  toplo2 <- bswqtmp %>% 
    filter(area %in% flt)
  
  # fillcol colors
  flcls <- toplo1 %>% 
    select(area, date, fillcl) %>% 
    unique
  
  p <- ggplot() + 
    geom_rect(data = toplo2, aes(xmin = datestr, xmax = dateend, ymin = minv, ymax = maxv, group = mo, fill = 'Monthly baseline (mean +/- 1 sd)'), alpha = 0.2) +
    geom_errorbar(data = toplo1, aes(x = date, ymin = minv, ymax = maxv), width = 0) + 
    geom_point(data = toplo1, aes(x = date, y = midv), fill = flcls$fillcl, pch = 21, colour = 'black', show.legend = F, size = 4) + 
    scale_fill_manual(NULL, values = 'blue') +
    scale_linetype_manual(values = 'dashed') + 
    scale_x_date(breaks = unique(rswqtmp$date), date_labels = '%b %d', expand = c(0.1, 0.1)) +
    labs(
      y = ylab, 
      x = 'Week'
    ) + 
    coord_cartesian(xlim = range(toplo1$date)) +
    theme_minimal(base_size = 14) + 
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.position = 'none', 
      strip.background = element_blank(), 
      axis.text.x = element_text(size = 9)
    )
  
  fl <- paste('figure/synth_', vr, '_', flt, '.png')
  png(fl, family = 'Lato', res = 200, height = 2.5, width = 6, units = 'in')
  print(p)
  dev.off()

}

##
# tracer plots

cols <- grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(11, 'Spectral')))

# mask

tmp1 <- tempfile(fileext = '.zip')
tmp2 <- tempdir()

drive_download(as_id("1EjIDcpNbBHvoJ09-wlEJDdDE7v323qEe"), path = tmp1, overwrite = TRUE)

unzip(tmp1, exdir = tmp2) 

bnd <- list.files(tmp2, full.names = T) %>% 
  grep('\\.shp$', ., value = T) %>% 
  st_read %>%
  st_transform(crs = 4326) %>% 
  st_intersection(tbshed) %>% 
  st_union()


sbmask <- st_read('T:/05_GIS/PineyPoint/SB_boundary.shp') %>% 
  st_transform(crs = 4326)

tmp1 <- bnd %>% 
  st_union %>% 
  st_geometry()
tmp2 <- sbmask %>% 
  st_union() %>% 
  st_geometry()

tmp <- c(tmp1, tmp2) %>% 
  st_union() %>% 
  st_buffer(dist = 0.0001) %>% 
  st_buffer(dist = -0.0001)  

segmask <- tmp %>% 
  st_bbox %>% 
  st_as_sfc() %>% 
  st_as_sf() %>% 
  st_difference(tmp)


rst1 <- raster('data/raw/HIND_20210410_18UTC_modified.tif') %>%
  mask(segmask) %>% 
  mask(ppseg) %>% 
  st_as_stars

p1 <- ggplot() + 
  geom_stars(data = rst1) + 
  geom_sf(data = ppseg, fill = NA, color = 'red', lwd = 1) + 
  coord_sf(ylim = c(27.45, 27.85), xlim = c(-82.85, -82.4)) + 
  theme_bw() +
  theme(
    legend.position = 'none', 
    axis.text = element_blank(), 
    axis.title = element_blank(),
    axis.ticks = element_blank(), 
    panel.grid = element_blank()
  ) + 
  scale_fill_distiller(palette= 'Spectral', na.value = NA)

rst2 <- raster('data/raw/SFC_20210425_12UTC_modified.tif') %>%
  mask(segmask) %>% 
  mask(ppseg) %>% 
  st_as_stars

p2 <- ggplot() + 
  geom_stars(data = rst2) + 
  geom_sf(data = ppseg, fill = NA, color = 'red', lwd = 1) + 
  coord_sf(ylim = c(27.45, 27.85), xlim = c(-82.85, -82.4)) + 
  theme_bw() +
  theme(
    legend.position = 'none', 
    axis.text = element_blank(), 
    axis.title = element_blank(),
    axis.ticks = element_blank(), 
    panel.grid = element_blank()
  ) + 
  scale_fill_distiller(palette= 'Spectral', na.value = NA)

rst3 <- raster('data/raw/SFC_20210518_02UTC_modified.tif') %>%
  mask(segmask) %>% 
  mask(ppseg) %>% 
  st_as_stars

p3 <- ggplot() + 
  geom_stars(data = rst3) + 
  geom_sf(data = ppseg, fill = NA, color = 'red', lwd = 1) + 
  coord_sf(ylim = c(27.45, 27.85), xlim = c(-82.85, -82.4)) + 
  theme_bw() +
  theme(
    legend.position = 'none', 
    axis.text = element_blank(), 
    axis.title = element_blank(),
    axis.ticks = element_blank(), 
    panel.grid = element_blank()
  ) + 
  scale_fill_distiller(palette= 'Spectral', na.value = NA)

png('figure/synth_trace1.png', heigh = 6, width = 6, units = 'in', res = 300)
print(p1)
dev.off()

png('figure/synth_trace2.png', heigh = 6, width = 6, units = 'in', res = 300)
print(p2)
dev.off()

png('figure/synth_trace3.png', heigh = 6, width = 6, units = 'in', res = 300)
print(p3)
dev.off()