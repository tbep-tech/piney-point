library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(tbeptools)
library(sf)

# get epc data ------------------------------------------------------------

epcdat <- read_importwq('data/epcdat.xlsx', download_latest = T)
epcraw <- read_excel('data/epcdat.xlsx', sheet = 'RWMDataSpreadsheet')

# manatee co data ---------------------------------------------------------

# raw data from https://tampabay.wateratlas.usf.edu/datadownload/Default.aspx
# search by surface water quality, by site Info (station name/ID), then STORET_21FLMANA
# selected stations close to Piney Point from map (have to select 'hydrology and samples' and then 'sampling location' form hamburger layer selectoin)
mandat <- read.table('data/DataDownload_2339399_row.txt', sep = '\t', header = T)

# combine data ------------------------------------------------------------

epcraw <- epcraw %>% 
  clean_names %>% 
  filter(station_number %in% 90) %>% 
  select(
    station = station_number,
    date = sample_time,
    latitude, 
    longitude,
    tn = total_nitrogen_mg_l, 
    tp = total_phosphorus_mg_l, 
    chla = chlorophyll_a_uncorr_ug_l, 
    p_h_bottom, 
    p_h_mid, 
    p_h_top, 
    sal_bottom_ppth, 
    sal_mid_ppth, 
    sal_top_ppth
  ) %>% 
  mutate(
    date = as.Date(date), 
    yr = year(date), 
    mo = month(date), 
    source = 'epchc'
  ) %>% 
  rowwise() %>% 
  mutate(
    sal = mean(c(sal_top_ppth, sal_mid_ppth, sal_bottom_ppth), na.rm = T), 
    ph = mean(c(p_h_top, p_h_mid, p_h_bottom), na.rm = T)
  ) %>% 
  ungroup() %>% 
  select(-sal_top_ppth, -sal_mid_ppth, -sal_bottom_ppth, -p_h_top, -p_h_mid, -p_h_bottom) %>% 
  gather('var', 'val', -source, -station, -date, -yr, -mo, -latitude, -longitude) %>% 
  mutate(
    val = as.numeric(val), 
    uni = case_when(
      var == 'sal' ~ 'ppt', 
      var == 'ph' ~ 'none', 
      var %in% c('tp', 'tn') ~ 'mg/l', 
      var %in% 'chla' ~ 'ug/l'
    )
  )

manraw <- mandat %>% 
  clean_names %>% 
  select(
    station = station_name,
    date = sample_date,
    latitude = actual_latitude, 
    longitude = actual_longitude,
    var = parameter, 
    val = result_value, 
    uni = result_unit
  ) %>% 
  filter(var %in% c('Chla_ugl', 'TN_ugl', 'TP_ugl', 'pH', 'Salinity_ppt')) %>% 
  mutate(
    date = as.Date(mdy_hms(date)), 
    yr = year(date), 
    mo = month(date), 
    station = as.character(station),
    source = 'manco', 
    var = case_when(
      var == 'Chla_ugl' ~ 'chla', 
      var == 'TN_ugl' ~ 'tn', 
      var == 'TP_ugl' ~ 'tp', 
      var == 'pH' ~ 'ph', 
      var == 'Salinity_ppt' ~ 'sal'
    ), 
    val = case_when(
      var %in% c('tn', 'tp') ~ val / 1000, 
      T ~ val
    ), 
    uni = case_when(
      var %in% c('tn', 'tp') ~ 'mg/l', 
      T ~ uni
    ), 
    uni = tolower(uni)
  ) %>% 
  tibble
  
wqdat <- bind_rows(epcraw, manraw)

statloc <- wqdat %>% 
  select(station, source, latitude, longitude) %>% 
  unique %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)

save(statloc, file = 'data/statloc.RData', version = 2)
save(wqdat, file = 'data/wqdat.RData', version = 2)
