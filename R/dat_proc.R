# setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(tbeptools)
library(sf)
library(stringr)
library(scales)
library(googlesheets4)
library(googledrive)
library(mapview)
library(httr)
library(jsonlite)
library(NADA)
library(rnoaa)
box::use(
  stringr[str_count]
)

prj <- 4326

source('R/funcs.R')

# deauth all so it can build with gh actions
drive_deauth()
gs4_deauth()

# parameter data dict -----------------------------------------------------

# parameter names, units
parms <- tibble(
  var = c('bod', 'chla', 'color', 'do', 'dosat', 'nh34', 'no23', 'orthop', 'ph', 'sal', 'secchi', 'temp', 'tkn', 'tn', 'tp', 'tss', 'turb'),
  uni = c('mgl', 'ugl', 'pcu', 'mgl', 'per', 'mgl', 'mgl', 'mgl', 'none', 'ppt', 'm', 'c', 'mgl', 'mgl', 'mgl', 'mgl', 'ntu'), 
  lbs = c('BOD (mg/L)', 'Chl-a (ug/L)', 'Color (PCU)', 'DO (mg/L)', 'DO (% sat.)', 'NH3, NH4+ (mg/L)', 'Nitrate/Nitrite (mg/L)', 'Ortho-P (mg/L)', 'pH', 'Sal (ppt)', 'Secchi (m)', 'Temp (C)', 'TKN (mg/L)', 'TN (mg/L)', 'TP (mg/L)', 'TSS (mg/l)', 'Turb (NTU)'),
  sigdig = c(2, 2, 2, 2, 1, 4, 5, 4, 1, 1, 1, 1, 3, 3, 3, 2, 2)
)

save(parms, file = 'data/parms.RData', version = 2)

# tb hires watershed boundary ---------------------------------------------

# https://drive.google.com/file/d/1EjIDcpNbBHvoJ09-wlEJDdDE7v323qEe/view?usp=sharing

tmp1 <- tempfile(fileext = '.zip')
tmp2 <- tempdir()

drive_download(as_id("1EjIDcpNbBHvoJ09-wlEJDdDE7v323qEe"), path = tmp1, overwrite = TRUE)

unzip(tmp1, exdir = tmp2) 

tbhished <- list.files(tmp2, full.names = T) %>% 
  grep('\\.shp$', ., value = T) %>% 
  st_read %>%
  st_transform(crs = 4326) %>% 
  st_intersection(tbshed) %>% 
  st_union()

save(tbhished, file = 'data/tbhished.RData', version = 2)

file.remove(tmp1)
file.remove(list.files(tmp2, full.names = T))

# tb watershed mask -------------------------------------------------------

data(tbhished)

segmask <- tbhished %>% 
  st_bbox %>% 
  st_as_sfc() %>% 
  st_as_sf() %>% 
  st_difference(tbhished)

save(segmask, file = 'data/segmask.RData', version = 2)

# PP segments for trend analysis ------------------------------------------

ppseg <- st_read('T:/05_GIS/PineyPoint/PP_segs.shp') %>% 
  st_transform(crs = 4326)

save(ppseg, file = 'data/ppseg.RData', version = 2)

# sb watershed ------------------------------------------------------------

# sarasota bay watershed
sbshed <- st_read('T:/05_GIS/PineyPoint/SB_boundary.shp') %>% 
  st_transform(crs = 4326)

save(sbshed, file = 'data/sbshed.RData', version = 2)

# normal ranges -----------------------------------------------------------

# 2006 to 2020 by month, epc and manco data
# secchi on bottom removed (>, L, S)
# all other non-detects handled by NADA (U, K)
# county partners use FDEP standards https://floridadep.gov/sites/default/files/62-160_help-document_0.pdf

data(parms)

# epc
epcdat <- read_importwq('data/raw/epcdat.xlsx', download_latest = T)
epcraw <- readxl::read_xlsx('data/raw/epcdat.xlsx', sheet="RWMDataSpreadsheet",
                            col_types = c("numeric", "numeric", "text", "text", "text", "text",
                                          "numeric", "numeric", "text", "numeric", "numeric",
                                          "text", "date", "text", "numeric", "text", "text",
                                          "numeric", "numeric", "numeric", "numeric", "text",
                                          "text", "text", "numeric", "text", "numeric", "text",
                                          "numeric", "text", "numeric", "text", "numeric",
                                          "text", "numeric", "text", "numeric", "text",
                                          "numeric", "text", "numeric", "text", "numeric",
                                          "text", "numeric", "text", "numeric", "text",
                                          "numeric", "text", "numeric", "text", "numeric",
                                          "text", "numeric", "text", "numeric", "text",
                                          "numeric", "text", "numeric", "text", "numeric",
                                          "text", "numeric", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text"),
                            na = '')

# epc data w/ qualifiers
epcvalqual <- epcraw %>% 
  clean_names %>% 
  filter(station_number %in% c(9, 11, 13, 14, 16, 19, 21, 22, 23, 24, 25, 28, 32, 33, 81, 82, 84, 90, 91, 92, 93, 95)) %>% 
  select(
    station = station_number,
    date = sample_time,
    latitude, 
    longitude, 
    color_pcu = color_345_c_pcu, 
    color_qual = color_345_c_q,
    tn_mgl = total_nitrogen_mg_l,
    tn_qual = total_nitrogen_q,
    nh34_mgl = ammonia_mg_l,
    nh34_qual = ammonia_q,
    no23_mgl = nitrates_nitrites_mg_l,
    no23_qual = nitrates_nitrites_q,
    tkn_mgl = kjeldahl_nitrogen_mg_l,
    tkn_qual = kjeldahl_nitrogen_q,
    orthop_mgl = ortho_phosphates_mg_l,
    orthop_qual = ortho_phosphates_q,
    tp_mgl = total_phosphorus_mg_l, 
    tp_qual = total_phosphorus_q,
    bod_mgl = bod_mg_l,
    bod_qual = bod_q,
    chla_ugl = chlorophyll_a_uncorr_ug_l, 
    chla_qual = chlorophyll_a_uncorr_q,
    tss_mgl = total_suspended_solids_mg_l, 
    tss_qual = total_suspended_solids_q,
    turb_ntu = turbidity_jtu_ntu,
    turb_qual = turbidity_q,
    secchi_m = secchi_depth_m,
    secchi_qual = secchi_q,
    p_h_bottom, 
    p_h_bottom_qual = p_h_bottom_q,
    p_h_mid,
    p_h_mid_qual = p_h_mid_q,
    p_h_top,
    p_h_top_qual = p_h_top_q,
    do_bottom_mg_l, 
    do_bottom_qual = do_bottom_q, 
    do_mid_mg_l,
    do_mid_qual = do_mid_q,
    do_top_mg_l, 
    do_top_qual = do_top_q,
    do_sat_bottom_percent, 
    do_sat_bottom_qual = do_percent_sat_bottom_q,
    do_sat_mid_percent, 
    do_sat_mid_qual = do_percent_sat_mid_q,
    do_sat_top_percent,
    do_sat_top_qual = do_sat_top_q,
    sal_bottom_ppth,
    sal_bottom_qual = sal_bottom_q,
    sal_mid_ppth,
    sal_mid_qual = sal_mid_q,
    sal_top_ppth, 
    sal_top_qual = sal_top_q,
    temp_water_bottom_deg_c,
    temp_water_bottom_qual = temp_water_bottom_q,
    temp_water_mid_deg_c,
    temp_water_mid_qual = temp_water_mid_q,
    temp_water_top_deg_c,
    temp_water_top_qual = temp_water_top_q
  ) %>% 
  mutate(
    date = as.Date(date), 
    yr = year(date), 
    mo = month(date)
  ) %>% 
  rowwise() %>% 
  mutate(
    do_mgl = mean(c(do_bottom_mg_l, do_mid_mg_l, do_top_mg_l), na.rm = T), 
    dosat_per = mean(c(do_sat_bottom_percent, do_sat_mid_percent, do_sat_top_percent), na.rm = T),
    ph_none = mean(c(p_h_top, p_h_mid, p_h_bottom), na.rm = T),
    temp_c = mean(c(temp_water_bottom_deg_c, temp_water_mid_deg_c, temp_water_top_deg_c), na.rm = T),
    sal_ppt = mean(c(sal_bottom_ppth, sal_mid_ppth, sal_top_ppth), na.rm = T), 
    do_qual = paste0(unique(c(do_bottom_qual, do_mid_qual, do_top_qual)), collapse = ''),
    do_qual = gsub('NA', '', do_qual),
    dosat_qual = paste0(unique(c(do_sat_bottom_qual, do_sat_mid_qual, do_sat_top_qual)), collapse = ''), 
    dosat_qual = gsub('NA', '', dosat_qual),
    ph_qual = paste0(unique(c(p_h_bottom_qual, p_h_mid_qual, p_h_top_qual)), collapse = ''), 
    ph_qual = gsub('NA', '', ph_qual),
    temp_qual = paste0(unique(c(temp_water_bottom_qual, temp_water_mid_qual, temp_water_top_qual)), collapse = ''), 
    temp_qual = gsub('NA', '', temp_qual),
    sal_qual = paste0(unique(c(sal_bottom_qual, sal_mid_qual, sal_top_qual)), collapse = ''), 
    sal_qual = gsub('NA', '', sal_qual)
  ) %>% 
  ungroup() %>% 
  select(-matches('bottom|mid|top'))

# separate epc results from qualifiers
epcval <- epcvalqual %>% 
  select(-contains('qual')) %>% 
  gather('var', 'val', -station, -date, -yr, -mo, -latitude, -longitude) %>% 
  separate(var, c('var', 'uni'), sep = '_') %>% 
  mutate(
    val = as.numeric(val), 
    source = 'epchc'
  ) 

# separate epc qualifiers from results
epcqual <- epcvalqual %>% 
  select(station, date, latitude, longitude, yr, mo, contains('qual')) %>% 
  gather('var', 'qual', -station, -date, -yr, -mo, -latitude, -longitude) %>% 
  mutate(
    var = gsub('\\_qual$', '', var), 
    qual = ifelse(qual == '', NA_character_, qual)
  )

# combine epcval and epcqual
epcraw <- full_join(epcval, epcqual, by = c('station', 'date', 'latitude', 'longitude', 'yr', 'mo', 'var'))

# get normal epc ranges
bswqrngsepchc <- epcraw %>% 
  filter(yr > 2005 & yr < 2021) %>% 
  filter(!(var == 'secchi' & qual %in% c('L', 'S', '>'))) %>% # remove secchi on bottom
  mutate(
    cens = grepl('U|K', qual)
  ) %>% 
  group_by(station, mo, var, uni) %>%
  summarise(
    lat = mean(latitude, na.rm = T),
    lng = mean(longitude, na.rm = T),
    avev = ifelse(
      any(cens), tryCatch(mean(cenfit(val, censored = cens), na.rm = T)[1], error = function(err) NA),
      mean(val, na.rm = T)
    ),
    stdv = ifelse(
      any(cens), tryCatch(sd(cenfit(val, censored = cens), na.rm = T), error = function(err) NA),
      sd(val, na.rm = T)
    ),
    .groups = 'drop'
  ) %>%
  left_join(parms, by = c('var', 'uni')) %>% 
  mutate(
    avev = round(avev, sigdig), 
    avev = ifelse(avev == 0, NA, avev),
    stdv = round(stdv, sigdig), 
    stdv = ifelse(stdv == 0, NA, stdv),
    minv = avev - stdv, 
    minv = pmax(0, minv),
    maxv = avev + stdv,
    lbunis = gsub('^.*\\s(\\(.*\\))$', '\\1', lbs), 
    lbunis = gsub('pH', '', lbunis), 
    source = 'epchc'
  ) %>% 
  rename(bswqstation = station)

# manatee county
# see baseline wq section for how this file was obtained
mandat <- read.table('data/raw/DataDownload_2351804_row.txt', sep = '\t', header = T)

# get normal manatee county ranges
bswqrngsmpnrd <- mandat %>% 
  clean_names %>% 
  select(
    station = station_id,
    date = sample_date,
    lat = actual_latitude, 
    lng = actual_longitude,
    var = parameter, 
    val = result_value, 
    uni = result_unit, 
    qual = qa_code
  ) %>% 
  mutate(
    var = case_when(
      var == 'Color_apparent_pcu' ~ 'color_pcu', 
      var == 'TN_ugl' ~ 'tn_ugl',
      var == 'NH3_N_ugl' ~ 'nh34_ugl', 
      var == 'NOx_ugl' ~ 'no23_ugl', 
      var == 'TKN_ugl' ~ 'tkn_ugl', 
      var == 'OP_mgl' ~ 'orthop_mgl', 
      var == 'TP_ugl' ~ 'tp_ugl', 
      var == 'BOD5_mgl' ~ 'bod_mgl', 
      var == 'ChlaC_ugl' ~ 'chla_ugl',
      var == 'TSS_mgl' ~ 'tss_mgl', 
      var == 'Turb_ntu' ~ 'turb_ntu', 
      var == 'Secchi_ft' ~ 'secchi_ft', 
      var == 'pH' ~ 'ph_none', 
      var == 'DO_mgl' ~ 'do_mgl', 
      var == 'DO_percent' ~ 'dosat_per', 
      var == 'Salinity_ppt' ~ 'sal_ppt', 
      var == 'TempW_C' ~ 'temp_c'
    ), 
    cens = grepl('U|K', qual)
  ) %>% 
  filter(!is.na(var)) %>% 
  separate(var, c('var', 'uni'), sep = '_', remove = F) %>% 
  filter(!(var == 'secchi' & qual %in% c('L', 'S', '>'))) %>% # remove secchi on bottom
  mutate(
    station = gsub('\\=', '', station), 
    date = as.Date(mdy_hms(date)), 
    yr = year(date), 
    mo = month(date), 
    station = as.character(station),
    val = case_when(
      var %in% c('tn', 'tp', 'nh34', 'no23', 'tkn') ~ val / 1000, # ug/l to mg/L
      var %in% 'secchi' ~ val * 0.3048, # ft to m 
      T ~ val
    ), 
    uni = case_when(
      var %in% c('tn', 'tp', 'nh34', 'no23', 'tkn') ~ 'mgl', 
      var %in% 'secchi' ~ 'm',
      T ~ uni
    )
  ) %>%  
  filter(yr > 2005 & yr < 2021) %>% 
  group_by(station, mo, var, uni) %>% 
  summarise(
    lat = mean(lat, na.rm = T), 
    lng = mean(lng, na.rm = T),
    avev = ifelse(
      any(cens), tryCatch(mean(cenfit(val, censored = cens), na.rm = T)[1], error = function(err) NA),
      mean(val, na.rm = T)
    ),
    stdv = ifelse(
      any(cens), tryCatch(sd(cenfit(val, censored = cens), na.rm = T), error = function(err) NA),
      sd(val, na.rm = T)
    ),
    .groups = 'drop'
  ) %>%
  left_join(parms, by = c('var', 'uni')) %>% 
  mutate(
    avev = round(avev, sigdig), 
    avev = ifelse(avev == 0, NA, avev),
    stdv = round(stdv, sigdig), 
    stdv = ifelse(stdv == 0, NA, stdv),
    minv = avev - stdv, 
    minv = pmax(0, minv),
    maxv = avev + stdv,
    lbunis = gsub('^.*\\s(\\(.*\\))$', '\\1', lbs), 
    lbunis = gsub('pH', '', lbunis), 
    source = 'mpnrd'
  ) %>% 
  rename(bswqstation = station) %>% 
  filter(!grepl('^BH', bswqstation)) # these are bishop harbor stations with incomplete data

# combine both, removing missing, get ave lat/lng for changing locations over time
bswqrngs <- bind_rows(bswqrngsepchc, bswqrngsmpnrd) %>% 
  filter(!is.na(stdv)) %>% 
  group_by(bswqstation, source) %>% 
  mutate(
    lat = mean(lat, na.rm = T), 
    lng = mean(lng, na.rm = T)
  ) %>% 
  ungroup()

save(bswqrngs, file = 'data/bswqrngs.RData', version = 2)

# baseline wq data --------------------------------------------------------

# epc
epcdat <- read_importwq('data/raw/epcdat.xlsx', download_latest = T)
epcraw <- readxl::read_xlsx('data/raw/epcdat.xlsx', sheet="RWMDataSpreadsheet",
                            col_types = c("numeric", "numeric", "text", "text", "text", "text",
                                          "numeric", "numeric", "text", "numeric", "numeric",
                                          "text", "date", "text", "numeric", "text", "text",
                                          "numeric", "numeric", "numeric", "numeric", "text",
                                          "text", "text", "numeric", "text", "numeric", "text",
                                          "numeric", "text", "numeric", "text", "numeric",
                                          "text", "numeric", "text", "numeric", "text",
                                          "numeric", "text", "numeric", "text", "numeric",
                                          "text", "numeric", "text", "numeric", "text",
                                          "numeric", "text", "numeric", "text", "numeric",
                                          "text", "numeric", "text", "numeric", "text",
                                          "numeric", "text", "numeric", "text", "numeric",
                                          "text", "numeric", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text", "text", "text", "text",
                                          "text", "text", "text"),
                            na = '')

# manatee co
# raw data from https://manatee.wateratlas.usf.edu/datadownload/Default.aspx
# search by surface water quality, by site Info (data source/provider, water body type), then STORET_21FLMANA (legacy) and WIN_21FLMANATEE (new) and bay (for Manatee Co., select Terra Ceia Bay, Palma Sola Bay, Manatee River Estuary, Sarasota Bay)
# then select all manatee county stations, there will be duplicates for each based on legacy, current data provider
# selected stations close to Piney Point from map (have to select 'hydrology and samples' and then 'sampling location' form hamburger layer selection)
mandat <- read.table('data/raw/DataDownload_2351804_row.txt', sep = '\t', header = T)

epcvalqual <- epcraw %>% 
  clean_names %>% 
  filter(station_number %in% c(9, 11, 13, 14, 16, 19, 21, 22, 23, 24, 25, 28, 32, 33, 81, 82, 84, 90, 91, 92, 93, 95)) %>% 
  select(
    station = station_number,
    date = sample_time,
    latitude, 
    longitude, 
    color_pcu = color_345_c_pcu, 
    color_qual = color_345_c_q,
    tn_mgl = total_nitrogen_mg_l,
    tn_qual = total_nitrogen_q,
    nh34_mgl = ammonia_mg_l,
    nh34_qual = ammonia_q,
    no23_mgl = nitrates_nitrites_mg_l,
    no23_qual = nitrates_nitrites_q,
    tkn_mgl = kjeldahl_nitrogen_mg_l,
    tkn_qual = kjeldahl_nitrogen_q,
    orthop_mgl = ortho_phosphates_mg_l,
    orthop_qual = ortho_phosphates_q,
    tp_mgl = total_phosphorus_mg_l, 
    tp_qual = total_phosphorus_q,
    bod_mgl = bod_mg_l,
    bod_qual = bod_q,
    chla_ugl = chlorophyll_a_uncorr_ug_l, 
    chla_qual = chlorophyll_a_uncorr_q,
    tss_mgl = total_suspended_solids_mg_l, 
    tss_qual = total_suspended_solids_q,
    turb_ntu = turbidity_jtu_ntu,
    turb_qual = turbidity_q,
    secchi_m = secchi_depth_m,
    secchi_qual = secchi_q,
    p_h_bottom, 
    p_h_bottom_qual = p_h_bottom_q,
    p_h_mid,
    p_h_mid_qual = p_h_mid_q,
    p_h_top,
    p_h_top_qual = p_h_top_q,
    do_bottom_mg_l, 
    do_bottom_qual = do_bottom_q, 
    do_mid_mg_l,
    do_mid_qual = do_mid_q,
    do_top_mg_l, 
    do_top_qual = do_top_q,
    do_sat_bottom_percent, 
    do_sat_bottom_qual = do_percent_sat_bottom_q,
    do_sat_mid_percent, 
    do_sat_mid_qual = do_percent_sat_mid_q,
    do_sat_top_percent,
    do_sat_top_qual = do_sat_top_q,
    sal_bottom_ppth,
    sal_bottom_qual = sal_bottom_q,
    sal_mid_ppth,
    sal_mid_qual = sal_mid_q,
    sal_top_ppth, 
    sal_top_qual = sal_top_q,
    temp_water_bottom_deg_c,
    temp_water_bottom_qual = temp_water_bottom_q,
    temp_water_mid_deg_c,
    temp_water_mid_qual = temp_water_mid_q,
    temp_water_top_deg_c,
    temp_water_top_qual = temp_water_top_q
  ) %>% 
  mutate(
    date = as.Date(date), 
    yr = year(date), 
    mo = month(date)
  ) %>% 
  rowwise() %>% 
  mutate(
    do_mgl = mean(c(do_bottom_mg_l, do_mid_mg_l, do_top_mg_l), na.rm = T), 
    dosat_per = mean(c(do_sat_bottom_percent, do_sat_mid_percent, do_sat_top_percent), na.rm = T),
    ph_none = mean(c(p_h_top, p_h_mid, p_h_bottom), na.rm = T),
    temp_c = mean(c(temp_water_bottom_deg_c, temp_water_mid_deg_c, temp_water_top_deg_c), na.rm = T),
    sal_ppt = mean(c(sal_bottom_ppth, sal_mid_ppth, sal_top_ppth), na.rm = T), 
    do_qual = paste0(unique(c(do_bottom_qual, do_mid_qual, do_top_qual)), collapse = ''),
    do_qual = gsub('NA', '', do_qual),
    dosat_qual = paste0(unique(c(do_sat_bottom_qual, do_sat_mid_qual, do_sat_top_qual)), collapse = ''), 
    dosat_qual = gsub('NA', '', dosat_qual),
    ph_qual = paste0(unique(c(p_h_bottom_qual, p_h_mid_qual, p_h_top_qual)), collapse = ''), 
    ph_qual = gsub('NA', '', ph_qual),
    temp_qual = paste0(unique(c(temp_water_bottom_qual, temp_water_mid_qual, temp_water_top_qual)), collapse = ''), 
    temp_qual = gsub('NA', '', temp_qual),
    sal_qual = paste0(unique(c(sal_bottom_qual, sal_mid_qual, sal_top_qual)), collapse = ''), 
    sal_qual = gsub('NA', '', sal_qual)
  ) %>% 
  ungroup() %>% 
  select(-matches('bottom|mid|top'))

# separate epc results from qualifiers
epcval <- epcvalqual %>% 
  select(-contains('qual')) %>% 
  gather('var', 'val', -station, -date, -yr, -mo, -latitude, -longitude) %>% 
  separate(var, c('var', 'uni'), sep = '_') %>% 
  mutate(
    val = as.numeric(val), 
    source = 'epchc'
  ) 

# separate epc qualifiers from results
epcqual <- epcvalqual %>% 
  select(station, date, latitude, longitude, yr, mo, contains('qual')) %>% 
  gather('var', 'qual', -station, -date, -yr, -mo, -latitude, -longitude) %>% 
  mutate(
    var = gsub('\\_qual$', '', var), 
    qual = ifelse(qual == '', NA_character_, qual)
    )

# combine epcval and epcqual
epcraw <- full_join(epcval, epcqual, by = c('station', 'date', 'latitude', 'longitude', 'yr', 'mo', 'var'))

# chloropyll includes chla and chlac for pheophytin
manraw <- mandat %>% 
  clean_names %>% 
  select(
    station = station_id,
    date = sample_date,
    latitude = actual_latitude, 
    longitude = actual_longitude,
    var = parameter, 
    val = result_value, 
    uni = result_unit, 
    qual = qa_code
  ) %>% 
  mutate(
    var = case_when(
      var == 'Color_apparent_pcu' ~ 'color_pcu', 
      var == 'TN_ugl' ~ 'tn_ugl',
      var == 'NH3_N_ugl' ~ 'nh34_ugl', 
      var == 'NOx_ugl' ~ 'no23_ugl', 
      var == 'TKN_ugl' ~ 'tkn_ugl', 
      var == 'OP_mgl' ~ 'orthop_mgl', 
      var == 'TP_ugl' ~ 'tp_ugl', 
      var == 'BOD5_mgl' ~ 'bod_mgl', 
      var == 'ChlaC_ugl' ~ 'chla_ugl',
      var == 'TSS_mgl' ~ 'tss_mgl', 
      var == 'Turb_ntu' ~ 'turb_ntu', 
      var == 'Secchi_ft' ~ 'secchi_ft', 
      var == 'pH' ~ 'ph_none', 
      var == 'DO_mgl' ~ 'do_mgl', 
      var == 'DO_percent' ~ 'dosat_per', 
      var == 'Salinity_ppt' ~ 'sal_ppt', 
      var == 'TempW_C' ~ 'temp_c'
    )
  ) %>% 
  filter(!is.na(var)) %>% 
  separate(var, c('var', 'uni'), sep = '_', remove = F) %>% 
  mutate(
    station = gsub('\\=', '', station), 
    source = 'manco',
    date = as.Date(mdy_hms(date)), 
    yr = year(date), 
    mo = month(date), 
    station = as.character(station),
    val = case_when(
      var %in% c('tn', 'tp', 'nh34', 'no23', 'tkn') ~ val / 1000, # ug/l to mg/L
      var %in% 'secchi' ~ val * 0.3048, # ft to m 
      T ~ val
    ), 
    uni = case_when(
      var %in% c('tn', 'tp', 'nh34', 'no23', 'tkn') ~ 'mgl', 
      var %in% 'secchi' ~ 'm',
      T ~ uni
    ), 
    qual = ifelse(qual == '', NA, qual)
  ) %>% 
  filter(!grepl('^BH', station)) # these are bishop harbor stations with incomplete data

bswqdat <- bind_rows(epcraw, manraw) %>% 
  arrange(station, date) %>% 
  filter(yr > 1995) %>% 
  mutate(date = floor_date(date, unit = 'month')) %>% 
  group_by(station, date, mo, yr, source, var, uni, qual) %>% 
  summarise(
    val = mean(val, na.rm = T), 
    .groups = 'drop'
  )

bswqdatsub <- bswqdat %>% 
  filter(mo %in% c(3, 4)) %>% 
  mutate(
    molab = month(date, label = T, abbr = T)
  ) %>% 
  unite('datesub', molab, yr, sep = ' ') 

levs <- bswqdatsub %>% 
  select(date, datesub) %>% 
  unique %>% 
  arrange(date) %>% 
  pull(datesub)

bswqdatsub <- bswqdatsub %>% 
  mutate(date = factor(datesub, levels = levs, ordered = T))

bsstatloc <- bind_rows(epcraw, manraw) %>% 
  select(station, source, latitude, longitude) %>% 
  unique %>% 
  group_by(station, source) %>% 
  summarise(
    latitude = mean(latitude), 
    longitude = mean(longitude), 
    .groups = 'drop'
  ) %>% 
  mutate(
    source = case_when(
      source == 'epchc' ~ 'Hillsborough Co.', 
      source == 'manco' ~ 'Manatee Co.'
    )
  ) %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)

save(bsstatloc, file = 'data/bsstatloc.RData', version = 2)
save(bswqdat, file = 'data/bswqdat.RData', version = 2)
save(bswqdatsub,file = 'data/bswqdatsub.RData', version = 2)

# seagrass baseline -------------------------------------------------------

# import entire transect dataset as JSON, filter sites near PP
bstransect <- read_transect(training = FALSE) %>% 
  dplyr::select(-Crew, -MonitoringAgency) %>% 
  filter(Transect %in% c('S3T5', 'S3T6', 'S4T1', 'S4T2', 'S4T3'))

# get transect species occurrence summaries
bstransectocc <- anlz_transectocc(bstransect)

# transect points, lines
trnpts <- trnpts %>% 
  filter(TRAN_ID %in% unique(bstransect$Transect)) %>% 
  filter(!duplicated(TRAN_ID)) %>% 
  select(
    station = TRAN_ID,
    lng = LONG_DD, 
    lat = LAT_DD
  )
trnlns <- trnlns %>% 
  filter(Site %in% unique(bstransect$Transect)) %>% 
  filter(Site %in% trnpts$station) %>% 
  filter(!duplicated(Site)) %>% 
  select(station = Site) %>% 
  st_cast('POINT') %>% 
  mutate(
    lng = st_coordinates(.)[, 1], 
    lat = st_coordinates(.)[, 2]
  ) %>% 
  st_set_geometry(NULL)

save(bstransect, file = 'data/bstransect.RData', version = 2)
save(bstransectocc, file = 'data/bstransectocc.RData', version = 2)
save(trnpts, file = 'data/trnpts.RData', version = 2)
save(trnlns, file = 'data/trnlns.RData', version = 2)

# rapid response seagrass, macroalgae surveys -----------------------------

## locations --------------------------------------------------------------

# https://docs.google.com/spreadsheets/d/1_wxkXJPjSlVRt9oVLO_AGxjupg8PI_PHkkLA6rq2zGQ/edit#gid=379130523
rstrnfl <- read_sheet('1_wxkXJPjSlVRt9oVLO_AGxjupg8PI_PHkkLA6rq2zGQ')

rstrnpts <- rstrnfl %>% 
  filter(!location %in% 'end') %>%
  select(-location) %>% 
  mutate(
    lng = longitude, 
    lat = latitude
  ) %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>% 
  select(source, station, type, lng, lat)

rstrnlns <- rstrnfl %>% 
  filter(!location %in% '') %>% 
  select(source, station, lng = longitude, lat = latitude)

save(rstrnpts, file = 'data/rstrnpts.RData', version = 2)
save(rstrnlns, file = 'data/rstrnlns.RData', version = 2)

## data -------------------------------------------------------------------

savlevs <- c('Thalassia testudinum', 'Halodule wrightii', 'Syringodium filiforme', 'Ruppia maritima', 'Halophila engelmannii', 'Halophila decipiens')
mcrlevs <- c('Gracilaria', 'Hypnea', 'Acanthophora', 'Caulerpa', 'Eucheuma', 'Halymenia', 'Ulva', 'Enteromorpha', 'Cladophora', 'Chaetomorpha', 'Codium', 'Unknown', 'Mixed Drift Reds')
grplevs <- c('Red', 'Green', 'Brown', 'Cyanobacteria')
epilevs <- c('Clean', 'Light', 'Moderate', 'Heavy')
abulevs <- c('<1%', '1-5%', '6-25%', '26-50%', '51-75%', '76-100%')

rstrndat <- read_sheet('1YYJ3c6jzOErt_d5rIBkwPr45sA1FCKyDd7io4ZMy56E') %>% 
  mutate(
    date = as.Date(Date)
  ) %>% 
  select(
    date, 
    station, 
    location, 
    sav_species, 
    sav_abundance,
    epibiota_density, 
    macroalgae_species,
    macroalgae_group,
    macroalgae_abundance
  ) %>% 
  filter(!grepl('^SSJS', station)) %>%  # remove control stations
  mutate(
    macroalgae_group = case_when(
      macroalgae_group == 'Geen' ~ 'Green', 
      T ~ macroalgae_group
    ),
    macroalgae_abundance = case_when(
      macroalgae_abundance %in% c('Solitary', '<1%') ~ '<1%', 
      T ~ macroalgae_abundance
    ),
    macroalgae_bb = case_when(
      macroalgae_abundance %in% '<1%' ~ 0, 
      macroalgae_abundance %in% '1-5%' ~ 1, 
      macroalgae_abundance %in% '6-25%' ~ 2,
      macroalgae_abundance %in% '26-50%' ~ 3,
      macroalgae_abundance %in% '51-75%' ~ 4,
      macroalgae_abundance %in% '76-100%' ~ 5
    ), 
    sav_species = case_when(
      sav_species == 'Halodule wrighttii' ~ 'Halodule wrightii',
      sav_species == 'None' ~ 'NA',
      T ~ sav_species
    ),
    sav_abundance = case_when(
      sav_abundance %in% c('Solitary', '<1%') ~ '<1%', 
      T ~ sav_abundance
    ),
    sav_bb = case_when(
      sav_abundance %in% '<1%' ~ 0, 
      sav_abundance %in% '1-5%' ~ 1, 
      sav_abundance %in% '6-25%' ~ 2,
      sav_abundance %in% '26-50%' ~ 3,
      sav_abundance %in% '51-75%' ~ 4,
      sav_abundance %in% '76-100%' ~ 5
    ), 
    sav_abundance = factor(sav_abundance, levels = abulevs),
    macroalgae_abundance = factor(macroalgae_abundance, levels = abulevs),
    epibiota_density = factor(epibiota_density, levels = epilevs), 
    location = case_when( # ESA stations need to be flipped
      station %in% c('SMB2', 'SMB1', 'STCB1', 'SBH2', 'SBH3') ~ abs(location - 50),
      T ~ location
    ),
    station = factor(station), 
    location = factor(location)
  ) 
  
rstrndatsav <- rstrndat %>% 
  select(date, station, location, sav_species, sav_abundance, sav_bb, epibiota_density) %>% 
  group_by(date, station, location) %>% 
  filter(sav_species != '.') %>% 
  mutate(
    sav_species = factor(sav_species, levels = savlevs),
    station = as.character(station), 
    location = as.numeric(as.character(location))
  ) %>% 
  select(date, station, location, taxa = sav_species, abundance = sav_abundance, bb = sav_bb) 

# find how many new macroalgae columns we'll need since can have multiple per cell
maxcols <- rstrndat$macroalgae_species %>% 
  str_count(',') %>% 
  na.omit %>% 
  max %>% 
  `+` (1) %>% 
  seq(1, .) %>% 
  paste('sppcol', ., sep = '')

# you will get a warning with separate, this is normal
rstrndatmcr <- rstrndat %>% 
  select(date, station, location, macroalgae_group, macroalgae_abundance, macroalgae_bb) %>% 
  # filter(macroalgae_group != 'NA') %>% 
  separate(macroalgae_group, maxcols, sep = ', ') %>% 
  gather('var', 'macroalgae_group', !!maxcols) %>% 
  filter(!is.na(macroalgae_group)) %>% 
  mutate(
    macroalgae_group = factor(macroalgae_group, levels = grplevs),
    station = as.character(station), 
    location = as.numeric(as.character(location)), 
  ) %>% 
  select(date, station, location, taxa = macroalgae_group, abundance = macroalgae_abundance, bb = macroalgae_bb) 

# expand all by locations sampled each date to get true zeroes
# fix some station names
rstrndat <- bind_rows(rstrndatmcr, rstrndatsav) %>% 
  group_by(date, station) %>% 
  complete(
    location, taxa,
    fill = list(bb = 0, abundance = '<1%')
  ) %>% 
  ungroup() %>% 
  mutate(
    typ = case_when(
      taxa %in% savlevs ~ 'sav', 
      taxa %in% grplevs ~ 'mcr'
    ), 
    station = case_when(
      station %in% c('CB0', 'CB2', 'CB18', 'JB1', 'JB2', 'JB6') ~ gsub('^([[:alpha:]]+)(\\d+)$', '\\1-\\2', station), 
      station %in% c('BH-1', 'BH-2', 'BH-3', 'BH-4') ~ paste0('S', gsub('-', '', station)),
      station == 'S4T2A' ~ 'S4T2a', 
      station == 'S4T2B' ~ 'S4T2b', 
      station == 'S4T3' ~ 'S4T3a', 
      station %in% c('SMB3A', 'SMB3a') ~ 'SMB3', 
      T ~ station
    )
  ) %>% 
  select(date, station, location, typ, taxa, abundance, bb) %>% 
  filter(!is.na(taxa))

save(rstrndat, file = 'data/rstrndat.RData', version = 2)

# for log
tms <- Sys.time()
attr(tms, 'tzone') <- 'America/New_York'
tms <- paste(as.character(tms), 'Eastern')

writeLines(tms, 'logs/rstrnlog.txt')

# macroalgae weight data --------------------------------------------------

gdrive_pth <- 'https://drive.google.com/drive/u/0/folders/1xiLuuvXQsiOWxLBZKq_81dA8ajVn9w2G'

fls <- drive_ls(gdrive_pth, type = 'spreadsheet')
fl <- fls[which(fls$name == 'Rapid_Macroalgae_Weight_Data'), 'id'] %>% pull(id)
flsht <- read_sheet(fl)

rstrnwts <- flsht %>% 
  mutate(
    date = as.Date(date),
    genus = case_when(
      genus == 'Acanthophora,Gracilaria' ~ 'Acanthophora and Gracilaria', 
      genus %in% c('Lynbgya', 'Lynbya') ~ 'Lyngbya', 
      genus == 'Mixed Drift Red' ~ 'Mixed Drift Reds',
      T ~ genus
    ), 
    station = case_when(
      station == 'STBC1' ~ 'STCB1', 
      T ~ station
    )
  )

save(rstrnwts, file = 'data/rstrnwts.RData', version = 2)

# contaminants from DEP ---------------------------------------------------

## thresholds -------------------------------------------------------------

# https://docs.google.com/spreadsheets/d/1cq75oTocUTsHOKnw8y2TAnSrT19aVOSXOHN_RotogmQ/edit#gid=74722523

rscntthr <- read_sheet('1cq75oTocUTsHOKnw8y2TAnSrT19aVOSXOHN_RotogmQ') %>% 
  mutate_if(is.list, as.numeric)

save(rscntthr, file = 'data/rscntthr.RData', version = 2)

## data -------------------------------------------------------------------

data(rscntthr)

# ##
# # from ftp
# fl <- 'http://publicfiles.dep.state.fl.us/DEAR/DEARweb/_PP_EventResponse/Latest_Analytical_Results_Final.xlsx'
# 
# tmpfl <- tempfile()
# download.file(fl, tmpfl, mode = 'wb')
# 
# rawdat <- read_excel(tmpfl)
# 
# file.remove(tmpfl)

# from gdrive
# 
# https://docs.google.com/spreadsheets/d/1bFhEFg8AC91IDn1ujM5s7knteq69wYKDtQlAv15yti8/edit?usp=sharing

rawdat <- read_sheet('1bFhEFg8AC91IDn1ujM5s7knteq69wYKDtQlAv15yti8', skip = 8)

# duplicate wq data are removed with rswqdat
rscntdat <- rawdat %>% 
  select(
    station = `SITE LOCATION`, 
    date = `DATE SAMPLED`, 
    var = COMPONENT, 
    val = RESULT, 
    uni = UNITS, 
    qual = `QUALIFIER CODE`
  ) %>% 
  mutate(
    date = as.Date(date, origin = as.Date('1899-12-30')),
    date = date(date),
    uni2 = paste0('(', uni, ')'),
    source = 'fldep', 
    station = gsub('Point\\-', 'Point ', station), 
    station = gsub('\\sPoint\\s', ' ', station), 
    var = gsub('(^.*\\s)\\(.*\\)$', '\\1', var), 
    val = unlist(val), 
    val = as.numeric(val)
  ) %>% 
  filter(!grepl('Blank', station)) %>% 
  filter(!var %in% c('Alpha-Counting Error', 'Alpha, Total', 'Ammonia-N', 'Chlorophyll-a, Corrected', 'Chlorophyll-a, Uncorrected', 'Dominant sample taxon', 'Kjeldahl Nitrogen', 'NO2NO3-N', 'O-Phosphate-P', 'Radium 228-Counting Error', 'Radium 226-Counting Error', 'Total-P', 'TSS', 'Turbidity')) %>% 
  unite('var', var, uni2, remove = F, sep = ' ') %>% 
  select(-uni2) 

# add threshold info
rscntdat <- rscntdat %>% 
  left_join(rscntthr, by = c('var', 'uni')) %>% 
  mutate(
    inrng = ifelse(val < thresh, 'below', 'above'), 
    inrng = ifelse(is.na(inrng), 'no threshold', inrng)
  )

save(rscntdat, file = 'data/rscntdat.RData', version = 2)

# for log
tms <- Sys.time()
attr(tms, 'tzone') <- 'America/New_York'
tms <- paste(as.character(tms), 'Eastern')

writeLines(tms, 'logs/rscntlog.txt')

# phytoplankton -----------------------------------------------------------

## data ------------------------------------------------------------------

gdrive_phypth <- 'https://drive.google.com/drive/u/0/folders/1_69VmwAPA3i0aeEHZg_-qJU5TqLiITBf'

# csv files must be opened/saved as spreadsheet in google sheets
fls <- drive_ls(gdrive_phypth, type = 'spreadsheet')

##
# fldep, fwri

# data from DEP, by way of FWRI
# https://docs.google.com/spreadsheets/d/12hmzOLidRSEZkZnk7lW5aoWilyVqg4Vjx-r8wBQxLy8/edit#gid=979605237
id <- fls[grep('^FLDEP', fls$name), 'id'] %>% pull(id)

# fldep
# rework stations to match those from wq, don't need to do for fwri since usf has its own stations
flphy1 <- read_sheet(id, sheet = 'FDEP')
rsphydatfldep <- flphy1 %>% 
  select(
    date = `Site Visit Date and Time`, 
    station = `Sample Location`, 
    species = `Algal ID`
  ) %>% 
  mutate(
    date = as.Date(date),
    station = gsub('^FDEP\\s', '', station), 
    station = gsub('\\s0', ' ', station), 
    station = gsub('^Piney Port', 'P Port', station),
    source = 'fldep',
    typ = 'Qualitative'
  )

# fwri - usf
flphy2 <- read_sheet(id, sheet = 'Other Agencies')
rsphydatfwri <- flphy2 %>% 
  select(
    date = `Site Visit Date and Time`, 
    station = `Sample Location`, 
    species = `Algal ID`
  ) %>% 
  mutate(
    date = purrr::map(date, function(x){
      if(inherits(x, 'POSIXct'))
        as.Date(x)
      else
        as.Date(gsub('N/A$', '', x), x, format = '%m/%d/%y')
    }),
    date = as.Date(unlist(date), origin = '1970-01-01'),
    source = 'fwri', 
    station = case_when(
      station %in% c('E PP01', 'PP01', 'PP 01') ~ 'PP01', 
      station %in% c('E PP02', 'PP02', 'PP 02') ~ 'PP02', 
      station %in% c('E PP03/USF Piney H (M)', 'PP 03/USF Piney H (M)') ~ 'PP03/USF Piney H (M)', 
      station %in% c('E PP04', 'PP04', 'PP 04') ~ 'PP04', 
      station %in% c('E PP05', 'PP05', 'PP 05') ~ 'PP05', 
      T ~ station
    ),
    typ = 'Qualitative'
  )

##
# pinco
id <- fls[grep('^PINCO', fls$name), 'id'] %>% pull(id)
flphy <- read_sheet(id)
rsphydatpinco <- flphy %>% 
  select(
    date = `Date collected`, 
    station = Site, 
    species = `Genus/Species`, 
    val = `cells/L`,
    valqual = Index
  ) %>% 
  mutate(
    date = date(date), 
    station = case_when(
      station == 'Rock Pond 1' ~ 'RP1',
      station == 'Rock Pond 2' ~ 'RP2', 
      station == 'S. Cockroach 1' ~ 'SCR1', 
      station == 'S. Cockroach 2' ~ 'SCR2', 
      station == 'Cockroach 1' ~ 'CR1', 
      station == 'Cockroach 2' ~ 'CR2',
      station %in% c('S4T1', 'S4T2', 'S4T3', 'S3T5', 'S3T6') ~ paste0(station, 'a'),
      station %in% c('BH0', 'BH15', 'BH25') ~ paste0('TBEP-', station),
      station == 'TBEP0' ~ 'TBEP-BH0',
      station == 'TBEP15' ~ 'TBEP-BH15',
      station == 'TBEP25' ~ 'TBEP-BH25',
      grepl('^Skyway\\sE', station) ~ gsub('^Skyway\\sE', 'SkywayE-', station),
      grepl('^Skyway\\sW', station) ~ gsub('^Skyway\\sW', 'SkywayW-', station),
      station == 'MB1' ~ 'MB-01', 
      station == 'MC202'~ 'MC-202',
      station == 'W8-A-21-PP' ~ 'W8 A 21 03',
      station == 'W8-B-21-PP' ~ 'W8 B 21 03', 
      station == 'W8-C-21-PP' ~ 'W8 C 21 03', 
      station == 'W8-D-21-PP' ~ 'W8 D 21 03', 
      grepl('^DeSoto\\s|^Desoto\\s', station) ~ gsub('^DeSoto\\s|^Desoto\\s', 'Desoto-', station),
      T ~ station
    ), 
    source = 'pinco', 
    typ = 'Quantitative',
    uni = 'cells/L'
  )

##
# EPC

# has TNTC for nanoplankton (too numerous to count), these are filtered
# counts in cells/0.1mL, converted to cells/L
id <- fls[grep('^EPC_Plankton$', fls$name), 'id'] %>% pull(id)
flphy <- read_sheet(id)
rsphydatepc1 <- flphy %>%
  select(
    date = Date,
    station = Station,
    species = Taxa,
    val = Count
    ) %>%
  mutate_if(is.list, as.character) %>%
  filter(!val %in% 'TNTC') %>% # too numerous to count
  filter(!species %in% 'Nanoplankton') %>%
  mutate(
    date = date(date),
    val = as.numeric(val),
    val = val * 10, # cells/0.1mL to cells/mL,
    val = val * 1000, # cells/L
    valqual = case_when(
      val < 1e3 ~ 'Not present/Background',
      val >= 1e3 & val < 1e4 ~ 'Very low',
      val >= 1e4 & val < 1e5 ~ 'Low',
      val >= 1e5 & val < 1e6 ~ 'Medium',
      val >= 1e6 & val ~ 'High'
    ),
    station = case_when(
      station %in% c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '20') ~ paste0('21PP', station, 's'),
      station %in% 'BP01' ~ 'PP-BP1',
      station %in% 'C01' ~ 'PP-C1',
      station %in% 'DB01' ~ 'PP-DB1',
      station %in% 'EGS01' ~ 'PP-EGS1',
      station %in% 'LC01' ~ 'PP-LC1',
      station %in% 'LM01' ~ 'PP-LM1',
      station %in% 'RP01' ~ 'PP-RP1',
      station %in% 'SC01' ~ 'PP-SC1',
      station %in% 'WB01' ~ 'PP-WB1',
      T ~ station
    ),
    source = 'epchc',
    typ = 'Quantitative',
    uni = 'cells/L'
  )

# counts in cells/0.1mL, converted to cells/L
# this sheet includes a bunch of phyto data not near PP

data(bswqdat)

ids <- fls[grep('^EPC_PP', fls$name), 'id'] %>% pull(id)
rsphydatepc2 <- NULL
for(id in ids){
  
  flphy <- read_sheet(id)
  out <- flphy %>%
    select(
      date = SampleTime,
      station = StationNumber,
      species = NAME,
      val = COUNT
    ) %>%
    filter(!species %in% 'Nanoplankton') %>%
    mutate(
      date = date(date),
      station = as.character(station),
      val = val * 10, # cells/0.1mL to cells/mL,
      val = val * 1000, # cells/L
      valqual = case_when(
        val < 1e3 ~ 'Not present/Background',
        val >= 1e3 & val < 1e4 ~ 'Very low',
        val >= 1e4 & val < 1e5 ~ 'Low',
        val >= 1e5 & val < 1e6 ~ 'Medium',
        val >= 1e6 & val ~ 'High'
      ),
      source = 'epchc',
      typ = 'Quantitative',
      uni = 'cells/L'
    ) %>% 
    filter(station %in% bswqdat$station)

  rsphydatepc2 <- bind_rows(rsphydatepc2, out)
  
}

# there is some overlap between the two files
rsphydatepc <- bind_rows(rsphydatepc1, rsphydatepc2) %>% 
  arrange(date, station) %>% 
  unique

## 
# combine all

rsphydat <- bind_rows(rsphydatfldep, rsphydatfwri, rsphydatpinco, rsphydatepc) %>% 
  filter(!valqual == 'Not present/Background' | is.na(valqual))

# clean up species list
rsphydat <- rsphydat %>% 
  mutate(
    species = gsub('BACILLARIOPHYTA', 'Bacillariophyta', species),
    species = gsub('CILIOPHORA', 'Ciliophora', species), 
    species = gsub('^.*taxa:\\s', '', species), 
    species = gsub('Centic', 'Centric', species),
    species = gsub('\\ssp\\.$', ' sp', species),
    species = gsub('^Nitzschia$', 'Nitzschia sp', species),
    species = gsub('^Nitzchia', 'Nitzschia', species),
    species = gsub('nitzchioides$|nitzschioides$', 'nitzschiodes', species),
    species = gsub('^Guinardia$', 'Guinardia sp', species),
    species = gsub('^Pseudo\\-nitzschia$|^Pseudo\\-nitzschia sp$|^Pseudo\\-nitzschia spp\\.$|^Pseudo\\-nitzschia\\sspp\\.', 'Pseudo-nitzschia sp.', species), 
    species = gsub('^Chaetoceros$', 'Chaetoceros sp', species),
    species = gsub('^Skeletonema$|^Skeletonema\\sspp\\.', 'Skeletonema sp.', species),
    species = gsub('^Thalassiosira$', 'Thalassiosira sp', species),
    species = gsub('; no dominant species in sample', '', species),
    species = gsub('\\.$', '', species),
    species = gsub('Karnia', 'Karenia', species),
    species = gsub('\\spresent', '', species), 
    species = gsub('^Prorocentrum$|^Prorocentrum\\ssp\\.$', 'Prorocentrum sp.', species),
    species = gsub('^Nitzchia$|^Nitzschia\\ssp\\.$', 'Nitzschia sp.', species),
    species = gsub('^Rhizosolenia$|^Rhizosolenia\\sspp\\.|Rhizosolenia\\sspp\\.$', 'Rhizosolenia sp.', species),
    species = gsub('^TINTINNIDA$', 'Tintinnida', species), 
    species = gsub('^Thalassionema nitzchioides$', 'Thalassionema nitzschioides', species),
    species = gsub('^Thalassionema nitzchioides$', 'Thalassionema nitzschioides', species), 
    species = gsub('^MIOZOA$', 'Miozoa', species),
    species = gsub('^Gyrosigma$', 'Gyrosigma sp.', species),
    species = gsub('^Gymnodinium$', 'Gymnodinium sp.', species),
    species = gsub('^EUGLENOZOA$', 'Euglenozoa', species),
    species = gsub('^CHLOROPHYTA$', 'Chlorophyta', species),
    species = gsub('^Dictyocha$', 'Dictyocha sp.', species), 
    species = gsub('^Mesodinium Fubrum$', 'Mesodinium rubrum', species), 
    species = gsub('^Mixed Picoplankton$|^Nanoplankton$|^Mixed Picoplankton and Nanoplankton$', 'mixed algae', species),
    species = gsub('^Protoperidinium$', 'Protoperidinium sp.', species),
    species = gsub('^Protoperdinium pellucidum$', 'Protoperidinium pellucidum', species), 
    species = gsub('^Dominant\\staxon\\:\\s', '', species), 
    species = gsub('dominant\\sspecies:\\s', '', species), 
    species = gsub('^Thalassiosira sp\\,\\sKarenia brevis$', 'Thalassiosira sp.; Karenia brevis', species),
    species = gsub('mixed algae;$', 'mixed algae', species),
    species = gsub('sp$|spp$|sp\\.$|spp\\.$', 'sp.', species)
  ) %>% 
  filter(species != 'chains')

# quantitative samples
rsphyquan <- rsphydat %>% 
  filter(typ == 'Quantitative')

# break out multiple species in a row from fldep/fwri to tidy
rsphyqual <- rsphydat %>% 
  filter(typ == 'Qualitative')

exspp <- str_count(rsphyqual$species, ';') %>% 
  max %>% 
  `+`(1) %>% 
  seq(1, .) %>% 
  paste('species', .)

rsphyqual <- rsphyqual %>% 
  separate(species, into = exspp, sep = '; ', fill = 'right') %>% 
  select(date, station, matches('^species'), source, typ) %>% 
  gather('var', 'species', -date, -station, -source, -typ) %>% 
  select(-var) %>% 
  filter(!is.na(species))
  
# recombine
rsphydat <- bind_rows(rsphyqual, rsphyquan) %>% 
  arrange(date, station, species) %>% 
  mutate(
    valqual = factor(valqual, levels = c('Very low', 'Low', 'Medium', 'High'))
  ) 

save(rsphydat, file = 'data/rsphydat.RData', version = 2)

# for log
tms <- Sys.time()
attr(tms, 'tzone') <- 'America/New_York'
tms <- paste(as.character(tms), 'Eastern')

writeLines(tms, 'logs/rsphylog.txt')

## locations --------------------------------------------------------------

data(rsphydat)

typs <- rsphydat %>% 
  select(station, typ) %>% 
  unique

# read master sheet
rsphypts <- read_sheet('1ju1vJpCxR58Iayr8DhBxGVqloZDTI4TDoQUohS0h-6c') %>%
  mutate_if(is.list, as.character) %>% 
  left_join(typs, by = 'station') %>% 
  mutate(col = case_when(
    typ == 'Quantitative' ~ col2hcl('plum1'), 
    typ == 'Qualitative' ~ col2hcl('cyan3')
    )
  )

# jitter overlapping
rsphypts <- rsphypts %>% 
  mutate(
    # lng = ifelse(duplicated(lng), jitter(lng, factor = 300), lng),
    lat = ifelse(duplicated(lat), jitter(lat, factor = 100), lat)
  ) %>% 
  st_as_sf(coords = c('lng', 'lat'), crs = 4326) %>% 
  mutate( 
    lng = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2], 
    source_lng = case_when(
      source == 'pinco' ~ 'Pinellas Co.', 
      source == 'epchc' ~ 'Hillsborough Co.', 
      source == 'fldep' ~ 'FWC-FWRI', 
      source == 'fwri' ~ 'FWC-FWRI'
    )
  ) %>% 
  select(
    source, source_lng, station, lng, lat, typ, col
  )

save(rsphypts, file = 'data/rsphypts.RData', version = 2)

# K brevis scrape ---------------------------------------------------------

data(segmask)

# query api 
path <- 'https://gis.ncdc.noaa.gov/arcgis/rest/services/ms/HABSOS_CellCounts/MapServer/0/query?'

request <- GET(
  url = path,
  query= list(       
    # where = "STATE_ID='FL'",
    where = "LATITUDE < 28.2 AND LATITUDE > 27 AND LONGITUDE > -83.4 AND LONGITUDE < -82.08",
    outFields = 'DESCRIPTION,SAMPLE_DATE,LATITUDE,LONGITUDE,SALINITY,SALINITY_UNIT,WATER_TEMP,WATER_TEMP_UNIT,GENUS,SPECIES,CATEGORY,CELLCOUNT,CELLCOUNT_UNIT',
    f = 'pjson'
  )
)

response <- content(request, as = "text", encoding = "UTF-8")
results <- fromJSON(response, flatten = T)

# format data
kbrdat <- results$features %>% 
  rename_all(function(x) gsub('^attributes\\.', '', x)) %>% 
  rename_all(tolower) %>% 
  mutate(
    date = format(sample_date, scientific = F),
    date = as.numeric(gsub('000$', '', date)), 
    date = as.POSIXct(date, origin = c('1970-01-01'), tz = 'UTC'), 
    date = as.Date(date)
  ) %>% 
  select(
    date, station = description, sal_ppt = salinity, temp_c = water_temp, kb_100kcelll = cellcount, longitude, latitude
  ) %>% 
  mutate(
    kb_100kcelll = kb_100kcelll / 1e5
  ) %>% 
  gather('var', 'val', -date, -station, -longitude, -latitude) %>% 
  separate(var, c('var', 'uni'), sep = '_') %>% 
  filter(!is.na(val)) %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>% 
  .[tbshed, ] %>%
  .[segmask, ]

save(kbrdat, file = 'data/kbrdat.RData', version = 2)

# rainfall ----------------------------------------------------------------

noaa_key <- Sys.getenv('NOAA_KEY')

yrs <- seq(2000, 2021)

res <- yrs %>%
  tibble::enframe('name', 'year') %>%
  dplyr::group_by(name) %>%
  tidyr::nest() %>%
  dplyr::mutate(
    ests = purrr::map(data, function(x){
      
      yr <- x$year
      cat(yr, '\n')
      
      start <- paste0(yr, "-01-01")
      end <- paste0(yr, "-12-31")
      
      # download NOAA UWS rainfall station data
      sp_rainfall <- rnoaa::ncdc(datasetid = "GHCND", stationid = "GHCND:USW00092806",
                                 datatypeid = "PRCP", startdate = start, enddate = end,
                                 limit = 500, add_units = TRUE, token = noaa_key)
      sp_rain <- sp_rainfall$data %>%
        mutate(
          date = as.Date(date),
          precip_cm = value / 100,
          station = 'St. Pete Albert Whitted'
        ) %>%
        select(station, date, precip_cm)
      
      # tia_rainfall <- rnoaa::ncdc(datasetid = "GHCND", stationid = "GHCND:USW00012842",
      #                             datatypeid = "PRCP", startdate = start, enddate = end,
      #                             limit = 500, add_units = TRUE, token = noaa_key)
      # tia_rain <- tia_rainfall$data %>%
      #   mutate(
      #     date = as.Date(date),
      #     precip_cm = value / 100,
      #     station = 'Tampa International'
      #   ) %>%
      #   select(station, date, precip_cm)

      # out <- bind_rows(sp_rain, tia_rain)
      out <- sp_rain
      
      return(out)
      
    })
  )

raindat <- res %>% 
  unnest('data') %>% 
  unnest('ests') %>% 
  ungroup() %>% 
  select(station, date, precip_cm)

save(raindat, file = 'data/raindat.RData')

# flow data ---------------------------------------------------------------

# hydrology is sum of flow from six stations
# flow is returned at ft3/s, we convert it to m3/d

yrs <- seq(1995, 2021)

res <- yrs %>%
  tibble::enframe('name', 'year') %>%
  dplyr::group_by(name) %>%
  tidyr::nest() %>%
  dplyr::mutate(
    ests = purrr::map(data, function(x){
      
      yr <- x$year
      cat(yr, '\n')
      
      start <- paste0(yr, "-01-01")
      end <- paste0(yr, "-12-31")
      
      # download USGS streamflow data
      hr <- dataRetrieval::readNWISdv("02303000", "00060", start, end) %>%
        dataRetrieval::renameNWISColumns()
      ar <- dataRetrieval::readNWISdv("02301500", "00060", start, end) %>%
        dataRetrieval::renameNWISColumns()
      lmr <- dataRetrieval::readNWISdv("02300500", "00060", start, end) %>%
        dataRetrieval::renameNWISColumns()
      tar <- dataRetrieval::readNWISdv("02307498", "00060", start, end) %>%
        dataRetrieval::renameNWISColumns()
      wl <- dataRetrieval::readNWISdv("02300042", "00060", start, end) %>%
        dataRetrieval::renameNWISColumns()
      mr <- dataRetrieval::readNWISdv("02299950", "00060", start, end) %>%
        dataRetrieval::renameNWISColumns()
      
      out <- bind_rows(hr, ar, lmr, tar, wl, mr) %>% 
        mutate(
          Flow = Flow * 86400 / 35.3147 # ft3/s to m3/d
        ) %>% 
        group_by(Date) %>% 
        summarise(Flow = sum(Flow, na.rm = T))
      
      return(out)
      
    })
  )

hydrodat <- res %>% 
  unnest('data') %>% 
  unnest('ests') %>% 
  ungroup() %>% 
  select(date = Date, flow_m3 = Flow)

save(hydrodat, file = 'data/hydrodat.RData')

# wind data ---------------------------------------------------------------

dt2 <- Sys.Date() 
dt1 <- as.Date('2021-01-01')
dts <- seq.Date(dt1, dt2, by = 'day')

# metric units selected, wind is m/2
# station is coast guard facility south side of Albert Whitted, 8726520

out <- NULL
for(i in seq_along(dts)){
  
  cat(i, 'of', length(dts), '\n')
  
  if(dts[i] == max(dts))
    next()
  
  dtstr <- dts[i] %>% 
    as.character %>% 
    gsub('\\-', '', .)
  dtend <- dts[i + 1] %>% 
    as.character %>% 
    gsub('\\-', '', .)
  
  url <- paste('https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?product=wind&application=NOS.COOPS.TAC.MET&begin_date=', dtstr, '&end_date=', dtend, '&station=8726520&time_zone=LST&units=metric&format=CSV', sep = '')
  
  dat <- try({read.table(url, sep = ',', header = T) %>% 
      .[, c(1:3)]
  })
  
  if(inherits(dat, 'try-error'))
    next()
  
  out <- rbind(out, dat)
  
}

winddat <- out %>% 
  unique %>% 
  select(
    datetime = Date.Time,
    wind_ms = Speed, 
    wind_dir = Direction
  ) %>% 
  mutate(datetime = ymd_hm(datetime, tz = 'America/Jamaica'))

save(winddat, file = 'data/winddat.RData')

# benthic -----------------------------------------------------------------

## data -------------------------------------------------------------------

# TBBI scores, pulled code from tbeptools
# salinity was not available at sites, so estimated from rswqdat

data(rswqdat)

# salinity avg values
salest <- rswqdat %>% 
  filter(var == 'sal') %>% 
  mutate(
    mo = month(date)
  ) %>% 
  group_by(mo) %>% 
  summarise(
    val = mean(val, na.rm = T), 
    .groups = 'drop'
  ) %>% 
  deframe

# spring (april) samples
fltax <- read_sheet('1p2qOLsRjKUxMkKxGaT7P30k8vPkfzoR_Wc3OhW46QEs')
salin <- salest['4'] #  this was the avg salinity in Apr
rsbntdat1 <- tbbi_fun(fltax, salin)

# fall (sep) samples
fltax <- read_sheet('1v2ARGkpp-528TLVBUJmLtOTrDo40PPWrpKr-9YX6VpQ')
salin <- salest['9'] # this was the avg salinity in Sep
rsbntdat2 <- tbbi_fun(fltax, salin)

# combine
rsbntdat <- bind_rows(rsbntdat1, rsbntdat2) %>% 
  mutate(
    station = gsub('s$|f$', '', station)
  )

save(rsbntdat, file = 'data/rsbntdat.RData', version = 2)

# for log
tms <- Sys.time()
attr(tms, 'tzone') <- 'America/New_York'
tms <- paste(as.character(tms), 'Eastern')

writeLines(tms, 'logs/rsbntlog.txt')

## locations --------------------------------------------------------------

# https://docs.google.com/spreadsheets/d/1eJ64uMX7WOrt2XrjxKV67W5Y_EiXDh_9gBFpaBXuQWo/edit#gid=0
flbnt <- read_sheet('1eJ64uMX7WOrt2XrjxKV67W5Y_EiXDh_9gBFpaBXuQWo')

rsbntpts <- flbnt %>% 
  mutate(
    longitude = lng, 
    latitude = lat, 
    source = 'epchc', 
    source_lng = 'Hillsborough Co.'
    ) %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)

save(rsbntpts, file = 'data/rsbntpts.RData', version = 2)

# coordinated response station locations ----------------------------------

epchcsta1 <- read.csv('data/raw/epc_stations.csv') %>% 
  mutate(
    source = 'epchc', 
    comment = NA_character_, 
    station = as.character(station)
  ) %>% 
  select(source, station, lat, lon, comment)
fldepsta1 <- read_excel('data/raw/FLDEP.xlsx') %>% 
  mutate(
    source = 'fldep', 
    comment = Description
  ) %>% 
  select(source, station = Name, lat = Latitude, lon = Longitude, comment)
mpnrdsta1 <- read.csv('data/raw/manco_stations.csv') %>% 
  mutate(
    source = 'mpnrd'
  )
pincosta1 <- read.csv('data/raw/pinco_stations.csv')
ncfsta1 <- read.csv('data/raw/ncf_stations.csv') %>% 
  select(
    station = Set,
    lon = Lon, 
    lat = Lat
  ) %>% 
  mutate(
    source = 'ncf', 
    comment = NA_character_
  )
usfsta1 <- read.csv('data/raw/usf_stations.csv')
tbepsta1 <- read.csv('data/raw/tbep_stations.csv')
ufsta1 <- read.csv('data/raw/uf_stations.csv')
cospsta1 <- read.csv('data/raw/cosp_stations.csv')

# esa
esa <- read.csv('data/raw/esa_stations.csv') 
chd <- substr(esa$lat, 3, 3)[1]
esastat1 <- esa %>% 
  separate(lat, c('lat1', 'lat2'), sep = chd) %>% 
  separate(lon, c('lon1', 'lon2'), sep = chd) %>% 
  mutate(
    lon2 = gsub("\\'", '', lon2),
    lat2 = gsub("\\'", '', lat2),
    lat1 = as.numeric(lat1),
    lat2 = as.numeric(lat2) / 60,
    lon1 = as.numeric(lon1),
    lon2 = as.numeric(lon2) / 60
  ) %>% 
  rowwise() %>% 
  mutate(
    lat = lat1 + lat2,
    lon = lon1 + lon2, 
    lon = -1 * lon
  ) %>% 
  ungroup() %>% 
  select(source, station, lon, lat)

# combine all
rsstatloc <- bind_rows(epchcsta1, fldepsta1, mpnrdsta1, pincosta1, ncfsta1, usfsta1, tbepsta1, esastat1, ufsta1, cospsta1) %>% 
  mutate(
    source = source,
    source_lng = case_when(
      source == 'pinco' ~ 'Pinellas Co.', 
      source == 'epchc' ~ 'Hillsborough Co.', 
      source == 'fldep' ~ 'Florida DEP', 
      source == 'mpnrd' ~ 'Manatee Co.', 
      source == 'ncf' ~ 'New College Fl.', 
      source == 'tbep' ~ 'TBEP', 
      source == 'usf' ~ 'USF', 
      source == 'esa' ~ 'ESA', 
      source == 'uf' ~ 'UF', 
      source == 'sbep' ~ 'SBEP',
      source == 'cosp' ~ 'City of St. Pete'
    )
  ) %>% 
  select(source_lng, source, station, lat, lon) %>% 
  arrange(source_lng, station)

write.csv(rsstatloc, 'data/raw/wq_stations_all.csv', row.names = F)

rsstatloc <- rsstatloc %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)

save(rsstatloc, file = 'data/rsstatloc.RData', version = 2)

# map of coordinated response stations w/ nearest long-term ---------------

data(rsstatloc)
data(bswqrngs)

bswqloc <- bswqrngs %>% 
  select(bswqstation, lat, lng) %>% 
  unique() %>% 
  as.data.frame(stringsAsFactors = F) %>% 
  st_as_sf(coords = c('lng', 'lat'), crs = prj)

# nearest ltm stations to rswq stations
rswqnear <- rsstatloc %>% 
  select(station) %>% 
  mutate(
    bswqstation = st_nearest_feature(rsstatloc, bswqloc), 
    bswqstation = bswqloc$bswqstation[bswqstation]
  ) %>% 
  st_set_geometry(NULL) %>% 
  unique

rswqlns <- rsstatloc %>% 
  select(source, station) %>% 
  group_by(source, station) %>% 
  nest() %>% 
  mutate(
    lns = purrr::map(data, function(x){

      out <- st_nearest_points(x, bswqloc) %>% 
        st_cast('LINESTRING') %>%
        st_as_sf() %>% 
        rename(geometry = x) %>% 
        mutate(
          dist = st_length(.)
        ) %>% 
        arrange(dist) %>% 
        .[1, ]
      
      return(out)
      
    })
  ) %>% 
  select(-data) %>%
  unnest('lns') %>%
  ungroup() %>% 
  mutate(
    dist = units::set_units(dist, 'km'), 
    km = as.numeric(dist)
  ) %>% 
  select(-dist) %>% 
  st_as_sf()

bswqloc <- bswqloc %>%
  filter(bswqstation %in% as.character(rswqnear$bswqstation)) %>% 
  mutate(type = 'Long-term')

wqrefmap <- mapview(rswqlns, color = 'grey', homebutton = F, layer.name = 'Distance to closest', label = paste(round(rswqlns$km, 1), 'km')) +
  mapview(rsstatloc, col.regions = 'lightblue', alpha.regions = 1, lwd = 0.5, cex = 4, label = paste0('Current station ', rsstatloc$station), layer.name = 'Current stations', homebutton = F) +
  mapview(bswqloc, col.regions = 'tomato1', alpha.regions = 1, lwd = 0.5, cex = 4, label = paste0('Reference station ', bswqloc$bswqstation), layer.name = 'Reference stations', homebutton = F)

save(wqrefmap, file = 'data/wqrefmap.RData')

# coordinated response data -----------------------------------------------

data(bswqrngs)
data(parms)
data(rsstatloc)

gdrive_pth <- 'https://drive.google.com/drive/folders/1SWlBbZtjZ8SF43MCz5nv5YLCWDvn7T_x'

# sleep time in seconds
wait <- 10

# csv files must be opened/saved as spreadsheet in google sheets
fls <- drive_ls(gdrive_pth, type = 'spreadsheet')

## fldep ------------------------------------------------------------------

fl <- fls[which(fls$name == 'FLDEP_20211217'), 'id'] %>% pull(id)
flsht <- read_sheet(fl)
out1 <- flsht %>% 
  clean_names %>% 
  select(
    station = station_id, 
    date = sample_date, 
    secchi_m = secchi_depth_m, 
    temp_c = water_temperature_o_c_surface, 
    sal_ppt = salinity_0_00_surface, 
    dosat_per = d_o_percent_sat_surface, 
    ph_none = p_h_surface, 
    nh34_mgl = ammonia_n_mg_n_l, 
    orthop_mgl = orthophosphate_p_mg_p_l, 
    chla_ugl = chlorophyll_a_corrected_aeg_l, # this is ugl, janitor think mu is m
    turb_ntu = turbidity_ntu, 
    tp_mgl = total_phosphorus_mg_p_l, 
    tn_mgl = total_nitrogen_calculated_mg_n_l,
  ) %>% 
  mutate_if(is.list, as.character) %>% 
  gather('var', 'val', -station, -date) %>% 
  separate(var, c('var', 'uni'), sep = '_') %>% 
  mutate(
    date = as.Date(date), 
    val = case_when(
      val %in% c('NULL', 'not detected') ~ '', 
      T ~ val
    ),
    val = gsub('\\"', '', val),
    qual = gsub('^\\d+\\.\\d+|^\\d+', '', val),
    qual = gsub('^\\s+', '', qual),
    qual = case_when(
      qual == '' ~ NA_character_, 
      T ~ qual
    ),
    val = gsub('(^\\d+\\.\\d+|^\\d+)\\s.*$', '\\1', val),
    val = as.numeric(val),
    val = case_when(
      station == 'Piney 17' & var == 'secchi' & val == 21 ~ 2.1, 
      T ~ val
    ),
    station = gsub('^Piney\\s\\s17$', 'Piney 17', station),
    source = 'fldep'
  ) %>% 
  select(station, date, source, var, uni, val, qual) %>% 
  filter(!is.na(val))

# supplement NOx data from temp sheet
tmpsht <- 'https://publicfiles.dep.state.fl.us/DEAR/DEARweb/_PP_EventResponse/Latest_Analytical_Results.xlsx'
tmpfl <- tempfile(fileext = '.xlsx')
download.file(url = tmpsht, destfile = tmpfl, method = 'libcurl', mode = 'wb')

out2 <- read_excel(tmpfl, skip = 8) %>% 
  select(
    station = `SITE LOCATION`, 
    date = `DATE SAMPLED`,
    var = COMPONENT, 
    uni = UNITS, 
    val = RESULT,
    qual = `QUALIFIER CODE`
  ) %>% 
  filter(
    var %in% c('Ammonia-N', 'Chlorophyll-a, Corrected', 'NO2NO3-N', 'O-Phosphate-P', 'Total-P', 'TSS', 'Turbidity')
  ) %>% 
  mutate(
    date = as.Date(date), 
    val = as.numeric(val), 
    var = case_when(
      var == 'Ammonia-N' ~ 'nh34', 
      var == 'Chlorophyll-a, Corrected' ~ 'chla', 
      var == 'NO2NO3-N' ~ 'no23', 
      var == 'O-Phosphate-P' ~ 'orthop', 
      var == 'Total-P' ~ 'tp', 
      var == 'TSS' ~ 'tss', 
      var == 'Turbidity' ~ 'turb'
    ), 
    uni = case_when(
      uni == 'mg N/L' ~ 'mgl', 
      uni == 'mg P/L' ~ 'mgl',
      uni == 'mg/L' ~ 'mgl', 
      uni == 'NTU' ~ 'ntu', 
      uni == 'ug/L' ~ 'ugl'
    ),
    station = gsub('^Piney\\s\\s', 'Piney ', station),
    source = 'fldep'
  )

file.remove(tmpfl)

# remove variables in out1 that are in out2
# but first check there are more observations in out2 since it's not regularly updated
vrs <- unique(out2$var)
tmp1 <- out1 %>% 
  filter(var %in% vrs)
tmp2 <- out2

# if there are more variables for the relevant params in out2, remove same from out1
if(nrow(tmp2) > nrow(tmp1)){
  
  out1 <- out1 %>% 
    filter(!var %in% vrs)

  fldep1 <- bind_rows(out1, out2) %>% 
    arrange(date, var)

}

# if there are less variables for the relevent params in out2, keep only out1
if(nrow(tmp2) < nrow(tmp1)){
  
  warning('something bad!')
  fldep1 <- out1 %>% 
    arrange(date, var)

}

##
# file from Nia 
fl <- fls[which(fls$name == 'FLDEP_NW_20210927'), 'id'] %>% pull(id)
flsht <- read_sheet(fl)
out3 <- flsht %>% 
  select(
    station = `SITE LOCATION`, 
    date = `DATE SAMPLED`,
    var = COMPONENT, 
    uni = UNITS, 
    val = RESULT,
    qual = `QUALIFIER CODE`
  ) %>% 
  filter(
    var %in% c('Ammonia-N', 'Chlorophyll-a, Corrected', 'NO2NO3-N', 'O-Phosphate-P', 'Total-P', 'TSS', 'Turbidity')
  ) %>% 
  mutate(
    date = as.Date(date), 
    val = as.numeric(val), 
    var = case_when(
      var == 'Ammonia-N' ~ 'nh34', 
      var == 'Chlorophyll-a, Corrected' ~ 'chla', 
      var == 'NO2NO3-N' ~ 'no23', 
      var == 'O-Phosphate-P' ~ 'orthop', 
      var == 'Total-P' ~ 'tp', 
      var == 'TSS' ~ 'tss', 
      var == 'Turbidity' ~ 'turb'
    ), 
    uni = case_when(
      uni == 'mg N/L' ~ 'mgl', 
      uni == 'mg P/L' ~ 'mgl',
      uni == 'mg/L' ~ 'mgl', 
      uni == 'NTU' ~ 'ntu', 
      uni == 'ug/L' ~ 'ugl'
    ),
    station = gsub('^Piney\\s\\s', 'Piney ', station),
    source = 'fldep'
  )

# find what's in out3 that's not what's in fldep1
out3 <- anti_join(out3, fldep1, by = c('station', 'date', 'var', 'uni', 'source', 'val', 'qual'))

# join with fldep1
fldep1 <- bind_rows(fldep1, out3) %>% 
  arrange(date, var)

# remove long text strings from qual codes
fldep1 <- fldep1 %>% 
  mutate(
    qual = gsub('(^[A-Z]+)\\s.*$', '\\1', qual)
  )

## mpnrd ------------------------------------------------------------------

# mpnrd, creek and outfall samples
ids <- fls[grep('Comp', fls$name), 'id'] %>% pull(id)
out1 <- NULL
for(id in ids) {
  
  # sleep to not bonk api limit
  Sys.sleep(wait)   
  
  tmp <- read_sheet(id)
 
  sta <- tmp[[1, 7]][[1]]
  dt <- as.Date(tmp[[2, 7]][[1]])
  
  tmp <- tmp[5:nrow(tmp), c(2, 5, 8, 9)] 
  
  names(tmp) <- c('var', 'val', 'qual', 'uni')
  
  tmp <- tmp %>% 
    mutate_if(is.list, as.numeric) %>%
    mutate(
      var = case_when(
        var == 'Color' ~ 'color',
        var == 'Turbidity' ~ 'turb',
        var == 'Total Suspended Solids' ~ 'tss',
        var == 'Phosphorus, Total' ~ 'tp',
        var == 'Nitrate + Nitrite as N' ~ 'no23', 
        var == 'Temperature' ~ 'temp',
        var == 'Chlorophyll-a' ~ 'chla',
        var == 'Total Kjedahl Nitrogen - Saltwater' ~ 'tkn',
        var == 'Ammonia as N' ~ 'nh34', 
        var == 'Dissolved Oxygen' ~ 'do',
        var == 'Dissolved Oxygen % Saturation' ~ 'dosat', 
        var == 'pH' ~ 'ph', 
        var == 'Salinity' ~ 'sal', 
        var == 'Transparency (Secchi Depth)' ~ 'secchi'
      ),
      uni = case_when(
        uni == 'PCU' ~ 'pcu',
        uni == 'NTU' ~ 'ntu', 
        uni == 'mg/L' ~ 'mgl', 
        uni == 'mg/m3' ~ 'ugl', 
        uni == 'PSS' ~ 'ppt',
        uni == 'SU' ~ 'none', 
        uni == '%' ~ 'per', 
        uni == 'deg C' ~ 'c',
        T ~ uni
      ),
      station = sta, 
      date = dt,
      source = 'mpnrd'
    ) %>% 
    filter(!is.na(var))
  
  out1 <- bind_rows(out1, tmp)
  
}

out1all <- out1 %>% 
  mutate(
    station = case_when(
      grepl('^\\(Piney\\sPoint\\sOutfall|^\\(PP\\sOutfall', station) ~ 'PM Out', 
      grepl('^\\(Piney\\sPoint\\sCreek|^\\(PP\\sCreek', station) ~ 'PPC41', 
      T ~ station
    ), 
  ) %>% 
  select(station, date, source, var, uni, val, qual) %>% 
  unique

# mpnrd, estuary samples
ids <- fls[grep('Results_By_Test_Param', fls$name), 'id'] %>% pull(id)
out2 <- NULL
for(id in ids) {

  # sleep to not bonk api limit
  Sys.sleep(15)   
  
  cat('mpnrd', which(id == ids), 'of', length(ids), '\n')
  
  var <- fls[fls$id == id, 'name'] %>% pull('name')

  tmp <- read_sheet(id) %>% 
    clean_names() %>% 
    select(
      station = sample_location, 
      date = sampled_date, 
      val = reported_result, 
      qual = qual,
      type = sample_name, 
      uni = unit
    ) %>% 
    mutate(
      station = as.character(station), 
      var = var
    )
  
  out2 <- bind_rows(out2, tmp)
  
}

out2all <- out2 %>% 
  mutate(
    source = 'mpnrd',
    date = as.Date(date), 
    var = gsub('\\sResults_By_Test_Param', '', var), 
    var = case_when(
      grepl('NH4$', var) ~ 'nh34',
      grepl('NN$', var) ~ 'no23',
      grepl('TKN$', var) ~ 'tkn',
      grepl('TP$', var) ~ 'tp',
      grepl('TSS$', var) ~ 'tss',
      grepl('Turb$', var) ~ 'turb', 
      grepl('Color$', var) ~ 'color', 
      grepl('Chla$', var) ~ 'chla'
    ), 
    uni = case_when(
      uni == 'PCU' ~ 'pcu',
      uni == 'NTU' ~ 'ntu', 
      uni == 'mg/L' ~ 'mgl', 
      uni == 'mg/m3' ~ 'ugl', 
      T ~ uni
    ),
    station = case_when(
      grepl('^PMB\\s', station) ~ gsub('^PMB\\s', 'PMB', station), 
      T ~ station
    )
  ) %>% 
  filter(!type %in% 'EQB') %>% # remove field blanks (other are duplicates FD)
  select(station, date, source, var, uni, val, qual)

mpnrd1 <- bind_rows(out1all, out2all)

## pinco ------------------------------------------------------------------

# # lab results
# ids <- fls[grep('^PINCO_labresults', fls$name), 'id'] %>% pull(id)
# out1 <- NULL
# for(id in ids) {
# 
#   # sleep to not bonk api limit
#   Sys.sleep(wait)
# 
#   dat <- read_sheet(id)
# 
#   tmp <- dat %>%
#     clean_names() %>%
#     select(
#       station = collection_site,
#       date = collect_date,
#       var = analyte_name,
#       val = formatted_result,
#       uni = result_units,
#       qual = qualifiers
#     ) %>%
#     mutate(
#       station = case_when(
#         station == 'PMOutfall' ~ 'PM Out',
#         station == 'TBEP-MCBHO2' ~ 'TBEP-MCBH02',
#         station %in% c('BH0', 'BH15', 'BH25') ~ paste0('TBEP-', station),
#         station == 'TBEP0' ~ 'TBEP-BH0',
#         station == 'TBEP15' ~ 'TBEP-BH15',
#         station == 'TBEP25' ~ 'TBEP-BH25',
#         station == 'Clamber 4' ~ 'CB4',
#         station == 'Clamber 6' ~ 'CB6',
#         station == 'E7-2A' ~ 'E7-A',
#         station == 'E7-2B' ~ 'E7-B',
#         station == 'E7-2C' ~ 'E7-C',
#         station == 'E7-2D' ~ 'E7-D',
#         grepl('\\sS$', station) ~ paste0('MC', station),
#         T ~ station
#       ),
#       station = gsub('\\sS$', '', station),
#       station = gsub('^PC\\s', 'PC', station),
#       station = gsub('^MC\\s', 'MC', station),
#       station = gsub('^Skyway\\s', 'Skyway', station),
#       var = case_when(
#         var == 'Ammonia' ~ 'nh34',
#         var == 'Nitrate+Nitrite (N)' ~ 'no23',
#         var == 'Orthophosphate as P(Dissolved)' ~ 'orthop',
#         var == 'Total Phosphorus' ~ 'tp',
#         var == 'Total Suspended Solids' ~ 'tss',
#         var == 'Turbidity' ~ 'turb',
#         var == 'Chlorophyll a (Corrected)' ~ 'chla',
#         var == 'Total Kjeldahl Nitrogen' ~ 'tkn',
#         T ~ var
#       ),
#       val = as.numeric(gsub('[^0-9.-]', '', val)),
#       uni = case_when(
#         uni == 'mg/L' ~ 'mgl',
#         uni == 'NTU' ~ 'ntu',
#         uni == 'mg/m3' ~ 'ugl',
#         T ~ uni
#       ),
#       source = 'pinco',
#       source = case_when(
#         station %in% c("TBEP-BH0", "TBEP-BH15", "TBEP-BH25", "TBEP-MCBH02") ~ 'tbep',
#         T ~ source
#       ),
#       date = as.Date(date)
#     ) %>%
#     filter(!station %in% c('MC- FB', 'PP-FB')) %>% # these are validation samples, no data
#     filter(var %in% parms$var)
# 
#   if(nrow(tmp) == 0)
#     next()
#   
#   out1 <- bind_rows(out1, tmp)
# 
# }

# compiled results
ids <- fls[grep('^PINCO_compiledresults', fls$name), 'id'] %>% pull(id)
flsht1 <- read_sheet(ids)
pincowide <- flsht1 %>%
  clean_names() %>%
  select(
    date,
    station = site,
    temp_c = temp_c,
    ph_none = p_h,
    sal_ppt = sal_ppt,
    secchi_m = secchi_depth_m,
    do_mgl = odo_mg_l,
    dosat_per = odo_percent_sat,
    secchi_qual = vob,
    tkn_mgl = tkn_mg_l_as_n,
    tkn_qual = tkn_flag,
    nh34_mgl = nh3_mg_l_as_n, 
    nh34_qual = nh3_flag, 
    no23_mgl = n_ox_mg_l_as_n, 
    no23_qual = nox_flag, 
    tp_mgl = total_p_mg_l_as_p, 
    tp_qual = tp_flag, 
    orthop_mgl = dissd_o_p_mg_l_as_p, 
    orthop_qual = op_flag, 
    chla_ugl = chlorophyll_a_corrd_mg_m3,
    chla_qual = cha_flag, 
    tss_mgl = tss_mg_l, 
    tss_qual = tss_flag, 
    turb_ntu = turbidity_ntu, 
    turb_qual = turb_flag, 
    bod_mgl = total_bod_mg_l, 
    bod_qual = bod_flag
  )

pinco1wq <- pincowide %>%
  select(-matches('\\_qual$')) %>%
  gather('var', 'val', -station, -date) %>%
  separate(var, c('var', 'uni'), sep = '_') %>% 
  mutate(
    val = as.character(val), 
    val = case_when(
      grepl('^NULL$|^NA$|^\\.$', val) ~ NA_character_, 
      T ~ val
      ),
    val = as.numeric(val)
  )

pinco1qual <- pincowide %>%
  select(date, station, matches('\\_qual$')) %>%
  gather('var', 'qual', -date, -station) %>%
  mutate(var = gsub('\\_qual$', '', var)) %>%
  filter(grepl('\\w', qual))

out2 <- full_join(pinco1wq, pinco1qual, by = c('date', 'station', 'var')) %>%
  mutate(
    source = 'pinco',
    date = ymd(date),
    station = ifelse(grepl('\\sS$', station), paste0('MC', station), station),
    station = gsub('\\sS$', '', station),
    station = gsub('\\s+', '', station),
    station = gsub('\\-21\\-PP$', '', station),
    station = case_when(
      station == 'PMOutfall' ~ 'PM Out',
      station == 'TBEP-MCBHO2' ~ 'TBEP-MCBH02',
      station %in% c('BH0', 'BH15', 'BH25') ~ paste0('TBEP-', station),
      station == 'TBEP0' ~ 'TBEP-BH0',
      station == 'TBEP15' ~ 'TBEP-BH15',
      station == 'TBEP25' ~ 'TBEP-BH25',
      station %in% c('Clamber 4', 'Clambar4') ~ 'CB4',
      station %in% c('Clamber 6', 'Clambar6') ~ 'CB6',
      station %in% c('E7-2A', 'E7-2A-21-02') ~ 'E7-A',
      station %in% c('E7-2B', 'E7-2B-21-02') ~ 'E7-B',
      station %in% c('E7-2C', 'E7-2C-21-01') ~ 'E7-C',
      station %in% c('E7-2D', 'E7-2D-21-02') ~ 'E7-D',
      station %in% c("FD-A-21-03", "FD-B-21-03", "FD-C-21-03", "FD-D-21-03", "W7-A-21-03", 
                     "W7-B-21-03", "W7-C-21-03", "W7-D-21-03", "W8-A-21-03", "W8-B-21-03", 
                     "W8-C-21-03", "W8-D-21-03") ~ gsub('\\-', ' ', station),
      grepl('^DeSoto', station) ~ gsub('^DeSoto', 'Desoto', station),
      grepl('\\sS$', station) ~ paste0('MC', station),
      T ~ station
    ),
    source = case_when(
      station %in% c("TBEP-BH0", "TBEP-BH15", "TBEP-BH25", "TBEP-MCBH02") ~ 'tbep', 
      T ~ source
    ), 
    var = case_when( # this is a fix for mislabelled dosat as do and do as dosat
      var == 'do' & station %in% c("W7-A-21-04", "W7-B-21-02", "W7-C-21-04", "W7-D-21-04", "W8-B-21-04", 
                                   "W8-C-21-04", "W8-D-21-04", "W8-A-21-04") &
        date == as.Date('2021-07-08') ~ 'dosat', 
      var == 'dosat' & station %in% c("W7-A-21-04", "W7-B-21-02", "W7-C-21-04", "W7-D-21-04", "W8-B-21-04", 
                                   "W8-C-21-04", "W8-D-21-04", "W8-A-21-04") &
        date == as.Date('2021-07-08') ~ 'do', 
      T ~ var
    ), 
    uni = case_when(
      var == 'do' & station %in% c("W7-A-21-04", "W7-B-21-02", "W7-C-21-04", "W7-D-21-04", "W8-B-21-04", 
                                   "W8-C-21-04", "W8-D-21-04", "W8-A-21-04") &
        date == as.Date('2021-07-08') ~ 'mgl', 
      var == 'dosat' & station %in% c("W7-A-21-04", "W7-B-21-02", "W7-C-21-04", "W7-D-21-04", "W8-B-21-04", 
                                      "W8-C-21-04", "W8-D-21-04", "W8-A-21-04") &
        date == as.Date('2021-07-08') ~ 'per', 
      T ~ uni
    )
  ) %>%
  select(station, date, source, var, uni, val, qual) %>%
  filter(!is.na(val)) %>%
  filter(!station %in% c('MC- FB', 'PP-FB')) %>% # this is a validation site, no data
  unique

# # sleep to not bonk api limit
# Sys.sleep(wait)
# 
# # field results
# ids <- fls[grep('^PINCO_fieldresults', fls$name), 'id'] %>% pull(id)
# flsht1 <- read_sheet(ids) 
# pincowide <- flsht1 %>% 
#   clean_names() %>% 
#   select(
#     date,
#     station = site,
#     temp_c = temp_c, 
#     ph_none = p_h, 
#     sal_ppt = sal_ppt, 
#     secchi_m = secchi_depth_m,
#     do_mgl = odo_mg_l, 
#     dosat_per = odo_percent_sat,
#     secchi_qual = vob,
#   )
# 
# pinco1wq <- pincowide %>% 
#   select(-matches('\\_qual$')) %>% 
#   gather('var', 'val', -station, -date) %>% 
#   separate(var, c('var', 'uni'), sep = '_')
# 
# pinco1qual <- pincowide %>% 
#   select(date, station, matches('\\_qual$')) %>% 
#   gather('var', 'qual', -date, -station) %>% 
#   mutate(var = gsub('\\_qual$', '', var)) %>% 
#   filter(grepl('\\w', qual))
# 
# out2 <- full_join(pinco1wq, pinco1qual, by = c('date', 'station', 'var')) %>% 
#   mutate(
#     source = 'pinco', 
#     date = ymd(date), 
#     station = gsub('\\s+', '', station), 
#     station = case_when(
#       station == 'CB1' ~ 'Clambar Bay 1', 
#       station == 'CB2' ~ 'Clambar Bay 2', 
#       station == 'CB3' ~ 'Clambar Bay 3', 
#       station == 'CB4' ~ 'Clambar Bay 4', 
#       station == 'CB5' ~ 'Clambar Bay 5', 
#       station == 'CB6' ~ 'Clambar Bay 6', 
#       station == 'CB7' ~ 'Clambar Bay 7', 
#       station == 'CB8' ~ 'Clambar Bay 8', 
#       station == 'JB1' ~ 'Joe Bay 1', 
#       station == 'JB2' ~ 'Joe Bay 2', 
#       station == 'JB3' ~ 'Joe Bay 3', 
#       station == 'JB4' ~ 'Joe Bay 4',
#       station == 'JB5' ~ 'Joe Bay 5', 
#       station == 'JB6' ~ 'Joe Bay 6',
#       station == 'TBEP-MCBHO2' ~ 'TBEP-MCBH02',
#       T ~ station
#     )
#   ) %>% 
#   select(station, date, source, var, uni, val, qual) %>% 
#   filter(!is.na(val)) %>% 
#   filter(!station %in% 'MC- FB') %>% # this is a validation site, no data
#   unique

# there are diff samples by depth for in situ, avg out
pinco1 <- bind_rows(out2) %>% 
  group_by(station, date, source, var, uni, qual) %>% 
  summarise(
    val = mean(val, na.rm = T), 
    .groups = 'drop'
  ) %>% 
  filter(
    !(var == 'ph' & val > 14)
  ) %>% 
  select(station, date, source, var, uni, val, qual)

## ncf --------------------------------------------------------------------

ids <- fls[grepl('^NCF\\_waterquality', fls$name), 'id'] %>% pull(id) 

ncfout1 <- NULL
for(id in ids){
  
  # sleep to not bonk api limit
  Sys.sleep(wait)
  
  flsht <- read_sheet(id)
  tmp <- flsht %>% 
    select(
      station = Set, 
      date = Date, 
      temp = Temp_C, 
      secchi = Turbidity_m, 
      sal = Salinity_ppt,
      do = DO_mg
    ) %>% 
    gather('var', 'val', -station, -date) %>% 
    mutate(
      date = as.Date(date),
      source = 'ncf', 
      qual = NA_character_, 
      uni = case_when(
        var == 'temp' ~ 'c', 
        var == 'secchi' ~ 'm', 
        var == 'sal' ~ 'ppt', 
        var == 'do' ~ 'mgl'
      )
    )
  
  ncfout1 <- bind_rows(ncfout1, tmp)

}

ncf1 <- ncfout1

## epc --------------------------------------------------------------------

# in situ
ids <- fls[grep('^EPC\\_PP\\_insitu', fls$name), 'id'] %>% pull(id)
epcout1 <- NULL
for(id in ids){
  
  # sleep to not bonk api limit
  Sys.sleep(wait)
  
  flsht <- read_sheet(id)
  epctmp <- flsht %>% 
    select(
      station = `EPCStation`, 
      date = `Date`, 
      temp = `Temp-C`, 
      ph = `pH`, 
      sal = `Salin-PSS`,
      dosat = `DO%-Sat`, 
      do = `DO-mg/L`
    ) %>% 
    mutate(
      date = date(date), 
      source = 'epchc', 
      station = as.character(station),
      station = gsub('^PP', '', station),
      qual = NA_character_
    ) %>% 
    gather('var', 'val', -station, -date, -source, -qual) %>% 
    left_join(parms, by = 'var') %>% 
    group_by(station, date, source, var, uni, qual) %>% 
    summarise(val = mean(val, na.rm = T), .groups = 'drop') %>% 
    select(station, date, source, var, uni, val, qual)
  
  epcout1 <- bind_rows(epcout1, epctmp)
  
}

# rwm, runs (routine monitoring)
ids <- fls[grep('^EPC\\_RWM\\_', fls$name), 'id'] %>% pull(id)
epcout2 <- NULL
for(id in ids){

  # sleep to not bonk api limit
  Sys.sleep(wait)
  flsht <- read_sheet(id)
  epctmp <- flsht %>% 
    select(
      station = `StationNumber`, 
      date = `SampleTime`, 
      secchi_m = SecchiDepth, 
      secchi_qual = Secchi_Q, 
      temp_c_top = `TempWater-T`,
      temp_c_mid = `TempWater-M`,
      temp_c_bot = `TempWater-B`,
      ph_none_top = `pH-T`, 
      ph_none_mid = `pH-M`, 
      ph_none_bot = `pH-B`, 
      sal_ppt_top = `Sal-T`,
      sal_ppt_mid = `Sal-M`,
      sal_ppt_bot = `Sal-B`,
      do_mgl_top = `DO-T`,
      do_mgl_mid = `DO-M`,
      do_mgl_bot = `DO-B`,
      dosat_per_top = `DOp-T`,
      dosat_per_mid = `DOp-M`,
      dosat_per_bot = `DOp-B`,
      nh34_mgl = `Ammonia`,
      nh34_qual = `AmmoniaQ`, 
      tkn_mgl = Kjeldahl_Nitrogen,
      tkn_qual = Kjeldahl_NitrogenQ,
      no23_mgl = Nitrates_Nitrites, 
      no23_qual = Nitrates_NitritesQ,
      tn_mgl = Total_Nitrogen,
      tn_qual = Total_NitrogenQ,
      tp_mgl = Total_Phosphorus, 
      tp_qual = Total_PhosphorusQ, 
      bod_mgl = BOD, 
      bod_qual = BODQ,
      orthop_mgl = Ortho_Phosphates, 
      orthop_qual = Ortho_PhosphatesQ, 
      chla_ugl = Chlorophylla_Corr,
      chla_qual = Chlorophylla_CorrQ,
      color_pcu = `Color(345)C`, 
      color_qual = `Color(345)CQ`,
      tss_mgl = Total_Suspended_Solids, 
      tss_qual = Total_Suspended_SolidsQ, 
      turb_ntu = Turbidity, 
      turb_qual = TurbidityQ
    ) %>% 
    rowwise() %>% 
    mutate(
      temp_c = mean(c(temp_c_top, temp_c_mid, temp_c_bot), na.rm = T),
      ph_none = mean(c(ph_none_top, ph_none_mid, ph_none_bot), na.rm = T), 
      sal_ppt = mean(c(sal_ppt_top, sal_ppt_mid, sal_ppt_bot), na.rm = T),
      do_mgl = mean(c(do_mgl_top, do_mgl_mid, do_mgl_bot), na.rm = T),
      dosat_per = mean(c(dosat_per_top, dosat_per_mid, dosat_per_bot), na.rm = T),
      dosat_per = mean(c(dosat_per_top, dosat_per_mid, dosat_per_bot), na.rm = T)
    ) %>% 
    ungroup() %>% 
    select(-matches('bot$|mid$|top$'))
      
  epctmpwq <- epctmp %>%
    select(-matches('\\_qual$')) %>%
    gather('var', 'val', -station, -date) %>%
    separate(var, c('var', 'uni'), sep = '_')
  
  epctmpqual <- epctmp %>%
    select(date, station, matches('\\_qual$')) %>%
    gather('var', 'qual', -date, -station) %>%
    mutate(var = gsub('\\_qual$', '', var)) 
  
  epctmp <- full_join(epctmpwq, epctmpqual, by = c('date', 'station', 'var')) %>% 
    mutate(
      date = date(date), 
      source = 'epchc', 
      station = as.character(station),
      station = gsub('^PP', '', station), 
      val = as.numeric(val)
    ) %>% 
    group_by(station, date, source, var, uni, qual) %>% 
    summarise(val = mean(val, na.rm = T), .groups = 'drop') %>% 
    select(station, date, source, var, uni, val, qual) %>% 
    filter(!is.na(val))
  
  epcout2 <- bind_rows(epcout2, epctmp)
  
}

# lab results
ids <- fls[grep('^EPC\\_PP\\_labresults|^EPC\\_PP\\_LabResults', fls$name), 'id'] %>% pull(id)
epcout3 <- NULL
for(id in ids){
  
  # sleep to not bonk api limit
  Sys.sleep(wait)
  
  flsht <- read_sheet(id)
  epctmp <- flsht %>% 
    select(
      station = `EPCStation`, 
      date = `Time Received`, 
      var = Parameter,
      val = Value, 
      qual = DEPQualifier
    ) %>%
    mutate(
      date = date(date), 
      source = 'epchc', 
      station = as.character(station),
      station = gsub('^PP', '', station),
      qual = case_when(
        qual == 'NULL' ~ NA_character_, 
        T ~ qual
      ),
      val = as.character(val),
      val = case_when(
        grepl('^NULL$', val) ~ NA_character_, 
        grepl('999999', val) ~ NA_character_, 
        T ~ val
      ),
      var = case_when(
        var == 'Ammonia' ~ 'nh34_mgl', 
        var == 'Ortho Phosphates' ~ 'orthop_mgl',
        var == 'Chlorophyll a' ~ 'chla_ugl', 
        var == 'Kjeldahl Nitrogen' ~ 'tkn_mgl', 
        var == 'Nitrates/Nitrites' ~ 'no23_mgl', 
        var == 'pH' ~ 'ph_none',
        var == 'Total Nitrogen' ~ 'tn_mgl', 
        var == 'Total Phosphorus' ~ 'tp_mgl', 
        var == 'Turbidity' ~ 'turb_ntu', 
        var == 'Color(345)F.45' ~ 'color_pcu'
      )
    ) %>% 
    filter(!is.na(var)) %>% 
    separate(var, c('var', 'uni'), remove = F) %>% 
    select(station, date, source, var, uni, val, qual) %>% 
    mutate(val = as.numeric(val)) %>% 
    filter(!is.na(val))

  epcout3 <- bind_rows(epcout3, epctmp)
  
}

# combine
epc1 <- bind_rows(epcout1, epcout2, epcout3)

## esa --------------------------------------------------------------------

ids <- fls[grep('ESA\\_PP', fls$name), 'id'] %>% pull(id)
out1 <- NULL
for(id in ids){
  
  Sys.sleep(wait)
  flsht <- read_sheet(id)
  out <- flsht %>% 
    select(
      station = `ESA Station`, 
      date = Date, 
      matches('^Temp-C$|^pH$|^Salin\\-PSU$|^Salin\\-PPT$|^DO\\%\\-Sat$|^DO\\-mg/L$')
    ) %>% 
    gather('var', 'val', -station, -date) %>% 
    mutate(
      var = case_when(
        var == 'Temp-C' ~ 'temp_c', 
        var == 'pH' ~ 'ph_none', 
        var %in%  c('Salin-PSU', 'Salin-PPT') ~ 'sal_ppt', 
        var == 'DO%-Sat' ~ 'dosat_per', 
        var == 'DO-mg/L' ~ 'do_mgl', 
        var == 'Chl ug/L-2' ~ 'chla_ugl'
      ), 
      date = as.Date(date), 
      qual = NA_character_, 
      source = 'esa'
    ) %>% 
    separate(var, c('var', 'uni'), sep = '_') %>% 
    group_by(source, station, date, var, uni, qual) %>% 
    summarise(val = mean(val, na.rm = T), .groups = 'drop') %>% 
    select(station, date, source, var, uni, val, qual) %>% 
    filter(!is.na(val))
  
  out1 <- bind_rows(out1, out)
  
}

esa1 <- out1

## usf ---------------------------------------------------------------------

ids <- fls[grep('USF\\_labresults', fls$name), 'id'] %>% pull(id)
out1 <- NULL
for(id in ids){
  
  Sys.sleep(wait)
  flsht <- read_sheet(id)
  out <- flsht %>% 
    clean_names %>% 
    select(
      station, 
      date,
      var = variable, 
      val = value, 
      uni = unit,
      qual = qualifier
    ) %>% 
    mutate(
      date = date(date), 
      val = as.character(val), 
      val = case_when(
        grepl('LOD', val) ~ as.character(gsub('^LOD=', '', qual)),
        grepl('nda', val) ~ NA_character_,
        T ~ as.character(val)
      ),
      val = as.numeric(val),
      # var = ifelse(var == 'tdn', 'tn', var), 
      source = 'usf'
    ) %>% 
    filter(var %in% parms$var) %>% 
    group_by(source, station, date, var, uni, qual) %>% 
    summarise(val = mean(val, na.rm = T), .groups = 'drop') %>% 
    select(station, date, source, var, uni, val, qual) %>% 
    filter(!is.na(val))
  
  out1 <- bind_rows(out1, out)
  
}

usf1 <- out1

## uf ----------------------------------------------------------------------

ids <- fls[grep('UF\\_Morrison', fls$name), 'id'] %>% pull(id)
out1 <- NULL
for(id in ids){
  
  Sys.sleep(wait)
  flsht <- read_sheet(id)
  outtmp <- flsht %>% 
    clean_names %>% 
    select(
      station = tbep_transect_id,
      uf_sample_id, 
      site_description, 
      source = sampling_partner,
      date,
      nh34_mgl = nh4n_mg_l, 
      nh34_qual = nh4n_flag, 
      no23_mgl = no3n_mg_l, 
      no23_qual = no3n_flag, 
      orthop_mgl = ortho_p_ug_l, 
      orthop_qual = ortho_p_flag, 
      tp_mgl = total_p_ug_l, 
      tp_qual = total_p_flag
    ) %>% 
    mutate(
      station = case_when(
        station == 'NA' ~ site_description, 
        T ~ station
      ), 
      station = gsub('^SPP', 'SPPC', station),
      source = tolower(source),
      date = date(date), 
      orthop_mgl = orthop_mgl / 1000, # ug/l to mg/l
      tp_mgl = tp_mgl / 1000 # ug/l to mg/l
    ) %>% 
    select(-uf_sample_id, -site_description) %>% 
    filter(!grepl('^SSJS|St\\.\\sJoseph\\sSound$', station)) # remove st. joseph sound stations
  
  outwq <- outtmp %>%
    select(-matches('\\_qual$')) %>%
    gather('var', 'val', -station, -source, -date) %>%
    separate(var, c('var', 'uni'), sep = '_') %>% 
    mutate(val = pmax(0, val))
  
  outqual <- outtmp %>%
    select(date, station, source, matches('\\_qual$')) %>%
    gather('var', 'qual', -date, -source, -station) %>%
    mutate(var = gsub('\\_qual$', '', var)) 
  
  out <- full_join(outwq, outqual, by = c('date', 'station', 'var', 'source')) %>% 
    group_by(source, station, date, var, uni, qual) %>% 
    summarise(val = mean(val, na.rm = T), .groups = 'drop') %>% 
    select(station, date, source, var, uni, val, qual) %>% 
    filter(!is.na(val))
  
  out1 <- bind_rows(out1, out)
  
}

uf1 <- out1

## city of st pete --------------------------------------------------------

ids <- fls[grep('COSP\\sOpen', fls$name), 'id'] %>% pull(id)
out1 <- NULL
for(id in ids){
  
  Sys.sleep(wait)
  flsht <- read_sheet(id)
  out <- flsht %>% 
    clean_names %>% 
    select(
      station = site, 
      date = sampled,
      var = analyte, 
      val = result, 
      uni = units,
      qual = dep_qualifier
    ) %>% 
    mutate(
      date = mdy_hms(date),
      date = date(date),
      val = as.numeric(val),
      var = case_when(
        var == 'Ammonia-N' ~ 'nh34', 
        var == 'Chlorophyll-a' ~ 'chla', 
        var == 'Dissolved Oxygen' ~ 'do', 
        var == 'Dissolved Oxygen Saturation' ~ 'dosat', 
        var == 'Nitrate+Nitrite-N' ~ 'no23', 
        var == 'Orthophosphate as P' ~ 'orthop', 
        var == 'pH' ~ 'ph', 
        var == 'Salinity' ~ 'sal', 
        var == 'Secchi Disk' ~ 'secchi', 
        var == 'Temperature' ~ 'temp', 
        var == 'Total Kjeldahl Nitrogen' ~ 'tkn', 
        var == 'Total Nitrogen' ~ 'tn', 
        var == 'Total Phosphorous' ~ 'tp', 
        var == 'TSS' ~ 'tss', 
        var == 'Turbidity' ~ 'turb'
      ),
      uni = case_when(
        uni %in% c('mg/L', 'mg/l') ~ 'mgl', 
        uni == 'meter' ~ 'm', 
        uni == 'mg/m3' ~ 'ugl', 
        uni == 'NTU' ~ 'ntu', 
        uni == 'PSU' ~ 'ppt', 
        uni == 'deg C' ~ 'c', 
        uni == 'SU' ~ 'none', 
        uni == '%' ~ 'per'
      ),
      source = 'cosp'
    ) %>% 
    filter(var %in% parms$var) %>% 
    group_by(source, station, date, var, uni, qual) %>% 
    summarise(val = mean(val, na.rm = T), .groups = 'drop') %>% 
    select(station, date, source, var, uni, val, qual) %>% 
    filter(!is.na(val))
  
  out1 <- bind_rows(out1, out)
  
}

cosp1 <- out1

## combine all ------------------------------------------------------------

rswqdat <- rswqdat %>%
  select(station, date, source, var, uni, val, qual) %>%
  filter(!source %in% c('epc'))
rswqdat <- bind_rows(epc1, rswqdat) %>%
# rswqdat <- bind_rows(fldep1, mpnrd1, pinco1, ncf1, epc1, esa1, usf1, uf1, cosp1) %>%
  ungroup %>% 
  unique %>%
  filter(!is.na(val)) %>%
  arrange(source, station, date, var)

## 
# clean up qual codes
# follows these: https://floridadep.gov/sites/default/files/62-160_help-document_0.pdf
# all secchi on bottom use S
rswqdat <- rswqdat %>% 
  mutate(
    qual = case_when(
      var == 'secchi' & grepl('>|Yes|VOB|L|S', qual) ~ 'S', 
      grepl('LOD', qual) ~ 'U', 
      qual %in% c('No', 'NULL') ~ NA_character_,
      T ~ qual
    ), 
    qual = gsub('\\,|\\s', '', qual)
  )

## 
# calc tn if tkn, no23 provided 

# find stations, dates that already have tn
# remove from calc below
tnexists <- rswqdat %>% 
  select(station, date, var) %>%
  unique %>% 
  filter(var %in% 'tn')

# calculate tn from tkn, no23
tncalc <- rswqdat %>% 
  select(station, date, source, var, val) %>% 
  filter(var %in% c('tkn', 'no23')) %>%
  filter(!(station %in% tnexists$station & date %in% tnexists$date)) %>%  # remove stations, dates that already have tn
  group_by(station, date, source, var) %>%
  summarise(val = mean(val, na.rm = T), .groups = 'drop') %>% # some stations, dates have repeat samples
  spread(var, val) %>% 
  mutate(tn = no23 + tkn) %>% 
  gather('var', 'val', -station, -date, -source) %>% 
  filter(var %in% 'tn') %>% 
  filter(!is.na(val)) %>% 
  mutate(
    uni = 'mgl',
    qual = NA_character_
  )

# join tn calc to complete data
rswqdat <- rswqdat %>% 
  bind_rows(tncalc) %>% 
  arrange(source, date, var)

## 
# estimate in range/out of range from bswqrngs

# sf object of bswq stations
bswqloc <- bswqrngs %>% 
  select(bswqstation, mo, var, lat, lng) %>% 
  unique() %>% 
  as.data.frame(stringsAsFactors = F) %>% 
  st_as_sf(coords = c('lng', 'lat'), crs = prj)

# nearest longterm stations to rswq stations
rswqnear <- rswqdat %>%
  mutate(mo = month(date)) %>% 
  select(station, source, mo, var) %>% 
  unique %>% 
  inner_join(rsstatloc, ., by = c('station', 'source')) %>% 
  select(station, source, mo, var) %>% 
  unique() %>% 
  group_by(station, source, mo, var) %>% 
  nest() %>% 
  mutate(
    bswqstation = purrr::pmap(list(mo, var, data), function(mo, var, data){
      
      # subset ref stations by var
      varsel <- var
      mosel <- mo
      bswqlocsub <- bswqloc %>% dplyr::filter(var %in% varsel & mo %in% mosel)
      
      # find nearest
      out <- data %>% 
        mutate(
          bswqstation = st_nearest_feature(data, bswqlocsub), 
          bswqstation = bswqlocsub$bswqstation[bswqstation]
        )
      
      return(out)
      
    })
  ) %>% 
  select(-data) %>% 
  unnest('bswqstation') %>% 
  select(-geometry) %>% 
  ungroup

# add column if in/out of range based on closest epc station
rswqdat <- rswqdat %>% 
  mutate(mo = month(date)) %>% 
  left_join(., rswqnear, by = c('station', 'var', 'mo', 'source')) %>% 
  left_join(., bswqrngs, by = c('bswqstation', 'var', 'mo', 'uni')) %>% 
  rename(source = source.x) %>% 
  select(-source.y) %>% 
  rowwise() %>% 
  mutate(
    inrng = case_when(
      val < minv ~ 'below', 
      val > maxv ~ 'above', 
      T ~ 'in range'
    ), 
    val = round(val, sigdig), 
    minv = round(minv, sigdig), 
    maxv = round(maxv, sigdig)
  ) %>% 
  unite(nrmrng, c('minv', 'maxv'), sep = '-') %>% 
  ungroup %>% 
  select(-mo, -avev, -stdv, -sigdig, -lat, -lng)

save(rswqdat, file = 'data/rswqdat.RData', version = 2)

# for log
tms <- Sys.time()
attr(tms, 'tzone') <- 'America/New_York'
tms <- paste(as.character(tms), 'Eastern')

writeLines(tms, 'logs/rswqlog.txt')

# all points - update anytime location data are changed -------------------

data(rsstatloc)
data(rstrnpts)
data(rscntdat)
data(rsphypts)
data(rsphydat)
data(rstrndat)
data(rsbntpts)

# response sampling locations
rsallpts <- list(
  `water quality` = rsstatloc %>%
    select(station, source), 
  `seagrass and macroalgae` = rstrnpts %>% 
    filter(station %in% rstrndat$station) %>% 
    select(station, source), 
  `contaminants` = rscntdat %>%  
    inner_join(rsstatloc, ., by = c('station', 'source')) %>% 
    select(station, source) %>% 
    unique,
  `algae` = rsphypts %>% 
    left_join(rsphydat, by = c('station', 'source')) %>% 
    select(station, source) %>%
    na.omit() %>% 
    unique,
  `benthic` = rsbntpts %>% 
    select(station, source) %>% 
    unique
  ) %>% 
  enframe('type', 'value') %>% 
  unnest('value') %>%
  st_as_sf() %>% 
  mutate(
    lng = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>% 
  st_set_geometry(NULL) %>% 
  group_by(station, source) %>% 
  arrange(type) %>% 
  nest() %>% 
  mutate(
    data = purrr::map(data, function(x){
      
      type <- paste(x$type, collapse = ', ')
      
      lng <- mean(x$lng)
      lat <- mean(x$lat)
      
      out <- tibble(
        type = type, 
        lng = lng, 
        lat = lat
      )
      
      return(out)
      
    })
  ) %>% 
  unnest(data) %>% 
  arrange(station, source) %>% 
  ungroup %>% 
  mutate(
    source = tolower(source),
    lng = ifelse(duplicated(lng), jitter(lng, factor = 10), lng),
    lat = ifelse(duplicated(lat), jitter(lat, factor = 10), lat), 
    source_lng = case_when(
      source == 'pinco' ~ 'Pinellas Co.', 
      source == 'epchc' ~ 'Hillsborough Co.', 
      source == 'fldep' ~ 'Florida DEP', 
      source == 'mpnrd' ~ 'Manatee Co.', 
      source == 'ncf' ~ 'New College Fl.', 
      source == 'tbep' ~ 'TBEP', 
      source == 'usf' ~ 'USF', 
      source == 'uf' ~ 'UF',
      source == 'esa' ~ 'ESA', 
      source == 'sbep' ~ 'SBEP', 
      source == 'cosp' ~ 'City of St. Pete'
    ), 
    source_lng = case_when(
      source %in% c('fwri', 'fldep') & type == 'algae' ~ 'FWC-FWRI', 
      T ~ source_lng
    )
  ) %>%
  select(-source) %>% 
  st_as_sf(coords = c('lng', 'lat'), crs = 4326)

save(rsallpts, file = 'data/rsallpts.RData', version = 2)
