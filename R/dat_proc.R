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

prj <- 4326

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

# get March, April ranges from 2006 to present

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

# get normal epc ranges
bswqrngsepchc <- epcraw %>% 
  clean_names %>% 
  filter(station_number %in% stations$epchc_station) %>% # from tbeptools
  select(
    station = station_number,
    date = sample_time,
    lat = latitude, 
    lng = longitude, 
    color_pcu = color_345_c_pcu, 
    tn_mgl = total_nitrogen_mg_l, 
    nh34_mgl = ammonia_mg_l,
    no23_mgl = nitrates_nitrites_mg_l,
    tkn_mgl = kjeldahl_nitrogen_mg_l,
    orthop_mgl = ortho_phosphates_mg_l,
    tp_mgl = total_phosphorus_mg_l, 
    bod_mgl = bod_mg_l,
    chla_ugl = chlorophyll_a_uncorr_ug_l, 
    tss_mgl = total_suspended_solids_mg_l, 
    turb_ntu = turbidity_jtu_ntu,
    secchi_m = secchi_depth_m,
    p_h_bottom, 
    p_h_mid, 
    p_h_top,
    do_bottom_mg_l, 
    do_mid_mg_l,
    do_top_mg_l, 
    do_sat_bottom_percent, 
    do_sat_mid_percent, 
    do_sat_top_percent, 
    sal_bottom_ppth,
    sal_mid_ppth,
    sal_top_ppth, 
    temp_water_bottom_deg_c,
    temp_water_mid_deg_c,
    temp_water_top_deg_c
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
    sal_ppt = mean(c(sal_bottom_ppth, sal_mid_ppth, sal_top_ppth), na.rm = T)
  ) %>% 
  ungroup() %>% 
  select(-matches('bottom|mid|top')) %>% 
  gather('var', 'val', -station, -date, -lat, -lng, -yr, -mo) %>% 
  separate(var, c('var', 'uni'), sep = '_') %>% 
  mutate(
    val = as.numeric(val)
  ) %>% 
  filter(yr > 2005 & mo %in% c(3, 4)) %>% 
  group_by(station, var, uni) %>% 
  summarise(
    lat = mean(lat, na.rm = T), 
    lng = mean(lng, na.rm = T),
    avev = mean(val, na.rm = T), 
    stdv = sd(val, na.rm = T), 
    .groups = 'drop'
  ) %>%
  left_join(parms, by = c('var', 'uni')) %>% 
  mutate(
    avev = round(avev, sigdig), 
    stdv = round(stdv, sigdig), 
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
    uni = result_unit
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
  filter(yr > 2005 & mo %in% c(3, 4)) %>% 
  group_by(station, var, uni) %>% 
  summarise(
    lat = mean(lat, na.rm = T), 
    lng = mean(lng, na.rm = T),
    avev = mean(val, na.rm = T), 
    stdv = sd(val, na.rm = T), 
    .groups = 'drop'
  ) %>%
  left_join(parms, by = c('var', 'uni')) %>% 
  mutate(
    avev = round(avev, sigdig), 
    stdv = round(stdv, sigdig), 
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

epcraw <- epcraw %>% 
  clean_names %>% 
  filter(station_number %in% c(14, 16, 19, 21, 22, 23, 24, 25, 28, 81, 82, 84, 9, 90, 91, 92, 93, 95)) %>% 
  select(
    station = station_number,
    date = sample_time,
    latitude, 
    longitude, 
    color_pcu = color_345_c_pcu, 
    tn_mgl = total_nitrogen_mg_l, 
    nh34_mgl = ammonia_mg_l,
    no23_mgl = nitrates_nitrites_mg_l,
    tkn_mgl = kjeldahl_nitrogen_mg_l,
    orthop_mgl = ortho_phosphates_mg_l,
    tp_mgl = total_phosphorus_mg_l, 
    bod_mgl = bod_mg_l,
    chla_ugl = chlorophyll_a_uncorr_ug_l, 
    tss_mgl = total_suspended_solids_mg_l, 
    turb_ntu = turbidity_jtu_ntu,
    secchi_m = secchi_depth_m,
    p_h_bottom, 
    p_h_mid, 
    p_h_top,
    do_bottom_mg_l, 
    do_mid_mg_l,
    do_top_mg_l, 
    do_sat_bottom_percent, 
    do_sat_mid_percent, 
    do_sat_top_percent, 
    sal_bottom_ppth,
    sal_mid_ppth,
    sal_top_ppth, 
    temp_water_bottom_deg_c,
    temp_water_mid_deg_c,
    temp_water_top_deg_c
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
    sal_ppt = mean(c(sal_bottom_ppth, sal_mid_ppth, sal_top_ppth), na.rm = T)
  ) %>% 
  ungroup() %>% 
  select(-matches('bottom|mid|top')) %>% 
  gather('var', 'val', -station, -date, -yr, -mo, -latitude, -longitude) %>% 
  separate(var, c('var', 'uni'), sep = '_') %>% 
  mutate(
    val = as.numeric(val), 
    uni = case_when(
      var == 'sal' ~ 'ppt', 
      var == 'ph' ~ 'none', 
      var %in% c('tp', 'tn', 'nh34') ~ 'mg/l', 
      var %in% 'chla' ~ 'ug/l'
    ), 
    source = 'epchc'
  ) 

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
    uni = result_unit
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
    )
  ) %>% 
  filter(!grepl('^BH', station)) # these are bishop harbor stations with incomplete data

bswqdat <- bind_rows(epcraw, manraw) %>% 
  arrange(station, date) %>% 
  filter(yr > 1995) %>% 
  mutate(date = floor_date(date, unit = 'month')) %>% 
  group_by(station, date, mo, yr, source, var, uni) %>% 
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
    date = as.Date(date)
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
fl <- fls[which(fls$name == 'SBEP_TBEP_Rapid_Macroalgae_Weight_Data'), 'id'] %>% pull(id)
flsht <- read_sheet(fl)

macrodat <- flsht %>% 
  mutate(
    date = as.Date(date),
    genus = case_when(
      genus == 'ACAN' ~ 'Acan', 
      genus == 'CODIUM' ~ 'Codium', 
      genus == 'GRAC' ~ 'Grad', 
      genus == 'GRAC/ACAN' ~ 'Grac/Acan', 
      genus == 'GRAC/EUCH' ~ 'Grac/Euch', 
      genus == 'GRAC/HALY' ~ 'Grac/Haly', 
      genus == 'HYP/GRAC' ~ 'Hyp/Grac', 
      genus == 'LAUR' ~ 'Laur', 
      genus == 'MIXED RED' ~ 'Mixed Red', 
      genus == 'ULVA' ~ 'Ulva', 
      genus == 'UNKOWN' ~ 'Unknown',
      T ~ genus
    )
  )

save(macrodat, file = 'data/macrodat.RData', version = 2)

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
    species = `Algal ID`, 
    microcyst_ugl = `Total Microcystin Toxin (micrograms/L)`, 
    othertox_ugl = `Other Toxin (micrograms/L)`
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
    species = `Algal ID`, 
    microcyst_ugl = `Total Microcystin Toxin (micrograms/L)`, 
    othertox_ugl = `Other Toxin (micrograms/L)`
  ) %>% 
  mutate(
    date = unlist(date),
    date = as.Date(date), 
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

id <- fls[grep('^EPC_PP', fls$name), 'id'] %>% pull(id)
flphy <- read_sheet(id)
rsphydatepc2 <- flphy %>%
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

# there is some overlap between the two files
rsphydatepc <- bind_rows(rsphydatepc1, rsphydatepc2) %>% 
  arrange(date, station) %>% 
  unique

## 
# combine all

rsphydat <- bind_rows(rsphydatfldep, rsphydatfwri, rsphydatpinco, rsphydatepc) %>% 
  filter(!valqual == 'Not present/Background' | is.na(valqual)) %>% 
  mutate(
    valqual = factor(valqual, levels = c('Very low', 'Low', 'Medium', 'High'))
  ) 
  
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
    species = gsub('^Pseudo\\-nitzschia$|^Pseudo\\-nitzschia sp$|^Pseudo\\-nitzschia spp\\.$', 'Pseudo-nitzschia sp', species), 
    species = gsub('^Chaetoceros$', 'Chaetoceros sp', species),
    species = gsub('^Skeletonema$', 'Skeletonema spp', species),
    species = gsub('^Thalassiosira$', 'Thalassiosira sp', species),
    species = gsub('; no dominant species in sample', '', species),
    species = gsub('\\.$', '', species), 
    species = gsub('sp$', 'sp.', species), 
    species = gsub('spp$', 'spp.', species),
    species = gsub('Karnia', 'Karenia', species),
    species = gsub('\\spresent', '', species), 
    species = gsub('^Prorocentrum$|^Prorocentrum\\ssp\\.$', 'Prorocentrum sp.', species),
    species = gsub('^Nitzchia$|^Nitzschia\\ssp\\.$', 'Nitzschia sp.', species),
    species = gsub('^Rhizosolenia$|^Rhizosolenia\\sspp\\.$', 'Rhizosolenia sp.', species),
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
    species = gsub('dominant\\sspecies:\\s', '', species)
  ) %>% 
  filter(species != 'chains')

# break out karenia samples in separate rows
tmp <- rsphydat %>% 
  mutate(
    species = case_when(
      !grepl(';', species)  ~ paste0(species, '; nothing'), 
      T ~ species
    )
  ) %>% 
  separate(species, into = c('species1', 'species2'), sep = '; ')
  
tmp1 <- tmp %>% 
  select(-species2) %>% 
  rename(species = species1)

tmp2 <- tmp %>% 
  filter(species2 != 'nothing') %>% 
  select(-species1) %>%
  rename(species = species2)

rsphydat <- bind_rows(tmp1, tmp2) %>% 
  arrange(date, station, species)
  
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

# benthic sampling stations -----------------------------------------------

# https://docs.google.com/spreadsheets/d/1eJ64uMX7WOrt2XrjxKV67W5Y_EiXDh_9gBFpaBXuQWo/edit#gid=0
flbnt <- read_sheet('1eJ64uMX7WOrt2XrjxKV67W5Y_EiXDh_9gBFpaBXuQWo')

rsbntpts <- flbnt %>% 
  mutate(
    longitude = lng, 
    latitude = lat
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

# nearest epc stations to rswq stations
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
        st_cast('LINESTRING')
      len <- st_length(out) %>% 
        which.min
      out <- out[len]

      return(out)
      
    })
  ) %>% 
  select(-data) %>%
  unnest('lns') %>%
  ungroup() %>% 
  st_as_sf() %>% 
  st_geometry()

bswqloc <- bswqloc %>%
  filter(bswqstation %in% as.character(rswqnear$bswqstation)) %>% 
  mutate(type = 'Long-term')

wqrefmap <- mapview(rswqlns, color = 'grey', homebutton = F, layer.name = 'Distance to closest') +
  mapview(rsstatloc, col.regions = 'lightblue', alpha.regions = 1, lwd = 0.5, cex = 4, label = paste0('Current station ', rsstatloc$station), layer.name = 'Current stations', homebutton = F) +
  mapview(bswqloc, col.regions = 'tomato1', alpha.regions = 1, lwd = 0.5, cex = 4, label = paste0('Reference station ', bswqloc$bswqstation), layer.name = 'Reference stations', homebutton = F)

save(wqrefmap, file = 'data/wqrefmap.RData', version = 2)

# coordinated response data -----------------------------------------------
+
# all points - update anytime location data are changed -------------------

data(rsstatloc)
data(rstrnpts)
data(rscntdat)
data(rsphypts)
data(rsphydat)
data(rstrndat)

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
