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

# tb high res boundary ----------------------------------------------------

# https://drive.google.com/file/d/1EjIDcpNbBHvoJ09-wlEJDdDE7v323qEe/view?usp=sharing

tmp1 <- tempfile(fileext = '.zip')
tmp2 <- tempdir()

drive_download(as_id("1EjIDcpNbBHvoJ09-wlEJDdDE7v323qEe"), path = tmp1, overwrite = TRUE)

unzip(temp, exdir = tmp2) 

bnd <- list.files(tmp2, full.names = T) %>% 
  grep('\\.shp$', ., value = T) %>% 
  st_read %>%
  st_transform(crs = 4326) %>% 
  st_intersection(tbshed) %>% 
  st_union()

segmask <- bnd %>% 
  st_bbox %>% 
  st_as_sfc() %>% 
  st_as_sf() %>% 
  st_difference(bnd)

file.remove(tmp1)
file.remove(list.files(tmp2, full.names = T))

save(segmask, file = 'data/segmask.RData', version = 2)

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
    tn = total_nitrogen_mg_l, 
    nh34 = ammonia_mg_l,
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
      var %in% c('tp', 'tn', 'nh34') ~ 'mg/l', 
      var %in% 'chla' ~ 'ug/l'
    )
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
      var == 'ChlaC_ugl' ~ 'Chla_ugl', 
      T ~ var
    )
  ) %>% 
  filter(var %in% c('Chla_ugl', 'TN_ugl', 'NH3_N_ugl', 'TP_ugl', 'pH', 'Salinity_ppt')) %>% 
  mutate(
    station = gsub('\\=', '', station), 
    date = as.Date(mdy_hms(date)), 
    yr = year(date), 
    mo = month(date), 
    station = as.character(station),
    source = 'manco', 
    var = case_when(
      var == 'Chla_ugl' ~ 'chla', 
      var == 'TN_ugl' ~ 'tn', 
      var == 'NH3_N_ugl' ~ 'nh34',
      var == 'TP_ugl' ~ 'tp', 
      var == 'pH' ~ 'ph', 
      var == 'Salinity_ppt' ~ 'sal'
    ), 
    val = case_when(
      var %in% c('tn', 'tp', 'nh34') ~ val / 1000, 
      T ~ val
    ), 
    uni = case_when(
      var %in% c('tn', 'tp') ~ 'mg/l', 
      T ~ uni
    ), 
    uni = tolower(uni)
  ) %>% 
  tibble %>% 
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
  filter(date < as.Date('2021-05-01')) %>%  # REMOVE ME!!!!!
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
    sav_species = factor(sav_species, levels = savlevs),
    macroalgae_species= ifelse(macroalgae_species == 'NA', NA, macroalgae_species),
    epibiota_density = factor(epibiota_density, levels = epilevs), 
    station = factor(station), 
    location = factor(location)
  )

rstrndatsav <- rstrndat %>% 
  select(date, station, location, sav_species, sav_abundance, sav_bb, epibiota_density) %>% 
  group_by(date, station, location) %>% 
  tidyr::complete(
    sav_species,  
    fill = list(sav_bb = 0)
  ) %>% 
  ungroup() %>% 
  mutate(
    station = as.character(station), 
    location = as.numeric(as.character(location))
  )

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
  separate(macroalgae_group, maxcols, sep = ', ') %>% 
  gather('var', 'macroalgae_group', !!maxcols) %>% 
  mutate(
    macroalgae_group = factor(macroalgae_group, levels = grplevs)
  ) %>% 
  group_by(station, date, location) %>% 
  tidyr::complete(
    macroalgae_group,
    fill = list(macroalgae_bb = 0)
  ) %>%
  ungroup() %>% 
  mutate(
    station = as.character(station), 
    location = as.numeric(as.character(location))
  ) %>% 
  filter(!is.na(macroalgae_group)) %>%
  select(date, station, location, macroalgae_group, macroalgae_abundance, macroalgae_bb) 

save(rstrndatsav, file = 'data/rstrndatsav.RData', version = 2)
save(rstrndatmcr, file = 'data/rstrndatmcr.RData', version = 2)

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
# https://docs.google.com/spreadsheets/d/19FWq7gjRfLj_aFIdFkQGXfZZNn-mlEDPvm5HUleGgb8/edit#gid=1628126622

rawdat <- read_sheet('19FWq7gjRfLj_aFIdFkQGXfZZNn-mlEDPvm5HUleGgb8')

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
    station = gsub('\\sPoint\\s', ' ', station)
  ) %>% 
  filter(!grepl('Blank', station)) %>% 
  filter(!var %in% c('Ammonia-N', 'Chlorophyll-a, Corrected', 'Chlorophyll-a, Uncorrected', 'Dominant sample taxon', 'Kjeldahl Nitrogen', 'NO2NO3-N', 'O-Phosphate-P', 'Total-P', 'TSS', 'Turbidity')) %>% 
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
    date = as.Date(date), 
    source = 'usf', 
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
id <- fls[grep('^EPC', fls$name), 'id'] %>% pull(id)
flphy <- read_sheet(id)
rsphydatepc <- flphy %>%
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

## 
# combine all

rsphydat <- bind_rows(rsphydatfldep, rsphydatfwri, rsphydatpinco, rsphydatepc) %>% 
  filter(!valqual == 'Not present/Background' | is.na(valqual)) %>% 
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
    lat = st_coordinates(.)[,2]
  ) %>% 
  select(
    station, lng, lat, typ, col
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

# these are stations actively being monitoring by four agencies

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
rsstatloc <- bind_rows(epchcsta1, fldepsta1, mpnrdsta1, pincosta1, ncfsta1, usfsta1, tbepsta1, esastat1) %>% 
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
      source == 'esa' ~ 'ESA'
    )
  ) %>% 
  select(source_lng, source, station, lat, lon, comment) %>% 
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
  select(station) %>% 
  group_by(station) %>% 
  nest() %>% 
  mutate(
    lns = purrr::map(data, function(x){
      
      out <- st_nearest_points(x, bswqloc) 
      len <- st_length(out) %>% 
        which.min
      out <- out[len]
      
      return(out)
      
    })
  ) %>% 
  select(-data) %>%
  unnest('lns') %>% 
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

data(bswqrngs)
data(parms)
data(rsstatloc)

gdrive_pth <- 'https://drive.google.com/drive/folders/1SWlBbZtjZ8SF43MCz5nv5YLCWDvn7T_x'

# sleep time in seconds
wait <- 10

# csv files must be opened/saved as spreadsheet in google sheets
fls <- drive_ls(gdrive_pth, type = 'spreadsheet')

## fldep ------------------------------------------------------------------

fl <- fls[which(fls$name == 'FLDEP_20210512'), 'id'] %>% pull(id)
flsht <- read_sheet(fl)
fldep1 <- flsht %>% 
  clean_names %>% 
  select(
    station = station_id, 
    date = sample_date, 
    secchi_m = secchi_depth_m, 
    temp_c = water_temperature_c_surface, 
    sal_ppt = salinity_0_00_surface, 
    dosat_per = d_o_percent_sat_surface, 
    ph_none = p_h_surface, 
    nh34_mgl = ammonia_n_mg_n_l, 
    orthop_mgl = orthophosphate_p_mg_p_l, 
    chla_ugl = chlorophyll_a_corrected_mg_l, # this is ugl, janitor think mu is m
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
    source = 'fldep'
  ) %>% 
  select(station, date, source, var, uni, val, qual) %>% 
  filter(!is.na(val))

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
  Sys.sleep(20)   
  
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

# lab results
ids <- fls[grep('^PINCO_labresults', fls$name), 'id'] %>% pull(id)
out1 <- NULL
for(id in ids) {

  # sleep to not bonk api limit
  Sys.sleep(wait)

  dat <- read_sheet(id)

  tmp <- dat %>%
    clean_names() %>%
    select(
      station = collection_site,
      date = collect_date,
      var = analyte_name,
      val = formatted_result,
      uni = result_units,
      qual = qualifiers
    ) %>%
    mutate(
      station = case_when(
        station == 'PMOutfall' ~ 'PM Out',
        station == 'TBEP-MCBHO2' ~ 'TBEP-MCBH02',
        station %in% c('BH0', 'BH15', 'BH25') ~ paste0('TBEP-', station),
        station == 'TBEP0' ~ 'TBEP-BH0',
        station == 'TBEP15' ~ 'TBEP-BH15',
        station == 'TBEP25' ~ 'TBEP-BH25',
        station == 'Clamber 4' ~ 'CB4',
        station == 'Clamber 6' ~ 'CB6',
        station == 'E7-2A' ~ 'E7-A',
        station == 'E7-2B' ~ 'E7-B',
        station == 'E7-2C' ~ 'E7-C',
        station == 'E7-2D' ~ 'E7-D',
        grepl('\\sS$', station) ~ paste0('MC', station),
        T ~ station
      ),
      station = gsub('\\sS$', '', station),
      station = gsub('^PC\\s', 'PC', station),
      station = gsub('^MC\\s', 'MC', station),
      station = gsub('^Skyway\\s', 'Skyway', station),
      var = case_when(
        var == 'Ammonia' ~ 'nh34',
        var == 'Nitrate+Nitrite (N)' ~ 'no23',
        var == 'Orthophosphate as P(Dissolved)' ~ 'orthop',
        var == 'Total Phosphorus' ~ 'tp',
        var == 'Total Suspended Solids' ~ 'tss',
        var == 'Turbidity' ~ 'turb',
        var == 'Chlorophyll a (Corrected)' ~ 'chla',
        var == 'Total Kjeldahl Nitrogen' ~ 'tkn',
        T ~ var
      ),
      val = as.numeric(gsub('[^0-9.-]', '', val)),
      uni = case_when(
        uni == 'mg/L' ~ 'mgl',
        uni == 'NTU' ~ 'ntu',
        uni == 'mg/m3' ~ 'ugl',
        T ~ uni
      ),
      source = 'pinco',
      source = case_when(
        station %in% c("TBEP-BH0", "TBEP-BH15", "TBEP-BH25", "TBEP-MCBH02") ~ 'tbep',
        T ~ source
      ),
      date = as.Date(date)
    ) %>%
    filter(!station %in% c('MC- FB', 'PP-FB')) %>% # these are validation samples, no data
    filter(var %in% parms$var)

  if(nrow(tmp) == 0)
    next()
  
  out1 <- bind_rows(out1, tmp)

}

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
      T ~ station
    ),
    station = gsub('^Skyway\\s', 'Skyway', station),
    source = case_when(
      station %in% c("TBEP-BH0", "TBEP-BH15", "TBEP-BH25", "TBEP-MCBH02") ~ 'tbep', 
      T ~ source
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
pinco1 <- bind_rows(out1, out2) %>% 
  group_by(station, date, source, var, uni, qual) %>% 
  summarise(
    val = mean(val, na.rm = T), 
    .groups = 'drop'
  ) %>% 
  select(station, date, source, var, uni, val, qual)

## ncf --------------------------------------------------------------------

# sleep to not bonk api limit
Sys.sleep(wait)

fl <- fls[which(fls$name == 'NCF_waterquality_Apr2021'), 'id'] %>% pull(id)
flsht <- read_sheet(fl)
ncf1 <- flsht %>% 
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
ids <- fls[grep('^EPC\\_PP\\_labresults', fls$name), 'id'] %>% pull(id)
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

## combine all ------------------------------------------------------------

rswqdat <- bind_rows(fldep1, mpnrd1, pinco1, ncf1, epc1, esa1, usf1) %>%
  ungroup %>% 
  unique %>%
  filter(!is.na(val)) %>%
  arrange(source, station, date, var)

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
  select(bswqstation, var, lat, lng) %>% 
  unique() %>% 
  as.data.frame(stringsAsFactors = F) %>% 
  st_as_sf(coords = c('lng', 'lat'), crs = prj)

# nearest epc stations to rswq stations
rswqnear <- rswqdat %>% 
  select(station, var) %>% 
  unique %>% 
  inner_join(rsstatloc, ., by = 'station') %>% 
  select(station, var) %>% 
  group_by(var) %>% 
  nest() %>% 
  mutate(
    bswqstation = purrr::pmap(list(var, data), function(var, data){
      
      # subset ref stations by var
      varsel <- var
      bswqlocsub <- bswqloc %>% dplyr::filter(var %in% varsel)
      
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
  ungroup %>% 
  unique

# add column if in/out of range based on closest epc station
rswqdat <- rswqdat %>% 
  left_join(., rswqnear, by = c('station', 'var')) %>% 
  left_join(., bswqrngs, by = c('bswqstation', 'var', 'uni')) %>% 
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
  select(-avev, -stdv, -sigdig, -lbs, -lat, -lng)

save(rswqdat, file = 'data/rswqdat.RData', version = 2)

# for log
tms <- Sys.time()
attr(tms, 'tzone') <- 'America/New_York'
tms <- paste(as.character(tms), 'Eastern')

writeLines(tms, 'logs/rswqlog.txt')



