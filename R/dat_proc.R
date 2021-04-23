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

# deauth all so it can build with gh actions
drive_deauth()
gs4_deauth()

# parameter data dict -----------------------------------------------------

# parameter names, units
parms <- tibble(
  var = c('bod', 'chla', 'color', 'do', 'dosat', 'nh34', 'no23', 'orthop', 'ph', 'sal', 'secchi', 'temp', 'tkn', 'tn', 'tp', 'tss', 'turb'),
  uni = c('mgl', 'ugl', 'pcu', 'mgl', 'per', 'mgl', 'mgl', 'mgl', 'none', 'ppt', 'm', 'c', 'mgl', 'mgl', 'mgl', 'mgl', 'ntu'), 
  lbs = c('BOD (mg/L)', 'Chl-a (ug/L)', 'Color (PCU)', 'DO (mg/L)', 'DO (% sat.)', 'NH3, NH4+ (mg/L)', 'Nitrate/Nitrite (mg/L)', 'Ortho-P (mg/L)', 'pH', 'Sal (ppt)', 'Secchi (m)', 'Temp (C)', 'TKN (mg/L)', 'TN (mg/L)', 'TP (mg/L)', 'TSS (mg/l)', 'Turb (NTU)')
)

save(parms, file = 'data/parms.RData', version = 2)

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

bswqrngs <- epcraw %>% 
  clean_names %>% 
  filter(station_number %in% c(21, 22, 90)) %>% 
  select(
    station = station_number,
    date = sample_time,
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
  gather('var', 'val', -station, -date, -yr, -mo) %>% 
  separate(var, c('var', 'uni'), sep = '_') %>% 
  mutate(
    val = as.numeric(val)
  ) %>% 
  filter(yr > 2005 & mo %in% c(3, 4)) %>% 
  group_by(var, uni) %>% 
  summarise(
    avev = mean(val, na.rm = T), 
    stdv = sd(val, na.rm = T), 
    .groups = 'drop'
  ) %>% 
  mutate(
    sigdig = c(2, 2, 2, 2, 1, 4, 5, 4, 1, 1, 1, 1, 3, 3, 3, 2, 2),
    avev = round(avev, sigdig), 
    stdv = round(stdv, sigdig), 
    minv = avev - stdv, 
    minv = pmax(0, minv),
    maxv = avev + stdv
  ) %>% 
  left_join(parms, by = c('var', 'uni')) %>% 
  mutate(
    lbunis = gsub('^.*\\s(\\(.*\\))$', '\\1', lbs), 
    lbunis = gsub('pH', '', lbunis)
    )

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
# raw data from https://tampabay.wateratlas.usf.edu/datadownload/Default.aspx
# search by surface water quality, by site Info (data source/provider), then STORET_21FLMANA (legacy) and WIN_21FLMANATEE (new)
# then select stations 336, 357, 361, 362
# selected stations close to Piney Point from map (have to select 'hydrology and samples' and then 'sampling location' form hamburger layer selection)
mandat <- read.table('data/raw/DataDownload_2339909_row.txt', sep = '\t', header = T)

epcraw <- epcraw %>% 
  clean_names %>% 
  filter(station_number %in% c(21, 22, 90)) %>% 
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
  tibble

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

##
# locations
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

##
# data

savlevs <- c('Thalassia testudinum', 'Halodule wrightii', 'Syringodium filiforme', 'Ruppia maritima', 'Halophila engelmannii', 'Halophila decipiens')
mcrlevs <- c('Gracilaria', 'Hypnea', 'Acanthophora', 'Caulerpa', 'Eucheuma', 'Halymenia', 'Ulva', 'Enteromorpha', 'Cladophora', 'Chaetomorpha', 'Codium', 'Unknown', 'Mixed Drift Reds')
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
    epibiota_density = factor(epibiota_density, levels = epilevs), 
    station = factor(station), 
    location = factor(location)
  )

rstrndatsav <- rstrndat %>% 
  select(date, station, location, sav_species, sav_abundance, sav_bb, epibiota_density) %>% 
  tidyr::complete(
    sav_species, 
    tidyr::nesting(date, station, location), 
    fill = list(sav_bb = 0)
  ) %>% 
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

rstrndatmcr <- rstrndat %>% 
  select(date, station, location, macroalgae_species, macroalgae_abundance, macroalgae_bb) %>% 
  separate(macroalgae_species, maxcols, sep = ', ') %>% 
  gather('var', 'macroalgae_species', !!maxcols) %>% 
  filter(!is.na(macroalgae_species)) %>% 
  mutate(
    macroalgae_species = factor(macroalgae_species, levels = mcrlevs)
  ) %>% 
  tidyr::complete(
    macroalgae_species, 
    tidyr::nesting(tidyr::expand(., date, station, location)), 
    fill = list(macroalgae_bb = 0)
  ) %>% 
  mutate(
    station = as.character(station), 
    location = as.numeric(as.character(location))
  ) %>% 
  select(date, station, location, macroalgae_species, macroalgae_abundance, macroalgae_bb) 

save(rstrndatsav, file = 'data/rstrndatsav.RData', version = 2)
save(rstrndatmcr, file = 'data/rstrndatmcr.RData', version = 2)

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

##
# from ftp
fl <- 'http://publicfiles.dep.state.fl.us/DEAR/DEARweb/_PP_EventResponse/Latest_Analytical_Results_Final.xlsx'

tmpfl <- tempfile()
download.file(fl, tmpfl, mode = 'wb')

rawdat <- read_excel(tmpfl)

file.remove(tmpfl)

# from gdrive
# https://docs.google.com/spreadsheets/d/1CzyJEPAmvUntUFAbTOgjnyOglHHrS03iV1tDk_NbXBE/edit#gid=749260864

rawdat <- read_sheet('1CzyJEPAmvUntUFAbTOgjnyOglHHrS03iV1tDk_NbXBE')

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

save(rscntdat, file = 'data/rscntdat.RData', version = 2)

# phytoplankton data ------------------------------------------------------

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

# fwri - usf-rains
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
    source = 'usf-rains', 
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

# phytoplankon sampling stations ------------------------------------------

data(rsphydat)

typs <- rsphydat %>% 
  select(station, typ) %>% 
  unique

# read master sheet
rsphypts <- read_sheet('1ju1vJpCxR58Iayr8DhBxGVqloZDTI4TDoQUohS0h-6c') %>%
  mutate_if(is.list, as.character) %>% 
  left_join(typs, by = 'station') %>% 
  mutate(col = case_when(
    typ == 'Quantitative' ~ col2hcl('burlywood1'), 
    typ == 'Qualitative' ~ col2hcl('cyan3')
    )
  )

# remove PP- stations from epc, these were sampled week of 4/19, will need to add once I have data in hand
rsphypts <- rsphypts %>% 
  filter(!grepl('^PP\\-', station))

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

# combine all
rsstatloc <- bind_rows(epchcsta1, fldepsta1, mpnrdsta1, pincosta1, ncfsta1, usfsta1, tbepsta1) %>% 
  mutate(
    source = source,
    source_lng = case_when(
      source == 'pinco' ~ 'Pinellas Co.', 
      source == 'epchc' ~ 'Hillsborough Co.', 
      source == 'fldep' ~ 'Florida DEP', 
      source == 'mpnrd' ~ 'Manatee Co.', 
      source == 'ncf' ~ 'New College Fl.', 
      source == 'tbep' ~ 'TBEP', 
      source == 'usf-rains' ~ 'USF'
    )
  ) %>% 
  select(source_lng, source, station, lat, lon, comment) %>% 
  arrange(source_lng, station)

write.csv(rsstatloc, 'data/raw/wq_stations_all.csv', row.names = F)

rsstatloc <- rsstatloc %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)

save(rsstatloc, file = 'data/rsstatloc.RData', version = 2)

# coordinated response data -----------------------------------------------

data(bswqrngs)
data(parms)
data(rsstatloc)

gdrive_pth <- 'https://drive.google.com/drive/folders/1SWlBbZtjZ8SF43MCz5nv5YLCWDvn7T_x'

# sleep time in seconds
wait <- 10

# csv files must be opened/saved as spreadsheet in google sheets
fls <- drive_ls(gdrive_pth, type = 'spreadsheet')

##
# fldep dump 20210411
fl <- fls[which(fls$name == 'FLDEP_20210421'), 'id'] %>% pull(id)
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

##
# mpnrd dump 2021-04-06

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

##
# pinellas co

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
        T ~ station
      ),
      station = gsub('^PC\\s', 'PC', station),
      station = gsub('^MC\\s', 'MC', station),
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
  separate(var, c('var', 'uni'), sep = '_')

pinco1qual <- pincowide %>%
  select(date, station, matches('\\_qual$')) %>%
  gather('var', 'qual', -date, -station) %>%
  mutate(var = gsub('\\_qual$', '', var)) %>%
  filter(grepl('\\w', qual))

out2 <- full_join(pinco1wq, pinco1qual, by = c('date', 'station', 'var')) %>%
  mutate(
    source = 'pinco',
    date = ymd(date),
    station = gsub('\\s+', '', station),
    station = case_when(
      station == 'PMOutfall' ~ 'PM Out',
      station == 'TBEP-MCBHO2' ~ 'TBEP-MCBH02',
      T ~ station
    ), 
    source = case_when(
      station %in% c("TBEP-BH0", "TBEP-BH15", "TBEP-BH25", "TBEP-MCBH02") ~ 'tbep', 
      T ~ source
    )
  ) %>%
  select(station, date, source, var, uni, val, qual) %>%
  filter(!is.na(val)) %>%
  filter(!station %in% 'MC- FB') %>% # this is a validation site, no data
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

##
# new college dump 20210410

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

##
# epc

ids <- fls[grep('^EPC\\_PP\\_', fls$name), 'id'] %>% pull(id)
epcout <- NULL
for(id in ids){
  
  # sleep to not bonk api limit
  Sys.sleep(wait)
  
  flsht <- read_sheet(id)
  epctmp <- flsht %>% 
    select(
      station = `EPC Station`, 
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
  
  epcout <- bind_rows(epcout, epctmp)
  
}

epc1 <- epcout

##
# combine all

rswqdat <- bind_rows(fldep1, mpnrd1, pinco1, ncf1, epc1) %>%
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

# add column if in/out of range
rswqdat <- rswqdat %>% 
  left_join(bswqrngs, by = c('var', 'uni')) %>% 
  rowwise() %>% 
  mutate(
    inrng = case_when(
      val < minv ~ 'below', 
      val > maxv ~ 'above', 
      T ~ 'in range'
      ), 
    val = round(val, sigdig)
  ) %>% 
  ungroup %>% 
  select(-avev, -stdv, -sigdig, -minv, -maxv, -lbs)

save(rswqdat, file = 'data/rswqdat.RData', version = 2)

# for log
tms <- Sys.time()
attr(tms, 'tzone') <- 'America/New_York'
tms <- paste(as.character(tms), 'Eastern')

writeLines(tms, 'logs/indexlog.txt')



