library(tidyverse)
library(janitor)
library(lubridate)
library(WtRegDO)

dt <- Sys.Date() %>% 
  as.character %>% 
  gsub('\\-', '', .)


# lobo data retrieval -----------------------------------------------------

url <- paste('http://tampabay.loboviz.com/cgi-data/nph-data.cgi?node=82&min_date=20210321&max_date=', dt, '&y=salinity,temperature,co2,oxygen,pht,par,pressure&data_format=text', sep = '')

# note that do is ml/L, to convert use [mg/L] = [ml/L] * 1.42903 (from email 4/16/21)
datraw <- read.table(url, skip = 2, sep = '\t', header = T) %>% 
  clean_names() %>% 
  select(
    DateTimeStamp = date_est, 
    Sal = salinity_psu, 
    Temp = temperature_c, 
    co2 = co2_ppm, 
    DO_obs = dissolved_oxygen_ml_l, 
    ph = p_ht, 
    par = par_u_m_m_2_sec,
    Tide = pressure_d_bar
  ) %>% 
  mutate(
    DateTimeStamp = ymd_hms(DateTimeStamp, tz = 'America/Jamaica') 
  )

lobodat <- datraw

# ports data --------------------------------------------------------------

# 8726412 is middle tampa bay station, at LOBO I think
# Port Manatee is 8726384
# PORTS/NOAA API description is here https://api.tidesandcurrents.noaa.gov/api/prod/#units
# Data can be retrieved here https://tidesandcurrents.noaa.gov/met.html?id=8726412

url <- paste('https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?product=ENTERVAR&application=NOS.COOPS.TAC.MET&begin_date=20210321&end_date=', dt, '&station=8726412&time_zone=LST&units=metric&format=CSV', sep = '')

# metric units selected, wind is m/2, air temp is C, pressure is mb
vrs <- c('wind', 'air_temperature', 'air_pressure')

out <-  list()
for(vr in vrs){
  
  url_tmp <- gsub('ENTERVAR', vr, url)
  
  dat <- read.table(url_tmp, sep = ',', header = T) %>% 
    .[, c(1:2)] %>% 
    gather('var', 'val', -Date.Time)
  
  out <- rbind(out, dat)
  
}

portsdat <- out %>% 
  spread(var, val) %>% 
  select(
    DateTimeStamp = Date.Time, 
    ATemp = Air.Temperature, 
    BP = Pressure, 
    WSpd = Speed
  ) %>% 
  mutate(DateTimeStamp = ymd_hm(DateTimeStamp, tz = 'America/Jamaica'))

# combine ports with lobo -------------------------------------------------

lobodat <- lobodat %>% 
  left_join(portsdat, by = 'DateTimeStamp')

save(lobodat, file = 'data/lobodat.RData', version = 2)


# lobo metabolism ---------------------------------------------------------

lobodat <- lobodat %>% 
  mutate(
    DO_obs = DO_obs * 1.42903 
  )

tz <- attr(lobodat$DateTimeStamp, which = 'tzone')
lat <- 27.6594677
long <- -82.6043877

loboeco <- WtRegDO::ecometab(lobodat, DO_var = "DO_obs", tz = tz, lat = lat, long = long)

save(loboeco, file = 'data/loboeco.RData', version = 2)

# log file ----------------------------------------------------------------

# for log
tms <- Sys.time()
attr(tms, 'tzone') <- 'America/New_York'
tms <- paste(as.character(tms), 'Eastern')

writeLines(tms, 'logs/lobolog.txt')


