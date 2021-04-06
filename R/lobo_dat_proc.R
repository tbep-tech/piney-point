library(tidyverse)
library(janitor)
library(lubridate)

dt <- Sys.Date()
url <- paste('http://tampabay.loboviz.com/cgi-data/nph-data.cgi?node=82&min_date=20210328&max_date=', dt, '&y=salinity,temperature,co2,oxygen,pht,par,pressure&data_format=text', sep = '')

datraw <- read.table(url, skip = 2, sep = '\t', header = T) %>% 
  clean_names() %>% 
  select(
    datetime = date_est, 
    sal = salinity_psu, 
    temp = temperature_c, 
    co2 = co2_ppm, 
    do = dissolved_oxygen_ml_l, 
    ph = p_ht, 
    par = par_u_m_m_2_sec,
    press = pressure_d_bar
  ) %>% 
  mutate(
    datetime = ymd_hms(datetime, tz = 'America/Jamaica') 
  )

lobodat <- datraw
save(lobodat, file = 'data/lobodat.RData', version = 2)

# for log
tms <- Sys.time()
attr(tms, 'tzone') <- 'America/Jamaica'
tms <- as.character(tms)

writeLines(tms, 'logs/lobolog.txt')

