library(tidyverse)
library(lubridate)

data(rswqdat)
data(bswqdat)
data(rsstatloc)
data(ppseg)

rswqtmp <- rswqdat %>% 
  filter(var == 'chla')

bswqtmp <- bswqdat %>% 
  filter(var == 'chla')
