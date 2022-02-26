library(dplyr)
library(tidyr)
library(lubridate)
library(here)
library(googlesheets4)

# deauth all so it can build with gh actions
gs4_deauth()

datraw <- read_sheet('1TOIquQt2WtM1FpQjc1uKp1GRdbYxen0zD9g78gYcmV8', sheet = 'Raw')

dat <- datraw %>% 
  mutate(
    Time = as.character(unlist(Time)), 
    Date = as.character(Date)
  ) %>% 
  unite('DateTime', Date, Time) %>% 
  mutate(
    DateTime = ymd_hm(DateTime, tz = 'EST')
  )

save(dat, file = here('data/dat.RData'))
