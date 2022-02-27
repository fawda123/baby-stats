library(dplyr)
library(tidyr)
library(lubridate)
library(here)
library(googlesheets4)

# deauth all so it can build with gh actions
gs4_deauth()

# daily feeding and change logs -------------------------------------------

dlydatraw <- read_sheet('1TOIquQt2WtM1FpQjc1uKp1GRdbYxen0zD9g78gYcmV8', sheet = 'Raw')

dlydat <- dlydatraw %>% 
  mutate(
    Time = as.character(unlist(Time)), 
    Date = as.character(Date)
  ) %>% 
  unite('DateTime', Date, Time) %>% 
  mutate(
    DateTime = ymd_hm(DateTime, tz = 'EST')
  )

save(dlydat, file = here('data/dlydat.RData'))

# weight data -------------------------------------------------------------

# manually entered weight data
wgtdat <- data.frame(
  date = ymd(c('2022-02-17', '2022-02-22')),
  weight_lb = c(6, 6),
  weight_oz = c(13, 10)
) %>% 
  mutate(
    months = as.numeric((date - min(date)) / 31),
    weight_kg = (weight_lb + weight_oz / 16 ) / 2.205
  ) %>% 
  select(months, weight_kg)

save(wgtdat, file = here('data/wgtdat.RData'))

# length data -------------------------------------------------------------

# manually entered length data
lendat <- data.frame(
    date = ymd(c('2022-02-17')),
    length_in = c(19.8)
  ) %>% 
  mutate(
    months = as.numeric((date - min(date)) / 31),
    length_cm = length_in * 2.54
  ) %>% 
  select(months, length_cm)

save(lendat, file = here('data/lendat.RData'))