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
  date = ymd(c('2022-02-17', '2022-02-22', '2022-03-04', '2022-03-18', '2022-04-18', '2022-06-20', '2022-08-26', '2022-11-18', '2023-02-20', '2023-06-05', '2023-09-11')),
  weight_lb = c(6, 6, 6, 9, 11, 14, 17, 18, 19, 20, 22),
  weight_oz = c(13, 10, 12.5, 1.6, 10.5, 11.5, 13.5, 9.5, 6, 8, 11.2)
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
    date = ymd(c('2022-02-17', '2022-03-04', '2022-04-18', '2022-06-20', '2022-08-26', '2022-11-18', '2023-02-20', '2023-06-05', '2023-09-11')),
    length_in = c(19.8, 20, 22.7, 25, 28.5, 29.2, 30.7, 31.7, 32.6)
  ) %>% 
  mutate(
    months = as.numeric((date - min(date)) / 31),
    length_cm = length_in * 2.54
  ) %>% 
  select(months, length_cm)

save(lendat, file = here('data/lendat.RData'))