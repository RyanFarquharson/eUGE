library(tidyverse)

meterreads <- read_csv("data/meterreads.csv")
meterreads

#plot power reads i.e. peak, offpeak and PV

ggplot(meterreads, aes(x = date)) + 
  geom_line(aes(y = peak_reading, colour = "peak")) +
  geom_line(aes(y = offpeak_reading, colour = "offpeak")) +
  geom_line(aes(y = PV_reading, colour = "PV")) +
  geom_line(aes(y = feedin_reading, colour = "PV feed-in")) +
  labs(x = "Date", y = "kwh", title = "Power reads", colour = "Read type") +
  scale_color_manual(values = c("blue","red","green","black"))

# plot PV hours in a seperate graph

ggplot(meterreads, aes(x = date, y = PVhours_reading)) +
  geom_line() +
  labs(x = "Date", y = "hours", title = "PVhours")

# plot water reading in a seperate graph

ggplot(meterreads, aes(x = date, y = water_reading)) + 
  geom_line() +
  labs(x = "Date", y = "litres", title = "Water")

# make a new tibble called 'usage' from 'meterreads' and
# add a column called peak_usage by using mutate and lag
# mutate makes a new column called peak_usage by starting with peak_reading and subtracting a value
# this value is also from the peak_reading column, but by using lag it is from the previous row.

usage <- meterreads %>% 
  mutate(peak_usage = peak_reading - 
           lag(peak_reading, default = first(peak_reading)))

# now repeat the process for the other readings

usage <- usage %>% 
  mutate(offpeak_usage = offpeak_reading -
           lag(offpeak_reading, default = first(offpeak_reading)))

usage <- usage %>% 
  mutate(feedin_usage = feedin_reading -
           lag(feedin_reading, default = first(feedin_reading)))

usage <- usage %>% 
  mutate(PV_usage = PV_reading -
           lag(PV_reading, default = first(PV_reading)))

usage <- usage %>% 
  mutate(PVhours_usage = PVhours_reading -
           lag(PVhours_reading, default = first(PVhours_reading)))

usage <- usage %>% 
  mutate(water_usage = water_reading -
           lag(water_reading, default = first(water_reading)))

usage <- usage %>% 
  mutate(days = as.integer(date -
           lag(date, default = first(date))))

usage

# make a new table called perday.  divide usage by number of days to get usage per day.

perday <- usage %>% 
  mutate(peak_perday = peak_usage / days)

perday <- perday %>% 
  mutate(offpeak_perday = offpeak_usage / days)

perday <- perday %>% 
  mutate(feedin_perday = feedin_usage / days)

perday <- perday %>% 
  mutate(PVgen_perday = PV_usage / days)

perday <- perday %>% 
  mutate(PVhours_perday = PVhours_usage / days)

perday <- perday %>% 
  mutate(water_perday = water_usage / days)

perday <- perday %>% 
  mutate(PVefficiency = PVgen_perday / PVhours_perday)

perday

# lets try plotting this stuff

ggplot(perday, aes(x = date)) + 
  geom_line(aes(y = peak_perday, colour = "peak")) +
  geom_line(aes(y = offpeak_perday, colour = "offpeak")) +
  geom_line(aes(y = PVgen_perday, colour = "PV generation")) +
  geom_line(aes(y = feedin_perday, colour = "PV feed-in")) +
  labs(x = "Date", y = "kwh per day", title = "Daily electricity", colour = "Type") +
  scale_color_manual(values = c("blue","red","green","black"))

ggplot(perday, aes(x = date, y = water_perday)) + 
  geom_line() +
  labs(x = "Date", y = "litres per day", title = "Water usage") +
  geom_hline(yintercept=740)

ggplot(perday, aes(x = date, y = PVefficiency)) + 
  geom_line() +
  labs(x = "Date", y = "kwh per hour", title = "PV efficiency")

# It's interesting to note that a few things have happened in our household
# 1st child was born July 2011.  An RCAC was installed that summer.
# 2nd child born January 2014.  Time at home has changed with part-time hours etc.
# We've been playing with the mix of RCAC and heatbank (offpeak) for heating.
# Off peak kicks in in summer when visitors are over due to supplementary HWS.
# We also have rainfall data, and climate data can be downloaded from BOM.
