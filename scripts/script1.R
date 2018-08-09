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
  mutate(totalusage_perday = peak_perday + offpeak_perday)

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

perday <- perday %>% 
  mutate(feedinefficiency = feedin_perday / PVhours_perday)

perday

# lets try plotting this stuff...

# Electricity overview in kwh per day

ggplot(perday, aes(x = date)) + 
  geom_line(aes(y = peak_perday, colour = "peak")) +
  geom_line(aes(y = offpeak_perday, colour = "offpeak")) +
  geom_line(aes(y = PVgen_perday, colour = "PV generation")) +
  geom_line(aes(y = feedin_perday, colour = "PV feed-in")) +
  geom_line(aes(y = totalusage_perday, colour = "total usage")) +
  labs(x = "Date", y = "kwh per day", title = "Electricity use, generation and export", colour = "Type") +
  scale_color_manual(values = c("blue","red","green","black", "orange")) +
  geom_hline(yintercept = 15.5, linetype = 2) +
  geom_hline(yintercept = 19, linetype = 2) +
  geom_hline(yintercept = 13.09, linetype = 2, colour = "orange") 
  

ggplot(perday, aes(x = date, y = PVefficiency)) + 
  geom_line() +
  labs(x = "Date", y = "kwh per hour", title = "PV efficiency")

ggplot(perday, aes(x = date, y = feedinefficiency)) + 
  geom_line() +
  labs(x = "Date", y = "kwh per hour", title = "feedin efficiency")

ggplot(perday, aes(x = date)) + 
  geom_line(aes(y = PVefficiency, colour = "PV generation efficiency")) +
  geom_line(aes(y = feedinefficiency, colour = "feed-in efficiency")) +
  labs(x = "Date", y = "kwh per hour", title = "PV efficiency") +
  scale_color_manual(values = c("black","green"))

# Water use

ggplot(perday, aes(x = date, y = water_perday)) + 
  geom_line() +
  labs(x = "Date", y = "litres per day", title = "Water usage") +
  geom_hline(yintercept=740)

# have put in some hlines to give seasonal averages for usage of 4 person households
# calculate average total usage per day and add in an hline.

mean(perday$totalusage_perday, na.rm = TRUE)

# I feel the need to clean out the unnecesary data and make a new table.

eUGE <-  
  select(perday,"date","peak_perday","offpeak_perday","totalusage_perday",
         "PVgen_perday","feedin_perday","PVhours_perday","PVefficiency", "feedinefficiency")

eUGE <- slice(eUGE,-1)
eUGE

# It's interesting to note that a few things have happened in our household
# 1st child was born July 2011.  An RCAC was installed that summer.
# 2nd child born January 2014.  Time at home has changed with part-time hours etc.
# We've been playing with the mix of RCAC and heatbank (offpeak) for heating.
# Off peak kicks in in summer when visitors are over due to supplementary HWS.
# PV efficiency dips at he height of summer - sun rises and sets south of E and W.
# We also have rainfall data, and climate data can be downloaded from BOM. 
# TOgether with daily (rather than monthly) data, there may be some interesting correlations.


