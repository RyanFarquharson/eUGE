library(tidyverse)

meterreads <- read_csv("data/meterreads.csv")
meterreads



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

usage <- slice(usage, -1)

usage <- slice(usage, -110)

usage <- slice(usage, -109)

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
mean(perday$totalusage_perday, na.rm = TRUE)

# I feel the need to clean out the unnecesary data and make a new table.

eUGE1 <-  
  select(perday,"date","peak_perday","offpeak_perday","totalusage_perday",
         "PVgen_perday","feedin_perday","PVhours_perday","PVefficiency", "feedinefficiency")


# got rid of other data and trimmed to give 9 complete years

monthlist1 <- c(6,7,8,9,10,11,12)
monthlist2 <- c(1,2,3,4,5,6,7,8,9,10,11,12)
monthlist3 <- c(1,2,3,4,5)
monthlist <- c(monthlist1, monthlist2, monthlist2, monthlist2, monthlist2, monthlist2, monthlist2, monthlist2, monthlist2, monthlist3)

# Make eUGE2 which has a month list

eUGE2 <- eUGE1 %>% 
  mutate(month = as.factor(monthlist))

# Make a year list
yearlist1 <- c(2009,2009,2009,2009,2009,2009,2009)
yearlist2 <- c(2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010)
yearlist3 <- yearlist2 + 1
yearlist4 <- yearlist2 + 2
yearlist5 <- yearlist2 + 3
yearlist6 <- yearlist2 + 4
yearlist7 <- yearlist2 + 5
yearlist8 <- yearlist2 + 6
yearlist9 <- yearlist2 + 7
yearlist10 <- c(2018,2018,2018,2018,2018)

yearlist <- c(yearlist1,yearlist2,yearlist3,yearlist4,yearlist5,yearlist6,yearlist7,yearlist8,yearlist9,yearlist10)

# Add year list to eUGE2

eUGE2 <- eUGE2 %>% 
  mutate(year = as.factor(yearlist))

# Make a season list

seasonlist1 <- c('Winter', 'Winter', 'Winter', 'Spring', 'Spring', 'Spring', 'Summer')
seasonlist2 <- c('Summer', 'Summer', 'Autumn', 'Autumn', 'Autumn')
seasonlist3 <- c(seasonlist2, seasonlist1)
seasonlist <- c(seasonlist1, seasonlist3, seasonlist3, seasonlist3, seasonlist3, seasonlist3, seasonlist3, seasonlist3, seasonlist3, seasonlist2)
seasonlist

eUGE3 <- eUGE2 %>% 
  mutate(season = as.factor(seasonlist))

eUGE3


# lets try plotting this stuff...

#plot power reads i.e. peak, offpeak and PV

ggplot(meterreads, aes(x = date)) + 
  geom_line(aes(y = peak_reading, colour = "peak")) +
  geom_line(aes(y = offpeak_reading, colour = "offpeak")) +
  geom_line(aes(y = PV_reading, colour = "PV")) +
  geom_line(aes(y = feedin_reading, colour = "PV feed-in")) +
  labs(x = "Date", y = "kwh", title = "Power reads", colour = "Read type") +
  scale_color_manual(values = c("blue","red","green","black")) +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))

# plot PV hours in a seperate graph

ggplot(meterreads, aes(x = date, y = PVhours_reading)) +
  geom_line() +
  labs(x = "Date", y = "hours", title = "PVhours") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))

# plot water reading in a seperate graph

ggplot(meterreads, aes(x = date, y = water_reading)) + 
  geom_line() +
  labs(x = "Date", y = "litres", title = "Water") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))

# Electricity overview in kwh per day
# How to put hlines with legends in

ggplot(eUGE3, aes(x = as.factor(month), y = totalusage_perday, colour = season)) +
  geom_point(size = 4, alpha = 0.5) +
  labs(x = "Month", y = "kWh per day", title = "Total usage") +
  scale_color_manual(values = c("orange","green","red", "blue")) +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA)) +
  geom_hline(aes(yintercept = 15.7, linetype = "Local Summer ave"), colour = "red") + # average for local summer
  geom_hline(aes(yintercept = 15.5, linetype = "Local Autumn ave"), colour = "orange") + # average for local autumn
  geom_hline(aes(yintercept = 19.0, linetype = "Local Winter ave"), colour = "blue") + # average for local winter
  geom_hline(aes(yintercept = 16.1, linetype = "Local Spring ave"), colour = "green") + # average for local spring
  geom_hline(aes(yintercept = 12.9, linetype = "Our average"), colour = "black") + # average for our household
  scale_linetype_manual(name = "Average local 4 person household", values = c(2, 2, 2, 2, 2), 
                        guide = guide_legend(override.aes = list(color = c("orange", "green", "red","blue", "black"))))


# average household usage from energymadeeasy.gov.au.  
# 4 person household postcode 5051
# Summer 15.7; Autumn 15.5; WInter 19.0; Spring 16.1 kWh 


# More PV plots

ggplot(perday, aes(x = date, y = PVefficiency)) + 
  geom_line() +
  labs(x = "Date", y = "kwh per hour", title = "PV efficiency") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))

ggplot(perday, aes(x = date, y = feedinefficiency)) + 
  geom_line() +
  labs(x = "Date", y = "kwh per hour", title = "feedin efficiency") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))

ggplot(perday, aes(x = date)) + 
  geom_line(aes(y = PVefficiency, colour = "PV generation efficiency")) +
  geom_line(aes(y = feedinefficiency, colour = "feed-in efficiency")) +
  labs(x = "Date", y = "kwh per hour", title = "PV efficiency") +
  scale_color_manual(values = c("black","blue")) +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))

# Water use

ggplot(perday, aes(x = date, y = water_perday)) + 
  geom_line() +
  labs(x = "Date", y = "litres per day", title = "Water usage") +
  geom_hline(yintercept=740) +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))

# have put in some hlines to give seasonal averages for usage of 4 person households
# calculate average total usage per day and add in an hline.



# Some more exploratory plots.  Need to figure out how to get a better colour scheme.

# Peak
#Figured out how join points properly!

ggplot(eUGE3, aes(x = as.factor(month), y = peak_perday, colour = year, group = year)) +
  geom_point(size = 4, alpha = 0.7) +
  labs(x = "Month", y = "kWh per day", title = "Peak usage") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA)) +
  scale_color_viridis_d() +
  geom_line()

# Peak by season
ggplot(eUGE3, aes(x = as.factor(month), y = peak_perday, colour = season)) +
  geom_point(size = 4, alpha = 0.7) +
  labs(x = "Month", y = "kWh per day", title = "Peak usage") +
  scale_color_manual(values = c("orange","green","red", "blue")) +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))

#ggsave(AugustWithoutOutlier, path = "~/results/")

# Offpeak

ggplot(eUGE3, aes(x = as.factor(month), y = offpeak_perday, colour = year, group = year)) +
  geom_point(size = 4, alpha = 0.7) +
  labs(x = "Month", y = "kWh per day", title = "OffPeak usage") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA)) +
  scale_color_viridis_d() +
  geom_line()

ggplot(eUGE3, aes(x = as.factor(month), y = offpeak_perday, colour = season)) +
  geom_point(size = 4, alpha = 0.7) +
  labs(x = "Month", y = "kWh per day", title = "OffPeak usage") +
  scale_color_manual(values = c("orange","green","red", "blue")) +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))

# Total usage

ggplot(eUGE3, aes(x = as.factor(month), y = totalusage_perday, colour = year, group = year)) +
  geom_point(size = 4, alpha = 0.7) +
  labs(x = "Month", y = "kWh per day", title = "Total usage") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA)) +
  scale_color_viridis_d() +
  geom_line()

ggplot(eUGE3, aes(x = as.factor(month), y = totalusage_perday, colour = season)) +
  geom_point(size = 4, alpha = 0.5) +
  labs(x = "Month", y = "kWh per day", title = "Total usage") +
  scale_color_manual(values = c("orange","green","red", "blue")) +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA)) +
  geom_hline(aes(yintercept = 15.7, linetype = "Local Summer ave"), colour = "red") + # average for local summer
  geom_hline(aes(yintercept = 15.5, linetype = "Local Autumn ave"), colour = "orange") + # average for local autumn
  geom_hline(aes(yintercept = 19.0, linetype = "Local Winter ave"), colour = "blue") + # average for local winter
  geom_hline(aes(yintercept = 16.1, linetype = "Local Spring ave"), colour = "green") + # average for local spring
  geom_hline(aes(yintercept = 12.9, linetype = "Our average"), colour = "black") + # average for our household
  scale_linetype_manual(name = "Average local 4 person household", values = c(2, 2, 2, 2, 2), 
                          guide = guide_legend(override.aes = list(color = c("orange", "green", "red","blue", "black"))))
    

# Energymadeeasy.gov.au household usage 4 person no pool
# AHsummer <- 15.7
# AHautumn <- 15.5
# AHwinter <- 19.0
# AHspring <- 16.1
# AHave <- 16.5
# OUr average

mean(eUGE3 $totalusage_perday)


# PV generation

ggplot(eUGE3, aes(x = as.factor(month), y = PVgen_perday, colour = year, group = year)) +
  geom_point(size = 4, alpha = 0.7) +
  labs(x = "Month", y = "kWh per day", title = "PV generation") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA)) +
  scale_color_viridis_d() +
  geom_line()

# PV export

ggplot(eUGE3, aes(x = as.factor(month), y = feedin_perday, colour = year, group = year)) +
  geom_point(size = 4, alpha = 0.7) +
  labs(x = "Month", y = "kWh per day", title = "PV export") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA)) +
  scale_color_viridis_d() +
  geom_line()

# PV hours 

ggplot(eUGE3, aes(x = as.factor(month), y = PVhours_perday, colour = year, group = year)) +
  geom_point(size = 4, alpha = 0.7) +
  labs(x = "Month", y = "PV hours per day", title = "PV hours") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA)) +
  scale_color_viridis_d() +
  geom_line()

# PV generation efficiency

ggplot(eUGE3, aes(x = as.factor(month), y = PVefficiency, colour = year, group = year)) +
  geom_point(size = 4, alpha = 0.7) +
  labs(x = "Month", y = "kWh per day", title = "PV generation efficiency") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA)) +
  scale_color_viridis_d() +
  geom_line()

# PV export efficiency

ggplot(eUGE3, aes(x = as.factor(month), y = feedinefficiency, colour = year, group = year)) +
  geom_point(size = 4, alpha = 0.7) +
  labs(x = "Month", y = "kWh per day", title = "PV feed-in efficiency") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA)) +
  scale_color_viridis_d() +
  geom_line()

# PV performance - generation

ggplot(eUGE3, aes(x = PVhours_perday, y = PVgen_perday, colour = month)) +
  geom_point(size = 4, alpha = 0.7) +
  labs(x = "Hours of operation", y = "kWh generated", title = "PV generation performance") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))


ggplot(eUGE3, aes(x = PVhours_perday, y = PVgen_perday, colour = month, group = year)) +
  geom_point(size = 4, alpha = 0.7) +
  labs(x = "Hours of operation", y = "kWh generated", title = "PV generation performance") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA)) +
  geom_line()


# PV performance - export

ggplot(eUGE3, aes(x = PVhours_perday, y = feedin_perday, colour = month)) +
  geom_point(size = 4, alpha = 0.7) +
  labs(x = "Hours of operation", y = "kWh exported", title = "PV export performance") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))

write.csv(eUGE1, file = "eUGE1.csv")
write.csv(eUGE3, file = "eUGE3.csv")

# It's interesting to note that a few things have happened in our household
# 1st child was born July 2011.  An RCAC was installed that summer.
# 2nd child born January 2014.  Time at home has changed with part-time hours etc.
# We've been playing with the mix of RCAC and heatbank (offpeak) for heating.
# Off peak kicks in in summer when visitors are over due to supplementary HWS.
# PV efficiency dips at he height of summer - sun rises and sets south of E and W.
# We also have rainfall data, and climate data can be downloaded from BOM. 
# Together with daily (rather than monthly) data, there may be some interesting correlations.


