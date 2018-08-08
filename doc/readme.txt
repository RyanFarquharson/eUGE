meterreads.csv is a file containg meter reads for peak electricity, offpeak electricity, solar feedin, PV generation, PV running hours and water
headers: date, peak_reading, feedin_reading PV_reading, PVhours_reading, water_reading
date: yyyy/mm/dd
units:
peak_reading: kwh
feedingreading: kwh
PV_reading: kwh
PVhours_reading: hours
water_reading: litres
To use the data, calculate usage. This will require calculating the increment for both usage and date, then dividing usage by date.
Consider pulling in climate data, as there could be some interesting correlations between usage and air temperature.
Consider pulling in household occupancy data.  In particular, arrival of offspring.
