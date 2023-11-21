##############################################################################
# About this file
##############################################################################
# 
# This script contains the code for analyzing the 2019 Chicago taxi data.
#
# Please note that the parquet file is not stored in the repo so this script
# will not be able to run unless one downloads the data from the City of
# Chicago website (see format_raw_chicago_data.R script).
#
##############################################################################

# install.packages(c("tidyverse", "devtools", "arrow", "lubridate))

library(devtools)
library(arrow)
library(tidyverse)
library(lubridate)

# Raw data from:
# https://data.cityofchicago.org/Transportation/Taxi-Trips-2019/h4cq-z3dy

# Open data file ----

ds <- open_dataset(here::here('raw_data', 'taxi-trips-2019.parquet'))

# Fare per mile (fpm) ----

df <- ds %>%
  select(trip_miles, fare) %>% 
  filter(trip_miles > 0) %>% 
  filter(fare > 0) %>% 
  collect() %>% 
  mutate(fpm = fare / trip_miles) 

# Drop the top 5% to remove outliers

fpm_upper_bound <- quantile(df$fpm, 0.95)

fpm <- df %>%
  filter(fpm < fpm_upper_bound) %>%
  pull(fpm)

hist(fpm)
fpm_summary <- summary(fpm)

# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.00007  3.24468  5.00000  5.70309  6.75000 42.47573 

# Miles per trip (mpt) ----

df <- ds %>%
  select(trip_miles) %>% 
  filter(trip_miles > 0) %>% 
  collect()

# Drop the top 1% of miles to remove outliers

miles_upper_bound <- quantile(df$trip_miles, 0.99)

df <- df %>% 
  filter(trip_miles < miles_upper_bound)

mpt <- df$trip_miles

hist(mpt)
mpt_summary <- summary(mpt)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.010   0.890   1.520   4.067   4.140  21.690 
