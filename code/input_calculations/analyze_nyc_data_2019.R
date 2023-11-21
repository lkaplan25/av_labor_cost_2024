##############################################################################
# About this file
##############################################################################
#
# This script contains the code for analyzing the 2019 New York City taxi data.
#
# Please note that the parquet file is not stored in the repo so this script
# will not be able to run unless one downloads the data from the NYC website
# (see format_raw_nyc_data.R script).
#
##############################################################################

# install.packages(c("tidyverse", "devtools", "arrow"))
library(devtools)
library(arrow)
library(tidyverse)

# Open data file ----

ds <- open_dataset(here::here('raw_data', 'nyc-yellow-taxi-2019.parquet')) %>% 
  filter(trip_distance > 0)

# Fare per mile ----

dt <- ds %>% 
  mutate(fpm = fare / trip_distance) %>% 
  select(fpm) %>% 
  filter(fpm > 0) %>% 
  collect()

fpm_upper_bound <- quantile(dt$fpm, 0.95)

fpm <- dt %>%
  filter(fpm < fpm_upper_bound) %>%
  pull(fpm)

hist(fpm)
summary(fpm)

# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.008425  4.360465  5.454545  5.692248  6.818182 10.601719 