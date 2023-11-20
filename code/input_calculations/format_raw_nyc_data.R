#################################################################################################
# About this file
#################################################################################################
# 
# This script pulls the New York City taxicab data from the New York City Taxi and Limousine
# Commission website and writes it to a parquet file. The parquet file is not included in the 
# repo to save storage space.
#
#################################################################################################

library(tidyverse)
library(arrow)

# 2019 data from:
# https://www.nyc.gov/site/tlc/about/tlc-trip-record-data.page

ds <- open_dataset(here::here('raw_data', 'nyc-yellow-taxi', 'nyc_raw'))

ds %>% 
  filter(RatecodeID == 1) %>% 
  select(
    trip_distance,
    trip_start_time = tpep_pickup_datetime,
    trip_end_time = tpep_dropoff_datetime, 
    fare = fare_amount
  ) %>% 
  write_parquet(here::here('raw_data', 'nyc-yellow-taxi-2019.parquet'))
