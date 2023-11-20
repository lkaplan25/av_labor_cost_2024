#################################################################################################
# About this file
#################################################################################################
# 
# This script pulls the Chicago taxicab data from the City of Chicago website 
# and writes it to a parquet file. The parquet file is not included in the 
# repo to save storage space.
#
#################################################################################################

library(tidyverse)
library(arrow)

# 2019 data from:
# https://data.cityofchicago.org/Transportation/Taxi-Trips-2019/h4cq-z3dy

open_csv_dataset(here::here('raw_data', 'taxi-trips-2019.csv')) %>% 
  select(
    trip_id = 'Trip ID', 
    taxi_id = 'Taxi ID', 
    trip_start_time = 'Trip Start Timestamp',
    trip_end_time = 'Trip End Timestamp',
    trip_seconds = 'Trip Seconds', 
    trip_miles = 'Trip Miles', 
    fare = 'Fare', 
    tips = 'Tips', 
    tolls = 'Tolls', 
    extras = 'Extras', 
    trip_total_cost = 'Trip Total', 
    company = 'Company'
  ) %>% 
  write_parquet(here::here('raw_data', 'taxi-trips-2019.parquet'))

