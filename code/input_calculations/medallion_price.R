##############################################################################
# About this file
##############################################################################
# 
# This script compiles data on New York City medallion transfers from 2019 and
# calculates summary statistics.
#
##############################################################################

#install.packages(c("tidyverse", "readxl", "janitor", "here", "stats", "Hmisc"))
library(readxl)
library(tidyverse)
library(janitor)
library(here)
library(stats)
library(Hmisc)

file_path_2019 <- here('input_calculations', 'medallion_data', 'december_2019_medallion_price_list.xls')

# Create an empty data frame to store the extracted data
df <- data.frame()

# Define a list of sheet names and ranges to extract
sheet_ranges <- list(
  list(sheet_name = "December 2019", range = "A3:D52"),
  list(sheet_name = "Novenber 2019", range = "A3:D68"),
  list(sheet_name = "October 2019", range = "A3:D82"),
  list(sheet_name = "September 2019", range = "A3:D61"),
  list(sheet_name = "August 2019", range = "A3:D63"),
  list(sheet_name = "July 2019", range = "A3:D50"),
  list(sheet_name = "June 2019", range = "A3:D53"),
  list(sheet_name = "May 2019", range = "A3:D46"),
  list(sheet_name = "April 2019", range = "A3:D68"),
  list(sheet_name = "March 2019", range = "A3:D45"),
  list(sheet_name = "February 2019", range = "A3:D50"),
  list(sheet_name = "January 2019", range = "A3:D81")
)

for (sheet_range in sheet_ranges) {
  sheet_name       <- sheet_range$sheet_name
  range            <- sheet_range$range
  sample_df        <- read_excel(file_path_2019, sheet = sheet_name, range = range)
  sample_df$Prices <- as.numeric(sample_df$Prices)
  df               <- bind_rows(df, sample_df)
}

# Remove rows that do not contain price information
df <- df %>% 
  clean_names() %>% 
  filter(!is.na(prices))

# Calculate summary stats
# Note: Did not apply medallion classification. The following statistics describe all types of medallions included in the data set.

weighted_mean <- weighted.mean(df$prices, df$number_of_medallions, na.rm = TRUE)
# mean medallion price is 255768.9

weighted_variance <- sum((df$prices - weighted_mean)^2 * df$number_of_medallions) / sum(df$number_of_medallions)
weighted_sd <- sqrt(weighted_variance)
# stdev medallion price is 141720.6

quantiles <- wtd.quantile(df$prices, weights = df$number_of_medallions, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
# median medallion price is 225000