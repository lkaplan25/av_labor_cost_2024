#################################################################################################
# About this file
#################################################################################################
# 
# This script contains lists with the input values used in the model. The baseline_inputs list
# contains the inputs for the baseline scenario. The Scenario Inputs section creates the
# input lists for the other four scenario models. The Monte Carlo (MC) Analysis section creates input 
# lists with draws from normal distributions for each of the input variables. 
#
#################################################################################################

# Baseline (no MC) ----

baseline_inputs <- list(
  
  ## Time conversion variables ----
  
  weeks_per_year  = 52, 
  months_per_year = 12,
  hours_per_day   = 24, 
  
  ## Fleet info ----------
  
  ### Miles per trip ----
  
  miles_per_trip = 3.09, 
  
  ### Fleet size ----
  
  fleet_size = 1313, 
  
  ### Mileage ---- 
  
  mileage_annual = 65000, 
  
  ## Labor costs ---------
  
  overhead_rate     = 1.59, 
  general_and_admin = 0.05,           # ($/mi)
  is_taxi           = 0,
  
  ### Wages ----
  
  wage_dispatcher      = 17.05,      # ($/hour) 
  wage_driver          = 15.82,      # ($/hour) 
  wage_monitor         = 19,         # ($/hour)
  wage_fieldsupport    = 21,         # ($/hour)
  wage_customersupport = 19,         # ($/hour)
  wage_coordinator     = 29,         # ($/hour) 
  
  
  ### Shift Information ----
  
  # shift_length (hours/shift) 
  shift_length_dispatcher      = 8,
  shift_length_driver          = 12,
  shift_length_monitor         = 8, 
  shift_length_fieldsupport    = 8, 
  shift_length_customersupport = 8, 
  shift_length_coordinator     = 8,            
  
  # shifts_per_day (shifts/day)
  shifts_per_day_dispatcher      = 3,
  shifts_per_day_driver          = 2,
  shifts_per_day_monitor         = 3,  
  shifts_per_day_fieldsupport    = 3,  
  shifts_per_day_customersupport = 3, 
  shifts_per_day_coordinator     = 3,
  
  
  # shift_days_per_year (days/year)
  shift_days_per_year_dispatcher      = 365,
  shift_days_per_year_driver          = 365,
  shift_days_per_year_monitor         = 365, 
  shift_days_per_year_fieldsupport    = 365, 
  shift_days_per_year_customersupport = 365, 
  shift_days_per_year_coordinator     = 365,
  
  
  ### Ratios ----
  
  # vehicles_per_cluster 
  vehicles_per_cluster_dispatcher      = 15, 
  vehicles_per_cluster_driver          = 1,  
  vehicles_per_cluster_monitor         = 16, 
  vehicles_per_cluster_fieldsupport    = 6,  
  vehicles_per_cluster_customersupport = 16,  
  vehicles_per_cluster_coordinator     = 80, 
  
  
  # workers_per_cluster_shift (workers per cluster per shift)
  workers_per_cluster_shift_dispatcher      = 1,
  workers_per_cluster_shift_driver          = 1,
  workers_per_cluster_shift_monitor         = 1,
  workers_per_cluster_shift_fieldsupport    = 1,
  workers_per_cluster_shift_customersupport = 1,
  workers_per_cluster_shift_coordinator     = 1,
  
  
  ## Maintenance, Insurance ----
  
  # vehicle_operations
  vehicle_operations_maintenance = 0.06,  # ($/mi)   
  vehicle_operations_insurance   = 682,   # ($/month) 
  
  
  # av_operations_factor
  av_operations_factor_maintenance = 1,   
  av_operations_factor_insurance   = 1,      
  av_operations_factor_fuel        = 1,          
  
  ## Cleaning ----
  
  exterior_cleaning_price    = 10, # ($)
  interior_cleaning_price    = 6,  # ($)   
  
  
  ## Financing ---------------
  
  vehicle_price              = 28000,  # ($)
  tech_price                 = 150000, # ($)
  vehicle_financing_lifespan = 3,      # (years)
  annual_interest_rate       = 0.07,   # (%)
  vehicle_lifespan           = 5,      # (years)
  payment_periods_per_year   = 12,
  av_tech_lifespan           = 5,      # (years)
  discount_rate              = 0.05,   # (%)
  
  ## Licensing  Fees  ----
  
  ### For Taxis: ----
  
  # Non-Medallion Licensing
  taxi_licensing_taxi_medallion_license    = 250,     # ($/year)
  taxi_licensing_ground_transportation_tax = 98,      # ($/month)
  taxi_licensing_accessibility_fund        = 22,      # ($/month)
  taxi_licensing_advertising_fee           = 100,     # ($/year)
  
  # Medallion Financing (used for Medallion Scenario)
  
  medallion_system             = "n",
  taxi_medallion_price         = 0,
  downpayment_percent          = 0.2,
  downpayment_upfront_percent  = 0.25,
  financing_period_downpayment = 7,     # (years)
  financing_period_remainder   = 5,     # (years)
  medallion_interest_rate      = 0.054,
  payment_periods_per_year     = 12,
  medallion_lifespan           = 20,    # (years)
  
  ### For TNCs: ----
  
  tnc_licensing_admin_fee_per_year = 10000,      # ($/year per fleet)
  tnc_licensing_admin_fee_per_trip = 0.02,       # ($/trip)
  tnc_licensing_ground_transport   = 1,          # ($/trip)          
  tnc_licensing_access_fund        = 0.10,       # ($/trip)
  tnc_licensing_advertising_fee    = 100,        # ($/year per vehicle)
  
  
  ## Fuel  ----
  
  fuel_cost_per_gal = 3.829,  # ($/gallon)
  fuel_efficiency   = 45,     # (miles/gallon)
  
  ## Capacity Utilization Rate  ----
  
  profit             = 0.27, 
  capacity_util_rate = 0.50
  
)


# Scenario inputs (no MC) ----

# Create input lists for each of the scenarios.
# See paper for details about how the input lists are adjusted for each scenario.

## Non-AV Taxi ----

taxi_inputs <- baseline_inputs

# Remove cost of AV tech
taxi_inputs$tech_price <- 0

# Calculate taxi labor instead of av labor
taxi_inputs$is_taxi <- 1

## AV Advanced Technology ----

adv_av_tech_inputs <- baseline_inputs

# Decrease labor requirements
adv_av_tech_inputs$vehicles_per_cluster_monitor        <- 30
adv_av_tech_inputs$vehicles_per_cluster_monitor        <- 30
adv_av_tech_inputs$vehicles_per_cluster_fieldsupport   <- 8

# Improve tech and vehicle operational performance
adv_av_tech_inputs$av_operations_factor_maintenance <- 0.9
adv_av_tech_inputs$av_operations_factor_insurance   <- 0.5
adv_av_tech_inputs$av_operations_factor_fuel        <- 0.8
adv_av_tech_inputs$tech_price                       <- 10000


## High Usage ----

high_usage_inputs <- baseline_inputs

high_usage_inputs$mileage_annual                   <- 80000
high_usage_inputs$av_operations_factor_maintenance <- 1.2
high_usage_inputs$capacity_util_rate               <- 0.7
high_usage_inputs$vehicle_lifespan                 <- 4
high_usage_inputs$vehicle_lifespan                 <- 4

## Medallion System ----

medallion_sys_inputs <- baseline_inputs

medallion_sys_inputs$medallion_system                         <- 'y'
medallion_sys_inputs$taxi_licensing_taxi_medallion_license    <- 0
medallion_sys_inputs$taxi_licensing_ground_transportation_tax <- 0
medallion_sys_inputs$taxi_licensing_accessibility_fund        <- 0
medallion_sys_inputs$taxi_licensing_advertising_fee           <- 0
medallion_sys_inputs$taxi_medallion_price                     <- 225000

## Lower Density City ----

lower_density_city_inputs <- baseline_inputs

lower_density_city_inputs$mileage_annual     <- 64000
lower_density_city_inputs$capacity_util_rate <- 0.436
lower_density_city_inputs$vehicle_lifespan   <- 6
lower_density_city_inputs$av_tech_lifespan   <- 6

# Monte Carlo Simulation inputs ----

N <- 10000

### Baseline ----

draws_baseline_inputs <- list(
  
  # Normal distributions with mean values derived from data sets 
  miles_per_trip = rlnorm(N, 1, 0.5),
  vehicle_price  = rnorm(N, mean = 28000, sd = 300),
  
  # Normal distributions with mean values equal to assumed baseline values
  fleet_size           = round(rnorm(N, mean = 1313, sd = 350),0),
  mileage_annual       = rnorm(N, mean = 65000, sd = 1000),
  overhead_rate        = rnorm(N, mean = 1.59, sd = 0.1),
  wage_driver          = rnorm(N, mean = 15.82, sd = 1), 
  wage_dispatcher      = rnorm(N, mean = 17.05, sd = 1),   
  wage_monitor         = rnorm(N, mean = 19, sd = 1),
  wage_fieldsupport    = rnorm(N, mean = 21, sd = 1),
  wage_customersupport = rnorm(N, mean = 19, sd = 1),
  wage_coordinator     = rnorm(N, mean = 29, sd = 1), 
  
  vehicles_per_cluster_dispatcher      = rnorm(N, mean = 20, sd = 1), 
  vehicles_per_cluster_monitor         = rnorm(N, mean = 16, sd = 1), 
  vehicles_per_cluster_fieldsupport    = rnorm(N, mean = 6, sd = 1), 
  vehicles_per_cluster_customersupport = rnorm(N, mean = 16, sd = 1),
  vehicles_per_cluster_coordinator     = rnorm(N, mean = 80, sd = 1), 
  
  vehicle_operations_maintenance   = rnorm(N, mean = 0.06, sd = .01),
  vehicle_operations_insurance     = rnorm(N, mean = 682, sd = 10),
  
  vehicle_lifespan               = rnorm(N, mean = 5, sd = 0.5),
  av_tech_lifespan               = rnorm(N, mean = 5, sd = 0.5),
  tnc_licensing_ground_transport = rlnorm(N, 1, sd = 0.22),
  fuel_cost_per_gal              = rnorm(N, mean = 3.829, sd = 0.3),
  fuel_efficiency                = rnorm(N, mean = 45, sd = 3),
  capacity_util_rate             = rnorm(N, mean = 0.50, sd = 0.01)
)

# Create list of draws for each of the scenarios.

### Taxi ----
draws_taxi_inputs <- draws_baseline_inputs

# Remove cost of AV tech
draws_taxi_inputs$tech_price <- array(rep(0,N))

# Calculate taxi labor instead of av labor
draws_taxi_inputs$is_taxi    <- array(rep(1,N))


### Advanced AV Tech ----

draws_adv_av_tech_inputs <- draws_baseline_inputs

# Decrease labor requirements
draws_adv_av_tech_inputs$vehicles_per_cluster_monitor        <- rnorm(N, mean = 30, sd = 1) 
draws_adv_av_tech_inputs$vehicles_per_cluster_monitor        <- rnorm(N, mean = 30, sd = 1) 
draws_adv_av_tech_inputs$vehicles_per_cluster_fieldsupport   <- rnorm(N, mean = 8, sd = 1) 
draws_adv_av_tech_inputs$vehicles_per_cluster_coordinator    <- rnorm(N, mean = 150, sd = 1) 

# Improve tech and vehicle operational performance
draws_adv_av_tech_inputs$av_operations_factor_maintenance    <- rnorm(N, mean = 0.9, sd = 0.1)
draws_adv_av_tech_inputs$av_operations_factor_insurance      <- rnorm(N, mean = 0.5, sd = 0.025)
draws_adv_av_tech_inputs$av_operations_factor_fuel           <- rnorm(N, mean = 0.8, sd = 0.1)
draws_adv_av_tech_inputs$tech_price                          <- rnorm(N, mean = 10000, sd = 1000)

### High Usage ----

draws_high_usage_inputs <- draws_baseline_inputs

# Increasing usage
draws_high_usage_inputs$mileage_annual     <- rnorm(N, mean = 80000, sd = 1000)
draws_high_usage_inputs$capacity_util_rate <- rnorm(N, mean = 0.7, sd = 0.01)

# Increasing maintenance requirements and decreasing vehicle lifespan due to higher usage
draws_high_usage_inputs$av_operations_factor_maintenance <- rnorm(N, mean = 1.2, sd = 0.1)
draws_high_usage_inputs$vehicle_lifespan                 <- rnorm(N, mean = 4, sd = 0.5)
draws_high_usage_inputs$av_tech_lifespan                 <- rnorm(N, mean = 4, sd = 0.5)

## Medallion system ----

draws_medallion_sys_inputs <- draws_baseline_inputs

# Variation in medallion prices
draws_medallion_sys_inputs$medallion_system                         <- array(rep('y',N))
draws_medallion_sys_inputs$taxi_licensing_taxi_medallion_license    <- array(rep(0,N))
draws_medallion_sys_inputs$taxi_licensing_ground_transportation_tax <- array(rep(0,N))
draws_medallion_sys_inputs$taxi_licensing_accessibility_fund        <- array(rep(0,N))
draws_medallion_sys_inputs$taxi_licensing_advertising_fee           <- array(rep(0,N))
draws_medallion_sys_inputs$taxi_medallion_price                     <- rnorm(N, mean = 225000, sd = 25000)

## Lower Density City ----

draws_lower_density_city_inputs <- draws_baseline_inputs

# Decreasing usage
draws_lower_density_city_inputs$mileage_annual     <- rnorm(N, mean = 64000, sd = 1000)
draws_lower_density_city_inputs$capacity_util_rate <- rnorm(N, mean = 0.436, sd = .01)

# Increasing lifespan due to reduced use
draws_lower_density_city_inputs$vehicle_lifespan   <- rnorm(N, mean = 6, sd = 0.5)
draws_lower_density_city_inputs$av_tech_lifespan   <- rnorm(N, mean = 6, sd = 0.5)
