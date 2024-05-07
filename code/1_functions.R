#################################################################################################
# About this file
#################################################################################################
# 
# This script contains functions that are called by the analysis.R script. These functions include
# calculations for each of the cost model components, as well as the calculation of the total 
# fare per mile value.
#
#################################################################################################

# Compute Fare per Mile ----
# This function computes the fare per mile using the following equation:
# Fare per Mile = (Vehicle Financing + Technology Financing + Licensing + Insurance + Maintenance + Cleaning + Fuel + Profit + Labor) / Capacity Utilization Rate 

compute_fpm <- function(i){
  
  # Pull specific values from the inputs list and save them as variables 
  capacity_util_rate = i$capacity_util_rate
  mileage_annual     = i$mileage_annual
  av_tech_lifespan   = i$av_tech_lifespan
  discount_rate      = i$discount_rate
  
  # * vehicle financing ----
  # Call the compute_financing function below to calculate the financing cost for the vehicle
  
  vehicle_financing <- compute_financing(
    price                      =   i$vehicle_price, 
    mileage_annual             =   i$mileage_annual, 
    lifespan                   =   i$vehicle_lifespan, 
    payment_periods_per_year   =   i$payment_periods_per_year, 
    financing_lifespan         =   i$vehicle_financing_lifespan,
    annual_interest_rate       =   i$annual_interest_rate
  )
  
  # * technology financing ----
  # Call the compute_financing function below to calculate the financing cost for the AV technology (robotaxis only)
  
  tech_financing <- compute_financing(
    price                      =   i$tech_price, 
    mileage_annual             =   i$mileage_annual, 
    lifespan                   =   i$av_tech_lifespan, 
    payment_periods_per_year   =   i$payment_periods_per_year, 
    financing_lifespan         =   i$vehicle_financing_lifespan,
    annual_interest_rate       =   i$annual_interest_rate
  )
  
  
  # * licensing ----
  # Taxis:
  # Call the compute_licensing_taxi function below to calculate the licensing costs for taxi-based licensing scheme
  
  licensing_taxi <- compute_licensing_taxi(
    medallion_system                         = i$medallion_system,
    taxi_medallion_price                     = i$taxi_medallion_price, 
    downpayment_percent                      = i$downpayment_percent,
    downpayment_upfront_percent              = i$downpayment_upfront_percent,
    financing_period_downpayment             = i$financing_period_downpayment,
    financing_period_remainder               = i$financing_period_remainder,
    medallion_interest_rate                  = i$medallion_interest_rate,
    payment_periods_per_year                 = i$payment_periods_per_year,
    medallion_lifespan                       = i$medallion_lifespan,
    taxi_licensing_taxi_medallion_license    = i$taxi_licensing_taxi_medallion_license,    
    taxi_licensing_ground_transportation_tax = i$taxi_licensing_ground_transportation_tax,    
    taxi_licensing_accessibility_fund        = i$taxi_licensing_accessibility_fund,      
    taxi_licensing_advertising_fee           = i$taxi_licensing_advertising_fee,     
    mileage_annual                           = i$mileage_annual,
    months_per_year                          = i$months_per_year
  )
  
  # TNCs (ride-hailing companies):
  # Call the compute_licensing_tnc function below to calculate the licensing costs for TNC-based licensing scheme
  
  licensing_tnc <- compute_licensing_tnc(
    tnc_licensing_admin_fee_per_year         =   i$tnc_licensing_admin_fee_per_year,      
    tnc_licensing_admin_fee_per_trip         =   i$tnc_licensing_admin_fee_per_trip,       
    tnc_licensing_ground_transport           =   i$tnc_licensing_ground_transport,             
    tnc_licensing_access_fund                =   i$tnc_licensing_access_fund,     
    tnc_licensing_advertising_fee            =   i$tnc_licensing_advertising_fee, 
    mileage_annual                           =   i$mileage_annual,
    fleet_size                               =   i$fleet_size,
    miles_per_trip                           =   i$miles_per_trip
  ) 
  
  # * insurance ----
  # Call the compute_insurance_av function below to calculate the insurance costs 
  
  insurance <- compute_insurance_av(
    vehicle_operations_insurance            =   i$vehicle_operations_insurance,
    mileage_annual                          =   i$mileage_annual,
    av_operations_factor_insurance          =   i$av_operations_factor_insurance,
    months_per_year                         =   i$months_per_year
  )
  
  # * maintenance ----
  # Call the compute_maintenance_av function below to calculate the maintenance costs 
  
  maintenance <- compute_maintenance_av(
    vehicle_operations_maintenance     =   i$vehicle_operations_maintenance,
    av_operations_factor_maintenance   =   i$av_operations_factor_maintenance
  )
  
  # * cleaning ----
  # Call the compute_cleaning function below to calculate cleaning costs
  
  cleaning <- compute_cleaning(
    is_taxi                 = i$is_taxi,
    weeks_per_year          = i$weeks_per_year,
    exterior_cleaning_price = i$exterior_cleaning_price,
    interior_cleaning_price = i$interior_cleaning_price,
    mileage_annual
  )
  
  # * fuel ----
  # Call the compute_fuel_cost_av function below to calculate the fuel costs
  
  fuel <- compute_fuel_cost_av(
    mileage_annual             =   i$mileage_annual,
    fuel_efficiency            =   i$fuel_efficiency, 
    fuel_cost_per_gal          =   i$fuel_cost_per_gal, 
    av_operations_factor_fuel  =   i$av_operations_factor_fuel
  )
  
  
  # * labor ----
  # Call the different labor computation functions below to calculate the labor costs
  
  labor <- if_else(i$is_taxi == 1,
                   ## * taxi labor ----
                   compute_labor_taxi(
                     overhead_rate                                = i$overhead_rate,
                     # wage
                     wage_dispatcher                              = i$wage_dispatcher,
                     wage_driver                                  = i$wage_driver,
                     # shift length
                     shift_length_dispatcher                      = i$shift_length_dispatcher,
                     shift_length_driver                          = i$shift_length_driver,
                     # shifts per day
                     shifts_per_day_dispatcher                    = i$shifts_per_day_dispatcher,
                     shifts_per_day_driver                        = i$shifts_per_day_driver,
                     # shift days per year
                     shift_days_per_year_dispatcher               = i$shift_days_per_year_dispatcher,
                     shift_days_per_year_driver                   = i$shift_days_per_year_driver,
                     # vehicles per cluster
                     vehicles_per_cluster_dispatcher              = i$vehicles_per_cluster_dispatcher,
                     vehicles_per_cluster_driver                  = i$vehicles_per_cluster_driver,
                     # workers per cluster
                     workers_per_cluster_shift_dispatcher         = i$workers_per_cluster_shift_dispatcher,
                     workers_per_cluster_shift_driver             = i$workers_per_cluster_shift_driver,
                     mileage_annual                               = i$mileage_annual
                   ), 
                   ## * av_labor ----
                   compute_labor_av(
                     overhead_rate                                = i$overhead_rate,
                     # wage
                     wage_monitor                                 = i$wage_monitor,
                     wage_fieldsupport                            = i$wage_fieldsupport,
                     wage_customersupport                         = i$wage_customersupport,
                     wage_coordinator                             = i$wage_coordinator,
                     # shift length
                     shift_length_monitor                         = i$shift_length_monitor,
                     shift_length_fieldsupport                    = i$shift_length_fieldsupport,
                     shift_length_customersupport                 = i$shift_length_customersupport,
                     shift_length_coordinator                     = i$shift_length_coordinator,
                     # shifts per day
                     shifts_per_day_monitor                       = i$shifts_per_day_monitor,
                     shifts_per_day_fieldsupport                  = i$shifts_per_day_fieldsupport,
                     shifts_per_day_customersupport               = i$shifts_per_day_customersupport,
                     shifts_per_day_coordinator                   = i$shifts_per_day_coordinator, 
                     # shift days per year
                     shift_days_per_year_monitor                  = i$shift_days_per_year_monitor,
                     shift_days_per_year_fieldsupport             = i$shift_days_per_year_fieldsupport,
                     shift_days_per_year_customersupport          = i$shift_days_per_year_customersupport,
                     shift_days_per_year_coordinator              = i$shift_days_per_year_coordinator,
                     # vehicles per cluster
                     vehicles_per_cluster_monitor                 = i$vehicles_per_cluster_monitor,
                     vehicles_per_cluster_fieldsupport            = i$vehicles_per_cluster_fieldsupport,
                     vehicles_per_cluster_customersupport         = i$vehicles_per_cluster_customersupport,
                     vehicles_per_cluster_coordinator             = i$vehicles_per_cluster_coordinator,
                     # workers per cluster
                     workers_per_cluster_shift_monitor            = i$workers_per_cluster_shift_monitor,
                     workers_per_cluster_shift_fieldsupport       = i$workers_per_cluster_shift_fieldsupport,
                     workers_per_cluster_shift_customersupport    = i$workers_per_cluster_shift_customersupport,
                     workers_per_cluster_shift_coordinator        = i$workers_per_cluster_shift_coordinator,
                     mileage_annual                               = i$mileage_annual
                   )
  )
  
  # * fare per mile ----
  
  fare_per_mile = (vehicle_financing + tech_financing + licensing_taxi + insurance + maintenance + cleaning + fuel + labor + i$profit + i$general_and_admin) / capacity_util_rate
  
  # * cost categories
  
  categories  = list(vehicle_financing = vehicle_financing, tech_financing = tech_financing, licensing = licensing_taxi, insurance = insurance, maintenance = maintenance, fuel = fuel, labor = labor, profit = i$profit, cleaning = cleaning, general_and_admin = i$general_and_admin)
  
  return(list(fare_per_mile = fare_per_mile, categories = categories))
}


# * vehicle & technology financing ----

compute_financing <- function(
    price, 
    mileage_annual, 
    lifespan, 
    payment_periods_per_year, 
    financing_lifespan,
    annual_interest_rate
) {
  n                    <- payment_periods_per_year * financing_lifespan
  i                    <- annual_interest_rate / payment_periods_per_year
  monthly_loan_payment <- price / ((((1 + i)^n) - 1) / (i * (1 + i)^n))
  payments_total       <- monthly_loan_payment * n
  cost                 <- (payments_total) / (mileage_annual * lifespan)
  return(cost)
}


# * licensing ----
compute_medallion_financing <- function(
    taxi_medallion_price, 
    downpayment_percent,
    downpayment_upfront_percent,
    financing_period_downpayment,
    financing_period_remainder,
    medallion_interest_rate,
    payment_periods_per_year,
    medallion_lifespan
){
  downpayment_total      <-  downpayment_percent * taxi_medallion_price
  downpayment_upfront    <-  downpayment_upfront_percent * downpayment_total
  downpayment_remainder  <-  downpayment_total - downpayment_upfront
  loan_remainder         <-  taxi_medallion_price - downpayment_total
  n_d                    <-  payment_periods_per_year * financing_period_downpayment
  n_r                    <-  payment_periods_per_year * financing_period_remainder
  i                      <-  medallion_interest_rate / payment_periods_per_year
  
  downpayment_monthly_payment           <- downpayment_remainder / ((((1 + i)^n_d) - 1) / (i * (1 + i)^n_d))
  loan_monthly_payment                  <- loan_remainder / ((((1 + i)^n_r) - 1) / (i * (1 + i)^n_r))
  
  payments_total                        <- downpayment_upfront + (downpayment_monthly_payment * n_d) + (loan_monthly_payment * n_r)
  medallion_financing                   <- payments_total / medallion_lifespan
}

# Taxis:
compute_licensing_taxi <- function(
    medallion_system,
    taxi_medallion_price, 
    downpayment_percent,
    downpayment_upfront_percent,
    financing_period_downpayment,
    financing_period_remainder,
    medallion_interest_rate,
    payment_periods_per_year,
    medallion_lifespan,
    taxi_licensing_taxi_medallion_license,
    taxi_licensing_ground_transportation_tax,
    taxi_licensing_accessibility_fund,
    taxi_licensing_advertising_fee,
    mileage_annual, 
    months_per_year
) {
  if(medallion_system == "n"){
    license_annual_taxi <- taxi_licensing_taxi_medallion_license + (taxi_licensing_ground_transportation_tax * months_per_year) + (taxi_licensing_accessibility_fund * months_per_year) + taxi_licensing_advertising_fee
  } else {
    license_annual_taxi <- compute_medallion_financing(
      taxi_medallion_price         = taxi_medallion_price, 
      downpayment_percent          = downpayment_percent,
      downpayment_upfront_percent  = downpayment_upfront_percent,
      financing_period_downpayment = financing_period_downpayment,
      financing_period_remainder   = financing_period_remainder,
      medallion_interest_rate      = medallion_interest_rate,
      payment_periods_per_year     = payment_periods_per_year,
      medallion_lifespan           = medallion_lifespan
    )
  }
  licensing_taxi      <- license_annual_taxi / mileage_annual
  return(licensing_taxi)
}

# TNCs (ride-hailing companies): 
compute_licensing_tnc <- function(
    tnc_licensing_admin_fee_per_year,
    tnc_licensing_admin_fee_per_trip,
    tnc_licensing_ground_transport,      
    tnc_licensing_access_fund,
    tnc_licensing_advertising_fee,
    mileage_annual,
    fleet_size,
    miles_per_trip
) {
  licensing_tnc <- (tnc_licensing_admin_fee_per_year / (mileage_annual * fleet_size)) + 
    ((tnc_licensing_admin_fee_per_trip + tnc_licensing_ground_transport + tnc_licensing_access_fund) / miles_per_trip) +
    (tnc_licensing_advertising_fee / mileage_annual)
  return(licensing_tnc)
}

# * insurance ----

compute_insurance_taxi <- function(
    vehicle_operations_insurance, 
    mileage_annual, 
    months_per_year
) {
  insurance_taxi <- (vehicle_operations_insurance * months_per_year) / mileage_annual
  return(insurance_taxi)
}

compute_insurance_av <- function(
    vehicle_operations_insurance, 
    mileage_annual, 
    av_operations_factor_insurance, 
    months_per_year
) {
  insurance_av <- ((vehicle_operations_insurance * months_per_year) / mileage_annual) * av_operations_factor_insurance
  return(insurance_av)
}

# * maintenance ----

compute_maintenance_av <- function(
    vehicle_operations_maintenance, 
    av_operations_factor_maintenance
) {
  vehicle_operations_maintenance * av_operations_factor_maintenance
}


# * cleaning ----

compute_cleaning <- function(
    is_taxi,
    weeks_per_year,
    exterior_cleaning_price,
    interior_cleaning_price,
    mileage_annual
){
  if(is_taxi == 1){
    exterior = weeks_per_year * exterior_cleaning_price
    interior = (365/2) * interior_cleaning_price 
    cleaning_cost = (exterior + interior)/mileage_annual
    
  } else{
    cleaning_cost = 0
  }
  return(cleaning_cost)
}

# * fuel ----

compute_fuel_cost_taxi <- function(
    mileage_annual, 
    fuel_efficiency, 
    fuel_cost_per_gal
) {
  annual_cost_fuel <- (mileage_annual / fuel_efficiency * fuel_cost_per_gal)
  fuel_cost        <- (annual_cost_fuel / mileage_annual) 
  return(fuel_cost)
}


compute_fuel_cost_av <- function(
    mileage_annual, 
    fuel_efficiency, 
    fuel_cost_per_gal, 
    av_operations_factor_fuel
) {
  annual_cost_fuel <- (mileage_annual / fuel_efficiency * fuel_cost_per_gal)
  fuel_cost        <- ((annual_cost_fuel / mileage_annual) * av_operations_factor_fuel)
  return(fuel_cost)
}


# * labor ----
compute_worker_cost_per_mile <- function(
    include_overhead,
    workers_per_cluster_shift, 
    shifts_per_day, 
    shift_days_per_year, 
    shift_length, 
    wage, 
    overhead_rate, 
    vehicles_per_cluster,
    mileage_annual
){
  
  workers_per_cluster_day   <- workers_per_cluster_shift * shifts_per_day
  
  if (include_overhead == TRUE) {
    worker_cost_per_cluster <- (shift_days_per_year * 1 * shift_length * wage * overhead_rate) +
      (shift_days_per_year * (workers_per_cluster_day - 1) * shift_length * wage)
  } else {
    worker_cost_per_cluster <- shift_days_per_year * workers_per_cluster_day * shift_length * wage
  }
  
  miles_per_cluster        <- mileage_annual * vehicles_per_cluster
  worker_cost_per_mile     <- worker_cost_per_cluster / miles_per_cluster
  return(worker_cost_per_mile)
}

compute_labor_av <- function(
    # Labor costs
  overhead_rate,
  # Wage
  wage_monitor,
  wage_fieldsupport,
  wage_customersupport,
  wage_coordinator,
  # shift length
  shift_length_monitor,
  shift_length_fieldsupport,
  shift_length_customersupport,
  shift_length_coordinator,
  # shifts per day
  shifts_per_day_monitor,
  shifts_per_day_fieldsupport,
  shifts_per_day_customersupport,
  shifts_per_day_coordinator,
  # shift days per year
  shift_days_per_year_monitor,
  shift_days_per_year_fieldsupport,
  shift_days_per_year_customersupport,
  shift_days_per_year_coordinator,
  # vehicles per cluster
  vehicles_per_cluster_monitor,
  vehicles_per_cluster_fieldsupport,
  vehicles_per_cluster_customersupport,
  vehicles_per_cluster_coordinator,
  # vehicles per cluster shift
  workers_per_cluster_shift_monitor,     
  workers_per_cluster_shift_fieldsupport,
  workers_per_cluster_shift_customersupport,
  workers_per_cluster_shift_coordinator,
  mileage_annual
) {
  
  ## Remote monitors
  cost_monitor         <- compute_worker_cost_per_mile(
    include_overhead          =   TRUE, 
    workers_per_cluster_shift =   workers_per_cluster_shift_monitor, 
    shifts_per_day            =   shifts_per_day_monitor, 
    shift_days_per_year       =   shift_days_per_year_monitor, 
    shift_length              =   shift_length_monitor, 
    wage                      =   wage_monitor, 
    overhead_rate             =   overhead_rate,
    vehicles_per_cluster      =   vehicles_per_cluster_monitor, 
    mileage_annual            =   mileage_annual)
  
  # Field support
  cost_fieldsupport    <- compute_worker_cost_per_mile(
    include_overhead          =   TRUE, 
    workers_per_cluster_shift =   workers_per_cluster_shift_fieldsupport, 
    shifts_per_day            =   shifts_per_day_fieldsupport, 
    shift_days_per_year       =   shift_days_per_year_fieldsupport, 
    shift_length              =   shift_length_fieldsupport, 
    wage                      =   wage_fieldsupport, 
    overhead_rate             =   overhead_rate,
    vehicles_per_cluster      =   vehicles_per_cluster_fieldsupport, 
    mileage_annual            =   mileage_annual)
  
  # Customer support
  cost_customersupport <- compute_worker_cost_per_mile(
    include_overhead          =   TRUE, 
    workers_per_cluster_shift =   workers_per_cluster_shift_customersupport, 
    shifts_per_day            =   shifts_per_day_customersupport, 
    shift_days_per_year       =   shift_days_per_year_customersupport, 
    shift_length              =   shift_length_customersupport, 
    wage                      =   wage_customersupport, 
    overhead_rate             =   overhead_rate,
    vehicles_per_cluster      =   vehicles_per_cluster_customersupport, 
    mileage_annual            =   mileage_annual)
  
  # Coordinators
  cost_coordinator <- compute_worker_cost_per_mile(
    include_overhead          =   TRUE, 
    workers_per_cluster_shift =   workers_per_cluster_shift_coordinator, 
    shifts_per_day            =   shifts_per_day_coordinator, 
    shift_days_per_year       =   shift_days_per_year_coordinator, 
    shift_length              =   shift_length_coordinator, 
    wage                      =   wage_coordinator, 
    overhead_rate             =   overhead_rate,
    vehicles_per_cluster      =   vehicles_per_cluster_coordinator, 
    mileage_annual            =   mileage_annual)
  
  labor_cost_av         <- cost_monitor + cost_fieldsupport + cost_customersupport + cost_coordinator
  
  return(labor_cost_av)
}

compute_labor_taxi <- function(
    # Labor costs
  overhead_rate,
  # Wage
  wage_dispatcher,
  wage_driver,
  # shift length
  shift_length_dispatcher,
  shift_length_driver,
  # shifts per day
  shifts_per_day_dispatcher,
  shifts_per_day_driver,
  # shift days per year
  shift_days_per_year_dispatcher,
  shift_days_per_year_driver,
  # vehicles per cluster
  vehicles_per_cluster_dispatcher,
  vehicles_per_cluster_driver,
  # vehicles per cluster shift
  workers_per_cluster_shift_dispatcher,
  workers_per_cluster_shift_driver,
  mileage_annual
) {
  
  
  ## Dispatcher
  cost_dispatcher         <- compute_worker_cost_per_mile(
    include_overhead          =   TRUE, 
    workers_per_cluster_shift =   workers_per_cluster_shift_dispatcher, 
    shifts_per_day            =   shifts_per_day_dispatcher, 
    shift_days_per_year       =   shift_days_per_year_dispatcher, 
    shift_length              =   shift_length_dispatcher, 
    wage                      =   wage_dispatcher, 
    overhead_rate             =   overhead_rate,
    vehicles_per_cluster      =   vehicles_per_cluster_dispatcher, 
    mileage_annual            =   mileage_annual)
  
  ## Driver
  cost_driver         <- compute_worker_cost_per_mile(
    include_overhead          =   FALSE, 
    workers_per_cluster_shift =   workers_per_cluster_shift_driver, 
    shifts_per_day            =   shifts_per_day_driver, 
    shift_days_per_year       =   shift_days_per_year_driver, 
    shift_length              =   shift_length_driver, 
    wage                      =   wage_driver, 
    overhead_rate             =   overhead_rate,
    vehicles_per_cluster      =   vehicles_per_cluster_driver, 
    mileage_annual            =   mileage_annual)
  
  labor_cost_taxi         <- cost_dispatcher + cost_driver
  
  return(labor_cost_taxi)
}

# Monte Carlo simulation ----
# Function to perform the fare per mile calculation for each random draw

conduct_mc_analysis <- function(draws_inputs){
  # Extract the variable names for MC analysis
  draws_inputs_names <- names(draws_inputs) 
  
  fare_per_mile      <- list()
  vehicle_financing  <- list()
  tech_financing     <- list()
  licensing          <- list()
  insurance          <- list()
  maintenance        <- list()
  cleaning           <- list()
  fuel               <- list()
  general_and_admin  <- list()
  labor              <- list()
  
  for (i in 1:N) {
    # MC inputs
    inputs_mc <- baseline_inputs
    
    # Pull draws from each of the variables that has distributions
    for(j in 1:length(draws_inputs_names)){
      draws_name <- draws_inputs_names[j]
      inputs_mc[[draws_name]] <- draws_inputs[[draws_name]][i]
    }
    # Compute the cost categories
    out <- compute_fpm(inputs_mc)
    
    # Extract the fare per mile and categories from the output
    fare_per_mile[[i]]     <- out$fare_per_mile
    vehicle_financing[[i]] <- out$categories$vehicle_financing
    tech_financing[[i]]    <- out$categories$tech_financing
    licensing[[i]]         <- out$categories$licensing 
    insurance[[i]]         <- out$categories$insurance  
    maintenance[[i]]       <- out$categories$maintenance
    cleaning[[i]]          <- out$categories$cleaning
    fuel[[i]]              <- out$categories$fuel
    general_and_admin[[i]] <- out$categories$general_and_admin
    labor[[i]]             <- out$categories$labor  
  }
  
  fare_per_mile        <- unlist(fare_per_mile)
  categories      <- data.frame(
    vehicle_financing   = unlist(vehicle_financing),
    tech_financing      = unlist(tech_financing),
    licensing           = unlist(licensing),
    insurance           = unlist(insurance),
    maintenance         = unlist(maintenance),
    cleaning            = unlist(cleaning),
    fuel                = unlist(fuel),
    general_and_admin   = unlist(general_and_admin),
    labor               = unlist(labor)
  )
  
  return(list(fare_per_mile = fare_per_mile, categories = categories))
}

# Store the category costs for each draw
extract_categories <- function(out, scenario){
  categories <- data.frame(
    vehicle_financing   = out$categories$vehicle_financing,
    tech_financing      = out$categories$tech_financing,
    licensing           = out$categories$licensing,
    insurance           = out$categories$insurance,
    maintenance         = out$categories$maintenance,
    cleaning            = out$categories$cleaning,
    fuel                = out$categories$fuel,
    general_and_admin   = out$categories$general_and_admin,
    labor               = out$categories$labor
  ) %>% 
    pivot_longer(
      cols = c(
        'vehicle_financing','tech_financing', 'licensing', 'insurance', 'maintenance', 'cleaning', 'fuel', 'general_and_admin', 'labor'
      ),
      names_to = "category",
      values_to = "val"
    ) %>%
    mutate(
      val = as.double(val),
      category = as.factor(category),
      scenario = scenario
    )
  return(categories)
}

# Exploring sensitive variables over ranges ----

conduct_variable_testing_mc <- function(
    draws_baseline_inputs,
    variable_name,
    variable_range
){
  
  # Create a data frame to store the results
  results <- data.frame(
    val          = NA,
    mean_fpm  = NA,
    quantile_low = NA,
    quantile_high = NA
  )
  # Set values for all inputs
  inputs <- draws_baseline_inputs
  
  # Compute the fare per mile over a range for the specified variable
  for (i in 1:length(variable_range)) {
    
    # Change input value and compute fare per mile
    inputs[[variable_name]] <- array(rep(variable_range[i], N))
    out <- conduct_mc_analysis(inputs)
    
    # Store results
    results[i,]$val           <- variable_range[i]  
    results[i,]$mean_fpm      <- mean(out$fare_per_mile)
    results[i,]$quantile_low  <- quantile(out$fare_per_mile, 0.0275)
    results[i,]$quantile_high <- quantile(out$fare_per_mile, 0.975)
    
  }
  return(results)
}


# Estimate worker numbers & wage distribution ----

# Function to calculate the number of workers for each system
compute_worker_dist <- function(i, fleet){
  
  # AV labor - Calculate the number of workers for robotaxis
  num_monitor         <- ceiling(((i$workers_per_cluster_shift_monitor * i$shifts_per_day_monitor) / i$vehicles_per_cluster_monitor) * fleet)
  num_field_support   <- ceiling(((i$workers_per_cluster_shift_fieldsupport * i$shifts_per_day_fieldsupport) / i$vehicles_per_cluster_monitor) * fleet)
  num_customersupport <- ceiling(((i$workers_per_cluster_shift_customersupport * i$shift_length_customersupport) / i$vehicles_per_cluster_monitor) * fleet)
  num_coordinator     <- ceiling(((i$workers_per_cluster_shift_coordinator * i$shifts_per_day_coordinator) / i$vehicles_per_cluster_coordinator) * fleet)
  num_total_av        <- num_monitor + num_field_support + num_customersupport + num_coordinator
  
  # Taxi labor - Calculate the number of workers for traditional taxis
  num_driver          <- ceiling(((i$workers_per_cluster_shift_driver * i$shifts_per_day_driver) / i$vehicles_per_cluster_driver) * fleet)
  num_dispatcher      <- ceiling(((i$workers_per_cluster_shift_dispatcher * i$shifts_per_day_dispatcher) / i$vehicles_per_cluster_dispatcher) * fleet)
  num_total_taxi      <- num_driver + num_dispatcher
  
  wage_dist_av  <- data.frame(wage = c(i$wage_monitor, i$wage_fieldsupport, i$wage_customersupport, i$wage_coordinator),
                              count = c(num_monitor, num_field_support, num_customersupport, num_coordinator)) %>% 
    mutate(
      system = "av", 
      total = num_total_av
    )
  
  wage_dist_taxi <- data.frame(wage = c(i$wage_driver, i$wage_dispatcher),
                               count = c(num_driver, num_dispatcher)) %>% 
    mutate(
      system = "taxi",
      total = num_total_taxi
    )
  
  wage_dist <- rbind(wage_dist_av, wage_dist_taxi)
  return(wage_dist)
}

# Function to compute the worker numbers and wage distributions for each Monte Carlo simulation draw
compute_worker_dist_mc <- function(
    draws_inputs,
    fleet
){
  # Extract the variable names for MC analysis
  draws_inputs_names <- names(draws_inputs) 
  result <- list()
  
  for (i in 1:N) { 
    # MC inputs
    inputs_mc <- baseline_inputs
    
    # Pull draws from each of the variables that has distributions
    for(j in 1:length(draws_inputs_names)){
      draws_name <- draws_inputs_names[j]
      inputs_mc[[draws_name]] <- draws_inputs[[draws_name]][i]
    }
    # Compute the wage distribution
    result[[i]] <- compute_worker_dist(inputs_mc, fleet = fleet) %>% 
      mutate(iter = i)
    
  } 
  
  # Merge all iterations together
  result <- do.call(bind_rows, result)
  total_workers <- result %>%
    distinct(iter, system, total) %>% 
    group_by(system) %>% 
    summarise(total_workers = sum(total) / N)
  total_workers_av <- total_workers %>% 
    filter(system == "av") %>% 
    pull(total_workers)
  total_workers_taxi <- total_workers %>% 
    filter(system == "taxi") %>% 
    pull(total_workers)
  
  # Create bins for wage ranges 
  wage_increment <- 1
  wage_lb_ranges <- seq(0, 50, wage_increment)
  wage_bins_av   <- tibble(lb = wage_lb_ranges, count = 0)
  wage_bins_taxi <- wage_bins_av
  
  for (i in seq_len(length(wage_lb_ranges))) {
    lb <- wage_lb_ranges[i]
    ub <- lb + wage_increment 
    
    wage_bins_av[i,]$count <- result %>% 
      filter(system == "av" & wage > lb & wage <= ub) %>% 
      summarise(count = sum(count) / N) %>% 
      pull(count)
    
    wage_bins_taxi[i,]$count <- result %>% 
      filter(system == "taxi", wage > lb & wage <= ub) %>% 
      summarise(count = sum(count) / N) %>% 
      pull(count)
    
  }
  
  wage_bins_av <- wage_bins_av %>% 
    mutate(
      p = count / total_workers_av, 
      system = 'av'
    )
  wage_bins_taxi <- wage_bins_taxi %>% 
    mutate(
      p = count / total_workers_taxi, 
      system = 'taxi'
    )
  # Combine data frames
  wage_bins <- rbind(wage_bins_av, wage_bins_taxi)
  
  return(wage_bins = wage_bins)
  
}
