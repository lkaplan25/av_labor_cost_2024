#################################################################################################
# About this file
#################################################################################################
# 
# This script contains functions that are called by the analysis.R script. These functions include
# calculations for each of the cost model components, as well as the calculation of the total 
# automation budget.
#
#################################################################################################

# Compute Automation Budget ----
# This function computes the total automation budget using the following equation:
# Automation Budget = (Fare per Mile  * Capacity Utilization Rate) - Vehicle Financing - Licensing - Insurance - Maintenance - Fuel - Labor 

compute_automation_budget <- function(i){
  
  # Pull specific values from the inputs list and save them as variables 
  fare_per_mile      = i$fare_per_mile
  capacity_util_rate = i$capacity_util_rate
  mileage_annual     = i$mileage_annual
  av_tech_lifespan   = i$av_tech_lifespan
  discount_rate      = i$discount_rate
  
  # * vehicle financing ----
  # Call the compute_financing function below to calculate the financing cost
 
   vehicle_financing <- compute_financing(
    vehicle_price              =   i$vehicle_price, 
    mileage_annual             =   i$mileage_annual, 
    vehicle_lifespan           =   i$vehicle_lifespan, 
    payment_periods_per_year   =   i$payment_periods_per_year, 
    vehicle_financing_lifespan =   i$vehicle_financing_lifespan,
    annual_interest_rate       =   i$annual_interest_rate
  )

  # * licensing ----
  # Taxis:
  # Call the compute_licensing_taxi function below to calculate the licensing costs for taxis
  
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
  # Call the compute_licensing_tnc function below to calculate the licensing costs for TNCs
  
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
  # Call the compute_insurance_taxi function below to calculate the insurance costs for conventional taxis
  
  insurance_taxi <- compute_insurance_taxi(
    vehicle_operations_insurance            =  i$vehicle_operations_insurance, 
    mileage_annual                          =  i$mileage_annual, 
    months_per_year                         =  i$months_per_year
    )
  
  # Call the compute_insurance_av function below to calculate the insurance costs for robotaxis
  
  insurance_av <- compute_insurance_av(
    vehicle_operations_insurance            =   i$vehicle_operations_insurance,
    mileage_annual                          =   i$mileage_annual,
    av_operations_factor_insurance          =   i$av_operations_factor_insurance,
    months_per_year                         =   i$months_per_year
    )
  
  # * maintenance ----
  # Call the compute_maintenance_av function below to calculate the maintenance costs 
  
  maintenance_av <- compute_maintenance_av(
   vehicle_operations_maintenance     =   i$vehicle_operations_maintenance,
   av_operations_factor_maintenance   =   i$av_operations_factor_maintenance
    )
  
  # * fuel ----
  # Call the compute_fuel_cost_av function below to calculate the fuel costs
  
  fuel_cost <- compute_fuel_cost_av(
    mileage_annual             =   i$mileage_annual,
    fuel_efficiency            =   i$fuel_efficiency, 
    fuel_cost_per_gal          =   i$fuel_cost_per_gal, 
    av_operations_factor_fuel  =   i$av_operations_factor_fuel
    )
  
  # * labor ----
  # Call the different labor computation functions below to calculate the labor costs
  
  labor_cost_av <- compute_labor_av(
    overhead_rate                                = i$overhead_rate,
    
    # wage
    wage_monitor                                 = i$wage_monitor,
    wage_fieldsupport                            = i$wage_fieldsupport,
    wage_customersupport                         = i$wage_customersupport,
    wage_cleaner_basic                           = i$wage_cleaner_basic,
    wage_cleaner_deep                            = i$wage_cleaner_deep,
    
    # shift length
    shift_length_monitor                         = i$shift_length_monitor,
    shift_length_fieldsupport                    = i$shift_length_fieldsupport,
    shift_length_customersupport                 = i$shift_length_customersupport,
    shift_length_cleaner_basic                   = i$shift_length_cleaner_basic,
    shift_length_cleaner_deep                    = i$shift_length_cleaner_deep,
    
    # shifts per day
    shifts_per_day_monitor                       = i$shifts_per_day_monitor,
    shifts_per_day_fieldsupport                  = i$shifts_per_day_fieldsupport,
    shifts_per_day_customersupport               = i$shifts_per_day_customersupport,
    shifts_per_day_cleaner_basic                 = i$shifts_per_day_cleaner_basic,
    shifts_per_day_cleaner_deep                  = i$shifts_per_day_cleaner_deep,
    
    # shift days per year
    shift_days_per_year_monitor                  = i$shift_days_per_year_monitor,
    shift_days_per_year_fieldsupport             = i$shift_days_per_year_fieldsupport,
    shift_days_per_year_customersupport          = i$shift_days_per_year_customersupport,
    shift_days_per_year_cleaner_basic            = i$shift_days_per_year_cleaner_basic,
    shift_days_per_year_cleaner_deep             = i$shift_days_per_year_cleaner_deep,
    
    # vehicles per cluster
    vehicles_per_cluster_monitor                 = i$vehicles_per_cluster_monitor,
    vehicles_per_cluster_fieldsupport            = i$vehicles_per_cluster_fieldsupport,
    vehicles_per_cluster_customersupport         = i$vehicles_per_cluster_customersupport,
    vehicles_per_cluster_cleaner_basic           = i$vehicles_per_cluster_cleaner_basic,
    vehicles_per_cluster_cleaner_deep            = i$vehicles_per_cluster_cleaner_deep,
    
    # workers per cluster
    workers_per_cluster_shift_monitor            = i$workers_per_cluster_shift_monitor,
    workers_per_cluster_shift_fieldsupport       = i$workers_per_cluster_shift_fieldsupport,
    workers_per_cluster_shift_customersupport    = i$workers_per_cluster_shift_customersupport,
    workers_per_cluster_shift_cleaner_basic      = i$workers_per_cluster_shift_cleaner_basic,
    workers_per_cluster_shift_cleaner_deep       = i$workers_per_cluster_shift_cleaner_deep,
    mileage_annual                               = i$mileage_annual
  )
  
  # * profit ----
  # Set the profit equal to zero to calculate breakeven cost. 
  
  profit <- 0

  # * NPV ---- 
  # calculate net present value of the total automation budget 
  
  cash_flow <- list()
  
  for (yr in 1:av_tech_lifespan){
    if (yr <= i$vehicle_lifespan) {
    # calculate annual automation budget on a per-vehicle basis WITH vehicle financing
      budget_per_mile <- (( fare_per_mile *  capacity_util_rate) - vehicle_financing - licensing_taxi - insurance_av - maintenance_av - fuel_cost - profit - labor_cost_av)
      cash_flow[yr] <- (budget_per_mile *  mileage_annual)
    }
    else{
    # calculate annual automation budget on a per-vehicle basis for the remaining years WITH NO vehicle financing
      budget_per_mile <- (( fare_per_mile *  capacity_util_rate) - 0 - licensing_taxi - insurance_av - maintenance_av - fuel_cost - profit - labor_cost_av)
      cash_flow[yr] <- (budget_per_mile *  mileage_annual)
    }
  }

  cash_flow <- unlist(cash_flow)
  total_automation_budget_npv <- NPV(cf0 = 0, cf = cash_flow, times = c(1:av_tech_lifespan), i = discount_rate)
  budget_per_mile <- (( fare_per_mile *  capacity_util_rate) - vehicle_financing - licensing_taxi - insurance_av - maintenance_av - fuel_cost - profit - labor_cost_av)
  
  # * Cost categories ----
  
  cost_categories = list(financing = vehicle_financing, licensing = licensing_taxi, insurance = insurance_av, maintenance = maintenance_av, fuel = fuel_cost, labor = labor_cost_av)
  
  return(list(budget = total_automation_budget_npv, categories = cost_categories, budget_per_mile = budget_per_mile))
}


# * vehicle financing ----

compute_financing <- function(
    vehicle_price, 
    mileage_annual, 
    vehicle_lifespan, 
    payment_periods_per_year, 
    vehicle_financing_lifespan,
    annual_interest_rate
) {
  n                    <- payment_periods_per_year * vehicle_financing_lifespan
  i                    <- annual_interest_rate / payment_periods_per_year
  monthly_loan_payment <- vehicle_price / ((((1 + i)^n) - 1) / (i * (1 + i)^n))
  payments_total       <- monthly_loan_payment * n
  cost                 <- (payments_total) / (mileage_annual * vehicle_lifespan)
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
      taxi_medallion_price        = taxi_medallion_price, 
      downpayment_percent          = downpayment_percent,
      downpayment_upfront_percent  =  downpayment_upfront_percent,
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

# * fuel ----

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
  wage_cleaner_basic,
  wage_cleaner_deep,
  # shift length
  shift_length_monitor,
  shift_length_fieldsupport,
  shift_length_customersupport,
  shift_length_cleaner_basic,
  shift_length_cleaner_deep,
  # shifts per day
  shifts_per_day_monitor,
  shifts_per_day_fieldsupport,
  shifts_per_day_customersupport,
  shifts_per_day_cleaner_basic,
  shifts_per_day_cleaner_deep,
  # shift days per year
  shift_days_per_year_monitor,
  shift_days_per_year_fieldsupport,
  shift_days_per_year_customersupport,
  shift_days_per_year_cleaner_basic,
  shift_days_per_year_cleaner_deep,
  # vehicles per cluster
  vehicles_per_cluster_monitor,
  vehicles_per_cluster_fieldsupport,
  vehicles_per_cluster_customersupport,
  vehicles_per_cluster_cleaner_basic,
  vehicles_per_cluster_cleaner_deep,
  # vehicles per cluster shift
  workers_per_cluster_shift_monitor,     
  workers_per_cluster_shift_fieldsupport,
  workers_per_cluster_shift_customersupport,
  workers_per_cluster_shift_cleaner_basic,
  workers_per_cluster_shift_cleaner_deep,
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

  ## Cleaners
  # Basic Cleaning
  cost_cleaner_basic   <- compute_worker_cost_per_mile(
    include_overhead           =   FALSE, 
    workers_per_cluster_shift =   workers_per_cluster_shift_cleaner_basic, 
    shifts_per_day            =   shifts_per_day_cleaner_basic, 
    shift_days_per_year       =   shift_days_per_year_cleaner_basic, 
    shift_length              =   shift_length_cleaner_basic, 
    wage                      =   wage_cleaner_basic, 
    overhead_rate             =   overhead_rate,
    vehicles_per_cluster      =   vehicles_per_cluster_cleaner_basic,
    mileage_annual            =   mileage_annual
    )

  # Deep Cleaning
  cost_cleaner_deep    <- compute_worker_cost_per_mile(
    include_overhead          =   FALSE, 
    workers_per_cluster_shift =   workers_per_cluster_shift_cleaner_deep, 
    shifts_per_day            =   shifts_per_day_cleaner_deep, 
    shift_days_per_year       =   shift_days_per_year_cleaner_deep, 
    shift_length              =   shift_length_cleaner_deep, 
    wage                      =   wage_cleaner_deep, 
    overhead_rate             =   overhead_rate,
    vehicles_per_cluster      =   vehicles_per_cluster_cleaner_deep,
    mileage_annual            =   mileage_annual
  )

  labor_cost_av         <- cost_monitor + cost_fieldsupport + cost_customersupport + cost_cleaner_basic + cost_cleaner_deep

  return(labor_cost_av)
}

conduct_mc_analysis <- function(draws_inputs){
  # Extract the variable names for MC analysis
  draws_inputs_names <- names(draws_inputs) 

  budget          <- list()
  budget_per_mile <- list()
  financing       <- list()
  licensing       <- list()
  insurance       <- list()
  maintenance     <- list()
  fuel            <- list()
  labor           <- list()

  for (i in 1:N) {
   # MC inputs
    inputs_mc <- baseline_inputs
  
  # Pull draws from each of the variables that has distributions
    for(j in 1:length(draws_inputs_names)){
      draws_name <- draws_inputs_names[j]
      inputs_mc[[draws_name]] <- draws_inputs[[draws_name]][i]
    }
  # Compute the cost categories
  out <- compute_automation_budget(inputs_mc)
  
  # Extract the total budget, budget per mile, and categories from the output  
  budget[[i]]                <- out$budget
  budget_per_mile[[i]]       <- out$budget_per_mile
  financing[[i]]             <- out$categories$financing
  licensing[[i]]             <- out$categories$licensing
  insurance[[i]]             <- out$categories$insurance
  maintenance[[i]]           <- out$categories$maintenance
  fuel[[i]]                  <- out$categories$fuel
  labor[[i]]                 <- out$categories$labor

  } 
  budget          <- unlist(budget)
  budget_per_mile <- unlist(budget_per_mile)
  categories      <- data.frame(
    financing   = unlist(financing),
    licensing   = unlist(licensing),
    insurance   = unlist(insurance),
    maintenance = unlist(maintenance),
    fuel        = unlist(fuel),
    labor       = unlist(labor)
  )
  
  return(list(budget = budget, budget_per_mile = budget_per_mile, categories = categories))
}

# Exploring sensitive variables over ranges ----
# conduct_variable_testing <- function(
#     baseline_inputs,
#     variable_name,
#     variable_range){
#   
#   # Create a data frame to store the results
#   results <- data.frame(
#     val = NA,
#     budget = NA
#   )
#   # Set values for all inputs
#   inputs <- baseline_inputs
#   
#   # Compute the budget over a range for the specified variable
#   for (i in 1:length(variable_range)){
#     # Change input value and compute budget
#     inputs[[variable_name]] <- variable_range[i]
#     out <- compute_automation_budget(inputs)
# 
#     # Store results
#     results[i,]$val <- variable_range[i]  
#     results[i,]$budget <- out$budget
#   }
#   return(results)
# }

# With MC analysis

conduct_variable_testing_mc <- function(
  draws_baseline_inputs,
  variable_name,
  variable_range
){
  
  # Create a data frame to store the results
  results <- data.frame(
    val          = NA,
    mean_budget  = NA,
    quantile_low = NA,
    quantile_high = NA
  )
  # Set values for all inputs
  inputs <- draws_baseline_inputs
  
  # Compute the budget over a range for the specified variable
  for (i in 1:length(variable_range)) {
    
    # Change input value and compute budget
    inputs[[variable_name]] <- array(rep(variable_range[i], N))
    out <- conduct_mc_analysis(inputs)
    
    # Store results
    results[i,]$val          <- variable_range[i]  
    results[i,]$mean_budget  <- mean(out$budget)
    results[i,]$quantile_low <- quantile(out$budget, 0.0275)
    results[i,]$quantile_high <- quantile(out$budget, 0.975)
    
  }
  return(results)
}
