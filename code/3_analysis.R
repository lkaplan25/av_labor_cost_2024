#################################################################################################
# About this file
#################################################################################################
# 
# This script performs all of the analysis calculations including computing the fare per mile values
# for each scenario as a point estimate and as a distribution using Monte Carlo Analysis simulation.
# It also contains code to generate all of the figures included in the manuscript.
#
#################################################################################################

source(here::here('model', 'setup.R'))
source(here::here('model', 'functions.R'))
source(here::here('model', 'inputs.R'))

# Point estimates ----

# AV
out <- compute_fpm(baseline_inputs)
out

# Taxi
out <- compute_fpm(taxi_inputs)
out


# Monte Carlo simulation ----

# Conduct Monte Carlo simulation for each of the scenarios 

# AV Baseline

out  <- conduct_mc_analysis(draws_inputs = draws_baseline_inputs)

mc_base_fpm        <- out$fare_per_mile
mc_base_categories <- extract_categories(out = out, scenario = "baseline")


# Non-AV Taxi
out  <- conduct_mc_analysis(draws_inputs = draws_taxi_inputs)

mc_taxi_fpm        <- out$fare_per_mile
mc_taxi_categories <- extract_categories(out = out, scenario = "taxi")


# AV Advanced Technology

out <- conduct_mc_analysis(draws_inputs = draws_adv_av_tech_inputs)

mc_adv_av_tech_fpm        <- out$fare_per_mile
mc_adv_av_tech_categories <- extract_categories(out = out, scenario = "adv_av_tech")

# AV High Usage 

out <- conduct_mc_analysis(draws_inputs = draws_high_usage_inputs)

mc_high_use_fpm        <- out$fare_per_mile
mc_high_use_categories <- extract_categories(out = out, scenario = "high_use")

# AV Medallion System

out <- conduct_mc_analysis(draws_inputs = draws_medallion_sys_inputs)

mc_medallion_system_fpm        <- out$fare_per_mile
mc_medallion_system_categories <- extract_categories(out = out, scenario = "medallion_sys")

# AV Lower Density City

out <- conduct_mc_analysis(draws_inputs = draws_lower_density_city_inputs)

mc_lower_density_fpm        <- out$fare_per_mile
mc_lower_density_categories <- extract_categories(out = out, scenario = "lower_density")

# Create table to summarize the differences between scenarios

results <- data.frame(
  base_av       = mc_base_fpm, 
  taxi          = mc_taxi_fpm,
  adv_av_tech   = mc_adv_av_tech_fpm,
  high_use      = mc_high_use_fpm,
  medallion_sys = mc_medallion_system_fpm,
  lower_density = mc_lower_density_fpm
) %>%  
  pivot_longer(
    names_to = 'scenario', 
    values_to = 'fare_per_mile', 
    cols = everything()
  ) %>% 
  mutate(
    scenario = as.factor(scenario)
  )

# Reordering scenarios to improve plot aesthetics
results$scenario <- recode_factor(
  results$scenario,
  "medallion_sys" = "Medallion System",
  "high_use" = "High Use",
  "taxi" = "Non-AV Taxi",
  "base_av" = "Baseline AV",
  "adv_av_tech" = "Advanced Tech",
  "lower_density" = "Lower Density City"
)
results$scenario <- fct_relevel(
  results$scenario,
  "Advanced Tech", "High Use", "Lower Density City", "Medallion System",
  "Baseline AV", "Non-AV Taxi"
)

# Create tables of results 
mc_fpm <- results %>% 
  group_by(scenario) %>% 
  summarise(
    mean_fpm = round(mean(fare_per_mile), 2), 
    sd_fpm = round(sd(fare_per_mile), 2)
  )

ft_fpm <- flextable(mc_fpm)
ft_fpm


## Bar Plot ----

# Combine all of the draws for each scenario into one data frame
df <- rbind(mc_taxi_categories,
            mc_base_categories, 
            mc_adv_av_tech_categories,
            mc_high_use_categories, 
            mc_lower_density_categories, 
            mc_medallion_system_categories)%>%
  mutate(
    val = as.double(val),
    category = as.factor(category),
    scenario = as.factor(scenario), 
    draw = rep(1:60000, each = 9)
  )

# Create a data frame that sums up the total cost for each scenario for each of the 10,000 draws
total_cost <- df %>% 
  group_by(draw, scenario) %>% 
  summarise(
    total_cost = sum(val)
  ) %>% 
  group_by(scenario) %>% 
  summarise(
    mean_total_cost = round(mean(total_cost),2),
    sd_total_cost   = round(sd(total_cost),2),
    quantile_05     = round(quantile(total_cost, 0.05),2),
    quantile_95     = round(quantile(total_cost, 0.95),2)
  )

# Create a data frame that summarizes the mean cost per category for each scenario 
cost_by_category <- df %>% 
  group_by(scenario, category) %>% 
  summarise(
    mean_value = round(mean(val),2)
  )

# Recombine the data frames

combined_df <- cost_by_category %>% 
  left_join(total_cost, by = "scenario")

# Reordering scenarios and cost categories to improve plot aesthetics
combined_df$category <- fct_relevel(combined_df$category, "labor", after = Inf) 
combined_df$category <- recode_factor(
  combined_df$category,
  "general_and_admin" = "general & admin",
  "tech_financing" = "tech financing",
  "vehicle_financing" = "vehicle financing"
)
combined_df$scenario <- recode_factor(
  combined_df$scenario,
  "taxi"          = "Non-AV Taxi",
  "medallion_sys" = "AV Medallion System",
  "lower_density" = "AV Lower Density City",
  "high_use" = "AV High Use",
  "baseline" = "AV Baseline",
  "adv_av_tech" = "AV Advanced Tech"
)
combined_df$scenario <- fct_relevel(
  combined_df$scenario,
  "AV Advanced Tech", "AV High Use", "AV Lower Density City",
  "AV Medallion System",  "AV Baseline", "Non-AV Taxi"
)

# Create a table of results
ft_cost_by_category <- flextable(combined_df)
ft_cost_by_category

# Create plot
cost_cat_plot <- combined_df %>% 
  ggplot() +
  geom_bar(aes(x = scenario, y = mean_value, fill = category), stat = "identity", position = "stack", width = 0.5) +
  geom_errorbar(aes(x = scenario, ymin = quantile_05, ymax = quantile_95), width = 0.2) +
  labs(
    title = "Cost by Category",
    subtitle = "Labor is a key cost category across scenarios",
    x = NULL,
    y = "Cost ($/mi)",
    fill = "Category"
  ) +
  scale_fill_brewer(type = "qual", palette = 'Paired', direction = -1) +
  theme_minimal_vgrid() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3), breaks = seq(0,3,0.5)) +
  coord_flip() +
  theme(text = element_text(family = font_main))
cost_cat_plot 


## Monte Carlo Simulation Plot ----

mc_analysis_plot <- results %>% 
  ggplot() +
  geom_boxplot(
    aes(x = fare_per_mile, y = scenario),
    outlier.shape = NA
  ) +
  labs(
    title = "Estimated Fares for Different Scenarios",
    subtitle = "Robotaxis likely to out-compete non-av taxis",
    y = NULL,
    x = "Fare per mile ($/mi)"
  ) +
  theme_minimal_vgrid() +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05)),
    labels = scales::dollar_format(),
    limits = c(0, 7),
    breaks = seq(0, 7, 1)
  ) +
  theme(text = element_text(family = font_main))



# Fare vs. variable range plots ----
# Calculate the automation budget over a range of capacity utilization, VMT, and labor ratio values


## Fare vs. Capacity Utilization ----

# Calculate fare per mile over variable range
fpm_capacity_util_taxi <- conduct_variable_testing_mc(
  draws_baseline_inputs = draws_taxi_inputs,
  variable_name = "capacity_util_rate",
  variable_range = round(seq(0.7, 0.2, by = -0.05), 2)
) %>% 
  mutate(
    mean_fpm = mean_fpm,
    quantile_low = quantile_low,
    quantile_high = quantile_high
  )

fpm_capacity_util_base <- conduct_variable_testing_mc(
  draws_baseline_inputs = draws_baseline_inputs,
  variable_name = "capacity_util_rate",
  variable_range = round(seq(0.7, 0.2, by = -0.05), 2)
) %>% 
  mutate(
    mean_fpm = mean_fpm,
    quantile_low = quantile_low,
    quantile_high = quantile_high
  )

fpm_capacity_util_adv_tech <- conduct_variable_testing_mc(
  draws_baseline_inputs = draws_adv_av_tech_inputs,
  variable_name = "capacity_util_rate",
  variable_range = round(seq(0.7, 0.2, by = -0.05), 2)
) %>% 
  mutate(
    mean_budget = mean_fpm,
    quantile_low = quantile_low,
    quantile_high = quantile_high
  )

# Plot settings
ymin <- min(fpm_capacity_util_adv_tech$quantile_low)
yint_taxi_capacity <- fpm_capacity_util_taxi %>%
  mutate(val = round(val,2)) %>% 
  filter(val == 0.50) %>% 
  pull(mean_fpm)
yint_base_capacity <- fpm_capacity_util_base %>%
  mutate(val = round(val,2)) %>% 
  filter(val == 0.50) %>% 
  pull(mean_fpm)
yint_adv_tech_capacity <- fpm_capacity_util_adv_tech %>%
  mutate(val = round(val,2)) %>% 
  filter(val == 0.50) %>% 
  pull(mean_fpm)
label_size <- 4

# Make plot
plot_capacity_util <- ggplot() +
  geom_hline(yintercept = 0, color = 'black')+
  # Add mean budget estimates over fare range with 95% confidence intervals
  geom_ribbon(
    data = fpm_capacity_util_base,
    aes(x = val, ymin = quantile_low, ymax = quantile_high),
    alpha = 0.2, fill = color_baseline
  ) +
  geom_ribbon(
    data = fpm_capacity_util_taxi,
    aes(x = val, ymin = quantile_low, ymax = quantile_high),
    alpha = 0.2, fill = color_taxi
  ) +
  geom_ribbon(
    data = fpm_capacity_util_adv_tech,
    aes(x = val, ymin = quantile_low, ymax = quantile_high),
    alpha = 0.2, fill = color_advanced
  ) +
  geom_line(
    data = fpm_capacity_util_base,
    aes(x = val, y = mean_fpm), 
    color = color_baseline
  ) +
  geom_line(
    data = fpm_capacity_util_taxi,
    aes(x = val, y = mean_fpm), 
    color = color_taxi
  ) +
  geom_line(
    data = fpm_capacity_util_adv_tech,
    aes(x = val, y = mean_fpm), 
    color = color_advanced
  ) +
  annotate(
    geom = 'text',
    x = 0.6, y = 1.9,
    label = "AV Advanced Tech",
    family = font_main,
    vjust = 1, hjust = 0.5, 
    fontface = 'bold',
    size = label_size,
    color = color_advanced
  ) +
  annotate(
    geom = 'text',
    x = 0.6, y = 3.3,
    label = "AV Baseline",
    family = font_main,
    vjust = 1, hjust = 0.5, 
    fontface = 'bold',
    size = label_size,
    color = color_baseline
  ) +
  annotate(
    geom = 'text',
    x = 0.6, y = 6.1,
    label = "Taxi",
    family = font_main,
    vjust = 1, hjust = 0.5, 
    fontface = 'bold',
    size = label_size,
    color = color_taxi
  ) +
  # Add chart labels and adjust axes 
  labs(
    #title = "Estimated automation budgets across a range of utilization rates",
    y = "Fare per Mile ($/mi)",
    x = "Capacity Utilization Rate"
  ) +
  coord_cartesian(
    xlim = c(0.2, 0.7),
    ylim = c(0, 10)
  ) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(
    breaks=seq(0.2, 0.7, 0.1),
    labels = scales::percent_format()
  ) +
  geom_point(aes(x = 0.50, y = yint_base_capacity), size = 2) +
  geom_point(aes(x = 0.50, y = yint_taxi_capacity), size = 2) +
  geom_point(aes(x = 0.50, y = yint_adv_tech_capacity), size = 2) +
  theme_minimal(base_family = font_main, base_size = 15) +
  theme(
    plot.title = element_text(face="bold"),
    panel.grid.minor = element_blank()
  )
plot_capacity_util


## Fare vs. VMT ----

# Calculate fare per mile over variable range
fpm_mileage_base <- conduct_variable_testing_mc(
  draws_baseline_inputs = draws_baseline_inputs,
  variable_name = "mileage_annual",
  variable_range = round(seq(90000, 10000, by = -5000))
) %>% 
  mutate(
    mean_fpm = mean_fpm,
    quantile_low = quantile_low,
    quantile_high = quantile_high
  )

fpm_mileage_taxi <- conduct_variable_testing_mc(
  draws_baseline_inputs = draws_taxi_inputs,
  variable_name = "mileage_annual",
  variable_range = round(seq(90000, 10000, by = -5000))
) %>% 
  mutate(
    mean_fpm = mean_fpm,
    quantile_low = quantile_low,
    quantile_high = quantile_high
  )

fpm_mileage_adv_tech <- conduct_variable_testing_mc(
  draws_baseline_inputs = draws_adv_av_tech_inputs,
  variable_name = "mileage_annual",
  variable_range = round(seq(90000, 10000, by = -5000))
) %>% 
  mutate(
    mean_fpm = mean_fpm,
    quantile_low = quantile_low,
    quantile_high = quantile_high
  )

# Plot settings
yint_base_vmt <- fpm_mileage_base %>%
  mutate(val = round(val,0)) %>% 
  filter(val == 65000) %>%
  pull(mean_fpm)
yint_taxi_vmt <- fpm_mileage_taxi %>%
  mutate(val = round(val,0)) %>% 
  filter(val == 65000) %>%
  pull(mean_fpm)
yint_adv_tech_vmt <- fpm_mileage_adv_tech %>%
  mutate(val = round(val,0)) %>% 
  filter(val == 65000) %>%
  pull(mean_fpm)
label_size <- 4

# Make plot
plot_mileage <- ggplot() +  
  geom_hline(yintercept = 0, color = 'black')+
  # Add mean budget estimates over fare range with 95% confidence intervals
  geom_ribbon(
    data = fpm_mileage_base,
    aes(x = val, ymin = quantile_low, ymax = quantile_high),
    alpha = 0.2, fill = color_baseline
  ) +
  geom_ribbon(
    data = fpm_mileage_taxi,
    aes(x = val, ymin = quantile_low, ymax = quantile_high),
    alpha = 0.2, fill = color_taxi
  ) +
  geom_ribbon(
    data = fpm_mileage_adv_tech,
    aes(x = val, ymin = quantile_low, ymax = quantile_high),
    alpha = 0.2, fill = color_advanced
  ) +
  geom_line(
    data = fpm_mileage_base,
    aes(x = val, y = mean_fpm), 
    color = color_baseline
  ) +
  geom_line(
    data = fpm_mileage_taxi,
    aes(x = val, y = mean_fpm), 
    color = color_taxi
  ) +
  geom_line(
    data = fpm_mileage_adv_tech,
    aes(x = val, y = mean_fpm), 
    color = color_advanced
  ) +
  annotate(
    geom = 'text',
    x = 40000, y = 3.1,
    label = "AV Advanced Tech",
    family=font_main,
    fontface = 'bold',
    color = color_advanced,
    size = label_size,
    vjust = 1, hjust = 0.5
  ) +
  annotate(
    geom = 'text',
    x = 40000, y = 11,
    label = "Taxi",
    family=font_main,
    fontface = 'bold',
    color = color_taxi,
    size = label_size,
    vjust = 1, hjust = 0.5
  ) +
  annotate(
    geom = 'text',
    x = 40000, y = 6,
    label = "AV Baseline",
    family=font_main,
    fontface = 'bold',
    color = color_baseline,
    size = label_size,
    vjust = 1, hjust = 0.5
  ) +
  # Add chart labels and adjust axes 
  labs(
    y = "Fare per Mile ($/mi)",
    x = "Annual VMT (thousands of miles)"
  ) +
  coord_cartesian(
    xlim = c(10000, 90000),
    ylim = c(0, 20)
  ) +
  scale_y_continuous(labels = scales::dollar_format())+
  scale_x_continuous(
    expand = c(0,1000),
    breaks = seq(10000, 90000, 10000),
    labels = scales::label_comma(scale = 1/1000)
  ) +
  geom_point(aes(x = 65000, y = yint_base_vmt), size = 2) +
  geom_point(aes(x = 65000, y = yint_adv_tech_vmt), size = 2) +
  geom_point(aes(x = 65000, y = yint_taxi_vmt), size = 2) +
  theme_minimal(base_family = font_main, base_size = 15) +
  theme(
    plot.title = element_text(face="bold"), 
    panel.grid.minor = element_blank()
  )
plot_mileage 



## Fare vs. Labor ----

labor_range <- c(seq(1, 5, by = 0.5), seq(7, 31, by = 2), 16, 30)
fpm_labor_base <- conduct_variable_testing_mc(
  draws_baseline_inputs = draws_baseline_inputs,
  variable_name = "vehicles_per_cluster_monitor",
  variable_range = labor_range
) %>%
  mutate(
    mean_fpm = mean_fpm,
    quantile_low = quantile_low,
    quantile_high = quantile_high
  )

fpm_labor_taxi <- conduct_variable_testing_mc(
  draws_baseline_inputs = draws_taxi_inputs,
  variable_name = "vehicles_per_cluster_dispatcher",
  variable_range = labor_range
) %>%
  mutate(
    mean_fpm = mean_fpm,
    quantile_low = quantile_low,
    quantile_high = quantile_high
  )

fpm_labor_adv_tech <- conduct_variable_testing_mc(
  draws_baseline_inputs = draws_adv_av_tech_inputs,
  variable_name = "vehicles_per_cluster_monitor",
  variable_range = labor_range
) %>%
  mutate(
    mean_fpm = mean_fpm,
    quantile_low = quantile_low,
    quantile_high = quantile_high
  )

# Plot settings
yint_base_labor <- fpm_labor_base %>%
  mutate(val = round(val,0)) %>% 
  filter(val == 16) %>%
  pull(mean_fpm)
yint_taxi_labor <- fpm_labor_taxi %>%
  mutate(val = round(val,0)) %>% 
  filter(val == 15) %>%
  pull(mean_fpm)
yint_adv_tech_labor <- fpm_labor_adv_tech %>% 
  mutate(val = round(val,0)) %>% 
  filter(val == 30) %>%
  pull(mean_fpm)
label_size <- 4

# Make plot
plot_labor <- ggplot() +  
  geom_hline(yintercept = 0, color = 'black')+
  # Add mean budget estimates over fare range with 95% confidence intervals
  geom_ribbon(
    data = fpm_labor_base,
    aes(x = val, ymin = quantile_low, ymax = quantile_high),
    alpha = 0.2, fill = color_baseline
  ) +
  geom_ribbon(
    data = fpm_labor_taxi,
    aes(x = val, ymin = quantile_low, ymax = quantile_high),
    alpha = 0.2, fill = color_taxi
  ) +
  geom_ribbon(
    data = fpm_labor_adv_tech,
    aes(x = val, ymin = quantile_low, ymax = quantile_high),
    alpha = 0.2, fill = color_advanced
  ) +
  geom_line(
    data = fpm_labor_base,
    aes(x = val, y = mean_fpm),
    color = color_baseline
  ) +
  geom_line(
    data = fpm_labor_taxi,
    aes(x = val, y = mean_fpm),
    color = color_taxi
  ) +
  geom_line(
    data = fpm_labor_adv_tech,
    aes(x = val, y = mean_fpm),
    color = color_advanced
  ) +
  # Add chart labels and adjust axes 
  labs(
    y = "Fare per Mile ($/mi)",
    x = "Labor ratio (worker to vehicle)"
  ) +
  # Add lines and labels for assumed labor ratios
  annotate(
    geom = 'text',
    x = 14.5, y = 2.6,
    label = "Advanced Tech",
    family=font_main,
    fontface = 'bold',
    color = color_advanced,
    size = label_size,
    vjust = 1, hjust = 0.5
  ) +
  annotate(
    geom = 'text',
    x = 14.5, y = 7.1,
    label = "Taxi",
    family=font_main,
    fontface = 'bold',
    color = color_taxi,
    size = label_size,
    vjust = 1, hjust = 0.5
  ) +
  annotate(
    geom = 'text',
    x = 14.5, y = 4.1,
    label = "Baseline",
    family=font_main,
    fontface = 'bold',
    color = color_baseline,
    size = label_size,
    vjust = 1, hjust = 0.5
  ) +
  geom_point(aes(x = 16, y = yint_base_labor), size = 2) +
  geom_point(aes(x = 15, y = yint_taxi_labor), size = 2) +
  geom_point(aes(x = 30, y = yint_adv_tech_labor), size = 2) +
  coord_cartesian(
    xlim = c(0, 31),
    ylim = c(0, 12)
  ) +
  scale_y_continuous(
    labels = scales::dollar_format(), 
    breaks = seq(0, 12, 2)
  )+
  scale_x_continuous(
    expand = c(0, 0),
    breaks = seq(1, 31, 2)
  ) +
  theme_minimal(base_family = font_main, base_size = 15) +
  theme(
    plot.title = element_text(face="bold"), 
    panel.grid.minor = element_blank()
  )
plot_labor


# Combined plot ----

title <- ggdraw() + 
  draw_label(
    "Estimated automation budget (per vehicle) across operational ranges",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 20),
    text=element_text(family=font_main)
  )

combo_plot <- plot_grid(
  plot_capacity_util,
  plot_mileage,
  plot_labor,
  ncol = 3,
  rel_widths = c(1, 1, 1), 
  labels = c('A', 'B', 'C')
) 

# Sensitivity analysis ----

## AV systems ----

# Check the sensitivity of the base case to each of the model parameters

est_base <-  as.double(mc_fpm %>% filter(scenario == "Baseline AV") %>% select(mean_fpm))

# Create a data frame to store the results
results <- data.frame(
  input_name = NA,
  val_base = NA,
  val_low = NA, 
  val_high = NA, 
  est_base = NA,
  est_low = NA,
  est_high = NA
)

# Extract the variable names
inputs_names <- names(baseline_inputs) 

# Loop through the list of input variables and calculate +/- 50% sensitivity
for (i in 1:length(inputs_names)) { 
  input_name <- inputs_names[i]
  
  # Skip the inputs "medallion_system" and "is_taxi" since they are binary classifiers
  if (input_name == "medallion_system" | input_name == "is_taxi"){
    next
  } else {
    # Set input values 
    val_base <- baseline_inputs[[input_name]]
    val_high <- val_base*1.5
    val_low  <- val_base*0.5
    
    # Change inputs
    temp_high <- baseline_inputs
    temp_low  <- baseline_inputs
    temp_high[[input_name]] <- val_high
    temp_low[[input_name]]  <- val_low
    
    # Store results
    results[i,]$input_name <- input_name 
    results[i,]$val_base   <- val_base
    results[i,]$val_low    <- val_low
    results[i,]$val_high   <- val_high
    results[i,]$est_base   <- est_base
    out_low                <- compute_fpm(temp_low)
    results[i,]$est_low    <- out_low$fare_per_mile
    out_high               <- compute_fpm(temp_high)
    results[i,]$est_high   <- out_high$fare_per_mile
  }
}

## Exclude time conversion variables and variables that could not logically change
names_exclude <- list('weeks_per_year', 'months_per_year', 'hours_per_day', 
                      'shift_days_per_year_monitor', 'shift_days_per_year_fieldsupport',  'shift_days_per_year_customersupport', "shift_days_per_year_coordinator",
                      'shift_length_monitor', 'shift_length_customersupport', 'shift_length_coordinator', 'shift_length_fieldsupport', 
                      'shifts_per_day_fieldsupport', 'shifts_per_day_monitor', 'shifts_per_day_customersupport', 'shifts_per_day_coordinator',
                      'workers_per_cluster_shift_monitor','workers_per_cluster_shift_fieldsupport', 'workers_per_cluster_shift_customersupport', 'workers_per_cluster_shift_coordinator' )

results <- results %>%
  filter(!(results$input_name %in% names_exclude))

# Calculate the difference between est_low and est_high
results$diff <- results$est_high - results$est_low
results$diff <- abs(results$diff)

# Reorder the data frame based on the difference
tornado_plot_av <- results %>%
  arrange(desc(diff))%>%
  head(10)

tornado_plot_av <- tornado_plot_av %>% 
  mutate(
    input_name = reorder(input_name, diff),
    est_base = est_base,
    est_low = est_low,
    est_high = est_high
  ) 

#write_csv(df_plot, here::here("sensitivity_analysis.csv"))

## Taxi systems ----

# Check the sensitivity of the base case to each of the model parameters

est_base <-  as.double(mc_fpm %>% filter(scenario == "Non-AV Taxi") %>% select(mean_fpm))

# Create a data frame to store the results
results <- data.frame(
  input_name = NA,
  val_base = NA,
  val_low = NA, 
  val_high = NA, 
  est_base = NA,
  est_low = NA,
  est_high = NA
)

# Extract the variable names
inputs_names <- names(taxi_inputs) 

# Loop through the list of input variables and calculate +/- 50% sensitivity
for (i in 1:length(inputs_names)) { 
  input_name <- inputs_names[i]
  
  # Skip the inputs "medallion_system" and "is_taxi" since they are binary classifiers
  if (input_name == "medallion_system" | input_name == "is_taxi"){
    next
  } else {
    # Set input values 
    val_base <- taxi_inputs[[input_name]]
    val_high <- val_base*1.5
    val_low  <- val_base*0.5
    
    # Change inputs
    temp_high <- taxi_inputs
    temp_low  <- taxi_inputs
    temp_high[[input_name]] <- val_high
    temp_low[[input_name]]  <- val_low
    
    # Store results
    results[i,]$input_name <- input_name 
    results[i,]$val_base   <- val_base
    results[i,]$val_low    <- val_low
    results[i,]$val_high   <- val_high
    results[i,]$est_base   <- est_base
    out_low                <- compute_fpm(temp_low)
    results[i,]$est_low    <- out_low$fare_per_mile
    out_high               <- compute_fpm(temp_high)
    results[i,]$est_high   <- out_high$fare_per_mile
  }
}

## Exclude time conversion variables and variables that could not logically change
names_exclude <- list('weeks_per_year', 'months_per_year', 'hours_per_day', 
                      'shift_days_per_year_dispatcher', 'shift_days_per_year_driver',  
                      'shift_length_dispatcher', 'shift_length_driver', 
                      'shifts_per_day_dispatcher', 'shifts_per_day_driver', 
                      'workers_per_cluster_shift_dispatcher','workers_per_cluster_shift_driver',
                      'av_operations_factor_insurance', 'av_operations_factor_maintenance', 'av_operations_factor_fuel')

results <- results %>%
  filter(!(results$input_name %in% names_exclude))

# Calculate the difference between est_low and est_high
results$diff <- results$est_high - results$est_low
results$diff <- abs(results$diff)

# Reorder the data frame based on the difference
tornado_plot_taxi <- results %>%
  arrange(desc(diff))%>%
  head(10)

tornado_plot_taxi <- tornado_plot_taxi %>% 
  mutate(
    input_name = reorder(input_name, diff),
    est_base = est_base,
    est_low = est_low,
    est_high = est_high
  ) 

# Plot results
## Tornado plots ----

plot1 <- tornado_plot_av %>% 
  ggplot(aes(x = est_base, y = input_name)) +
  geom_segment(aes(xend = est_low, yend = input_name, x = est_high, y = input_name, color = "lines"), 
               linewidth = 1) +
  geom_point(aes(x = est_low, color = "low"), size = 3) +
  geom_point(aes(x = est_high, color = "high"), size = 3) +
  labs(
    title = "Top 10 Most Sensitive Inputs (AV)",
    x = "Fare per mile ($/mi)", 
    y = NULL) +
  theme_minimal_vgrid() +
  scale_color_manual(
    values = c("low" = "blue", "high" = "red"),
    labels = c("low" = "50% lower", "high" = "50% higher")
  ) +
  guides(color = guide_legend(title = "Variation from\nbaseline value")) +
  scale_x_continuous(
    expand = expansion(mult = c(0,0.05)), 
    labels = scales::dollar_format(), 
    limits = c(0,13),
    breaks = seq(0, 13, 2)
  ) +
  theme(text=element_text(family=font_main), legend.position = "none", legend.text = element_text(size = 12))

plot2 <- tornado_plot_taxi %>% 
  ggplot(aes(x = est_base, y = input_name)) +
  geom_segment(aes(xend = est_low, yend = input_name, x = est_high, y = input_name, color = "lines"), 
               linewidth = 1) +
  geom_point(aes(x = est_low, color = "low"), size = 3) +
  geom_point(aes(x = est_high, color = "high"), size = 3) +
  labs(
    title = "Top 10 Most Sensitive Inputs (Taxi)",
    x = "Fare per mile ($/mi)", 
    y = NULL) +
  theme_minimal_vgrid() +
  scale_color_manual(
    values = c("low" = "blue", "high" = "red"),
    labels = c("low" = "50% lower", "high" = "50% higher")
  ) +
  guides(color = guide_legend(title = "Variation from\nbaseline value")) +
  scale_x_continuous(
    expand = expansion(mult = c(0,0.05)), 
    labels = scales::dollar_format(), 
    limits = c(0,13),
    breaks = seq(0, 13, 2)
  ) +
  theme(text=element_text(family=font_main), legend.position = "right", legend.text = element_text(size = 12))


tornado_plots <- plot_grid(
  plot1,
  plot2,
  ncol = 2,
  rel_widths = c(1, 1.1)
) 

# Wage distribution ----

# AV Baseline 
dist_baseline <- compute_worker_dist_mc(draws_inputs = draws_baseline_inputs, fleet = 100) %>% 
  mutate(system = ifelse(system == 'av', 'av_baseline', system))
# AV Advanced Technology
dist_adv_tech <- compute_worker_dist_mc(draws_inputs = draws_adv_av_tech_inputs, fleet = 100) %>% 
  filter(system == "av") %>% 
  mutate(system = 'av_advanced')

# Combine data
wage_plot_data <- rbind(dist_baseline, dist_adv_tech) %>% 
  group_by(system) %>% 
  mutate(total = round(sum(count))) %>% 
  ungroup() %>% 
  mutate(
    system = ifelse(
      system == 'av_advanced', 'AV Advanced Tech', ifelse(
        system == 'av_baseline', 'AV Baseline', 'Taxi'
      )), 
    system = fct_relevel(system, c(
      'Taxi', 'AV Baseline', 'AV Advanced Tech')
    )
  ) 
# Create plot
wage_hist <- wage_plot_data %>% 
  ggplot() +
  geom_col(aes(x = lb, y = p), width = 0.9) +
  facet_wrap(vars(system), axes = "all", ncol = 1) +
  geom_label(
    data = wage_plot_data %>% 
      filter(lb == 0) %>% 
      mutate(label = paste0('Total workers: ', total)), 
    aes(x = 28, y = 0.275, label = label), 
    family=font_main, 
    size = 5
  ) +
  labs(
    title = "Distribution of worker wages in a 100 vehicle fleet",
    x = "Wage ($/hour)",
    y = "Percent of all workers"
  )+
  coord_cartesian(xlim = c(10, 32)) +
  scale_x_continuous(
    breaks = seq(10, 32, 2), 
    limits = c(10, 33)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = c(0,0),
    breaks = seq(0, 0.36, 0.05)
  ) +
  theme_minimal_hgrid(font_family = font_main, font_size = 15) +
  panel_border() +
  theme(strip.background = element_rect(fill = 'grey80'))

