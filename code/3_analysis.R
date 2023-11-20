#################################################################################################
# About this file
#################################################################################################
# 
# This script performs all of the analysis calculations including computing the automation budget,
# conducting the scenario analyses, and performing a Monte Carlo Analysis for each scenario.
# It also contains code to generate all of the figures included in the manuscript.
#
#################################################################################################

source(here::here('model', 'setup.R'))
source(here::here('model', 'functions.R'))
source(here::here('model', 'inputs.R'))

# MC analysis ----

# Conduct Monte Carlo analysis on the scenarios to capture uncertainty in the model parameters

# Baseline

out  <- conduct_mc_analysis(draws_inputs = draws_baseline_inputs)

mc_base_budget     <- out$budget
mc_base_bpm        <- out$budget_per_mile
mc_base_categories <- data.frame(
  financing   = out$categories$financing,
  licensing   = out$categories$licensing,
  insurance   = out$categories$insurance,
  maintenance = out$categories$maintenance,
  fuel        = out$categories$fuel,
  labor       = out$categories$labor
) %>% 
  pivot_longer(
    cols = c(
      'financing', 'licensing', 'insurance', 'maintenance', 'fuel', 'labor'
    ),
    names_to = "category",
    values_to = "val"
  ) %>%
  mutate(
    val = as.double(val),
    category = as.factor(category),
    scenario = "baseline"
  )

# hist(mc_base$budget, breaks = 100)
#summary(mc_base)

# Advanced AV Tech

out <- conduct_mc_analysis(draws_inputs = draws_adv_av_tech_inputs)

mc_adv_av_tech_budget     <- out$budget
mc_adv_av_tech_bpm        <- out$budget_per_mile
mc_adv_av_tech_categories <- data.frame(
  financing   = out$categories$financing,
  licensing   = out$categories$licensing,
  insurance   = out$categories$insurance,
  maintenance = out$categories$maintenance,
  fuel        = out$categories$fuel,
  labor       = out$categories$labor
) %>% 
  pivot_longer(
    cols = c(
      'financing', 'licensing', 'insurance', 'maintenance', 'fuel', 'labor'
    ),
    names_to = "category",
    values_to = "val"
  ) %>%
  mutate(
    val = as.double(val),
    category = as.factor(category),
    scenario = "adv_av_tech"
  )

# High Usage 

out <- conduct_mc_analysis(draws_inputs = draws_high_usage_inputs)
mc_high_use_budget     <- out$budget
mc_high_use_bpm        <- out$budget_per_mile
mc_high_use_categories <- data.frame(
  financing   = out$categories$financing,
  licensing   = out$categories$licensing,
  insurance   = out$categories$insurance,
  maintenance = out$categories$maintenance,
  fuel        = out$categories$fuel,
  labor       = out$categories$labor
) %>% 
  pivot_longer(
    cols = c(
      'financing', 'licensing', 'insurance', 'maintenance', 'fuel', 'labor'
    ),
    names_to = "category",
    values_to = "val"
  ) %>%
  mutate(
    val = as.double(val),
    category = as.factor(category),
    scenario = "high_use"
  )

# Medallion System

out <- conduct_mc_analysis(draws_inputs = draws_medallion_sys_inputs)
mc_medallion_system_budget     <- out$budget
mc_medallion_system_bpm        <- out$budget_per_mile
mc_medallion_system_categories <- data.frame(
  financing   = out$categories$financing,
  licensing   = out$categories$licensing,
  insurance   = out$categories$insurance,
  maintenance = out$categories$maintenance,
  fuel        = out$categories$fuel,
  labor       = out$categories$labor
) %>% 
  pivot_longer(
    cols = c(
      'financing', 'licensing', 'insurance', 'maintenance', 'fuel', 'labor'
    ),
    names_to = "category",
    values_to = "val"
  ) %>%
  mutate(
    val = as.double(val),
    category = as.factor(category),
    scenario = "medallion_sys"
  )

# Lower Density City

out <- conduct_mc_analysis(draws_inputs = draws_lower_density_city_inputs)
mc_lower_density_budget     <- out$budget
mc_lower_density_bpm        <- out$budget_per_mile
mc_lower_density_categories <- data.frame(
  financing   = out$categories$financing,
  licensing   = out$categories$licensing,
  insurance   = out$categories$insurance,
  maintenance = out$categories$maintenance,
  fuel        = out$categories$fuel,
  labor       = out$categories$labor
) %>% 
  pivot_longer(
    cols = c(
      'financing', 'licensing', 'insurance', 'maintenance', 'fuel', 'labor'
    ),
    names_to = "category",
    values_to = "val"
  ) %>%
  mutate(
    val = as.double(val),
    category = as.factor(category),
    scenario = "lower_density"
  )

# Create table to summarize the differences between scenarios

results <- data.frame(
  base          = mc_base_budget, 
  adv_av_tech   = mc_adv_av_tech_budget,
  high_use      = mc_high_use_budget,
  medallion_sys = mc_medallion_system_budget,
  lower_density = mc_lower_density_budget
) %>%  
  pivot_longer(
    names_to = 'scenario', 
    values_to = 'budget', 
    cols = everything()
  ) %>% 
  mutate(
    scenario = as.factor(scenario)
  )

results_bpm <- data.frame(
  base          = mc_base_bpm, 
  adv_av_tech   = mc_adv_av_tech_bpm,
  high_use      = mc_high_use_bpm,
  medallion_sys = mc_medallion_system_bpm,
  lower_density = mc_lower_density_bpm
) %>%  
  pivot_longer(
    names_to = 'scenario', 
    values_to = 'budget_per_mile', 
    cols = everything()
  )

# Reordering scenarios to improve plot aesthetics
results$scenario <- recode_factor(
  results$scenario,
  "medallion_sys" = "Medallion System",
  "high_use" = "High Use",
  "base" = "Baseline",
  "adv_av_tech" = "Advanced Tech",
  "lower_density" = "Lower Density City"
)
results$scenario <- fct_relevel(
  results$scenario,
  "Advanced Tech", "High Use", "Lower Density City", "Medallion System",
  "Baseline"
)

# Create tables of budget results
mc_budget <- results %>% 
  mutate(budget = budget / 10^6) %>% 
  group_by(scenario) %>% 
  summarise(
    mean_budget = round(mean(budget), 2), 
    sd_budget = round(sd(budget), 2)
  )

ft_budget <- flextable(mc_budget)

mc_budget_per_mile <- results_bpm %>% 
  group_by(scenario) %>% 
  summarise(
    mean_bpm = round(mean(budget_per_mile),2), 
    sd_bpm = round(sd(budget_per_mile),2)
  )

ft_bpm <- flextable(mc_budget_per_mile)

## Bar Plot ----

# Combine all of the draws for each scenario into one data frame
df <- rbind(mc_base_categories, 
            mc_adv_av_tech_categories,
            mc_high_use_categories, 
            mc_lower_density_categories, 
            mc_medallion_system_categories)%>%
       mutate(
        val = as.double(val),
        category = as.factor(category),
        scenario = as.factor(scenario), 
        draw = rep(1:50000, each = 6)
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
combined_df$scenario <- recode_factor(
  combined_df$scenario,
  "medallion_sys" = "Medallion System",
  "lower_density" = "Lower Density City",
  "high_use" = "High Use",
  "baseline" = "Baseline",
  "adv_av_tech" = "Advanced Tech"
)
combined_df$scenario <- fct_relevel(
  combined_df$scenario,
  "Advanced Tech", "High Use", "Lower Density City",
  "Medallion System",  "Baseline"
)

ft_cost_by_category <- flextable(combined_df)

cost_cat_plot <- combined_df %>% 
  ggplot() +
  geom_bar(aes(x = scenario, y = mean_value, fill = category), stat = "identity", position = "stack", width = 0.5) +
  geom_errorbar(aes(x = scenario, ymin = quantile_05, ymax = quantile_95), width = 0.2) +
  labs(
    title = "Scenario Analyses",
    subtitle = "Labor is a key cost category across scenarios",
    x = NULL,
    y = "Cost ($/mi)",
    fill = "Category"
  ) +
  scale_fill_brewer(type = "qual", palette = 2, direction = -1) +
  theme_minimal_vgrid() +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  theme(text = element_text(family = font_main))

#ggsave(here::here('figs', 'figure_1.png'), cost_cat_plot, width=7, height=4)

## MC Plot ----

mc_analysis_plot <- ggplot(results) +
  geom_boxplot(
    aes(x = (budget/10^6), y = scenario),
    outlier.shape = NA
  ) +
  labs(
    title = "Estimated Automation Budgets",
    subtitle = "AV firms could afford high upfront technology investments",
    y = NULL,
    x = "NPV Automation Budget Per Vehicle ($ Mil)"
  ) +
  theme_minimal_vgrid() +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05)),
    labels = scales::dollar_format(),
    limits = c(0, 1.3),
    breaks = seq(0, 1.3, 0.25)
  ) +
  theme(text = element_text(family = font_main))

#ggsave(here::here('figs', 'figure_2.png'), mc_analysis_plot, width=6, height=4)

# Estimated budgets over variable ranges ----
# Calculate the automation budget over a range of fare per mile, capacity utilization, and VMT values

# Fare vs. Budget plot ----

budget_fpm_base <- conduct_variable_testing_mc(
  draws_baseline_inputs = draws_baseline_inputs,
  variable_name = "fare_per_mile",
  variable_range = seq(7, 0, by = -1)
) %>%
  mutate(
    mean_budget = mean_budget / 10^6,
    quantile_low = quantile_low / 10^6,
    quantile_high = quantile_high / 10^6
  )

budget_fpm_adv_tech <- conduct_variable_testing_mc(
  draws_baseline_inputs = draws_adv_av_tech_inputs,
  variable_name = "fare_per_mile",
  variable_range = round(seq(7, 0, by = -1))
) %>%
  mutate(
    mean_budget = mean_budget / 10^6,
    quantile_low = quantile_low / 10^6,
    quantile_high = quantile_high / 10^6
  )

# Plot settings
ymin <-  min(budget_fpm_base$quantile_low)
yint_base_fare <- budget_fpm_base %>%
  mutate(
    val = round(val)
  ) %>% 
  filter(val == 5) %>%
  pull(mean_budget)
yint_adv_tech_fare <- budget_fpm_adv_tech %>%
  mutate(
    val = round(val)
  ) %>% 
  filter(val == 5) %>%
  pull(mean_budget)

plot_fpm <- ggplot() +  
  geom_hline(yintercept = 0, color = color_hline)+
  # Add mean budget estimates over fare range with 95% confidence intervals
  geom_ribbon(
    data = budget_fpm_base,
    aes(x = val, ymin = quantile_low, ymax = quantile_high),
    alpha = 0.2, fill = color_baseline
  )+
  geom_ribbon(
    data = budget_fpm_adv_tech,
    aes(x = val, ymin = quantile_low, ymax = quantile_high),
    alpha = 0.2, fill = color_advanced
  )+
  geom_line(
    data = budget_fpm_base, 
    aes(x = val, y = mean_budget)
  ) +
  geom_line(
    data = budget_fpm_adv_tech,
    aes(x = val, y = mean_budget)
  ) +
  annotate(
    geom = 'text',
    x = 6.75, y = 1.3,
    label = "Advanced Tech",
    family = font_main,
    vjust = 1, hjust = 0
  ) +
  annotate(
    geom = 'text',
    x = 6.75, y = 0.75,
    label = "Baseline",
    family = font_main,
    vjust = 1, hjust = 0
  ) +
  # Add chart labels and adjust axes 
  labs(
    y = "NPV Automation Budget Per Vehicle ($ Mil)",
    x = "Fare per Mile"
  ) +
  coord_cartesian(
    xlim = c(7, 0),
    ylim = c(ymin, 1.85)
  ) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(
    expand = c(0, 0.1),
    labels = scales::dollar_format(),
    breaks = seq(0, 7, 1)
  )+
  # Add lines and labels for current taxi and personal vehicle costs
  geom_vline(xintercept = 0.72, color = color_vline)+
  annotate(
    geom = 'text',
    x = 0.75, y = ymin,
    label = "Personal Vehicle Cost",
    family=font_main,
    color = color_vline,
    vjust = 0, hjust = 1
  ) +
  geom_vline(xintercept = 5, color = color_vline)+
  annotate(
    geom = 'text',
    x = 5.05, y = ymin,
    label = "Taxi Fare", family=font_main,
    color = color_vline,
    vjust = 0, hjust = 1
  ) +
  geom_point(aes(x = 5, y = yint_base_fare), size = 2) +
  geom_point(aes(x = 5, y = yint_adv_tech_fare), size = 2) +
  theme_minimal(base_family = font_main) +
  theme(
    plot.title = element_text(face = "bold"), 
    panel.grid.minor = element_blank()
  )


# Fare vs. Capacity Utilization plot ----

# Calculate budget over variable range
budget_capacity_util_base <- conduct_variable_testing_mc(
  draws_baseline_inputs = draws_baseline_inputs,
  variable_name = "capacity_util_rate",
  variable_range = round(seq(0.7, 0, by = -0.05), 2)
) %>% 
  mutate(
    mean_budget = mean_budget / 10^6,
    quantile_low = quantile_low / 10^6,
    quantile_high = quantile_high / 10^6
  )

budget_capacity_util_adv_tech <- conduct_variable_testing_mc(
  draws_baseline_inputs = draws_adv_av_tech_inputs,
  variable_name = "capacity_util_rate",
  variable_range = round(seq(0.7, 0, by = -0.05), 2)
) %>% 
  mutate(
    mean_budget = mean_budget / 10^6,
    quantile_low = quantile_low / 10^6,
    quantile_high = quantile_high / 10^6
  )

# Plot settings
ymin <- min(budget_capacity_util_base$quantile_low)
yint_base_capacity <- budget_capacity_util_base %>%
  mutate(val = round(val,2)) %>% 
  filter(val == 0.50) %>% 
  pull(mean_budget)
yint_adv_tech_capacity <- budget_capacity_util_adv_tech %>%
  mutate(val = round(val,2)) %>% 
  filter(val == 0.50) %>% 
  pull(mean_budget)

# Make plot
plot_capacity_util <- ggplot() +
  geom_hline(yintercept = 0, color = color_hline)+
  # Add mean budget estimates over fare range with 95% confidence intervals
  geom_ribbon(
    data = budget_capacity_util_base,
    aes(x = val, ymin = quantile_low, ymax = quantile_high),
    alpha = 0.2, fill = color_baseline
  )+
  geom_ribbon(
    data = budget_capacity_util_adv_tech,
    aes(x = val, ymin = quantile_low, ymax = quantile_high),
    alpha = 0.2, fill = color_advanced
  ) +
  geom_line(
    data = budget_capacity_util_base,
    aes(x = val, y = mean_budget)
  ) +
  geom_line(
    data = budget_capacity_util_adv_tech,
    aes(x = val, y = mean_budget)
  ) +
  annotate(
    geom = 'text',
    x = 0.68, y = 1.7,
    label = "Advanced Tech",
    family = font_main,
    vjust = 1, hjust = 0
  ) +
  annotate(
    geom = 'text',
    x = 0.68, y = 0.8,
    label = "Baseline",
    family = font_main,
    vjust = 1, hjust = 0
  ) +
  # Add chart labels and adjust axes 
  labs(
    #title = "Estimated automation budgets across a range of utilization rates",
    #subtitle = "AVs unlikely to compete with personal vehicles, could compete with taxis",
    y = "NPV Automation Budget Per Vehicle ($ Mil)",
    x = "Capacity Utilization Rate"
  ) +
  coord_cartesian(
    xlim = c(0.7, 0),
    ylim = c(ymin, 1.85)
  ) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(
    expand = c(0,0),
    breaks=seq(0, 0.7, 0.1),
    labels = scales::percent_format()
  )+
  # Add lines and labels for current taxi and personal vehicle costs
  geom_vline(xintercept = 0.436, color = color_vline)+
  annotate(
    geom = 'text',
    x = 0.37, y = ymin,
    label = "Seattle",
    family=font_main,
    color = color_vline,
    vjust = 0, hjust = 1
  ) +
  geom_vline(xintercept = 0.5, color = color_vline) +
  annotate(
    geom = 'text',
    x = 0.51, y = ymin,
    label = "New York",
    family=font_main,
    color = color_vline,
    vjust = 0, hjust = 1
  ) +
  geom_point(aes(x = 0.50, y = yint_base_capacity), size = 2) +
  geom_point(aes(x = 0.50, y = yint_adv_tech_capacity), size = 2) +
  theme_minimal(base_family = font_main) +
  theme(
    plot.title = element_text(face="bold"), 
    panel.grid.minor = element_blank()
  )

# Fare vs. Annual Mileage plot ----

# Calculate budget over variable range
budget_mileage_base <- conduct_variable_testing_mc(
  draws_baseline_inputs = draws_baseline_inputs,
  variable_name = "mileage_annual",
  variable_range = round(seq(90000, 1000, by = -5000))
) %>% 
  mutate(
    mean_budget = mean_budget / 10^6,
    quantile_low = quantile_low / 10^6,
    quantile_high = quantile_high / 10^6
  )

budget_mileage_adv_tech <- conduct_variable_testing_mc(
  draws_baseline_inputs = draws_adv_av_tech_inputs,
  variable_name = "mileage_annual",
  variable_range = round(seq(90000, 1000, by = -5000))
) %>% 
  mutate(
    mean_budget = mean_budget / 10^6,
    quantile_low = quantile_low / 10^6,
    quantile_high = quantile_high / 10^6
  )

# Plot settings
yint_base_vmt <- budget_mileage_base %>%
  mutate(val = round(val,0)) %>% 
  filter(val == 65000) %>%
  pull(mean_budget)
yint_adv_tech_vmt <- budget_mileage_adv_tech %>%
  mutate(val = round(val,0)) %>% 
  filter(val == 65000) %>%
  pull(mean_budget)

# Make plot
plot_mileage <- ggplot() +  
  geom_hline(yintercept = 0, color = color_hline)+
  # Add mean budget estimates over fare range with 95% confidence intervals
  geom_ribbon(
    data = budget_mileage_base,
    aes(x = val, ymin = quantile_low, ymax = quantile_high),
    alpha = 0.2, fill = color_baseline
  ) +
  geom_ribbon(
    data = budget_mileage_adv_tech,
    aes(x = val, ymin = quantile_low, ymax = quantile_high),
    alpha = 0.2, fill = color_advanced
  ) +
  geom_line(
    data = budget_mileage_base,
    aes(x = val, y = mean_budget)
  ) +
  geom_line(
    data = budget_mileage_adv_tech,
    aes(x = val, y = mean_budget)
  ) +
  annotate(
    geom = 'text',
    x = 85000, y = 1.65,
    label = "Advanced Tech",
    family=font_main,
    vjust = 1, hjust = 0
  ) +
  annotate(
    geom = 'text',
    x = 85000, y = 0.75,
    label = "Baseline",
    family=font_main,
    vjust = 1, hjust = 0
  ) +
  # Add chart labels and adjust axes 
  labs(
    y = "NPV Automation Budget Per Vehicle ($ Mil)",
    x = "Annual VMT (thousands of miles)"
  ) +
  coord_cartesian(
    xlim = c(90000, 5000),
    ylim = c(ymin, 1.85)
  ) +
  scale_y_continuous(labels = scales::dollar_format())+
  scale_x_continuous(
    expand = c(0,1000),
    breaks = seq(10000, 90000, 10000),
    labels = scales::label_comma(scale = 1/1000)
  ) +
  # Add lines and labels for current taxi and personal vehicle costs
  geom_vline(xintercept = 64000, color = color_vline)+
  annotate(
    geom = 'text',
    x = 62000, y = ymin,
    label = "Seattle",
    family = font_main,
    color = color_vline,
    vjust = 1, hjust = 0
  ) +
  geom_vline(xintercept = 65000, color = color_vline)+
  annotate(
    geom = 'text',
    x = 66000, y = ymin,
    label = "New York",
    family = font_main,
    color = color_vline,
    vjust = 1, hjust = 1
  ) +
  geom_point(aes(x = 65000, y = yint_base_vmt), size = 2) +
  geom_point(aes(x = 65000, y = yint_adv_tech_vmt), size = 2) +
  theme_minimal(base_family = font_main) +
  theme(
    plot.title = element_text(face="bold"), 
    panel.grid.minor = element_blank()
  )


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
  plot_fpm,
  plot_capacity_util,
  plot_mileage,
  ncol = 3,
  rel_widths = c(1, 1, 1)
) 

# ggsave(
#   here::here('figs', 'figure_4.png'),
#   combo_plot, width = 15, height  = 4.5
# )

# Sensitivity analysis ----

# Check the sensitivity of the base case to each of the model parameters

est_base <-  as.double(mc_budget %>% filter(scenario == "Baseline") %>% select(mean_budget))

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
  
  # Skip the input "medallion_system" since it is a binary classifier
  if (input_name == "medallion_system"){
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
    out_low                <- compute_automation_budget(temp_low)
    results[i,]$est_low    <- out_low$budget
    out_high               <- compute_automation_budget(temp_high)
    results[i,]$est_high   <- out_high$budget
  }
}

# Exclude time conversion variables
names_exclude <- list('weeks_per_year', 'months_per_year', 'hours_per_day')
results <- results %>% 
  filter(!(results$input_name %in% names_exclude))

# Calculate the difference between est_low and est_high
results$diff <- results$est_high - results$est_low
results$diff <- abs(results$diff)

# Reorder the data frame based on the difference
df_plot <- results %>%
  arrange(desc(diff)) %>%
  head(10)

df_plot <- df_plot %>% 
  mutate(
    input_name = reorder(input_name, diff),
    est_base = est_base/10^6,
    est_low = est_low/10^6,
    est_high = est_high/10^6
    ) 

# Plot results
## Tornado plot ----
plot <- df_plot %>% 
  ggplot(aes(x = est_base, y = input_name)) +
  geom_segment(aes(xend = est_low, yend = input_name, x = est_high, y = input_name, color = "lines"), 
               linewidth = 1) +
  geom_point(aes(x = est_low, color = "low"), size = 3) +
  geom_point(aes(x = est_high, color = "high"), size = 3) +
  labs(
    title = "Top 10 Most Sensitive Inputs",
    x = "NPV AV Budget Estimate ($ Mil)", 
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
    limits = c(0, 0.8)
  ) +
  theme(text=element_text(family=font_main), legend.position = "right", legend.text = element_text(size = 12))

#ggsave(here::here('figs', 'figure_3.png'), plot, width=7, height=4)
