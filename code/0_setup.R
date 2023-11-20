#################################################################################################
# About this file
#################################################################################################
# 
# This script is used to install (if not already installed) and load the required packages.
#
#################################################################################################

# Install packages
# install.packages(c(
#     'here', 'readxl', 'tidyverse', 'FinancialMath', 'cowplot', 'forcats', 'ggplot2',
#      'RColorBrewer', 'gtsummary', 'gcookbook', 'colorspace'
# ))

# Data cleaning
library(tidyverse)
library(here)
library(readxl)

# Net present value calculation
library("FinancialMath")

# Plotting and creating tables
library(ggplot2)
library(cowplot)
library(forcats)
library(RColorBrewer)
library(gtsummary)
library(flextable)
library(gcookbook)
library(colorspace)

# View all columns in data frames
options(dplyr.width = Inf)

# Plot settings 
font_main <- "Roboto Condensed"
color_vline <- "#009E73"
color_hline <- "#E69F00"
color_advanced <- "steelblue"
color_baseline <- "#999999"
