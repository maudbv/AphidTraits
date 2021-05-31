# Master script for the Aphid Trait project

# Set options
Sys.setlocale("LC_ALL", "de_DE")

# Load libraries
library(tidyverse)
library(stringi)
library(data.table)
library(rio)

library(lme4)
library(lmerTest)
library(performance)
library(r2glmm)

# Import and modify data ####

## Import aphid traits
source('scripts/import data/import aphid trait measurements.R')

## Import plant traits
source('scripts/import data/import plant traits.R')

## Import plot parameters
source('scripts/import data/import colony parameters.R')

## import environment data (126 columns!)
source('scripts/import data/Import environmental data.R')


## Save clean and formatted data:
save(aphid_df, aphid_traits, aphid_traits_long,
     plot_data, colony_parameters,
     plant_traits, plant_traits_raw,
     file = "clean data/Aphid_trait_data_31.05.2021.Rdata")
     
write.csv(aphid_traits, file = "clean data/aphid_traits.csv")

