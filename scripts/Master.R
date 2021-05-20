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

## import environment data (126 columns!)
source('scripts/Import environmental data.R')

## Import aphid traits
source('scripts/import aphid trait measurements.R')

## Import plant traits
source('scripts/import plant traits.R')

## Import plot parameters


# Statistical analyses ####

# Illustrate ####

