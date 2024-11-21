
# Load required packages ---
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(rstudioapi)
library(readxl)
library(readr)

# Set working directory ---
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Set parameters ---
# when do we want data from?
data_start <- 2018
# when do we want the services comparison chart from?
servs_from <- 2020
# when do we want the data indexed to?
index_year <- 2018

# Is this a revision cycle? ---
# Yes - set to TRUE
# No - set to FALSE
revision_cycle <- FALSE

# Run scripts ---
source("source/1-oecd.R")
source("source/2-ons.R")
source("source/3-calcs-and-combine.R")
source("source/4-services.R")
