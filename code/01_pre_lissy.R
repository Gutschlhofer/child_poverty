## This file can be used to extract the dataset codes (country+year) from the
#  LIS documentation from METIS that would otherwise need to be manually coded.
#  This can then be copy-pasted into the file 02_ .R.

library(tidyverse)

file <- readxl::read_xlsx("input/our-lis-documentation-availability-matrix.xlsx")

datasets <- file[2,][!is.na(file[2,]) & file[2,]!="Dataset shortname"]
years <- as.integer(substr(datasets, 3, 4))
datasets <- datasets[years < 21] # filter out everything that is 19xx
datasets <- paste0(datasets, "h")

paste(tolower(datasets), sep = " ", collapse = "','")
# Here we manually copy-pasted the identifiers that resulted from that
# into 02_ .R.

# Extension countries
file <- readxl::read_xlsx("input/our-lis-documentation-availability-matrix_additions.xlsx")

datasets <- file[2,][!is.na(file[2,]) & file[2,]!="Dataset shortname"]
years <- as.integer(substr(datasets, 3, 4))
datasets <- datasets[years < 21] # filter out everything that is 19xx
datasets <- paste0(datasets, "h")

paste(tolower(datasets), sep = " ", collapse = "','")