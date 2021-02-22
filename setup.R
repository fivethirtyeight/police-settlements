# This file should be sourced at the beginning of individual city files to
# set up the right data and output paths
# the input path should point to a local copy of the Google Drive folder
# run XX Makefile to sync your local copy with the current most updated copy of the google drive folder
rm(list=ls())

list.of.packages <- c("ggplot2", "lubridate","readxl","plyr","dplyr","tidyverse","tabulizer","tidylog","stringr", "this.path")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#library(here)
#library(rmarkdown)
library(ggplot2)
library(lubridate)
library(readxl)
library(plyr)
library(dplyr)
library(tidyverse)
library(tabulizer)
library(tidylog)
library(stringr)
library(this.path)

# disable scientific notation
options(scipen = 999)

# Avoid conflicts
#here = here::here
summarize = dplyr::summarize
rename = dplyr::rename
count = dplyr::count

# functions to set path in files that call setup.R
get_inpath <- function(base_path){
  original_path <- file.path(base_path, 'original/')
  intermediate_path <- file.path(base_path, 'intermediate/')
  raw_data_path <- if(dir.exists(intermediate_path)) intermediate_path else original_path
  return(raw_data_path)
}

get_outpath <- function(base_path) {
  out_data_path <- file.path(base_path, 'final/')
  return(out_data_path)
}
