local <- TRUE
library(shiny)
library(shinydashboard)
library(sparkline)
library(jsonlite)
library(dplyr)
library(htmltools)
library(vembedr)
library(RPostgreSQL)
library(tidyverse)

# Source files
source_files <- dir('R')
source_files <- source_files[grepl('.R', source_files, fixed = T)]
source_files <- source_files[!source_files %in% c('assign_chunks.R')]
for(i in 1:length(source_files)){
  source(paste0('R/', source_files[i]))
}

# Connect to database
if(!local){
  credentials <- credentials_extract(credentials_file = 'R/credentials/credentials.yaml', all_in_file = TRUE)
} else {
  credentials <- credentials_extract(credentials_file = 'R/credentials/credentials_local.yaml', all_in_file = TRUE)
}

co <- credentials_connect(options_list = credentials)

# Read in tables from database
users <- get_data(query = 'SELECT * FROM users', connection_object = co)
transcriptions <- get_data(query = 'SELECT * FROM transcriptions', connection_object = co)
chunks <- get_data(query = 'SELECT * FROM chunks', connection_object = co)
