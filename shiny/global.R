local <- FALSE
library(shiny)
library(DT)
library(shinydashboard)
library(sparkline)
library(jsonlite)
library(dplyr)
library(htmltools)
library(vembedr)
library(RPostgreSQL)
library(tidyverse)
library(yaml)
library(shinyCAPTCHA)


# Get captcha info
captcha <- yaml::yaml.load_file('captcha_credentials.yaml')

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

# Custom css for images in dropdown
css <- 
  "
a#more-apps-by-dean {
display: none;
}
body {
background: #fff;
}
.container {
margin: 0;
padding: 10px;
}
.green {
background: green;
}
#expr-container .selectize-input {
font-size: 18px;
line-height: 18px;
text-align: left;
}
#expr-container .selectize-dropdown {
font-size: 14px;
line-height: 20px;
text-align: left;
}
#expr-container .selectize-dropdown-content {
max-height: 225px;
padding: 0;
text-align: left;
}
#expr-container .selectize-dropdown-content .option {
border-bottom: 1px dotted #ccc;
text-align: left;
}
#submitExpr {
font-weight: bold;
font-size: 12px;
padding: 5px 20px;
text-align: left;
}
#helpText {
font-size: 18px;
}
#btn {
font-size: 20px;
}
"

# Define people for images and dropdowns
people <- images <- dir('www/people')
people <- strsplit(people, split = '.', fixed = TRUE)
people <- unlist(lapply(people, function(x){x[1]}))
# people <- people[people != 'unknown']
# names(people) <- images
final <- images; names(final) <- people; people <- final
# Arrange
people <- sort(people)
peoplea <- people[names(people) %in% c('Testigo', 'No sé o no aparece en la llista')]
peoplea <- rev(peoplea)
peopleb <- people[!names(people) %in% names(peoplea)]
people <- c(peoplea, peopleb)
