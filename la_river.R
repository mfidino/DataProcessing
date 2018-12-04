source("sourcer.R")

packs <- c("jsonlite", "dplyr", "stringr", "lubridate", "reshape2",
           "doParallel", "data.table")

package_load(packs)


# Specify Project
classifications_file <- "main-la-river-project-classifications.csv"


# Read in the data.
jdata <- read.csv(classifications_file, stringsAsFactors = FALSE)



# parse through the different annotations and workflows
my_annos <- parse_annotations(jdata)