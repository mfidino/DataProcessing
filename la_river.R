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


# get the subject id's
my_photos <- parse_photos(jdata)

# drop NA id
my_photos <- my_photos[!is.na(my_photos$id),]

# from here you can connect user tags to photos via the classification_id