# source the necessary scripts
source("sourcer.R")

# load the necessary packages
packs <- c("jsonlite", "dplyr", "stringr", "lubridate", "reshape2",
           "doParallel", "data.table")
package_load(packs)

# Specify Project
classifications_file <- "wildlife-of-los-angeles-classifications.csv"
photos_file <-"wildlife-of-los-angeles-subjects.csv"

classifications_file <- "my_classification_csv_from_zooniverse.csv"
photos_file <- "my_subjects_csv_from_zooniverse.csv"

# Read in the data.
jdata <- read.csv(classifications_file, stringsAsFactors = FALSE)

# parse through the different annotations and workflows
my_annos <- parse_annotations(jdata)

# get just the first task, which is the species tagging
my_t0 <- my_annos[my_annos$task == "T0",]

# get the subject id's
my_photos <- parse_photos(jdata)

# drop NA id
my_photos <- my_photos[!is.na(my_photos$id),]

# make the unique id. This is the workflow id + workflow version + id columns
#  and join with the t0 annotations
with_rsid <- my_photos[!is.na(my_photos$retired.id),]
photo_id <- aggregate_photoID(with_rsid, my_t0)

# calculate pielou evenness
photo_id <- pielou(photo_id = photo_id)

