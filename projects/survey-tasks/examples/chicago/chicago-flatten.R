source("sourcer.R")

packs <- c("jsonlite", "dplyr", "stringr", "lubridate", "reshape2",
           "doParallel", "data.table")

package_load(packs)



# Specify Project
classifications_file <- "main-la-river-project-classifications.csv"
classifications_file <- "data/chicago-wildlife-watch-classifications.csv"

# Read in the data.
jdata <- read.csv(classifications_file, stringsAsFactors = FALSE)

#saveRDS(jdata, "jdata.RDS")

jdata <- readRDS("jdata.RDS")

# Chicago also needs to deal wth these versions too, possibly with separate calls to the flattening script, depending on the task structures. 
# workflow_id_num <- 2334
# workflow_version_num <- c(397.41, 406.45)
# workflow_id_num <- 3054
# workflow_version_num <- 5.70


# limit to relevant workflow id and version


set.seed(-160)
my_samp <- sort(sample(1:nrow(jdata), 50000))

# parse through the different annotations and workflows
my_annos <- parse_annotations(jdata)


# combine specific annotation columns
my_annos <- combine_columns("HOWMANY", "HWMN", my_annos, "n_count")
my_annos <- combine_columns("CLICKYESIFTHEDOGISOFFLEASH",
                                "CLCKSFTHDGSFFLSH", my_annos, "offleash")
my_annos <- combine_columns("offleash",
                                "CLCKSFDGSFFLSH", my_annos)
my_annos <- combine_columns("CLICKYESIFYOUNGAREPRESENT",
                                "CLCKSFNGRPRSNT", my_annos, "youngpresent")
my_annos <- combine_columns("LIKE", "LK", my_annos)
my_annos <- combine_columns("COAT","CT", my_annos)
my_annos <- combine_columns("value",
                            "CLCKWWFTHSSNWSMPHT", my_annos, "greatphoto")

# get just the first task, which is the species tagging
my_t0 <- my_annos[my_annos$task == "T0",]

# fix the species names
my_t0$choice <- combine_species(my_t0$choice)

# drop 'rprtthspht'
my_t0 <- my_t0[-which(my_t0$choice == "rprtthspht"),]


# get the subject id's
my_photos <- parse_photos(jdata)

my_photos <- combine_columns("file_path", "image_path", my_photos)
my_photos <- combine_columns("image1", "image_1", my_photos)
my_photos <- combine_columns("image1", "image_name", my_photos)

# drop NA id
my_photos <- my_photos[!is.na(my_photos$id),]

my_photos <- fread("my_photos.csv")
my_annos <- fread("my_annos.csv")
my_users <- fread("my_users.csv")

# make the unique id. This is the workflow id + workflow version + id columns
#  and join with the t0 annotations
with_rsid <- my_photos[!is.na(my_photos$retired.id),]
photo_id <- aggregate_photoID(with_rsid, my_t0)


# reduce the number of columns
photo_id <- photo_id[,c("subject_id", "classification_id", 
                        "gold_standard", "choice")]

# remove any of the non-retired images



no_retire <- my_photos[-which(my_photos$subject_id %in% t2$subject_id),]



photo_id <- left_join(my_photos[,c("subject_id", "classification_id")],
                      my_t0, by = "classification_id")



my_users <- parse_users(jdata)

table(jdata$workflow_id)


write.csv(test[[1]], "species_tags.csv")

# Identify task-specific details. These variable names are important, because I haven't figured out how to define them in the function call 
# (there's some weird referencing. I don't know. The function definitions and scripts could be improved, but things seem to generally work.)


# Flatten by calling the code from the flattening_functions file. This isn't the cleanest approach, but it'll have to do.
# If you want to combine multiple workflows or multiple tasks before aggregating, this is the time to do it.
survey_data <- run_json_parsing(data = jdata)

View(final_data)
write.csv(final_data, file = paste0(project_name, "-flattened.csv"))

# Now grab and flatten the question task (this should actually be called inside of the parsing script, as right now it's duplicating effort)
T1_data <- jdata %>% flatten_to_task %>% filter_to_task(task_id = question_id)
write.csv(T1_data, file = paste0(project_name, "-", question_task_id, "-flattened.csv"))

