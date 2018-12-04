


# This function is built to be used on a single workflow_id. It will collect
#  the users answer to a question and return them along with the classification
#  id, which is a unique value that connects the users answers to the image
#  and their user information.
parse_annotations_single <- function(class_data = NULL,
     ncores = NULL ) {
     
     if(length(unique(class_data$workflow_id))>1){
          stop("Please subset class_data so that a single workflow_id is supplied.")
     }
     
     # set number of cores if not specified
     ncores <- ifelse(is.null(ncores), detectCores() - 2, ncores)
     
     # splitting the data into ncore sections for running in parallel
     if ( nrow(class_data) < 100 ){
          ncores <- 1
          cuts <- factor(rep('1', nrow(class_data)))
     } else {
          cuts <- cut(1:nrow(class_data), ncores)
     }
     
     # register cores
     cl <- makeCluster(ncores)
     registerDoParallel(cl)
     
     x = levels(cuts)
     x = x[1]
     # parallel loop for the annotations
     annos <- foreach(
          x = levels(cuts) ,
          .packages = c('jsonlite', 'magrittr', 'dplyr', 'reshape2'),
          .export = 'melt_ntags',
          .multicombine = TRUE
     ) %dopar% {
          # This reads the json stream and returns it as a list.
          #  simplify set to FALSE so we do not have to deal with the odd structure
          json_list <-  class_data$annotations[cuts == x] %>%
               textConnection %>%
               stream_in(., simplifyVector = FALSE)
          
          # number of tasks a user is asked to complete per photo
          ntask <- length(json_list[[1]])
          
          # remove duplicated tasks,
          #  currently keeping the last instance of the task
          #  it's a bunch of nested lists, with each top level list
          #  being a single annotation and lists inside are different tasks
          #  The first lapply sapply group goes through each annotation and
          #  collects the task names (e.g., T0). From there we determine if 
          #  these are duplicated and figure out where in json_list those occur.
          to_check <- lapply(json_list, function(x)
               sapply(x, function(y)
                    y$task)) %>%
               lapply(., duplicated) %>%
               sapply(., function(x)
                    sum(x))
          to_check <- which(to_check > 0)
          
          # Here is where we remove the duplicates if there are some that
          #  need to go.
          if (length(to_check) > 0) {
               list_keep <- lapply(json_list[to_check], function(x)
                    sapply(x, function(y)
                         y$task)) %>%
                    lapply(., function(x)
                         ! duplicated(x, fromLast = TRUE))
               for (keep in 1:length(to_check)) {
                    json_list[[to_check[keep]]] <-
                         json_list[[to_check[keep]]][list_keep[[keep]]]
               }
          }
          
          
          
          # the object that is to be returned from this function
          task_list <- vector("list", length = ntask)
          for (task in 1:ntask) {
               # Changing each list element in json_list
               #  to a data.frame so that it can be binded together
               #  json_df will hold each element before it gets
               #  combined
               json_df <- vector("list", length = length(json_list))
               for (i in 1:length(json_list)) {
                    # transpose makes it a matrix first, which we
                    #  then change to a data.frame
                   # json_list[[i]]
                    json_df[[i]] <-
                         unlist(json_list[[i]][[task]]) %>%
                         t() %>% as.data.frame(., stringsAsFactors = FALSE)
                    # Make column names unique
                    colnames(json_df[[i]]) <-
                         make.unique(colnames(json_df[[i]]), sep = "_")
               }
               
               # Make json_df a data.frame now
               json_df <- bind_rows(json_df)
               
               # add classification_id to connect back to other data
               json_df$classification_id <-
                    class_data$classification_id[cuts == x]
               
               # add gold_standard
               json_df$gold_standard <- class_data$gold_standard[cuts == x]
               
               # add expert
               json_df$expert <- class_data$expert[cuts == x]

               # This assumes that nested json is seperated by a period
               # which appears to be the case
               temp <-
                    colnames(json_df)[grep("\\.", colnames(json_df))]
               vary_vars <- temp[-grep("_[0-9]", temp)]
               
               if (length(vary_vars) > 0) {
                    json_df <- melt_ntags(
                         json_df,
                         id_vars = c("classification_id", "task"),
                         var_stubs = vary_vars,
                         sep = "_"
                    )
                    rownames(json_df) <- NULL
                    if ("time" %in% colnames(json_df)) {
                         colnames(json_df)[colnames(json_df) == "time"] <- "tag"
                    }
                    # remove rows if vary_vars are all NA
                    to_keep <- apply(json_df[, vary_vars], 1,
                         function(x)
                              sum(is.na(x))) < length(vary_vars)
                    json_df <- json_df[to_keep,]
               }
               
               # this is the object that is getting returned
               task_list[[task]] <- json_df
               names(task_list)[[task]] <-
                    as.character(unique(json_df$task))
          }
          if (length(task_list) == 1) {
               return(task_list[[1]])
          } else {
               return(task_list)
          }
          
     }
     # stop parallel cluster
     stopCluster(cl)
     
     # THE OUTPUTS
     
     # Output for multiple lists with multiple tasks.
     #  These are formatted as a list nested within lists.
     #  The if statement looks for this. You cannot use is.list because
     #  data.frame is also considered a list. inherits checks if all objects
     #  inside of annos are lists.
     if (inherits(annos, "list") &
               any(sapply(annos, function(x) inherits(x, "list"))) &
               length(annos) > 1){
          # get the names of the tasks
          task_names <- unique(as.character(sapply(annos, names)))
          # the list that will be returned
          to_return <- vector("list", length = length(task_names))
          for (i in 1:length(to_return)) {
               # check to see if each list in annos has that named task
               have_task <- lapply(annos, names) %>%
                    sapply(., function(x)
                         x %in% task_names[i]) %>%
                    colSums(.)
               # get only the
               one_task <- lapply(annos[have_task == 1],
                    function(x) x[task_names[i]]) %>%
                    lapply(., '[[', 1)
               to_return[[i]] <- bind_rows(one_task)
          }
     } 
     # If there is multiple lists with a single task. This is a list
     #  of data.frames.
     if (inherits(annos, "list") &
               any(sapply(annos, function(x) inherits(x, "data.frame"))) &
               length(annos) > 1){
          mut_fun <- function(x) x %>% mutate_if(is.factor, as.character)
          annos <- lapply(annos, mut_fun)
          to_return <- try(bind_rows(sapply(annos, c)), TRUE)
          if(inherits(to_return, "try-error")){
               to_return <- bind_rows(annos)
          }
     }
     # Single list with multiple tasks. It is a single list with multiple
     #  Lists inside of it.
     if (inherits(annos, "list") &
               any(sapply(annos, function(x) inherits(x, "list"))) &
               length(annos) == 1){
          to_return <- annos[[1]]    
     }
     
     # if all that is returned is a data frame. Happens with a small
     # amount of data and a single task
     
     if(inherits(annos, "list") &
               any(sapply(annos, function(x) inherits(x, "data.frame"))) &
               length(annos) == 1){
          to_return <- annos[[1]]
     }
     # If there is a single list with nested lists. Happens with a small
     #  amount of data and multiple tasks.
     
     if (length(to_return) == 1 & inherits(to_return, "list")) {
          return(to_return[[1]])
     } else {
          return(to_return)
     }
     
}

# parse annotation uses parse_annotations_single in a for loop to go through 
#  each workflow and return the associated data.
parse_annotations <- function(class_data = NULL,
     ncores = NULL, combine_lists = TRUE){
     unq_workflow <- class_data %>% 
          select('workflow_id', 'workflow_version') %>% 
          unique #%>% 
          #arrange(.,workflow_id)
     workflow_list <- vector("list", length = nrow(unq_workflow))
     for(wf in 1:nrow(unq_workflow)){
          workflow_list[[wf]] <- 
              class_data %>%
               filter(., workflow_id == unq_workflow$workflow_id[wf],
                         workflow_version == unq_workflow$workflow_version[wf]) %>% 
               parse_annotations_single(class_data = ., ncores = ncores)
               
          
          names(workflow_list)[wf] <- 
           paste(c("wf",unq_workflow[wf,]), collapse = "_")

     }
     if(nrow(unq_workflow) == 1){
          if(combine_lists){
               return(melt_annotations(workflow_list[[1]]))
          }
          return(workflow_list[[1]])
     } else{
          if(combine_lists){
               return(melt_annotations(workflow_list))
          }
     return(workflow_list)
     }
}

# this goes through the 'subject_ids'


parse_photos <- function(class_data = NULL,
     ncores = NULL, combine_lists = TRUE){
     unq_workflow <- class_data %>% 
          select('workflow_id', 'workflow_version') %>% 
          unique
     workflow_list <- vector("list", length = nrow(unq_workflow))
     for(wf in 1:nrow(unq_workflow)){
          workflow_list[[wf]] <- 
               class_data %>%
               filter(., workflow_id ==
                         unq_workflow$workflow_id[wf],
                    workflow_version == unq_workflow$workflow_version[wf]) %>% 
               parse_photos_single(class_data = ., ncores = ncores)
          
          
          names(workflow_list)[wf] <- 
               paste(c("wf",unq_workflow[wf,]), collapse = "_")
          
     }
     if(nrow(unq_workflow) == 1){
          if(combine_lists){
               return(melt_annotations(workflow_list[[1]]))
          }
          return(workflow_list[[1]])
     } else{
          if(combine_lists){
               return(melt_annotations(workflow_list))
          }
          return(workflow_list)
     }
}


parse_photos_single <- function(class_data = NULL,
     ncores = NULL){
     
     ncores <- ifelse(is.null(ncores), detectCores() - 2, ncores)
     
     # splitting the data into ncore sections for running in parallel
     if ( nrow(class_data) < 100 ){
          ncores <- 1
          cuts <- factor(rep('1', nrow(class_data)))
     } else {
          cuts <- cut(1:nrow(class_data), ncores)
     }
     
     # register cores
     cl <- makeCluster(ncores)
     registerDoParallel(cl)
     
     # parallel loop for the annotations
     annos <- foreach(
          x = levels(cuts) ,
          .packages = c('jsonlite', 'magrittr', 'dplyr', 'reshape2', 'stringr'),
          .export = 'melt_ntags',
          .multicombine = TRUE
     ) %dopar% {
          # This reads the json stream and returns it as a list.
          #  simplify set to FALSE so we do not have to deal with the odd structure
          json_list <-  class_data$subject_data[cuts == x] %>%
               textConnection %>%
               stream_in(., simplifyVector = FALSE)
          
          # number of tasks a user is asked to complete per photo
          ntask <- length(json_list[[1]])
          
               # Changing each list element in json_list
               #  to a data.frame so that it can be binded together
               #  json_df will hold each element before it gets
               #  combined
               json_df <- vector("list", length = length(json_list))
               for (i in 1:length(json_list)) {
                    # transpose makes it a matrix first, which we
                    #  then change to a data.frame
                    json_df[[i]] <-
                         unlist(json_list[[i]][[1]]) %>%
                         t() %>% as.data.frame(., stringsAsFactors = FALSE)
                    # Make column names unique
                    colnames(json_df[[i]]) <-
                         make.unique(colnames(json_df[[i]]), sep = "_")
               }
               
               # Make json_df a data.frame now
               json_df <- bind_rows(json_df)
               
               # add classification_id to connect back to other data
               json_df$classification_id <-
                    class_data$classification_id[cuts == x]
               # add workflow and version
               json_df$workflow_id <- class_data$workflow_id[cuts == x]
               json_df$workflow_version <- class_data$workflow_version[cuts == x]
               # add subject_id
               json_df$subject_id <- class_data$subject_ids[cuts == x]
               return(json_df)
     }
     # stop parallel cluster
     
     stopCluster(cl)
     
     annos <- bind_rows(annos)
     return(annos)
}

parse_users <- function(class_data = NULL,
     ncores = NULL, combine_lists = TRUE){
     unq_workflow <- class_data %>% 
          select('workflow_id', 'workflow_version') %>% 
          unique
     workflow_list <- vector("list", length = nrow(unq_workflow))
     for(wf in 1:nrow(unq_workflow)){
          workflow_list[[wf]] <- 
               class_data %>%
               filter(., workflow_id ==
                         unq_workflow$workflow_id[wf],
                    workflow_version == unq_workflow$workflow_version[wf]) %>% 
               parse_users_single(class_data = ., ncores = ncores)
          
          
          names(workflow_list)[wf] <- 
               paste(c("wf",unq_workflow[wf,]), collapse = "_")
          
     }
     if(nrow(unq_workflow) == 1){
          if(combine_lists){
               return(melt_annotations(workflow_list[[1]]))
          }
          return(workflow_list[[1]])
     } else{
          if(combine_lists){
               return(melt_annotations(workflow_list))
          }
          return(workflow_list)
     }
}

parse_users_single <- function(class_data = NULL, ncores = NULL){
     
     ncores <- ifelse(is.null(ncores), detectCores() - 2, ncores)
     
     # splitting the data into ncore sections for running in parallel
     if ( nrow(class_data) < 100 ){
          ncores <- 1
          cuts <- factor(rep('1', nrow(class_data)))
     } else {
          cuts <- cut(1:nrow(class_data), ncores)
     }
     
     # register cores
     cl <- makeCluster(ncores)
     registerDoParallel(cl)
     
     # parallel loop for the annotations
     annos <- foreach(
          x = levels(cuts) ,
          .packages = c('jsonlite', 'magrittr', 'dplyr', 'reshape2'),
          .export = 'melt_ntags',
          .multicombine = TRUE
     ) %dopar% {
          # This reads the json stream and returns it as a list.
          #  simplify set to FALSE so we do not have to deal with the odd structure
          json_list <-  class_data$metadata[cuts == x] %>%
               textConnection %>%
               stream_in(., simplifyVector = FALSE)
          
          # number of tasks a user is asked to complete per photo
          ntask <- length(json_list[[1]])
          
          # Changing each list element in json_list
          #  to a data.frame so that it can be binded together
          #  json_df will hold each element before it gets
          #  combined
          json_df <- vector("list", length = length(json_list))
          for (i in 1:length(json_list)) {
               # transpose makes it a matrix first, which we
               #  then change to a data.frame
               json_df[[i]] <-
                    unlist(json_list[[i]]) %>%
                    t() %>% as.data.frame(., stringsAsFactors = FALSE)
               # Make column names unique
               colnames(json_df[[i]]) <-
                    make.unique(colnames(json_df[[i]]), sep = "_")
          }
          
          # Make json_df a data.frame now
          json_df <- bind_rows(json_df)
          
          # add classification_id to connect back to other data
          json_df$classification_id <-
               class_data$classification_id[cuts == x]
          json_df$user_name <- class_data$user_name[cuts == x]
          json_df$user_id <- class_data$user_id[cuts == x]
          json_df$user_ip <- class_data$user_ip[cuts == x]
          json_df$started_at <- as.POSIXct(json_df$started_at,
               format = "%Y-%m-%dT%H:%M:%OSZ", tz="GMT")
          json_df$finished_at <- as.POSIXct(json_df$finished_at,
               format = "%Y-%m-%dT%H:%M:%OSZ", tz="GMT")
          
          return(json_df)
     }
     # stop parallel cluster
     
     stopCluster(cl)
     
     annos <- bind_rows(annos)
     annos$session <- as.numeric(factor(annos$session))
     # make a mobile column
     annos$mobile <- FALSE
     annos$mobile[grep("mobile", annos$user_agent, ignore.case = TRUE)] <- TRUE
     annos <- annos[,-grep("viewport|subject_dimensions|user_group|agent", 
          colnames(annos))]
     annos$time_spent <- annos$finished_at - annos$started_at
     
     return(bind_rows(annos))
     
}





