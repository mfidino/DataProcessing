

pielou <- function(photo_id = NULL){
     photo_id <- data.table::data.table(photo_id)
     #remove duplicates
     if(any(duplicated(photo_id))){
     photo_id <- photo_id[!duplicated(photo_id),]
     }
     data.table::setindex(photo_id, subject_id, classification_id)
     user_tags <- photo_id[,list(species = paste(sort(choice), collapse = ", ")), 
                   by = classification_id]
     user_tags <- left_join(user_tags, 
                            photo_id[,c("subject_id", "classification_id",
                                        "gold_standard")],
                            by = "classification_id") %>% 
          data.table %>% 
          distinct
     
     # pielou calculation
     #  S is the number of unique 'tags'. We are treating the combo
     #  of the tags (all tags given by a user) as the species tag
     #  in a photo.
     S_calc <- user_tags[, .(S = length(unique(species))), .(subject_id)]
          #user_tags %>% group_by(subject_id) %>% 
          #summarise(S = length(unique(user_tags$species)))
          
     # Pi. This is the proportion of tags in a given 
     Pi_calc <- user_tags[, .N, 
                          by = .(subject_id, species)][, prop := N/sum(N), 
                                                       by = .(subject_id)]
     total_ids_per_image <- user_tags[,.N,
                                      by = .(subject_id)]
     
     
     both_calc <- left_join(Pi_calc, S_calc, by = "subject_id") %>% 
          data.table
     
     pielou <- both_calc[, .(pielou = -sum(prop * log(prop))/log(S)), 
                         by = .(subject_id)] %>% 
          distinct
     
     pielou$pielou[is.na(pielou$pielou)] <- 0
     
     
     
     to_return <- list(support = sort(Pi, decreasing = TRUE),
                       blank   = )
     return(answer)
     
     
}

aggregate_photoID <- function(photos = NULL, annotations = NULL){
     #photos$subject_id <- paste(photos$workflow_id, 
      #                          photos$workflow_version,
       #                         photos$id, sep = "-")
     
     ans <- left_join(photos[,c("subject_id", "classification_id")],
                      annotations, by = "classification_id")
     return(ans)
}
