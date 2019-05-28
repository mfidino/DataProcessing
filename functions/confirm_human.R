
# modify some images for just one stuff

just_one <- total_ids_per_image$subject_id[total_ids_per_image$N == 1]

one_view <- my_photos[which(my_photos$subject_id %in% just_one),]

# get file path

fp <- one_view$file_path

# remove double slash with sinle

one_view$file_path <- gsub("//", "/", one_view$file_path)

# get the tag given to each image

one_tags <- user_tags[which(user_tags$subject_id %in% just_one),]

all_info <- left_join(one_view, one_tags, by = "subject_id")




library(magick)

file.copy(fp, "test_image.JPG")
batch_photos <- sapply(strsplit(all_info$file_path[718:nrow(all_info)], "/"), function(x) x[length(x)]) %>% 
     paste0("./batch_photos/", .)
file.copy(all_info$file_path[718:nrow(all_info)], hm)
library(magick)
for(i in 5605:nrow(all_info)){
     fp <- all_info$file_path[i]
     tt <- all_info$species[i]
     file_name <- sapply(strsplit(fp, "/"), function(x) x[length(x)]) %>% 
          paste0("./human_photos/", .)
     my_image <- image_read(fp)
     my_image <- image_convert(my_image, "png")
     my_image <- image_convert(my_image, "jpg")
     my_image <- try(image_annotate(my_image, tt, size = 75, boxcolor = "white",
                    gravity = "northeast"), silent = TRUE)
     if(!class(my_image) == "try-error"){
     image_write(my_image, file_name)
     }
     rm(my_image)
     

}

htags <- read.csv("human_tags.csv", stringsAsFactors = FALSE)


     photos_to_view <- list.files("./human_photos/", full.names = TRUE)
     answers <- rep(FALSE, length(photos_to_view))



     for(i in nrow(htags):length(photos_to_view)){
          my_pic <- image_read(photos_to_view[i])
          my_pic <- image_scale(my_pic, "900")
          print(my_pic)

ans <-   menu(c("Yes", "No"),title = "Correct tag?")
if(ans == 1){
     answers <- TRUE
}else{
     answers <- FALSE
}
write.table(data.frame(photo = photos_to_view[i],
                       correct = answers),
            "human_tags.csv", append = TRUE, sep = ",",
            col.names = FALSE)

     }
     
}
