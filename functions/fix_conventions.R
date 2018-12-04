
combine_columns <- function(col1 = NULL, col2 = NULL, x = NULL, new_name = NULL){

     paste3 <- function(...,sep=", ") {
     L <- list(...)
     L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
     ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
                gsub(paste0(sep,sep),sep,
                     do.call(paste,c(L,list(sep=sep)))))
     is.na(ret) <- ret==""
     ret
}

to_keep <- which(colnames(x) == col1)
other_col <- which(colnames(x) == col2)

x[,to_keep] <- paste3(x[,to_keep], x[,other_col])

if(!is.null(new_name)){
     colnames(x)[to_keep] <- new_name
}

x <- x[,-other_col]

return(x)
}

combine_species <- function(x = NULL){
     x <- tolower(x)
     choices <- tolower(unique(x))
     if(any(is.na(choices))){
      choices <- choices[!is.na(choices)]    
     }
     choices_no_vow <- gsub("[aeiouy]", "", choices)
     choices_replace <- choices

     
     for(i in 1:length(choices)){
          if(choices[i] %in% choices_no_vow){
               longer <- which(choices[i] == choices_no_vow)
               nchar_species <- nchar(choices[longer])
               choices_replace[i] <- choices[longer][which.max(nchar_species)]
          }
     }
          
          
     for(j in 1:length(choices)){
          if(choices_replace[j] != choices[j] & any(x == choices[j])){
               x[x == choices[j]] <- choices_replace[j]
          
          }
     }
     return(x)
     
     
          
          

}
