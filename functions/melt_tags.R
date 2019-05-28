
# melt annotations
#  This function takes the varied outputs from different annotation
#  workflows and binds them all into a data.frame (or tibble)

melt_annotations <- function(annotations = NULL) {
     
     melt_wrap <- function(x){
          if(inherits(x, "list")){
               x <- dplyr::bind_rows(x)
          }
          return(x)
     }
     
     
     annotations <- lapply(annotations, melt_wrap)
     annotations <- data.frame(dplyr::bind_rows(annotations), stringsAsFactors = FALSE)
     to_drop <- "value\\.answers\\.|value\\.|value.\\filters\\.|filters\\."
     colnames(annotations) <- gsub(to_drop,
                                   "", colnames(annotations))
     
     # combining columns that have identical names. This happens
     #  with older workflows (the column names lack vowels)
     
     return(annotations)
}


# melt_ntags
#  This function is used inside of parse_annotations. Its used to create
#  multiple rows in the data.frame when a user has included multiple tags
#  on a single photo.

melt_ntags <- function(data, id_vars, var_stubs, sep) {
     # vectorized version of grep
     vGrep <- Vectorize(grep, "pattern", SIMPLIFY = FALSE)
     
     # Isolate the columns starting with the var_stubs
     temp <-
          names(data)[names(data) %in%
                    unlist(vGrep(var_stubs, names(data), value = TRUE))]
     # add _0 to the first tag
     temp[temp %in% var_stubs] <-
          paste0(temp[temp %in% var_stubs], "_0")
     
     # Split the vector and reasemble into a data.frame
     x <- do.call(rbind.data.frame, strsplit(temp, split = sep))
     
     names(x) <-
          c("VAR", paste(".time", 1:(length(x) - 1), sep = "_"))
     
     # Prep to decide whether normal reshape or unbalanced reshape
     xS <- split(x$.time_1, x$VAR)
     xL <- unique(unlist(xS))
     
     if (isTRUE(all(sapply(xS, function(x)
          all(xL %in% x))))) {
          # Everything looks ok for normal `reshape` to work
          reshape(
               data,
               direction = "long",
               idvar = id_vars,
               varying = lapply(vGrep(var_stubs, names(data), value = TRUE), sort),
               sep = sep,
               v.names = var_stubs
          )
     } else {
          # Padding required to "balance" the data
          
          # Find out which variables need to be padded
          newVars <- unlist(lapply(names(xS), function(y) {
               temp <- xL[!xL %in% xS[[y]]]
               if (length(temp) == 0) {
                    temp <- NULL
               } else {
                    paste(y, temp, sep = sep)
               }
          }))
          
          # Create matrix of NAs
          myMat <-
               setNames(data.frame(matrix(
                    NA,
                    nrow = nrow(data),
                    ncol = length(newVars)
               )), newVars)
          
          # Bind with original data.frame
          out <- cbind(data, myMat)
          
          # Use `reshape` as normal
          my_varying <- lapply(vGrep(var_stubs, names(out),
                                     value = TRUE), sort)
          
          if(var(lengths(my_varying)) != 0) {
               for(value in 1:length(my_varying)){
                   tmp <-  my_varying[[value]]
                   n_choice <- ncol(x)-1
                   should_be_there <- c(vary_vars[value],
                                        paste(vary_vars[value], n_choice,
                                              sep = "_"))
                   tk <- my_varying[[value]] %in% should_be_there
                   my_varying[[value]] <- my_varying[[value]][tk]
                   
               }
          }
          
          reshape(
               out,
               direction = "long",
               idvar = id_vars,
               varying = my_varying,
               sep = sep,
               v.names = var_stubs
          )
     }
}
