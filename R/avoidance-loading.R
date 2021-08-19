#' Loads all files from a folder and combines to a large table which is then processed 
#'
#'
#' @param folder folder with multiple animals loaded
#'
#' @return
#' @export
#'
#' @examples
load_folder <- function(folder){
  files <- list.files(folder, pattern = "\\.(csv|CSV)", full.names = TRUE)
  res <- data.frame()
  for(f in files){
    df <- load_table(f)
    ## Checks for the same animal names
    df <- add_unique_animal_code(df, res)
    res <- rbind(res, df)
  }
  res <- convert_table_to_objects(res)
  return(res)
}

#' Load data from a particular filepath
#'
#' @description The data are loaded and then preprocessed. 
#' Areas are added (see \code{\link{add_areas}}), position data are converted to 
#' navr object and classes are added to the resulting objects
#' 
#' @param filepath Either filepath or a text
#' @param text alternatively the data can be loaded from loaded text 
#' @return avoidance.single object
#' @export
#'
#' @examples
load_data <- function(filepath = NULL, text = NULL){
  df <- load_table(filepath, text)
  res <- convert_table_to_objects(df)
  return(res)
}

## HELPERS ------

load_table <- function(filepath = NULL, text = NULL){
  if(all(is.null(filepath), is.null(text))) return(NULL)
  if(!is.null(filepath)) df <- read.table(filepath, sep = ";", dec = ",", 
                                          skip = 1, header = TRUE)
  if(!is.null(text)) df <- read.table(text = text, sep = ";", dec = ",",
                                      skip = 1, header = TRUE)
  df <- process_table(df)
  df$source <- filepath
  return(df)
}

convert_table_to_objects <- function(df){
  res <- list()
  animals <- unique(df$AnimNo)
  for(animal in animals){
    obj <- list()
    df_animal <- filter_df_animal(df, animal)
    
    obj$filepath <- df_animal$source[1]
    df_animal$filepath <- NULL
    
    rownames(df_animal) <- 1:nrow(df_animal)
    position <- as.navr(df_animal)
    obj$position <- position
    class(obj) <- append(class(obj), "avoidance.single")
    obj <- add_areas(obj)
    
    res[[animal]] <- obj
  }
  class(res) <- append(class(res), "avoidance.multiple")
  return(res) 
}

#' Converts the coordinate data to navr
#'
#' @param df data from load_data
#' @return navr object
#' @noRd
as.navr <- function(df){
  df <- df[df$Event == "Coordinate",]
  df <- df[, c("Time", "Parameter1", "Parameter2")]
  colnames(df) <- c("timestamp", "position_x", "position_y")
  df$timestamp <- df$timestamp/1000
  obj <- navr::NavrObject()
  obj <- navr::load_position_data(obj, df)
  obj <- prepare_navr(obj)
  return(obj)
}

process_table <- function(df){
  df <- df[2:nrow(df),] #for some reaons the first row is always weird
  df$AnimNo <- create_animal_code(df$AnimNo)
  return(df)
}

filter_df_animal <- function(df, animal_code){
  res <- df[df$AnimNo == animal_code, ]
  return(res)
}

create_animal_code <- function(num){
  return(paste0("animal_", num))
}

add_unique_animal_code <- function(df_new, df_old){
  anim_code <- unique(df_new$AnimNo)
  existing_codes <- unique(df_old$AnimNo)
  enum <- 2
  for(code in anim_code){
    new_code <- code
    while(new_code %in% existing_codes){
      new_code <- paste0(code, "_", enum)
      enum <- enum + 1
    }
    df_new$AnimNo[df_new$AnimNo == code] <- new_code
  }
  return(df_new)
}