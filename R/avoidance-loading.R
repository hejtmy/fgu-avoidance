#' Loads all files from a folder and combines to a large table which is then processed 
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
#' @param filepath Either filepath or a text
#' @param text althernatively the data can be loaded from loaded text 
#' @return avoidance.single object
#' @export
#'
#' @examples
load_data <- function(filepath){
  res <- list()
  df <- load_table(filepath)
  res <- convert_table_to_objects(df)
  return(res)
}

#' Loads text data from already loaded text
#'
#' @param text 
#'
#' @return
#' @export
#'
#' @examples
load_text_data <- function(text){
  df <- read.table(text = text, sep=";", dec=",", skip=1, header=TRUE)
  df <- process_table(df)
  df <- convert_table_to_objects(df)
  return(df)
}

load_table <- function(filepath){
  df <- read.table(filepath, sep=";", dec=",", skip=1, header=TRUE)
  df <- process_table(df)
  return(df)
}
