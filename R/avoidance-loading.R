#' Loads all files from a folder and combines to a large table which is then processed 
#'
#' @param folder 
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
    res <- rbind(res, df)
  }
  res <- convert_table_to_objects(res)
  return(res)
}

#' Title
#'
#' @param filepath 
#'
#' @return
#' @export
#'
#' @examples
load_data <- function(filepath){
  res <- list()
  df <- load_table(filepath)
  res <- convert_table_to_objects(df)
  return(res)
}

load_table <- function(filepath){
  df <- read.table(filepath, sep=";", dec=",", skip=1, header=TRUE)
  df <- process_table(df)
  return(df)
}

