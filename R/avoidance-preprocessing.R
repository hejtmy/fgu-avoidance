#' Converts the coordinate data to navr
#'
#' @param df data from load_table
#'
#' @return navr object
#' @export
#'
#' @examples
as.navr <- function(df){
  df <- filter_coordinates(df)
  df <- df[, c("Time", "Parameter1", "Parameter2")]
  colnames(df) <- c("timestamp", "position_x", "position_y")
  obj <- navr::NavrObject()
  obj <- navr::load_position_data(obj, df)
  return(obj)
}

#' Filter out the coordinates from the loaded data
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
filter_coordinates <- function(df){
  res <- df[df$Event == "Coordinate",]
  return(res)
}

filter_animal <- function(df, animal_code){
  res <- df[df$AnimNo == animal_code, ]
  return(res)
}


create_animal_code <- function(num){
  return(paste0("animal", num))
}