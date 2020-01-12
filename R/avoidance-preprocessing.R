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

convert_table_to_objects <- function(df){
  res <- list()
  animals <- unique(df$AnimNo)
  for(animal in animals){
    obj <- list()
    df_animal <- filter_animal(df, animal)
    position <- as.navr(df_animal)
    obj$position <- position
    class(obj) <- append(class(obj), "avoidance.single")
    res[[animal]] <- obj
  }
  class(res) <- append(class(res), "avoidance.multiple")
  return(res) 
}

process_table <- function(df){
  df <- df[2:nrow(df),] #for some reaons the first row is always weird
  df$AnimNo <- create_animal_code(df$AnimNo)
  return(df)
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
  return(paste0("animal_", num))
}

## AREAs ----
add_areas <- function(obj){
  UseMethod("add_areas")
}

add_areas.avoidance.multiple <- function(obj){
  for(i in 1:length(obj)){
    obj[[i]] <- add_areas(obj[[i]])
  }
  return(obj)
}

add_areas.avoidance.single <- function(obj){
  obj$position <- navr::add_areas(obj$position)
}