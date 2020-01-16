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
  df$timestamp <- df$timestamp/1000
  obj <- navr::NavrObject()
  obj <- navr::load_position_data(obj, df)
  obj <- prepare_navr(obj)
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

add_unique_animal_code <- function(df_new, df_old){
  anim_code <- df_new$AnimNo[1]
  existing_codes <- unique(df_old$AnimNo)
  enum <- 2
  while(anim_code %in% existing_codes){
    anim_code <- paste0(anim_code, "_", enum)
    enum <- enum + 1
  }
  df_new$AnimNo <- anim_code
  return(df_new)
}

## AREAs ----

#' Adds areas of interest into the positioning data.
#'
#' @param obj avoidance.multiple object
#' @param areas list of areas of interest to add. Defaults to implemented left, central and right areas
#'
#' @return object which was passed with added areas
#' @export
#'
#' @examples
add_areas.avoidance.multiple <- function(obj, areas = default_zones()){
  for(i in 1:length(obj)){
    obj[[i]] <- add_areas(obj[[i]], areas)
  }
  return(obj)
}

#' Adds areas if ubterest into the positioning data
#' @param obj avoidance.single object
#' @param areas list of areas of interest to add. Defaults to implemented left, central and right areas
#'
#' @return object which was passed with added areas
#' @export
#'
#' @examples
add_areas.avoidance.single <- function(obj, areas = default_zones()){
  obj$position <- navr::add_areas(obj$position, areas)
  return(obj)
}