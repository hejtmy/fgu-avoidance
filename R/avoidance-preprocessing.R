#' Converts the coordinate data to navr
#'
#' @param df data from load_table
#'
#' @return navr object
#' @export
#'
#' @examples
as.navr <- function(df){
  df <- filter_df_coordinates(df)
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
    df_animal <- filter_df_animal(df, animal)
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

process_table <- function(df){
  df <- df[2:nrow(df),] #for some reaons the first row is always weird
  df$AnimNo <- create_animal_code(df$AnimNo)
  return(df)
}

filter_df_coordinates <- function(df){
  res <- df[df$Event == "Coordinate",]
  return(res)
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
#' in the function `default_zones`
#'
#' @return object which was passed with added areas
#' @export
#'
#' @examples
add_areas.avoidance.single <- function(obj, areas = default_zones()){
  obj$position <- navr::add_areas(obj$position, areas)
  return(obj)
}

## NAVR wrappers ------

#' @importFrom navr smooth_positions
#' @export
smooth_positions.avoidance.single <- function(obj, ...){
  obj$position <- smooth_positions(obj$position, ...)
  return(obj)
}

#' @importFrom navr smooth_speed
#' @export
smooth_speed.avoidance.single <- function(obj, ...){
  obj$position <- smooth_speed(obj$position, ...)
  return(obj)
}