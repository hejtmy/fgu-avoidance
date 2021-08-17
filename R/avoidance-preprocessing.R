## AREAs ----

#' Adds areas of interest into the positioning data.
#'
#' @param obj avoidance.multiple object
#' @param areas list of areas of interest to add. *Defaults* to 
#' implemented left, central and right areas
#' 
#' @return object which was passed with added areas
#' @export
add_areas.avoidance.multiple <- function(obj, areas = default_zones()){
  for(i in 1:length(obj)){
    obj[[i]] <- add_areas(obj[[i]], areas)
  }
  return(obj)
}

#' Adds areas into the positioning data
#' 
#' @description For a detailed description of how the areas are used and defined
#' see the \code{navr} package. Areas are used for cross calculation and during
#' plotting.
#' 
#' @param obj avoidance.single object
#' @param areas list of areas of interest to add. *Defaults* to implemented left, 
#' central and right areas in the function \code{\link{default_zones}}
#'
#' @return object which was passed with added areas
#' @export
add_areas.avoidance.single <- function(obj, areas = default_zones()){
  obj$position <- navr::add_areas(obj$position, areas)
  return(obj)
}

## NAVR wrappers ------
#' Smooths positioning data
#' 
#' @description S3 method for the `navr::smooth_positions`.`
#' 
#' @param obj avoidance.single object
#' @param ... parameters for the `navr::smooth_positions` function.
#' 
#' @export
smooth_positions.avoidance.single <- function(obj, ...){
  obj$position <- smooth_positions(obj$position, ...)
  return(obj)
}

#' Smooths speeds
#' 
#' @description S3 method for the `navr::smooth_speed`
#' 
#' @param obj avoidance.single object
#' @param ... parameters for the `navr::smooth_speed`` function
#'
#' @export
smooth_speed.avoidance.single <- function(obj, ...){
  obj$position <- smooth_speed(obj$position, ...)
  return(obj)
}