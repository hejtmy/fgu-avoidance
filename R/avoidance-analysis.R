## ANALYSIS ----

#' Basic results to give a report from a single session
#'
#' @param obj 
#'
#' @return
#' @export
#'
#' @examples
session_results <- function(obj){ 
  UseMethod("session_results")  
}
#' @export
session_results.avoidance.multiple <- function(obj){
  res <- data.frame(stringsAsFactors = FALSE)
  for(i in 1:length(obj)){
    animal_res <- session_results.avoidance.single(obj[[i]])
    animal_res$animal <- as.character(names(obj)[i])
    res <- rbind(res, as.data.frame(animal_res))
  }
  return(res)
}
#' @export
session_results.avoidance.single <- function(obj){
  pos <- obj$position
  if(!has_areas(pos)){
    warning("Areas have not been collectd. Have you run add_areas?")
    return(NULL)
  }
  res <- list()
  res$distance <- tail(pos$data$distance_total, 1)
  
  time_in_areas <- calculate_areas_time(pos)
  get_time_in_area <- function(df, area){
    time <- df[df$area == area, "duration"]
    if(length(time) != 1) time <- NA_real_
    return(time)
  }
  res$time_left <- get_time_in_area(time_in_areas, LEFT_ZONE_NAME)
  res$time_right <- get_time_in_area(time_in_areas, RIGHT_ZONE_NAME)
  res$time_center <- get_time_in_area(time_in_areas, CENTRAL_ZONE_NAME)
  
  crosses <- collect_crosses.avoidance.single(obj)
  res$crosses_right <- nrow(crosses[crosses$to == RIGHT_ZONE_NAME & crosses$from == LEFT_ZONE_NAME,])
  return(res)
}

# FREEZING

# CROSSES ------
#' Collects information about each cross in given object
#'
#' @param obj avoidance single or multiple object with default areas added. See \code{\link{`add_areas`}()
#'
#' @return dataframe with results
#' @export
#'
#' @examples
collect_crosses <- function(obj){
  UseMethod("collect_crosses")
}
#' @export
collect_crosses.avoidance.multiple <- function(obj){
  res <- data.frame(stringsAsFactors = FALSE)
  for(i in 1:length(obj)){
    df <- collect_crosses.avoidance.single(obj[[i]])
    if(is.null(df)) return(NULL)
    df$animal <- names(obj)[i]
    res <- rbind(res, df)
  }
  return(res)
}
#' @export
collect_crosses.avoidance.single <- function(obj){
  pos <- obj$position
  if(!has_areas(pos)){
    warning("Areas have not been collectd. Have you run add_areas?")
    return(NULL)
  }
  to_left_from_right <- collect_crosses.navr(pos, to = LEFT_ZONE_NAME, from = RIGHT_ZONE_NAME, between_allowed = 1)
  to_right_from_left <- collect_crosses.navr(pos, to = RIGHT_ZONE_NAME, from = LEFT_ZONE_NAME, between_allowed = 1)
  res <- rbind(to_right_from_left, to_left_from_right)
  return(res)
}

collect_crosses.navr <- function(obj, to, from, between_allowed){
  iVisits <- get_area_visits(obj, to, from = from, between_allowed = between_allowed)
  res <- data.frame(from = rep(from, length(iVisits)),
                    to = rep(to, length(iVisits)),
                    time = obj$data$timestamp[iVisits],
                    index = iVisits)
  return(res)
  
}