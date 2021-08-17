# ANALYSIS ----

#' Basic results to give a report from a single session
#'
#' @param obj either avoidance.single or avoidance.multiple objects. It needs to
#' be preprocessed, areas added (See \code{\link{add_areas}}).
#'
#' @return data frame with basic session results
#' @export
#'
#' @examples
session_results <- function(obj){ 
  UseMethod("session_results")  
}

#' @describeIn session_results returns session results for a single avoidance object
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

#' @describeIn session_results returns session results for a single avoidance object
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
  res$crosses_right <- nrow(crosses[crosses$to == RIGHT_ZONE_NAME & crosses$from == LEFT_ZONE_NAME, ])
  res$crosses_left <- nrow(crosses[crosses$to == LEFT_ZONE_NAME & crosses$from == RIGHT_ZONE_NAME, ])
  return(res)
}

# FREEZING ----
#' Searches for episodes of freezing behaviour
#'
#' @description This function is basically a complex wrapper around the navr::search_stops
#' function. The goal is to implement stable preprocessing nad smoothing here, so that 
#' it is not a mere wrapper around the navr::search_stops function
#'
#' @param obj valid object. Currently only avoidance single is recommended. The 
#' navr positioning data should optimally be smoothed for the best results to be
#' obtained. Use 
#' @param min_duration minimal duration of the freeze
#' @param speed_threshold what movement speed is considered unmoving (there is still 
#' some movement in the machine)
#' @param ... other optional parameters. see `navr::search_stops`` for the complete set
#'
#' @return
#' @export
#'
#' @examples
collect_freezes <- function(obj, min_duration, speed_threshold, ...){
  UseMethod("collect_freezes")
}

#' @describeIn collect_freezes Implementation for avoidance.single object
#' @export
collect_freezes.avoidance.single <- function(obj, min_duration, speed_threshold, ...){
  freezes <- search_stops(obj$position, speed_threshold = speed_threshold,
                          min_duration = min_duration, ...)
  return(freezes)
}

# CROSSES ------

#' Collects information about each cross in given object
#'
#' @description The function calculates crosses from each of the default areas 
#' definined in the object except the central area. The central area is considered 
#' a "buffer" - if an animal enters only the central area, cross is NOT counted. 
#' The areas need to be added during the preprocessing and area generally automatically 
#' added with \code{\link{load_data}}. If you need to define your own areas you
#' can do so using the \code{\link{add_areas}} function.

#' 
#' In case you want to use different then the default areas, you either need 
#' to define your own or just use the `get_area_visits` from the `navr` package, 
#' which this function uses under the hood
#' 
#' @param obj avoidance single or multiple object with areas added.
#'
#' @return dataframe with *from, to, time, index* columns. From defines which area was the cross from, to
#' which area it was made to. Time is the time of the cross and index is the index in the position data 
#' 
#' @export
#'
#' @examples
collect_crosses <- function(obj){
  UseMethod("collect_crosses")
}

#' @describeIn collect_crosses calculates all the crosses for all the animals in the 
#' object.
#' @export
collect_crosses.avoidance.multiple <- function(obj){
  res <- data.frame()
  for(i in 1:length(obj)){
    df <- collect_crosses.avoidance.single(obj[[i]])
    if(is.null(df)) return(NULL)
    if(nrow(df) > 0){
      df$animal <- names(obj)[i]
      res <- rbind.data.frame(res, df)
    }
  }
  return(res)
}

#' @describeIn collect_crosses calculates the crosses for the animal. Wrapper around
#' the \code{\link{navr::get_area_visits}}.
#' 
#' @export
collect_crosses.avoidance.single <- function(obj){
  pos <- obj$position
  if(!has_areas(pos)){
    warning("Areas have not been collectd. Have you run add_areas?")
    return(NULL)
  }
  to_left_from_right <- collect_crosses.navr(pos, to = LEFT_ZONE_NAME,
                                             from = RIGHT_ZONE_NAME,
                                             between_allowed = 1)
  to_right_from_left <- collect_crosses.navr(pos, to = RIGHT_ZONE_NAME,
                                             from = LEFT_ZONE_NAME,
                                             between_allowed = 1)
  res <- rbind.data.frame(to_right_from_left, to_left_from_right)
  return(res)
}

collect_crosses.navr <- function(obj, to, from, between_allowed){
  iVisits <- get_area_visits(obj, to, from = from, 
                             between_allowed = between_allowed)
  res <- data.frame(from = rep(from, length(iVisits)),
                    to = rep(to, length(iVisits)),
                    time = obj$data$timestamp[iVisits],
                    index = iVisits, 
                    stringsAsFactors = FALSE)
  return(res)
  
}

#' Creates a vector of length exp_length which contains number of crosses given area presence
#'
#' @param df_presence presence table calculated with \code{\link{get_area_presence}}
#' @param exp_length expected length of the experiment in seconds
#'
#' @return vector of length exp_length with number of crosses at that point
#' @export
#'
#' @examples
create_crosses_vector <- function(df_presence, exp_length = 3600){
  starting_location <- df_presence$where[1]
  ts_crosses <- c()
  n_crosses <- 0
  for(i in 1:nrow(df_presence)){
    line <- df_presence[i,]
    if(line$where != starting_location){
      n_crosses <- n_crosses + 1
    }
    ts_crosses <- c(ts_crosses, rep(n_crosses, round(line$end) - round(line$start)))
  }
  if(length(ts_crosses) != exp-length){
    last_element <- ts_crosses[length(ts_crosses)]
    ts_crosses <- c(ts_crosses, rep(last_element, exp_length - length(ts_crosses)))
  }
  return(ts_crosses)
}

# AREA PRESENCE -------

#' Returns times of animal presence in each area of interest
#'
#' @param obj avoidance.single object
#'
#' @return data.frame with times and animals location
#' @export
#'
#' @examples
collect_area_presence <- function(obj){
  if(!("avoidance.single" %in% class(obj))){
    warning("The object is not avoidance.single object")
    return(NULL)
  }
  if(!has_areas(obj$position)){
    warning("The object has not areas added. Have you run add_areas?")
    return(NULL)
  }
  crosses <- collect_crosses(obj)
  
  ordered <- crosses[order(crosses$time),]
  # if there were no crosses altogether, we create a fake crosses with the start area
  if(nrow(ordered) < 1){
    start_area <- get_position_table(obj)[1, 'area']
    ordered <- data.frame(time = 0, from=start_area, 
                          to = start_area, stringsAsFactors = FALSE)
  }
  # starts at 0 until last cross
  time_start <- c(0, ordered$time)
  # starts at first frossing until the end fo the recording  
  time_end <- c(ordered$time, tail(obj$position$data$timestamp, 1))
  # Aftr the last "from" is made, the last location from time end till the recording
  # end is the opposite location than was just left, 
  location <- c(ordered$from, tail(ordered$to, 1))
  df <- data.frame(where = location, start = time_start, end = time_end)
  df$time_spent <- df$end - df$start
  return(df)
}
