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
  res <- data.frame()
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