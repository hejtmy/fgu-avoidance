#' Filter the fgu.avoidance object for chosen animals
#'
#' @param obj avoidance.multiple object
#' @param codes animal codes as they appear in the object
#'
#' @return filtered avoidance.multiple object or empty list
#' @export
#'
#' @examples
filter_animals <- function(obj, codes){
  selection <- intersect(names(obj), codes)
  res <- obj[selection]
  class(res) <- append(class(res), 'avoidance.multiple')
  return(res)
}

#' Returns positioning data
#'
#' @param obj avoidance.single object
#'
#' @return
#' @export
#'
#' @examples
get_position_table <- function(obj){
  return(obj$position$data)
}

#' Creates a single navr object with all animals combined together
#'
#' @param obj avoidance.multiple object
#'
#' @return
#' @export
#'
#' @examples
combine_all <- function(obj){
  df <- data.frame()
  for(animal in names(obj)){
    df_animal <- get_position_table(obj[[animal]])
    df_animal$animal <- animal
    df <- rbind(df, df_animal)
  }
  position <- navr::load_position_data(NavrObject(), df)
  res <- list(position = position)
  return(res)
}

## NAVR getters -------
#' @export
filter_times.avoidance.single <- function(obj, times, zero_based = FALSE){
  obj$position <- filter_times(obj$position, times, zero_based = zero_based)
  return(obj)
}