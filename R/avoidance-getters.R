#' Returns positioning data
#'
#' @param obj 
#'
#' @return
#' @export
#'
#' @examples
get_position <- function(obj){
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
    df_animal <- get_position(obj[[animal]])
    df_animal$animal <- animal
    df <- rbind(df, df_animal)
  }
  position <- navr::load_position_data(NavrObject(), df)
  res <- list(position = position)
  return(res)
}