#' Title
#'
#' @param filepath 
#'
#' @return
#' @export
#'
#' @examples
load_data <- function(filepath){
  res <- list()
  df <- load_table(filepath)
  df <- df[df$Type == "Te", ]
  df$AnimNo <- create_animal_code(df$AnimNo)
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

load_table <- function(filepath){
  tab <- read.table(filepath, sep=";", dec=",", skip=1, header=TRUE)
  return(tab)
}