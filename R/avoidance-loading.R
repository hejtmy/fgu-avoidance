load_data <- function(filepath){
  obj <- list()
  tab <- load_table(filepath)
  position <- as.navr(tab)
  return(tab)
}

load_table <- function(filepath){
  tab <- read.table(filepath, sep=";", dec=",", skip=1, header=TRUE)
   return(tab)
}