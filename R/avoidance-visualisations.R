#' Creates heatmap from passed navr object
#'
#' @param obj nav_object
#'
#' @return
#' @export
#'
#' @examples
create_heatmap_polygon <- function(obj, bins = 50){
  df <- get_position(obj)
  plt <- ggplot(df, aes(position_x, position_y)) +
    gradient_style() +
    stat_density2d(aes(fill=..level..,alpha=..level..),
                   bins=bins, geom = 'polygon') +
    lims(x=c(0,450), y=c(0, 400)) +
    theme_bw()
  return(plt)
}

create_heatmap_rastr <- function(obj){
  df <- get_position(obj)
  plt <- ggplot(df, aes(x = position_x, y = position_y)) +
    gradient_style() +
    stat_density2d(aes(fill=..density..), geom = 'raster', contour = FALSE) +
    geom_path(color = "white", alpha = 0.3) + 
    theme_bw()
  return(plt)
}


gradient_style <- function(){
  return(scale_fill_gradientn(colours=rev(rainbow(100, start=0, end=0.75)))) 
}