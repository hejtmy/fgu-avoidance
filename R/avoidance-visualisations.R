#' Creates heatmap from passed navr object
#'
#' @param obj nav_object
#'
#' @return
#' @export
#'
#' @examples
#' 

create_heatmap_polygon <- function(obj, bins = 50, ...){
  UseMethod("create_heatmap_polygon")
}

create_heatmap_polygon.avoidance.single <- function(obj, bins = 50){
  df <- get_position(obj)
  plt <- ggplot(df, aes(position_x, position_y)) +
    gradient_style() +
    stat_density2d(aes(fill=..level..,alpha=..level..),
                   bins=bins, geom = 'polygon') +
    lims(x=c(0,450), y=c(0, 400)) +
    theme_bw()
  return(plt)
}

create_heatmap_polygon.avoidance.multiple <- function(obj, bins = 50){
  obj <- combine_all(obj)
  return(create_heatmap_polygon.avoidance.single(obj, bins))
}

create_heatmap_rastr <- function(obj, ...){
  UseMethod("create_heatmap_rastr")
}

create_heatmap_rastr.avoidance.single <- function(obj){
  df <- get_position(obj)
  plt <- ggplot(df, aes(x = position_x, y = position_y)) +
    gradient_style() +
    stat_density2d(aes(fill=..density..), geom = 'raster', contour = FALSE) +
    geom_path(color = "white", alpha = 0.3) + 
    theme_bw()
  return(plt)
}

create_heatmap_rastr.avoidance.multiple <- function(obj){
  obj <- combine_all(obj)
  return(create_heatmap_rastr.avoidance.single(obj))
}

# PATHS -----
create_path <- function(obj, ...){
  plt <- ggplot() +
    geom_box_room() +
    geom_navr_path(obj$position, size = 1.25, color = "#98959a") +
    guides(fill=FALSE) +
    theme_void()
  return(plt)
}

# ELEMENTS -----
geom_box_room <- function(){
  return(geom_rect(aes(xmin=10, xmax=380, ymin=80, ymax=340, fill="#65a9ff"),
                   color = "#61af93", alpha = 0.5, size = 1.5))
}

# STYLES -----

gradient_style <- function(){
  return(scale_fill_gradientn(colours=rev(rainbow(100, start=0, end=0.75)))) 
}