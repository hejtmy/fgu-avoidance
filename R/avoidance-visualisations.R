#' Creates heatmap from passed navr object
#'
#' @param obj nav_object
#'
#' @return
#' @export
#'
#' @examples
#' 
create_heatmap_polygon <- function(obj, bins = 100, ...){
  UseMethod("create_heatmap_polygon")
}
#' @export
create_heatmap_polygon.avoidance.single <- function(obj, bins = 50){
  df <- get_position(obj)
  plt <- ggplot(df, aes(position_x, position_y)) +
    gradient_style() +
    stat_density2d(aes(fill=..level..), bins=bins, geom = 'polygon') +
    lims(x=c(0,450), y=c(0, 400)) +
    coord_cartesian(xlim = BOX_ROOM$x, ylim = BOX_ROOM$y) + 
    theme_bw() +
    guides(fill = FALSE) +
    heatmap_theme()
  return(plt)
}
#' @export
create_heatmap_polygon.avoidance.multiple <- function(obj, bins = 50){
  obj <- combine_all(obj)
  return(create_heatmap_polygon.avoidance.single(obj, bins))
}

#' Title
#'
#' @param obj 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
create_heatmap_rastr <- function(obj, ...){
  UseMethod("create_heatmap_rastr")
}

#' @export
create_heatmap_rastr.avoidance.single <- function(obj){
  df <- get_position(obj)
  plt <- ggplot(df, aes(x = position_x, y = position_y)) +
    gradient_style() +
    stat_density2d(aes(fill=..density..), geom = 'raster', contour = FALSE) +
    guides(fill=FALSE, alpha = FALSE, level=FALSE) +
    coord_cartesian(xlim = BOX_ROOM$x, ylim = BOX_ROOM$y) + 
    theme_bw() +
    heatmap_theme()
  return(plt)
}

#' @export
create_heatmap_rastr.avoidance.multiple <- function(obj){
  obj <- combine_all(obj)
  return(create_heatmap_rastr.avoidance.single(obj))
}

# PATHS -----
#' Creates a path graph of a single trial
#'
#' @param obj 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
plot_path.avoidance.single <- function(obj, zone = central_zone()){
  plt <- base_path_plot(zone)
  plt <- plt + geom_navr_path(obj$position, size = 1.25, color = "#98959a")
  return(plt)
}

# CROSSES -----

#' plots crosses 
#'
#' @param obj 
#' @param iCrosses 
#'
#' @return
#' @export
#'
#' @examples
plot_crosses <- function(obj, iCrosses){
  plt <- base_path_plot()
  colors <- rainbow(length(iCrosses))
  for(i in 1:length(iCrosses)){
    times <- range(obj$position$data$timestamp[(iCrosses[i] - 10):(iCrosses[i] + 10)])
    cross <- filter_times(obj$position, times)
    plt <- plt + geom_navr_path(cross, color = colors[i], size = 1.25)
  }
  return(plt)
}


# ELEMENTS -----
#' @export
geom_box_room <- function(color = "#61af93", size = 1.25, fill = "white", ...){
  box <- box_room_size()
  return(geom_rect(aes(xmin=box$x[1], xmax=box$x[2], 
                       ymin=box$y[1], ymax=box$y[2]),
                   color = color, size = size, fill=fill, ...))
}

#' @export
geom_central_zone <- function(zone = central_zone(), color = "red", size = 1, ...){
  if (is.null(zone)) return(list())
  return(geom_navr_area(zone, color = color, size = size, ...))
}

base_path_plot <- function(zone = central_zone()){
  res <- ggplot() + geom_box_room() +
    geom_central_zone() +
    theme_void() + guides(fill=FALSE)
  return(res)
}
# STYLES -----

gradient_style <- function(){
  return(scale_fill_gradientn(colours = heatmap_color()))
}

heatmap_theme <- function(){
  return(theme(panel.background = element_rect(fill = heatmap_color()[1]),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()))
}

heatmap_color <- function(){
  return(rev(rainbow(100, start=0, end=0.7)))
}