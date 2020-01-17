# HEATMAPS -----

#' Creates heatmap from passed object
#'
#' @param obj object to calculate heatmap from
#' @param bins number of bins
#' @param geom which geom to use? possibile are "polygon"(default) or "raster"
#' @param ... optional params to the stat_density2d geom
#'
#' @return
#' @export
#'
#' @examples
create_heatmap <- function(obj, bins = 100, background = apparatus_image_path(), ...){
  UseMethod("create_heatmap")
}
#' @export
create_heatmap.avoidance.single <- function(obj, bins = 100, background = apparatus_image_path(), ...){
  plt <- create_heatmap_plot(obj, bins, background, ...)
  return(plt)
}
#' @export
create_heatmap.avoidance.multiple <- function(obj, bins = 100, background = apparatus_image_path(), ...){
  obj <- combine_all(obj)
  return(create_heatmap.avoidance.single(obj, bins, background, ...))
}

create_heatmap_plot <- function(obj, bins, background, ...){
  if(!is.null(background)){
    size <- box_room_size(type = "real")
    plt <- ggplot() +
      geom_navr_background(background, size$x, size$y)
  } else {
    size <- box_room_size()
    plt <- ggplot()
  }
  plt <- plt +
    geom_navr_heatmap(obj$position, bins, ...) +
    gradient_style() +
    lims(x=c(0,500), y = c(0,500)) +
    guides(fill=FALSE, alpha = FALSE, level=FALSE) +
    coord_cartesian(xlim = size$x, ylim = size$y) +
    theme_bw() +
    heatmap_theme()
  if(!is.null(background)){
    plt <- plt + theme(panel.background = element_rect(fill = "transparent")) 
  }
  return(plt) 
}

## PATHS -----

#' Creates a path graph of a single trial
#'
#' @param obj avoidance single object
#' @param center 
#' @param background 
#' @param color 
#' @param size 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
plot_path.avoidance.single <- function(obj, center = central_zone(), background = NULL, 
                                       color = "#98959a", size = 1.25, ...){
  if(!is.null(background)){
    size <- box_room_size(type = "real")
    plt <- background_path_plot(background) 
  } else {
    size <- box_room_size(type = "animal")
    plt <- base_path_plot(center)
  }
  plt <- plt +
    geom_central_zone(center) + 
    geom_navr_path(obj$position, size = 1.25, color = "#98959a") +
    coord_cartesian(xlim = size$x, ylim = size$y)
  return(plt)
}

## CROSSES -----

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

## AREA PRESENCE -----

#' PLots an image of area presence for avoidance.single 
#'
#' @param obj avoidance.single object
#'
#' @return
#' @export
#'
#' @examples
plot_area_presence <- function(obj){
  # check for class
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
  plt <- ggplot(df) +
    geom_rect(aes(xmin = start, xmax = end, ymin = -0, ymax = 1, fill = where)) +
    #geom_text(aes(x=(start+end)/2, y = 1.5 + 5/4*(where=="left"), label = where), check_overlap = TRUE) +
    xlab("Time since start") +
    scale_fill_manual(values = area_presence_scale()) +
    coord_fixed(ratio = 60) + ylim(0,6) +
    theme_classic() +
    guides(fill = guide_legend(nrow=1, title="")) +
    theme(axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = c(0.9, 0.75),
          legend.background = element_blank(),
          legend.box = "horizontal") 
  return(plt)
}

## ELEMENTS -----
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
  res <- ggplot() + 
    geom_box_room() +
    geom_central_zone(zone) +
    theme_void() + 
    guides(fill=FALSE)
  return(res)
}

background_path_plot <- function(background = apparatus_image_path()){
  res <- ggplot() +
    geom_navr_background(background, BOX_ROOM_REAL$x, BOX_ROOM_REAL$y) + 
    theme_void() + 
    guides(fill=FALSE)
  return(res)
}
## STYLES -----
#' Styling functions for the heatmap and paths
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
area_presence_scale <- function(){
  return(rev(c("#e2e2e2", "#000000")))
}