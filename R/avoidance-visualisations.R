## HEATMAPS -----

#' Creates heatmap from passed object
#'
#' @param obj object to calculate heatmap from
#' @param bins number of bins
#' @param geom which geom to use? possibile are "polygon"(default) or "raster"
#' @param background path to a backgroud image to be plotted. 
#' Use \code{\link{apparatus_image_path}} to construct or provide your own.
#' @param ... optional params to the stat_density2d geom
#' @return
#' @export
#'
#' @examples
create_heatmap <- function(obj, bins = 100, background = apparatus_image_path(), 
                           add_points = FALSE, ...){
  UseMethod("create_heatmap")
}

#' @describeIn create_heatmap creates heatmap for a single animal
#' @export
create_heatmap.avoidance.single <- function(obj, bins = 100, background = apparatus_image_path(), 
                                            add_points = FALSE, ...){
  plt <- create_heatmap_plot(obj, bins, background, add_points, ...)
  return(plt)
}

#' @describeIn create_heatmap merges all the data together and creates heatmap for all
#' @export
create_heatmap.avoidance.multiple <- function(obj, bins = 100, background = apparatus_image_path(),
                                              add_points = FALSE,...){
  obj <- combine_all(obj)
  return(create_heatmap.avoidance.single(obj, bins, background, add_points, ...))
}

create_heatmap_plot <- function(obj, bins, background, add_points, ...){
  if(!is.null(background)){
    size <- box_room_size(type = "real")
    plt <- ggplot() +
      geom_navr_background(background, size$x, size$y)
  } else {
    size <- box_room_size(type="animal")
    plt <- ggplot()
  }
  plt <- plt +
    geom_navr_heatmap(obj$position, bins, ...) +
    gradient_style() +
    lims(x=c(0,500), y = c(0,500)) +
    coord_cartesian(xlim = size$x, ylim = size$y) +
    theme_bw() +
    heatmap_theme() +
    labs(x = "", y = "")
  if(add_points){
    plt <- plt + 
      geom_point(data = obj$position$data, mapping = aes(position_x, position_y))
  }
  if(!is.null(background)){
    plt <- plt + theme(panel.background = element_rect(fill = "transparent")) 
  }
  return(plt) 
}

## PATHS -----

#' Creates a path graph of a single trial
#'
#' @param obj avoidance single object
#' @param background path to a backgroud image to be plotted. Use \code{\link{apparatus_image_path}} 
#' to construct or provide your own.
#' @param center logical determining if the central zone should be plotted
#' @param color color of the path
#' @param size path size
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
plot_path.avoidance.single <- function(obj, background = apparatus_image_path(), center = FALSE, 
                                       color = "#98959a", size = 1.25, ...){
  if(!is.null(background)){
    size <- box_room_size(type = "real")
    plt <- background_path_plot(background)
  } else {
    size <- box_room_size(type = "animal")
    plt <- base_path_plot()
  }
  if(center) plt <- plt + geom_central_zone()
  plt <- plt +
    geom_navr_path(obj$position, size = 1.25, color = "#98959a") +
    coord_cartesian(xlim = size$x, ylim = size$y)
  return(plt)
}

## TIMESERIES

#' Plots animal's speed
#'
#' @param obj avoidnace.single object
#' @param scaling scaling to implement. see `navr::plot_speed` for specifics
#' @param constraints if the speed is scaled, it can be constrained to defined values. 
#' Requires numeric(2)
#' @param ... other \code{\link{ggplot::geom_line}} parameters
#'
#' @return ggplot plot object
#' @export
#'
#' @examples
plot_speed.avoidance.single <- function(obj, scaling = "none", constraints = NULL, ...){
  return(plot_speed(obj$position, scaling=scaling,
                       constraints=  constraints , ...))
}

## CROSSES -----

#' Plots paths for crossings 
#'
#' @param obj avoidance.single
#' @param iCrosses indices of crossings. Can be obtained with \code{\link{collect_crosses}}
#' @param timewindow numeric(1 or 2) time in seconds defining window before and after the crossing to plot. 
#' E.g. tiemwindow = 1 will plot 1 second before and after the time of the cross completed. timewindow c(1,2)
#' will plot time 1s before and 2s after the cross. Defaults to 1
#'
#' @return ggpplot visualisation to animal crosses
#' @export
#'
#' @examples
plot_crosses <- function(obj, iCrosses, timewindow = 1){
  plt <- base_path_plot()
  colors <- rainbow(length(iCrosses))
  if(length(timewindow) == 1) timewindow <- rep(timewindow[1],2)
  for(i in 1:length(iCrosses)){
    cross_time <- obj$position$data$timestamp[iCrosses[i]]
    times <- c(cross_time-timewindow[1], cross_time+timewindow[2])
    cross <- filter_times(obj$position, times)
    plt <- plt + geom_navr_path(cross, color = colors[i], size = 1.25)
  }
  return(plt)
}

## AREA PRESENCE -----
#' Plots an image of area presence for avoidance.single 
#'
#' @param obj avoidance.single object
#' @param darkside which side is the dark? Can be either "left" or "right". 
#' Defaults to "right"
#' @param scale defines x scale through coord_fixed(ratio = scale). 
#' Larger values make the graph narrower. 
#' Defaults to 50
#' 
#' @return ggplot constructed with geom_rect
#' @export
#'
#' @examples
plot_area_presence <- function(obj, darkside, scale, colors){
  UseMethod("plot_area_presence")
}

#' @describeIn create_heatmap creates heatmap for a single animal
#' @export
plot_area_presence.avoidance.single <- function(obj, darkside = RIGHT_ZONE_NAME, 
                                                scale = 50, colors = NULL){
  df <- collect_area_presence(obj)
  if(is.null(df)) return(NULL)
  if(is.null(colors)) colors <- area_presence_scale(darkside)
  plt <- ggplot(df) +
    geom_rect(aes(xmin = start, xmax = end, ymin = -0, ymax = 1, fill = where)) +
    #geom_text(aes(x=(start+end)/2, y = 1.5 + 5/4*(where=="left"), label = where), check_overlap = TRUE) +
    xlab("Time since start") +
    scale_fill_manual(values = colors) +
    coord_fixed(ratio = scale) + 
    ylim(0,6) +
    theme_classic() +
    guides(fill = guide_legend(nrow = 1, title = "")) +
    theme(axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = c(0.9, 0.75),
          legend.background = element_blank(),
          legend.box = "horizontal") 
  return(plt)
}

#' @describeIn 
#' @export
plot_area_presence.avoidance.multiple <- function(obj, darkside = RIGHT_ZONE_NAME,
                                                  scale = 50, colors = NULL){
  df <- collect_area_presence(obj)
  if(is.null(df)) return(NULL)
  if(is.null(colors)) colors <- area_presence_scale(darkside)
  plt <- ggplot(df) +
    geom_rect(aes(xmin = start, xmax = end, ymin = -0, ymax = 1, fill = where)) +
    #geom_text(aes(x=(start+end)/2, y = 1.5 + 5/4*(where=="left"), label = where), check_overlap = TRUE) +
    xlab("Time since start") +
    scale_fill_manual(values = colors) +
    coord_fixed(ratio = scale) + 
    ylim(0,6) +
    theme_classic() +
    guides(fill = guide_legend(nrow = 1, title = "")) +
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
    guides(fill=!none)
  return(res)
}

background_path_plot <- function(background = apparatus_image_path(), darkside = RIGHT_ZONE_NAME){
  res <- ggplot() +
    geom_navr_background(background, BOX_ROOM_REAL$x, BOX_ROOM_REAL$y) + 
    theme_void() + 
    guides(fill="none")
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
area_presence_scale <- function(darkside){
  presence_colors <- c("#e2e2e2", "#000000")
  presence_colors <- setNames(presence_colors, c(other_side_name(darkside), darkside))
  return(presence_colors)
}
