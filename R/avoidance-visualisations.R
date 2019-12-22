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
    scale_fill_gradientn(colours=rev(rainbow(100, start=0, end=0.75))) +
    stat_density2d(aes(fill=..level..,alpha=..level..),
                   bins=bins, geom = 'polygon') +
    lims(x=c(0,450), y=c(0, 400)) +
    theme_bw()
  return(plt)
}

