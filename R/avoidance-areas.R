CENTRAL_ZONE_NAME = "central"
LEFT_ZONE_NAME = "left"
RIGHT_ZONE_NAME = "right"
ROOM_ZONE_NAME = "room"

#' @export
default_zones <- function(type = "animal", central_size = 20){
  return(list(
    central = central_zone(type, central_size),
    left = left_zone(),
    right = right_zone()
  ))
}

#' @export
room_zone <- function(type = "animal"){
  box <- box_room_size(type)
  area <- AreaObject(ROOM_ZONE_NAME, type = "rectangle",
                     points = limits_to_points(box$x, box$y))
  return(area)
}

#' @export
central_zone <- function(type = "animal", size = 20){
  box <- box_room_size(type)
  x <- sum(box$x)/2 + c(-size, size)
  y <- box$y
  area <- AreaObject(CENTRAL_ZONE_NAME, type="rectangle",
                     points = limits_to_points(x, y))
  return(area)
}

#' @export
left_zone <- function(){
  room <- room_zone()
  central <- central_zone()
  points <- rbind(room$points[c(1, 4),], central$points[c(4, 1), ])
  area <- AreaObject(LEFT_ZONE_NAME, type ="rectangle", 
                        points = points)
  return(area)
}

#' @export
right_zone <- function(){
  room <- room_zone()
  central <- central_zone()
  points <- rbind(room$points[c(2, 3),], central$points[c(3, 2), ])
  area <- AreaObject(RIGHT_ZONE_NAME, type = "rectangle", 
                     points = points)
  return(area)
}

limits_to_points <- function(x, y){
  return(matrix(c(x[1], y[1], x[2], y[1], x[2], y[2], x[1], y[2]), ncol=2, byrow=TRUE))
}