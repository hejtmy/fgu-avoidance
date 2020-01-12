BOX_ROOM_REAL <- list(x=c(0, 420), y = c(0, 420))
BOX_ROOM <- list(x=c(10, 420), y = c(80, 340))

box_room_size <- function(type = "animal"){
  if(type == "animal"){
    return(BOX_ROOM)
  }
  if (type == "real"){
    return(BOX_ROOM_REAL)
  }
}

room_zone <- function(type = "animal"){
  box <- box_room_size(type)
  area <- AreaObject("room", type = "rectangle",
                     points = limits_to_points(box$x, box$y))
  return(area)
}

central_zone <- function(type = "animal", size = 20){
  box <- box_room_size(type)
  x <- sum(box$x)/2 + c(-size, size)
  y <- box$y
  area <- AreaObject("midzone", type="rectangle",
                     points = limits_to_points(x, y))
  return(area)
}

left_zone <- function(){
  room <- room_zone()
  central <- central_zone()
  points <- rbind(room$points[c(1, 4),], central$points[c(4, 1), ])
  area <- AreaObject("left", type ="rectangle", 
                        points = points)
  return(area)
}
right_zone <- function(){
  room <- room_zone()
  central <- central_zone()
  points <- rbind(room$points[c(2, 3),], central$points[c(3, 2), ])
  area <- AreaObject("right", type = "rectangle", 
                     points = points)
  return(area)
}

limits_to_points <- function(x, y){
  return(matrix(c(x[1], y[1], x[2], y[1], x[2], y[2], x[1], y[2]), ncol=2, byrow=TRUE))
}