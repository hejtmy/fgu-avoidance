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

box_midline_zone <- function(type = "animal", size = 10){
  box <- box_room_size()
  x <- sum(box$x)/2 + c(-size, size)
  y <- box$y
  return(list(x = x, y = y))
}