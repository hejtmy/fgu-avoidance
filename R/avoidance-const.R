BOX_ROOM_REAL <- list(x = c(0, 420), y = c(0, 420))
BOX_ROOM <- list(x = c(0, 420), y = c(80, 340))

box_room_size <- function(type = "animal"){
  if(type == "animal"){
    return(BOX_ROOM)
  }
  if (type == "real"){
    return(BOX_ROOM_REAL)
  }
}

## IMAGES
#' Retuns path to the apparatus image
#'
#' @param darkside which zone should be dark? Defaults to "right"
#'
#' @export
apparatus_image_path <- function(darkside = RIGHT_ZONE_NAME){
  if(darkside == LEFT_ZONE_NAME) img <- "apparatus-flip.png"
  if(darkside == RIGHT_ZONE_NAME) img <- "apparatus.png"
  pth <- file.path(system.file("extdata", package = "fgu.avoidance"), "images", img)
  return(pth)
}