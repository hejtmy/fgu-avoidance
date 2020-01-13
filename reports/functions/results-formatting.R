library(knitr)
session_results_output <- function(results){
   
  txt <- ""
  txt <- paste0(txt, "<br>Distance:", round(results$distance, 2))
  txt <- paste0(txt, "<br>Time in left region", round(results$time_left))
  txt <- paste0(txt, "<br>Time in right region: ", round(results$time_right, 2))
  txt <- paste0(txt, "<br>Time in central region: ", round(results$time_center, 2))
  txt <- paste0(txt, "<br>Crosses to the right: ", round(results$crosses_right, 2))
  return(txt)
}