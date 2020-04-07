filepath <- "inst/extdata/run/three-animals.CSV"
library(dplyr)
library(ggplot2)
library(fgu.avoidance)

obj <- load_data(filepath)
create_heatmap_polygon(obj$animal_14)
plot_path(obj$animal_14)
tab <- load_table(filepath)

## Needs to be weighted by the time spent in each area
# https://stackoverflow.com/questions/24198514/ggplot2-modify-geom-density2d-to-accept-weights-as-a-parameter

a <- tab %>% filter(Event == "Coordinate") %>%
  filter(AnimNo == 14) %>%
  mutate(time_diff = c(diff(Time), 0)) %>%
  # summarize(x = rep(Parameter1, time_diff), y = rep(Parameter2, time_diff)) %>%
  select(Parameter1, Parameter2, time_diff)

prep <- data.frame(x = rep(a$Parameter1, round(a$time_diff/100, 0)), y= rep(a$Parameter2, round(a$time_diff/100, 0)))

ggplot(prep, aes(x, y)) + 
  scale_fill_gradientn(colours=rev(rainbow(100, start=0, end=0.75))) +
  stat_density2d(aes(fill=..density..), geom = 'raster', contour = FALSE) + 
  geom_path(color = "white", alpha = 0.3) + theme_bw()


## Hanka heatmaps
data_folder <- file.path("..", "data", "one-trial-shuttling-run34-15mins")
dirs <- list.dirs(data_folder, full.names = TRUE)

hab <- load_folder(dirs[2])

## analysis
filepath <- "../data/run35-house-session/"
obj <- load_folder(filepath)
obj <- add_areas(obj)
session_results(obj)
create_heatmap(obj$animal_1, alpha=`..density..`)
heatmap_theme()
