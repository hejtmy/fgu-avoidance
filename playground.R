filepath <- "example-data/three-animals.CSV"
library(dplyr)
library(ggplot2)
library(fgu.avoidance)

obj <- load_data(filepath)
create_heatmap_polygon(obj$animal_14)
create_path(obj$animal_14)
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

## Arena size

filepath <- "../data/arena-size/arena-size.CSV"
obj <- load_data(filepath)
fgu.avoidance::create_path(obj$animal_3)

range(obj$animal_3$position$data$position_x)
range(obj$animal_3$position$data$position_y)

## Areas search
obj14 <- obj$animal_14
obj14 <- add_areas(obj14, list(central_zone(size=50), left_zone(), right_zone()))
navr::calculate_areas_time(obj14$position)
get_area_visits(obj14$position, "left", from="right", between_allowed = 0)
get_area_visits(obj14$position, "left", from="central", between_allowed = 1)
get_area_visits(obj14$position, "left", from="right", between_allowed = 1)

# 2090 is considered left from central but not left from right
times <- range(obj14$position$data$timestamp[2080:2100])
obj_erroneous <- filter_times(obj14$position, times)
plot_path(obj_erroneous) + geom_navr_area(room_zone()) + geom_navr_area(central_zone())
