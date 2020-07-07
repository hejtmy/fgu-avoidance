library(dplyr)
library(ggplot2)
devtools::load_all(".")

filepath <- "inst/extdata/run/rat9.CSV"

obj <- load_data(filepath)
create_heatmap(obj$animal_14)
plot_path(obj$animal_14)
tab <- load_table(filepath)

# Needs to be weighted by the time spent in each area
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

## Analysis
filepath <- "../data/run35-house-session/"
obj <- load_folder(filepath)
obj <- add_areas(obj)
session_results(obj)
create_heatmap(obj$animal_1, alpha=`..density..`)
heatmap_theme()

## Searching freezings
filepath <- "inst/extdata/run/rat9.CSV"
obj <- load_data(filepath)
nav <- obj$animal_9$position

nav <- remove_unreal_speeds(nav, type="value", cutoff=100)
nav_smooth <- smooth_speed(nav, type="median", points=9)
plot_speed(nav_smooth)
freezes <- search_stops(nav_smooth, speed_threshold = 10, min_duration = 2)
plot_speed(nav_smooth) +
  geom_navr_timeseries_events(freezes$time_since_start,
                              durations = freezes$duration,
                              color = "red", size=2) +
  geom_hline(yintercept=10)

freezes <- collect_freezes(obj$animal_9, speed_threshold = 1, min_duration = 2)
plot_speed(obj$animal_9$position) +
   geom_navr_timeseries_events(freezes$time_since_start,
                              durations = freezes$duration,
                              color = "red", size=2) +
  geom_hline(yintercept=10)
 

freezes_times <- rbind(freezes$time, freezes$time + freezes$duration)
plot_path(obj$animal_9) +
  geom_navr_path_segments(nav, freezes_times)
