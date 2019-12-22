filepath <- "example-data/three-animals.CSV"
library(dplyr)
library(ggplot2)


obj <- load_data(filepath)
create_heatmap(obj$animal_14)

tab <- load_table(filepath)

## Needs to be weighted by the time spent in each area
# https://stackoverflow.com/questions/24198514/ggplot2-modify-geom-density2d-to-accept-weights-as-a-parameter

tab %>% filter(Event == "Coordinate") %>%
  filter(AnimNo == 14) %>%
  mutate(time_diff = c(diff(Time), 0)) %>%
  # summarize(x = rep(Parameter1, time_diff), y = rep(Parameter2, time_diff)) %>%
  select(Parameter1, Parameter2, time_diff) %>% 
  ggplot(aes(x = Parameter1, y = Parameter2)) + 
  scale_fill_gradientn(colours=rev(rainbow(100, start=0, end=0.75))) +
  stat_density2d(aes(fill=..density..), geom = 'raster', contour = FALSE) + 
  geom_path(color = "white", alpha = 0.3) + theme_bw()


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
