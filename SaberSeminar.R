library(baseballr)
library(tidyverse)
library(readr)

pitches <- read_csv("savant_pitch_level.csv")

view(head(pitches))

pitches |>
  group_by()

table(pitches$pitch_type)

density(pitches$spin_axis)

ggplot() +
  geom_desnity(aes(x=pitches$spin_axis))

qualified_pitchers <- pitches |>
  filter(game_year == 2023) |>
  group_by(player_name) |>
  reframe(n = n()) |>
  filter(n > 500)

dat <- pitches |>
  filter(game_year == 2023, 
         player_name %in% qualified_pitchers$player_name) |>
  select(1, 15, 3, 24, 25, 50, 83) |>
  na.omit() |>
  mutate(axis_x = cos((pi/180) * spin_axis),
         axis_y = sin((pi/180) * spin_axis)) 

ff_R <- dat |>
  filter(pitch_type == "FF",
         p_throws == "R") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate, axis_x, axis_y)

clust <- 1:8
sse_ff_R <- numeric(length(clust))

for (i in clust) {
  k_means <- kmeans(ff_R, centers=i)
  sse_ff_R[i] <- k_means$tot.withinss
}

plot(1:8, sse_ff_R, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

ff_R_clust <- as.data.frame(kmeans(ff_R, centers = 3)$centers)

ff_R_means <- ff_R_clust |>
  mutate(horz = pfx_x * 12,
         vert = pfx_z * 12,
         spin_axis_x = acos(axis_x) * (180/pi),
         spin_axis_y = asin(axis_y) * (180/pi)) 
  




ff_L <- dat |>
  filter(pitch_type == "FF",
         p_throws == "L") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate, axis_x, axis_y)

clust <- 1:8
sse_ff_L <- numeric(length(clust))

for (i in clust) {
  k_means <- kmeans(ff_L, centers=i)
  sse_ff_L[i] <- k_means$tot.withinss
}

plot(1:8, sse_ff_L, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")



