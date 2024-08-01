library(baseballr)
library(tidyverse)
library(readr)

pitches <- read_csv("savant_pitch_level.csv")

#view(head(pitches))

#pitches |>
#  group_by()

table(pitches$pitch_type)

#density(pitches$spin_axis)

#ggplot() +
#  geom_desnity(aes(x=pitches$spin_axis))

qualified_pitchers <- pitches |>
  filter(game_year == 2023) |>
  group_by(player_name) |>
  reframe(n = n()) |>
  filter(n > 500)

dat <- pitches |>
  filter(player_name %in% qualified_pitchers$player_name) |> # This is still using data from the first 2 years, not just 2023
  select(1, 15, 3, 24, 25, 50, 83) |>
  na.omit() |>
  mutate(axis_x = cos((pi/180) * spin_axis),
         axis_y = sin((pi/180) * spin_axis)) 



# Right-handed 4-seam

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
         spin_axis_x = (acos(axis_x)) * (180/pi),
         spin_axis_y = (asin(axis_y)) * (180/pi))

RFF <- pitches |>
  filter(player_name %in% qualified_pitchers$player_name,
         pitch_type == "FF",
         p_throws == "R",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(pitch_subtype = kmeans(ff_R, centers=3)$cluster)


# Left-handed 4-seam

ff_L <- dat |>
  filter(pitch_type == "FF",
         p_throws == "L") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate, axis_x, axis_y)

sse_ff_L <- numeric(length(clust))

for (i in clust) {
  k_means <- kmeans(ff_L, centers=i)
  sse_ff_L[i] <- k_means$tot.withinss
}

plot(1:8, sse_ff_L, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")
 
ff_L_clust <- as.data.frame(kmeans(ff_L, centers = 3)$centers)

ff_L_means <- ff_L_clust |>
  mutate(horz = pfx_x * 12,
         vert = pfx_z * 12,
         spin_axis_x = (acos(axis_x)) * (180/pi),
         spin_axis_y = (asin(axis_y)) * (180/pi))

LFF <- pitches |>
  filter(player_name %in% qualified_pitchers$player_name,
         pitch_type == "FF",
         p_throws == "L",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(pitch_subtype = kmeans(ff_L, centers=3)$cluster)



# Right-handed Slider

sl_R <- dat |>
  filter(pitch_type == "SL",
         p_throws == "R") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate, axis_x, axis_y)

sse_sl_R <- numeric(length(clust))

for (i in clust) {
  k_means <- kmeans(sl_R, centers=i)
  sse_sl_R[i] <- k_means$tot.withinss
}

plot(1:8, sse_sl_R, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

sl_R_clust <- as.data.frame(kmeans(sl_R, centers = 3)$centers)

sl_R_means <- sl_R_clust |>
  mutate(horz = pfx_x * 12,
         vert = pfx_z * 12,
         spin_axis_x = (acos(axis_x)) * (180/pi),
         spin_axis_y = (asin(axis_y)) * (180/pi))

RSL <- pitches |>
  filter(player_name %in% qualified_pitchers$player_name,
         pitch_type == "SL",
         p_throws == "R",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(pitch_subtype = kmeans(sl_R, centers=3)$cluster)



# Left-handed Slider

sl_L <- dat |>
  filter(pitch_type == "SL",
         p_throws == "L") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate, axis_x, axis_y)

sse_sl_L <- numeric(length(clust))

for (i in clust) {
  k_means <- kmeans(sl_L, centers=i)
  sse_sl_L[i] <- k_means$tot.withinss
}

plot(1:8, sse_sl_L, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

sl_L_clust <- as.data.frame(kmeans(sl_L, centers = 3)$centers)

sl_L_means <- sl_L_clust |>
  mutate(horz = pfx_x * 12,
         vert = pfx_z * 12,
         spin_axis_x = (acos(axis_x)) * (180/pi),
         spin_axis_y = (asin(axis_y)) * (180/pi))

LSL <- pitches |>
  filter(player_name %in% qualified_pitchers$player_name,
         pitch_type == "SL",
         p_throws == "L",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(pitch_subtype = kmeans(sl_L, centers=3)$cluster)



# Right-handed Sinker

si_R <- dat |>
  filter(pitch_type == "SI",
         p_throws == "R") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate, axis_x, axis_y)

sse_si_R <- numeric(length(clust))

for (i in clust) {
  k_means <- kmeans(si_R, centers=i)
  sse_si_R[i] <- k_means$tot.withinss
}

plot(1:8, sse_si_R, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

si_R_clust <- as.data.frame(kmeans(si_R, centers = 3)$centers)

si_R_means <- si_R_clust |>
  mutate(horz = pfx_x * 12,
         vert = pfx_z * 12,
         spin_axis_x = (acos(axis_x)) * (180/pi),
         spin_axis_y = (asin(axis_y)) * (180/pi))

RSI <- pitches |>
  filter(player_name %in% qualified_pitchers$player_name,
         pitch_type == "SI",
         p_throws == "R",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(pitch_subtype = kmeans(si_R, centers=3)$cluster)



# Left-handed Sinker

si_L <- dat |>
  filter(pitch_type == "SI",
         p_throws == "L") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate, axis_x, axis_y)

sse_si_L <- numeric(length(clust))

for (i in clust) {
  k_means <- kmeans(si_L, centers=i)
  sse_si_L[i] <- k_means$tot.withinss
}

plot(1:8, sse_si_L, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

si_L_clust <- as.data.frame(kmeans(si_L, centers = 3)$centers)

si_L_means <- si_L_clust |>
  mutate(horz = pfx_x * 12,
         vert = pfx_z * 12,
         spin_axis_x = (acos(axis_x)) * (180/pi),
         spin_axis_y = (asin(axis_y)) * (180/pi))

LSI <- pitches |>
  filter(player_name %in% qualified_pitchers$player_name,
         pitch_type == "SI",
         p_throws == "L",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(pitch_subtype = kmeans(si_L, centers=3)$cluster)



# Right-handed Changeup

ch_R <- dat |>
  filter(pitch_type == "CH",
         p_throws == "R") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate, axis_x, axis_y)

sse_ch_R <- numeric(length(clust))

for (i in clust) {
  k_means <- kmeans(ch_R, centers=i)
  sse_ch_R[i] <- k_means$tot.withinss
}

plot(1:8, sse_ch_R, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

ch_R_clust <- as.data.frame(kmeans(ch_R, centers = 3)$centers)

ch_R_means <- ch_R_clust |>
  mutate(horz = pfx_x * 12,
         vert = pfx_z * 12,
         spin_axis_x = (acos(axis_x)) * (180/pi),
         spin_axis_y = (asin(axis_y)) * (180/pi))

RCH <- pitches |>
  filter(player_name %in% qualified_pitchers$player_name,
         pitch_type == "CH",
         p_throws == "R",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(pitch_subtype = kmeans(ch_R, centers=3)$cluster)



# Left-handed Changeup

ch_L <- dat |>
  filter(pitch_type == "CH",
         p_throws == "L") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate, axis_x, axis_y)

sse_ch_L <- numeric(length(clust))

for (i in clust) {
  k_means <- kmeans(ch_L, centers=i)
  sse_ch_L[i] <- k_means$tot.withinss
}

plot(1:8, sse_ch_L, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

ch_L_clust <- as.data.frame(kmeans(ch_L, centers = 3)$centers)

ch_L_means <- ch_L_clust |>
  mutate(horz = pfx_x * 12,
         vert = pfx_z * 12,
         spin_axis_x = (acos(axis_x)) * (180/pi),
         spin_axis_y = (asin(axis_y)) * (180/pi))

LCH <- pitches |>
  filter(player_name %in% qualified_pitchers$player_name,
         pitch_type == "CH",
         p_throws == "L",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(pitch_subtype = kmeans(ch_L, centers=3)$cluster)



# Right-handed Curveball

cu_R <- dat |>
  filter(pitch_type == "CU",
         p_throws == "R") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate, axis_x, axis_y)

sse_cu_R <- numeric(length(clust))

for (i in clust) {
  k_means <- kmeans(cu_R, centers=i)
  sse_cu_R[i] <- k_means$tot.withinss
}

plot(1:8, sse_cu_R, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

cu_R_clust <- as.data.frame(kmeans(cu_R, centers = 3)$centers)

cu_R_means <- cu_R_clust |>
  mutate(horz = pfx_x * 12,
         vert = pfx_z * 12,
         spin_axis_x = (acos(axis_x)) * (180/pi),
         spin_axis_y = (asin(axis_y)) * (180/pi))

RCU <- pitches |>
  filter(player_name %in% qualified_pitchers$player_name,
         pitch_type == "CU",
         p_throws == "R",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(pitch_subtype = kmeans(cu_R, centers=3)$cluster)



# Left-handed Curveball

cu_L <- dat |>
  filter(pitch_type == "CU",
         p_throws == "L") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate, axis_x, axis_y)

sse_cu_L <- numeric(length(clust))

for (i in clust) {
  k_means <- kmeans(cu_L, centers=i)
  sse_cu_L[i] <- k_means$tot.withinss
}

plot(1:8, sse_cu_L, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

cu_L_clust <- as.data.frame(kmeans(cu_L, centers = 3)$centers)

cu_L_means <- cu_L_clust |>
  mutate(horz = pfx_x * 12,
         vert = pfx_z * 12,
         spin_axis_x = (acos(axis_x)) * (180/pi),
         spin_axis_y = (asin(axis_y)) * (180/pi))

LCU <- pitches |>
  filter(player_name %in% qualified_pitchers$player_name,
         pitch_type == "CU",
         p_throws == "L",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(pitch_subtype = kmeans(cu_L, centers=3)$cluster)



# Right-handed Cutter

fc_R <- dat |>
  filter(pitch_type == "FC",
         p_throws == "R") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate, axis_x, axis_y)

sse_fc_R <- numeric(length(clust))

for (i in clust) {
  k_means <- kmeans(fc_R, centers=i)
  sse_fc_R[i] <- k_means$tot.withinss
}

plot(1:8, sse_fc_R, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

fc_R_clust <- as.data.frame(kmeans(fc_R, centers = 3)$centers)

fc_R_means <- fc_R_clust |>
  mutate(horz = pfx_x * 12,
         vert = pfx_z * 12,
         spin_axis_x = (acos(axis_x)) * (180/pi),
         spin_axis_y = (asin(axis_y)) * (180/pi))

RFC <- pitches |>
  filter(player_name %in% qualified_pitchers$player_name,
         pitch_type == "FC",
         p_throws == "R",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(pitch_subtype = kmeans(fc_R, centers=3)$cluster)



# Left-handed Cutter

fc_L <- dat |>
  filter(pitch_type == "FC",
         p_throws == "L") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate, axis_x, axis_y)

sse_fc_L <- numeric(length(clust))

for (i in clust) {
  k_means <- kmeans(fc_L, centers=i)
  sse_fc_L[i] <- k_means$tot.withinss
}

plot(1:8, sse_fc_L, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

fc_L_clust <- as.data.frame(kmeans(fc_L, centers = 3)$centers)

fc_L_means <- fc_L_clust |>
  mutate(horz = pfx_x * 12,
         vert = pfx_z * 12,
         spin_axis_x = (acos(axis_x)) * (180/pi),
         spin_axis_y = (asin(axis_y)) * (180/pi))

LFC <- pitches |>
  filter(player_name %in% qualified_pitchers$player_name,
         pitch_type == "FC",
         p_throws == "L",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(pitch_subtype = kmeans(fc_L, centers=3)$cluster)
