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
  filter(player_name %in% qualified_pitchers$player_name,
         game_year == 2023) |>
  select(1, 15, 3, 24, 25, 50, 83) |>
  na.omit() |>
  mutate(axis_x = cos((pi/180) * spin_axis),
         axis_y = sin((pi/180) * spin_axis))

table(dat$pitch_type)


table(dat$pitch_type, dat$p_throws)


# Right-handed 4-seam

ff_R <- dat |>
  filter(pitch_type == "FF",
         p_throws == "R") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate, axis_x, axis_y)

clust <- 1:15
sse_ff_R <- numeric(length(clust))

for (i in clust) {
  k_means <- kmeans(ff_R, centers=i)
  sse_ff_R[i] <- k_means$tot.withinss
}

plot(1:15, sse_ff_R, type="b",
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
         game_year == 2023,
         pitch_type == "FF",
         p_throws == "R",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(cluster = kmeans(ff_R, centers=3)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))


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
         game_year == 2023,
         pitch_type == "FF",
         p_throws == "L",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(cluster = kmeans(ff_L, centers=3)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



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
         game_year == 2023,
         pitch_type == "SL",
         p_throws == "R",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(cluster = kmeans(sl_R, centers=3)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



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
         game_year == 2023,
         pitch_type == "SL",
         p_throws == "L",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(cluster = kmeans(sl_L, centers=3)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



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
         game_year == 2023,
         pitch_type == "SI",
         p_throws == "R",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(cluster = kmeans(si_R, centers=3)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



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
         game_year == 2023,
         pitch_type == "SI",
         p_throws == "L",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(cluster = kmeans(si_L, centers=3)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



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
         game_year == 2023,
         pitch_type == "CH",
         p_throws == "R",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(cluster = kmeans(ch_R, centers=3)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



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
         game_year == 2023,
         pitch_type == "CH",
         p_throws == "L",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(cluster = kmeans(ch_L, centers=3)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



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
         game_year == 2023,
         pitch_type == "CU",
         p_throws == "R",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(cluster = kmeans(cu_R, centers=3)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



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
         game_year == 2023,
         pitch_type == "CU",
         p_throws == "L",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(cluster = kmeans(cu_L, centers=3)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



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
         game_year == 2023,
         pitch_type == "FC",
         p_throws == "R",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(cluster = kmeans(fc_R, centers=3)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



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
         game_year == 2023,
         pitch_type == "FC",
         p_throws == "L",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(cluster = kmeans(fc_L, centers=3)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Right-handed Sweeper

st_R <- dat |>
  filter(pitch_type == "ST",
         p_throws == "R") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate, axis_x, axis_y)

sse_st_R <- numeric(length(clust))

for (i in clust) {
  k_means <- kmeans(st_R, centers=i)
  sse_st_R[i] <- k_means$tot.withinss
}

plot(1:8, sse_st_R, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

st_R_clust <- as.data.frame(kmeans(st_R, centers = 3)$centers)

st_R_means <- st_R_clust |>
  mutate(horz = pfx_x * 12,
         vert = pfx_z * 12,
         spin_axis_x = (acos(axis_x)) * (180/pi),
         spin_axis_y = (asin(axis_y)) * (180/pi))

RST <- pitches |>
  filter(player_name %in% qualified_pitchers$player_name,
         game_year == 2023,
         pitch_type == "ST",
         p_throws == "R",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(cluster = kmeans(st_R, centers=3)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Left-handed Sweeper

st_L <- dat |>
  filter(pitch_type == "ST",
         p_throws == "L") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate, axis_x, axis_y)

sse_st_L <- numeric(length(clust))

for (i in clust) {
  k_means <- kmeans(st_L, centers=i)
  sse_st_L[i] <- k_means$tot.withinss
}

plot(1:8, sse_st_L, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

st_L_clust <- as.data.frame(kmeans(st_L, centers = 3)$centers)

st_L_means <- st_L_clust |>
  mutate(horz = pfx_x * 12,
         vert = pfx_z * 12,
         spin_axis_x = (acos(axis_x)) * (180/pi),
         spin_axis_y = (asin(axis_y)) * (180/pi))

LST <- pitches |>
  filter(player_name %in% qualified_pitchers$player_name,
         game_year == 2023,
         pitch_type == "ST",
         p_throws == "L",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(cluster = kmeans(st_L, centers=3)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Right-handed Splitter

fs_R <- dat |>
  filter(pitch_type == "FS",
         p_throws == "R") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate, axis_x, axis_y)

sse_fs_R <- numeric(length(clust))

for (i in clust) {
  k_means <- kmeans(fs_R, centers=i)
  sse_fs_R[i] <- k_means$tot.withinss
}

plot(1:8, sse_fs_R, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

fs_R_clust <- as.data.frame(kmeans(fs_R, centers = 3)$centers)

fs_R_means <- fs_R_clust |>
  mutate(horz = pfx_x * 12,
         vert = pfx_z * 12,
         spin_axis_x = (acos(axis_x)) * (180/pi),
         spin_axis_y = (asin(axis_y)) * (180/pi))

RFS <- pitches |>
  filter(player_name %in% qualified_pitchers$player_name,
         game_year == 2023,
         pitch_type == "FS",
         p_throws == "R",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(cluster = kmeans(fs_R, centers=3)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Left-handed Splitter

fs_L <- dat |>
  filter(pitch_type == "FS",
         p_throws == "L") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate, axis_x, axis_y)

sse_fs_L <- numeric(length(clust))

for (i in clust) {
  k_means <- kmeans(fs_L, centers=i)
  sse_fs_L[i] <- k_means$tot.withinss
}

plot(1:8, sse_fs_L, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

fs_L_clust <- as.data.frame(kmeans(fs_L, centers = 3)$centers)

fs_L_means <- fs_L_clust |>
  mutate(horz = pfx_x * 12,
         vert = pfx_z * 12,
         spin_axis_x = (acos(axis_x)) * (180/pi),
         spin_axis_y = (asin(axis_y)) * (180/pi))

LFS <- pitches |>
  filter(player_name %in% qualified_pitchers$player_name,
         game_year == 2023,
         pitch_type == "FS",
         p_throws == "L",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(cluster = kmeans(fs_L, centers=3)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Right-handed Knuckle-curve

kc_R <- dat |>
  filter(pitch_type == "KC",
         p_throws == "R") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate, axis_x, axis_y)

sse_kc_R <- numeric(length(clust))

for (i in clust) {
  k_means <- kmeans(kc_R, centers=i)
  sse_kc_R[i] <- k_means$tot.withinss
}

plot(1:8, sse_kc_R, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

kc_R_clust <- as.data.frame(kmeans(kc_R, centers = 3)$centers)

kc_R_means <- kc_R_clust |>
  mutate(horz = pfx_x * 12,
         vert = pfx_z * 12,
         spin_axis_x = (acos(axis_x)) * (180/pi),
         spin_axis_y = (asin(axis_y)) * (180/pi))

RKC <- pitches |>
  filter(player_name %in% qualified_pitchers$player_name,
         game_year == 2023,
         pitch_type == "KC",
         p_throws == "R",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(cluster = kmeans(kc_R, centers=3)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Left-handed Knuckle-curve

kc_L <- dat |>
  filter(pitch_type == "KC",
         p_throws == "L") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate, axis_x, axis_y)

sse_kc_L <- numeric(length(clust))

for (i in clust) {
  k_means <- kmeans(kc_L, centers=i)
  sse_kc_L[i] <- k_means$tot.withinss
}

plot(1:8, sse_kc_L, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

kc_L_clust <- as.data.frame(kmeans(kc_L, centers = 3)$centers)

kc_L_means <- kc_L_clust |>
  mutate(horz = pfx_x * 12,
         vert = pfx_z * 12,
         spin_axis_x = (acos(axis_x)) * (180/pi),
         spin_axis_y = (asin(axis_y)) * (180/pi))

LKC <- pitches |>
  filter(player_name %in% qualified_pitchers$player_name,
         game_year == 2023,
         pitch_type == "KC",
         p_throws == "L",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(cluster = kmeans(kc_L, centers=3)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Right-handed Slurve

sv_R <- dat |>
  filter(pitch_type == "SV",
         p_throws == "R") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate, axis_x, axis_y)

sse_sv_R <- numeric(length(clust))

for (i in clust) {
  k_means <- kmeans(sv_R, centers=i)
  sse_sv_R[i] <- k_means$tot.withinss
}

plot(1:8, sse_sv_R, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

sv_R_clust <- as.data.frame(kmeans(sv_R, centers = 3)$centers)

sv_R_means <- sv_R_clust |>
  mutate(horz = pfx_x * 12,
         vert = pfx_z * 12,
         spin_axis_x = (acos(axis_x)) * (180/pi),
         spin_axis_y = (asin(axis_y)) * (180/pi))

RSV <- pitches |>
  filter(player_name %in% qualified_pitchers$player_name,
         game_year == 2023,
         pitch_type == "SV",
         p_throws == "R",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(cluster = kmeans(sv_R, centers=3)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Left-handed Slurve

sv_L <- dat |>
  filter(pitch_type == "SV",
         p_throws == "L") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate, axis_x, axis_y)

sse_sv_L <- numeric(length(clust))

for (i in clust) {
  k_means <- kmeans(sv_L, centers=i)
  sse_sv_L[i] <- k_means$tot.withinss
}

plot(1:8, sse_sv_L, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

sv_L_clust <- as.data.frame(kmeans(sv_L, centers = 3)$centers)

sv_L_means <- sv_L_clust |>
  mutate(horz = pfx_x * 12,
         vert = pfx_z * 12,
         spin_axis_x = (acos(axis_x)) * (180/pi),
         spin_axis_y = (asin(axis_y)) * (180/pi))

LSV <- pitches |>
  filter(player_name %in% qualified_pitchers$player_name,
         game_year == 2023,
         pitch_type == "SV",
         p_throws == "L",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(cluster = kmeans(sv_L, centers=3)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))

# Right-handed Forkball

fo_R <- dat |>
  filter(pitch_type == "FO",
         p_throws == "R") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate, axis_x, axis_y)

sse_fo_R <- numeric(length(clust))

for (i in clust) {
  k_means <- kmeans(fo_R, centers=i)
  sse_fo_R[i] <- k_means$tot.withinss
}

plot(1:8, sse_fo_R, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

fo_R_clust <- as.data.frame(kmeans(fo_R, centers = 3)$centers)

fo_R_means <- fo_R_clust |>
  mutate(horz = pfx_x * 12,
         vert = pfx_z * 12,
         spin_axis_x = (acos(axis_x)) * (180/pi),
         spin_axis_y = (asin(axis_y)) * (180/pi))

RFO <- pitches |>
  filter(player_name %in% qualified_pitchers$player_name,
         game_year == 2023,
         pitch_type == "FO",
         p_throws == "R",
         !is.na(release_speed),
         !is.na(pfx_x),
         !is.na(pfx_z),
         !is.na(release_spin_rate),
         !is.na(spin_axis)) |>
  mutate(cluster = kmeans(fo_R, centers=3)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))
#Seeing as all Forkballs are Kodai Senga, there is no reason to include them as it lacks the generality desired for this research


