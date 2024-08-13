library(baseballr)
library(tidyverse)
library(readr)
library(cluster)

pitches <- read_csv("savant_pitch_level.csv")

#view(head(pitches))

#pitches |>
#  group_by()

table(pitches$pitch_type)

dd <- pitches |> filter(player_name == "Duffy, Danny")

#density(pitches$spin_axis)

#ggplot() +
#  geom_desnity(aes(x=pitches$spin_axis))

qualified_pitchers <- pitches |>
  filter(game_year == 2023,
         role_key == "SP") |>
  group_by(player_name) |>
  reframe(n = n()) |>
  filter(n > 250)

dat <- pitches |>
  filter(player_name %in% qualified_pitchers$player_name,
         game_year == 2023) |>
  select(1, 15, 3, 24, 25, 50, 83) |>
  na.omit() |>
  mutate(horz = pfx_x * 12,
         vert = pfx_z * 12)

table(dat$pitch_type)


table(dat$pitch_type, dat$p_throws)


# Right-handed 4-seam

ff_R <- dat |>
  filter(pitch_type == "FF",
         p_throws == "R") |>
  select(release_speed, horz, vert, release_spin_rate) |>
  scale()

clust <- 1:20
sse_ff_R <- numeric(length(clust))

for (i in clust) {
  set.seed(740)
  k_means <- kmeans(ff_R, centers=i, iter.max=200, algorithm="MacQueen")
  sse_ff_R[i] <- k_means$tot.withinss
}

plot(1:20, sse_ff_R, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

# 6

ff_R_clust <- as.data.frame(kmeans(ff_R, centers = 6)$centers)

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
  mutate(cluster = kmeans(ff_R, centers=6)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))


# Left-handed 4-seam

ff_L <- dat |>
  filter(pitch_type == "FF",
         p_throws == "L") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate) |>
  scale()

sse_ff_L <- numeric(length(clust))

for (i in clust) {
  set.seed(740)
  k_means <- kmeans(ff_L, centers=i, iter.max=200, algorithm="MacQueen")
  sse_ff_L[i] <- k_means$tot.withinss
}

plot(1:20, sse_ff_L, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

# 5

ff_L_clust <- as.data.frame(kmeans(ff_L, centers = 5)$centers)

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
  mutate(cluster = kmeans(ff_L, centers=5)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Right-handed Slider

sl_R <- dat |>
  filter(pitch_type == "SL",
         p_throws == "R") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate) |>
  scale()

sse_sl_R <- numeric(length(clust))

for (i in clust) {
  k_means <- kmeans(sl_R, centers=i, iter.max=200, algorithm="MacQueen")
  sse_sl_R[i] <- k_means$tot.withinss
}

plot(1:20, sse_sl_R, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

# 6

sl_R_clust <- as.data.frame(kmeans(sl_R, centers = 6)$centers)

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
  mutate(cluster = kmeans(sl_R, centers=6)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Left-handed Slider

sl_L <- dat |>
  filter(pitch_type == "SL",
         p_throws == "L") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate) |>
  scale()

sse_sl_L <- numeric(length(clust))

for (i in clust) {
  set.seed(740)
  k_means <- kmeans(sl_L, centers=i, iter.max=200, algorithm="MacQueen")
  sse_sl_L[i] <- k_means$tot.withinss
}

plot(1:20, sse_sl_L, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

# 5

sl_L_clust <- as.data.frame(kmeans(sl_L, centers = 5)$centers)

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
  mutate(cluster = kmeans(sl_L, centers=5)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Right-handed Sinker

si_R <- dat |>
  filter(pitch_type == "SI",
         p_throws == "R") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate) |>
  scale()

sse_si_R <- numeric(length(clust))

for (i in clust) {
  set.seed(740)
  k_means <- kmeans(si_R, centers=i, iter.max=200, algorithm="MacQueen")
  sse_si_R[i] <- k_means$tot.withinss
}

plot(1:20, sse_si_R, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

# 5

si_R_clust <- as.data.frame(kmeans(si_R, centers = 5)$centers)

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
  mutate(cluster = kmeans(si_R, centers=5)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Left-handed Sinker

si_L <- dat |>
  filter(pitch_type == "SI",
         p_throws == "L") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate) |>
  scale()

sse_si_L <- numeric(length(clust))

for (i in clust) {
  set.seed(740)
  k_means <- kmeans(si_L, centers=i, iter.max=200, algorithm="MacQueen")
  sse_si_L[i] <- k_means$tot.withinss
}

plot(1:20, sse_si_L, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

# 5

si_L_clust <- as.data.frame(kmeans(si_L, centers = 5)$centers)

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
  mutate(cluster = kmeans(si_L, centers=5)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Right-handed Changeup

ch_R <- dat |>
  filter(pitch_type == "CH",
         p_throws == "R") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate) |>
  scale()

sse_ch_R <- numeric(length(clust))

for (i in clust) {
  set.seed(740)
  k_means <- kmeans(ch_R, centers=i, iter.max=200, algorithm="MacQueen")
  sse_ch_R[i] <- k_means$tot.withinss
}

plot(1:20, sse_ch_R, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

# 6

ch_R_clust <- as.data.frame(kmeans(ch_R, centers = 6)$centers)

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
  mutate(cluster = kmeans(ch_R, centers=6)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Left-handed Changeup

ch_L <- dat |>
  filter(pitch_type == "CH",
         p_throws == "L") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate) |>
  scale()

sse_ch_L <- numeric(length(clust))

for (i in clust) {
  set.seed(740)
  k_means <- kmeans(ch_L, centers=i, iter.max=200, algorithm="MacQueen")
  sse_ch_L[i] <- k_means$tot.withinss
}

plot(1:20, sse_ch_L, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

# 5

ch_L_clust <- as.data.frame(kmeans(ch_L, centers = 5)$centers)

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
  mutate(cluster = kmeans(ch_L, centers=5)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Right-handed Curveball

cu_R <- dat |>
  filter(pitch_type == "CU",
         p_throws == "R") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate) |>
  scale()

sse_cu_R <- numeric(length(clust))

for (i in clust) {
  set.seed(740)
  k_means <- kmeans(cu_R, centers=i, iter.max=200, algorithm="MacQueen")
  sse_cu_R[i] <- k_means$tot.withinss
}

plot(1:20, sse_cu_R, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

# 5

cu_R_clust <- as.data.frame(kmeans(cu_R, centers = 5)$centers)

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
  mutate(cluster = kmeans(cu_R, centers=5)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Left-handed Curveball

cu_L <- dat |>
  filter(pitch_type == "CU",
         p_throws == "L") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate) |>
  scale()

sse_cu_L <- numeric(length(clust))

for (i in clust) {
  set.seed(740)
  k_means <- kmeans(cu_L, centers=i, iter.max=200, algorithm="MacQueen")
  sse_cu_L[i] <- k_means$tot.withinss
}

plot(1:20, sse_cu_L, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

# 6

cu_L_clust <- as.data.frame(kmeans(cu_L, centers = 6)$centers)

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
  mutate(cluster = kmeans(cu_L, centers=6)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Right-handed Cutter

fc_R <- dat |>
  filter(pitch_type == "FC",
         p_throws == "R") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate) |>
  scale()

sse_fc_R <- numeric(length(clust))

for (i in clust) {
  set.seed(740)
  k_means <- kmeans(fc_R, centers=i, iter.max=200, algorithm="MacQueen")
  sse_fc_R[i] <- k_means$tot.withinss
}

plot(1:20, sse_fc_R, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

# 5

fc_R_clust <- as.data.frame(kmeans(fc_R, centers = 5)$centers)

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
  mutate(cluster = kmeans(fc_R, centers=5)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Left-handed Cutter

fc_L <- dat |>
  filter(pitch_type == "FC",
         p_throws == "L") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate) |>
  scale()

sse_fc_L <- numeric(length(clust))

for (i in clust) {
  set.seed(740)
  k_means <- kmeans(fc_L, centers=i, iter.max=200, algorithm="MacQueen")
  sse_fc_L[i] <- k_means$tot.withinss
}

plot(1:20, sse_fc_L, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

# 5

fc_L_clust <- as.data.frame(kmeans(fc_L, centers = 5)$centers)

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
  mutate(cluster = kmeans(fc_L, centers=5)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Right-handed Sweeper

st_R <- dat |>
  filter(pitch_type == "ST",
         p_throws == "R") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate) |>
  scale()

sse_st_R <- numeric(length(clust))

for (i in clust) {
  set.seed(740)
  k_means <- kmeans(st_R, centers=i, iter.max=200, algorithm="MacQueen")
  sse_st_R[i] <- k_means$tot.withinss
}

plot(1:20, sse_st_R, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

# 5

st_R_clust <- as.data.frame(kmeans(st_R, centers = 5)$centers)

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
  mutate(cluster = kmeans(st_R, centers=5)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Left-handed Sweeper

st_L <- dat |>
  filter(pitch_type == "ST",
         p_throws == "L") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate) |>
  scale()

sse_st_L <- numeric(length(clust))

for (i in clust) {
  set.seed(740)
  k_means <- kmeans(st_L, centers=i, iter.max=200, algorithm="MacQueen")
  sse_st_L[i] <- k_means$tot.withinss
}

plot(1:20, sse_st_L, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

# 4

st_L_clust <- as.data.frame(kmeans(st_L, centers = 4)$centers)

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
  mutate(cluster = kmeans(st_L, centers=4)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Right-handed Splitter

fs_R <- dat |>
  filter(pitch_type == "FS",
         p_throws == "R") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate) |>
  scale()

sse_fs_R <- numeric(length(clust))

for (i in clust) {
  set.seed(740)
  k_means <- kmeans(fs_R, centers=i, iter.max=200, algorithm="MacQueen")
  sse_fs_R[i] <- k_means$tot.withinss
}

plot(1:20, sse_fs_R, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

# 4

fs_R_clust <- as.data.frame(kmeans(fs_R, centers = 4)$centers)

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
  mutate(cluster = kmeans(fs_R, centers=4)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Left-handed Splitter

fs_L <- dat |>
  filter(pitch_type == "FS",
         p_throws == "L") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate) |>
  scale()

sse_fs_L <- numeric(length(clust))

for (i in clust) {
  set.seed(740)
  k_means <- kmeans(fs_L, centers=i, iter.max=200, algorithm="MacQueen")
  sse_fs_L[i] <- k_means$tot.withinss
}

plot(1:20, sse_fs_L, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

# 5, but there is only 58 pitches, so this is not going to be used

# fs_L_clust <- as.data.frame(kmeans(fs_L, centers = _)$centers)

#LFS <- pitches |>
#  filter(player_name %in% qualified_pitchers$player_name,
#         game_year == 2023,
#         pitch_type == "FS",
#         p_throws == "L",
#         !is.na(release_speed),
#         !is.na(pfx_x),
#         !is.na(pfx_z),
#         !is.na(release_spin_rate),
#         !is.na(spin_axis)) |>
#  mutate(cluster = kmeans(fs_L, centers=_)$cluster,
#         pitch_subtype = paste0(pitch_type,cluster))



# Right-handed Knuckle-curve

kc_R <- dat |>
  filter(pitch_type == "KC",
         p_throws == "R") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate) |>
  scale()

sse_kc_R <- numeric(length(clust))

for (i in clust) {
  set.seed(740)
  k_means <- kmeans(kc_R, centers=i, iter.max=200, algorithm="MacQueen")
  sse_kc_R[i] <- k_means$tot.withinss
}

plot(1:20, sse_kc_R, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

# 5

kc_R_clust <- as.data.frame(kmeans(kc_R, centers = 5)$centers)

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
  mutate(cluster = kmeans(kc_R, centers=5)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Left-handed Knuckle-curve

kc_L <- dat |>
  filter(pitch_type == "KC",
         p_throws == "L") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate) |>
  scale()

sse_kc_L <- numeric(length(clust))

for (i in clust) {
  set.seed(740)
  k_means <- kmeans(kc_L, centers=i, iter.max=200, algorithm="MacQueen")
  sse_kc_L[i] <- k_means$tot.withinss
}

plot(1:20, sse_kc_L, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

# 4

kc_L_clust <- as.data.frame(kmeans(kc_L, centers = 4)$centers)

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
  mutate(cluster = kmeans(kc_L, centers=4)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Right-handed Slurve

sv_R <- dat |>
  filter(pitch_type == "SV",
         p_throws == "R") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate) |>
  scale()

sse_sv_R <- numeric(length(clust))

for (i in clust) {
  set.seed(740)
  k_means <- kmeans(sv_R, centers=i, iter.max=200, algorithm="MacQueen")
  sse_sv_R[i] <- k_means$tot.withinss
}

plot(1:20, sse_sv_R, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

# 5

sv_R_clust <- as.data.frame(kmeans(sv_R, centers = 5)$centers)

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
  mutate(cluster = kmeans(sv_R, centers=5)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Left-handed Slurve

sv_L <- dat |>
  filter(pitch_type == "SV",
         p_throws == "L") |>
  select(release_speed, pfx_x, pfx_z, release_spin_rate) |>
  scale()

sse_sv_L <- numeric(length(clust))

for (i in clust) {
  set.seed(740)
  k_means <- kmeans(sv_L, centers=i, iter.max=200, algorithm="MacQueen")
  sse_sv_L[i] <- k_means$tot.withinss
}

plot(1:20, sse_sv_L, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

# 4

sv_L_clust <- as.data.frame(kmeans(sv_L, centers = 4)$centers)

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
  mutate(cluster = kmeans(sv_L, centers=4)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



