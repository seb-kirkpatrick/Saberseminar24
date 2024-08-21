#library(baseballr)
library(tidyverse)
library(cluster)
library(lme4)
library(merTools)

pitches <- read_csv("savant_pitch_level.csv")

#view(head(pitches))

#pitches |>
#  group_by()

table(pitches$pitch_type)

#dd <- pitches |> filter(player_name == "Duffy, Danny")

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
  dplyr::select(1, 15, 3, 24, 25, 50, 83) |>
  na.omit() |>
  mutate(horz = pfx_x * 12,
         vert = pfx_z * 12)

table(dat$pitch_type)


table(dat$pitch_type, dat$p_throws)



### Clustering ###

# Right-handed 4-seam

ff_R <- dat |>
  filter(pitch_type == "FF",
         p_throws == "R") |>
  dplyr::select(release_speed, horz, vert, release_spin_rate) |>
  scale()

clust <- 1:20
sse_ff_R <- numeric(length(clust))

for (i in clust) {
  set.seed(740)
  k_means <- kmeans(ff_R, centers=i, iter.max=300, algorithm="MacQueen")
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
  dplyr::select(release_speed, horz, vert, release_spin_rate) |>
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
  dplyr::select(release_speed, horz, vert, release_spin_rate) |>
  scale()

sse_sl_R <- numeric(length(clust))

for (i in clust) {
  set.seed(740)
  k_means <- kmeans(sl_R, centers=i, iter.max=200, algorithm="MacQueen")
  sse_sl_R[i] <- k_means$tot.withinss
}

plot(1:20, sse_sl_R, type="b",
     xlab = "Number of Clusters",
     yla = "Within groups sum of squares")

# 5

sl_R_clust <- as.data.frame(kmeans(sl_R, centers = 5)$centers)

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
  mutate(cluster = kmeans(sl_R, centers=5)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Left-handed Slider

sl_L <- dat |>
  filter(pitch_type == "SL",
         p_throws == "L") |>
  dplyr::select(release_speed, horz, vert, release_spin_rate) |>
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
  dplyr::select(release_speed, horz, vert, release_spin_rate) |>
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
  dplyr::select(release_speed, horz, vert, release_spin_rate) |>
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
  dplyr::select(release_speed, horz, vert, release_spin_rate) |>
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
  dplyr::select(release_speed, horz, vert, release_spin_rate) |>
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
  dplyr::select(release_speed, horz, vert, release_spin_rate) |>
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
  dplyr::select(release_speed, horz, vert, release_spin_rate) |>
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
  dplyr::select(release_speed, horz, vert, release_spin_rate) |>
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
  dplyr::select(release_speed, horz, vert, release_spin_rate) |>
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
  dplyr::select(release_speed, horz, vert, release_spin_rate) |>
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
  dplyr::select(release_speed, horz, vert, release_spin_rate) |>
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
  dplyr::select(release_speed, horz, vert, release_spin_rate) |>
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
  dplyr::select(release_speed, horz, vert, release_spin_rate) |>
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

pitches |>
  filter(player_name == "Rom, Drew",
         game_year == 2023,
         role_key == "SP") |>
  group_by(pitch_type) |> 
  reframe(count = n())

# Cool Drew Rom shout-out



# Right-handed Knuckle-curve

kc_R <- dat |>
  filter(pitch_type == "KC",
         p_throws == "R") |>
  dplyr::select(release_speed, horz, vert, release_spin_rate) |>
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
  dplyr::select(release_speed, horz, vert, release_spin_rate) |>
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
  dplyr::select(release_speed, horz, vert, release_spin_rate) |>
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

# Jose Berrios + Marcus Stroman, so not using



# Left-handed Slurve

sv_L <- dat |>
  filter(pitch_type == "SV",
         p_throws == "L") |>
  dplyr::select(release_speed, horz, vert, release_spin_rate) |>
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

# Julio Urias + Kyle Harrison, so not using this either

# Cluster visualizations

pitch <- rep(c("4-Seam", "Slider", "Sinker", "Changeup", "Curveball", "Cutter", "Sweeper", "Split-Finger", "Knuckle-Curve"), each=2)
handed <- rep(c("RHP", "LHP"), 9)
clusters <- c(6, 5, 5, 5, 5, 5, 6, 5, 5, 6, 5, 5, 5, 4, 4, 0, 5, 4)

clust_viz <- data.frame(pitch, handed, clusters)

clust_viz |>
  ggplot(
    aes(x=pitch, y=clusters, fill=handed)
  ) + 
  geom_col(position="dodge") + 
  geom_text(
    aes(label=clusters), 
    position=position_dodge(width=0.9), 
    vjust=-0.5
  ) + 
  labs(
    title = "Clusters by Pitch Type and Handedness",
    x = "Primary Pitch Type",
    y = "Number of Clusters",
    fill = "Pitcher"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  facet_wrap(~handed)

# Cluster center physical characteristics

## 4-seam

RFF_C <- cbind(data.frame("Subtype" = paste0("FF", 1:6)), rep(attr(ff_R, "scaled:scale"), each=6) * ff_R_clust + rep(attr(ff_R, "scaled:center"), each=6))
LFF_C <- cbind(data.frame("Subtype" = paste0("FF", 1:5)), rep(attr(ff_L, "scaled:scale"), each=5) * ff_L_clust + rep(attr(ff_L, "scaled:center"), each=5))

## Changeup

RCH_C <- cbind(data.frame("Subtype" = paste0("CH", 1:6)), rep(attr(ch_R, "scaled:scale"), each=6) * ch_R_clust + rep(attr(ch_R, "scaled:center"), each=6))
LCH_C <- cbind(data.frame("Subtype" = paste0("CH", 1:5)), rep(attr(ch_L, "scaled:scale"), each=5) * ch_L_clust + rep(attr(ch_L, "scaled:center"), each=5))

## Curveball

RCU_C <- cbind(data.frame("Subtype" = paste0("CU", 1:5)), rep(attr(cu_R, "scaled:scale"), each=5) * cu_R_clust + rep(attr(cu_R, "scaled:center"), each=5))
LCU_C <- cbind(data.frame("Subtype" = paste0("CU", 1:6)), rep(attr(cu_L, "scaled:scale"), each=6) * cu_L_clust + rep(attr(cu_L, "scaled:center"), each=6))

## Cutter

RFC_C <- cbind(data.frame("Subtype" = paste0("FC", 1:5)), rep(attr(fc_R, "scaled:scale"), each=5) * fc_R_clust + rep(attr(fc_R, "scaled:center"), each=5))
LFC_C <- cbind(data.frame("Subtype" = paste0("FC", 1:5)), rep(attr(fc_L, "scaled:scale"), each=5) * fc_L_clust + rep(attr(fc_L, "scaled:center"), each=5))

## Knuckle-Curve

RKC_C <- cbind(data.frame("Subtype" = paste0("KC", 1:5)), rep(attr(kc_R, "scaled:scale"), each=5) * kc_R_clust + rep(attr(kc_R, "scaled:center"), each=5))
LKC_C <- cbind(data.frame("Subtype" = paste0("KC", 1:4)), rep(attr(kc_L, "scaled:scale"), each=4) * kc_L_clust + rep(attr(kc_L, "scaled:center"), each=4))

## Sinker

RSI_C <- cbind(data.frame("Subtype" = paste0("SI", 1:5)), rep(attr(si_R, "scaled:scale"), each=5) * si_R_clust + rep(attr(si_R, "scaled:center"), each=5))
LSI_C <- cbind(data.frame("Subtype" = paste0("SI", 1:5)), rep(attr(si_L, "scaled:scale"), each=5) * si_L_clust + rep(attr(si_L, "scaled:center"), each=5))

## Slider

RSL_C <- cbind(data.frame("Subtype" = paste0("SL", 1:5)), rep(attr(sl_R, "scaled:scale"), each=5) * sl_R_clust + rep(attr(sl_R, "scaled:center"), each=5))
LSL_C <- cbind(data.frame("Subtype" = paste0("SL", 1:5)), rep(attr(sl_L, "scaled:scale"), each=5) * sl_L_clust + rep(attr(sl_L, "scaled:center"), each=5))

## Splitter

RFS_C <- cbind(data.frame("Subtype" = paste0("FS", 1:4)), rep(attr(fs_R, "scaled:scale"), each=4) * fs_R_clust + rep(attr(fs_R, "scaled:center"), each=4))

## Sweeper

RST_C <- cbind(data.frame("Subtype" = paste0("ST", 1:5)), rep(attr(st_R, "scaled:scale"), each=5) * st_R_clust + rep(attr(st_R, "scaled:center"), each=5))
LKC_C <- cbind(data.frame("Subtype" = paste0("ST", 1:4)), rep(attr(st_L, "scaled:scale"), each=4) * st_L_clust + rep(attr(st_L, "scaled:center"), each=4))



### Sequencing ###

# Put all the pitches together

dat1 <- rbind(RFF, RSL, RSI, RCH, RFC, RCU, RST, RFS, RKC,
              LFF, LCH, LSL, LSI, LCU, LFC, LST, LKC)

# Add at_bat_id to the pitches

dat2 <- dat1 |> 
  group_by(game_pk, at_bat_number) |>
  mutate(
    at_bat_id = cur_group_id()
  ) |>
  ungroup()

view(head(dat2))

# Create a variable for the previous pitch, both the primary type and the subtype

dat3 <- dat2 |>
  arrange(at_bat_id, pitch_number) |>
  group_by(at_bat_id) |> 
  mutate(prev_type = lag(pitch_type),
         prev_subtype = lag(pitch_subtype)) |>
  ungroup()
  
# Using p3 from the other R script, merge on the probability of an out before the pitch based on the count

pre_pitch_prob <- p3 |>
  dplyr::select(balls, strikes, percent_out) |>
  rename(pre_prob = percent_out)

dat4 <- dat3 |>
  left_join(pre_pitch_prob, by = c("balls", "strikes"))

# Establish dummy variables for when a batter gets a base or when an out occurs and make the count after the pitch

not_out_event <- c("double", "home_run", "single", "triple", "hit_by_pitch", "walk")
out_event <- c("double_play", "field_out", "fielders_choice", "fielders_choice_out", "force_out", "grounded_into_double_play", "strikeout", "strikeout_double_play", "triple_play")
balls_p  <- c("ball", "blocked_ball")
strikes_p <- c("called_strike", "foul", "foul_tip", "swinging_strike", "swinging_strike_blocked")

dat5 <- dat4 |>
  mutate(
    is_base = events %in% not_out_event,
    is_out = events %in% out_event,
    post_balls =
      ifelse(!is_out & !is_base & description %in% balls_p, balls + 1, balls),
    post_strikes = case_when(
      !is_out & !is_base & description %in% strikes_p & strikes == 2 ~ 2,
      !is_out & !is_base & description %in% strikes_p ~ strikes + 1,
      TRUE ~ strikes
      ),
  )

# Assign post-pitch probality of an out and construct the out value created by the pitch

post_pitch_prob <- p3 |>
  dplyr::select("balls", "strikes", "percent_out") |>
  rename(
    post_prob = percent_out,
    post_balls = balls,
    post_strikes = strikes
  )

dat6 <- dat5 |>
  left_join(post_pitch_prob, by = c("post_balls", "post_strikes")) |>
  mutate(post_prob = case_when(
    is_out ~ 1,
    is_base ~ 0,
    TRUE ~ post_prob
    )
  ) |>
  filter(
    !events %in% c("field_error","catcher_interf","caught_stealing_2b","caught_stealing_3b","caught_stealing_home","pickoff_1b","pickoff_2b","pickoff_3b","pickoff_caught_stealing_2b","sac_bunt", "sac_fly","sac_fly_double_play","other_out"),
    !description %in% c("bunt_foul_tip","foul_bunt","missed_bunt")) |>
  mutate(out_value = post_prob - pre_prob,
         on_base = !is.na(on_1b) | !is.na(on_2b) | !is.na(on_3b),
         count_impact = as.factor(case_when(
           balls > strikes ~ "behind",
           balls == strikes ~ "even",
           balls < strikes ~ "ahead"
         )),
         prev_type = as.factor(prev_type),
         pitch_type = as.factor(pitch_type),
         pitch_subtype = as.factor(pitch_subtype),
         prev_subtype = as.factor(prev_subtype),
         pitcher = as.factor(pitcher),
         batter = as.factor(batter),
         pitch_type = as.factor(pitch_type),
         prev_type = as.factor(prev_type),
         game_pk = as.factor(game_pk),
         batter = as.factor(batter),
         home_team = as.factor(home_team),
         fielder_2 = as.factor(fielder_2)
         ) |>
  mutate(
    prev_type = relevel(prev_type, ref= "FF"),
    pitch_type = relevel(pitch_type, ref = "FF"),
    count_impact = relevel(count_impact, ref = "even")
  )
  
save(dat6, file="PitchData.rda")

load("~/Saberseminar24/PitchData.rda")

dat6 |>
  group_by(prev_type, pitch_type) |>
  summarize(mean_out_value = mean(out_value, na.rm = T),
            sd_dev = sd(out_value, na.rm=T),
            count = n()) |>
  filter(count > 500,
         !is.na(prev_type)) |>
  arrange(mean_out_value)



### Start model building for RHP 

RHP <- dat6 |>
  filter(p_throws == "R")

# Investigation on the pitches to decide baseline pitch values

RHP |>
  group_by(pitch_type, prev_type) |>
  summarize(mean_out_value = mean(out_value, na.rm=T),
            count = n()) |>
  arrange(mean_out_value) |>
  view()

RHP |>
  filter(balls == 3) |>
  count(balls, strikes, pitch_type)

## Model time

# Initial model with just primary types

r_mod1 <- lmer(out_value ~  pitch_type + prev_type + pitch_type*prev_type + stand + on_base + count_impact + (1|pitcher) + (1|batter) + (1|home_team) + (1|fielder_2) + (1|game_pk), data=RHP)

summary(r_mod1)


# Build a new data set to find the predicted out values for each permutation of pitch type

pitch_types <- c("FF", "CH", "CU", "FS", "KC", "SI", "SL", "FC", "ST")

r_newdata <- data.frame(crossing(prev_type = pitch_types, pitch_type = pitch_types, stand=c("R", "L"), on_base = F, count_impact = "even", batter = "", pitcher = "", home_team = "", game_pk = "", fielder_2 = ""))
#This is assuming a 0-0 count with no one on base for the sake of predicted value

res <- predictInterval(merMod = r_mod1, newdata=r_newdata, 
                level = 0.8, n.sims = 1000, stat = "mean",
                type = "linear.prediction", which = "fixed",
                include.resid.var = F)

r_pred_mod1 <- cbind(r_newdata, res)

r_pred_mod1 |>
  filter(stand == "R") |>
  mutate(fit_sig = 
    ifelse(lwr < 0  & upr > 0, 0, fit)
  ) |>
  ggplot(aes(x=prev_type, y=pitch_type, fill = fit_sig)
         ) +
  geom_tile(color="black") +
  scale_fill_gradient2(low = "red", high = "darkgreen", limits = c(-0.04, 0.04))

r_pred_mod1 |>
  filter(stand == "L") |>
  mutate(fit_sig = 
           ifelse(lwr < 0  & upr > 0, 0, fit)
  ) |>
  ggplot(aes(x=prev_type, y=pitch_type, fill = fit_sig)
  ) +
  geom_tile(color="black") +
  scale_fill_gradient2(low = "red", high = "darkgreen", limits = c(-0.04, 0.04))

# Now going to make a higher level to ensure which combinations are successful

res2 <- predictInterval(merMod = r_mod1, newdata=r_newdata, 
                       level = 0.995, n.sims = 1000, stat = "mean",
                       type = "linear.prediction", which = "fixed",
                       include.resid.var = F)

r_pred2_mod1 <- cbind(r_newdata, res2)

r_pred2_mod1 |>
  filter(lwr>0 & upr > 0)

# Best combination for this is a Curve setting up a Slider

# Building a model with the pitch subtypes to figure out which combination of CU (pre) and SL is the best in terms of out value

dat6 |>
  filter(p_throws == "R",
         prev_type == "CU",
         pitch_type == "SL") |>
  group_by(prev_subtype, pitch_subtype) |>
  summarize(mean_out_value = mean(out_value),
             count = n())|>
  arrange(-count)

# Filter the data down to curveball setting up slider and find the worst performing subtype to make the baseline

cu_sl <- dat6 |>
  filter(p_throws == "R",
         prev_type == "CU" & pitch_type == "SL") |>
  mutate(
    prev_subtype = relevel(prev_subtype, ref = "CU4"),
    pitch_subtype = relevel(pitch_subtype, ref = "SL4")
  )

r_mod2 <- lmer(out_value ~ pitch_subtype + prev_subtype + pitch_subtype*prev_subtype + stand + on_base + count_impact + (1|pitcher) + (1|batter) + (1|home_team) + (1|fielder_2) + (1|game_pk), data=cu_sl)

summary(r_mod2)

cu <- c("CU1", "CU2", "CU3", "CU4", "CU5")
sl <- c("SL1", "SL2", "SL3", "SL4", "SL5")

cu_sl_r_newdata <- crossing(prev_subtype = cu, pitch_subtype = sl, stand="R", on_base = F, count_impact = "even", batter = "", pitcher = "", home_team = "", game_pk = "", fielder_2 = "")

cu_sl_res <- predictInterval(merMod = r_mod2, newdata=cu_sl_r_newdata, 
                       level = 0.8, n.sims = 1000, stat = "mean",
                       type = "linear.prediction", which = "fixed",
                       include.resid.var = F)

cu_sl_r_pred_mod2 <- cbind(cu_sl_r_newdata, cu_sl_res)

cu_sl_r_pred_mod2 |>
  mutate(fit_sig = 
           round(ifelse(lwr < 0  & upr > 0, 0, fit), 2)
  ) |>
  ggplot(aes(x=prev_subtype, y=pitch_subtype, fill = fit_sig)
  ) +
  geom_tile(color="black") +
  geom_text(aes(label = fit_sig), color= "black", size = 5) +
  scale_fill_gradient2(low = "white", high = "darkgreen", limits = c(0, 0.1))
  
dat6 |>
  filter(p_throws == "R",
    pitch_subtype == "SL3" & prev_subtype == "CU2") |>
  group_by(player_name) |>
  summarize(count = n()) |>
  arrange(-count)

dat6 |>
  filter(p_throws == "R") |>
  group_by(player_name) |>
  summarize(
    p_sl3 = round(sum(pitch_subtype == "SL3") / sum(pitch_type == "SL"), 2),
    p_cu2 = round(sum(pitch_subtype == "CU2") / sum(pitch_type == "CU"), 2)) |>
  filter(p_sl3 > .25 & p_cu2 > .25)

dat6 |>
  filter(player_name == "Lynn, Lance") |>
  group_by(pitch_subtype) |>
  summarize(count = n())

dat6 |>
  filter(player_name == "Cabrera, Edward") |>
  group_by(pitch_subtype) |>
  summarize(count = n())







LHP <- dat6 |>
  filter(p_throws == "L")
