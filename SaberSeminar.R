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
  dplyr::select(release_speed, horz, vert) |>
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

# 5

ff_R_clust <- as.data.frame(kmeans(ff_R, centers = 5)$centers)

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
  mutate(cluster = kmeans(ff_R, centers=5)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))


# Left-handed 4-seam

ff_L <- dat |>
  filter(pitch_type == "FF",
         p_throws == "L") |>
  dplyr::select(release_speed, horz, vert) |>
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

# 6

ff_L_clust <- as.data.frame(kmeans(ff_L, centers = 6)$centers)

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
  mutate(cluster = kmeans(ff_L, centers=6)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Right-handed Slider

sl_R <- dat |>
  filter(pitch_type == "SL",
         p_throws == "R") |>
  dplyr::select(release_speed, horz, vert) |>
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
  dplyr::select(release_speed, horz, vert) |>
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
  dplyr::select(release_speed, horz, vert) |>
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
  dplyr::select(release_speed, horz, vert) |>
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
  dplyr::select(release_speed, horz, vert) |>
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

# 5

ch_R_clust <- as.data.frame(kmeans(ch_R, centers = 5)$centers)

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
  mutate(cluster = kmeans(ch_R, centers=5)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Left-handed Changeup

ch_L <- dat |>
  filter(pitch_type == "CH",
         p_throws == "L") |>
  dplyr::select(release_speed, horz, vert) |>
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
  dplyr::select(release_speed, horz, vert) |>
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

# 4

cu_R_clust <- as.data.frame(kmeans(cu_R, centers = 4)$centers)

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
  mutate(cluster = kmeans(cu_R, centers=4)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Left-handed Curveball

cu_L <- dat |>
  filter(pitch_type == "CU",
         p_throws == "L") |>
  dplyr::select(release_speed, horz, vert) |>
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

# 5

cu_L_clust <- as.data.frame(kmeans(cu_L, centers = 5)$centers)

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
  mutate(cluster = kmeans(cu_L, centers=5)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Right-handed Cutter

fc_R <- dat |>
  filter(pitch_type == "FC",
         p_throws == "R") |>
  dplyr::select(release_speed, horz, vert) |>
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

# 4

fc_R_clust <- as.data.frame(kmeans(fc_R, centers = 4)$centers)

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
  mutate(cluster = kmeans(fc_R, centers=4)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Left-handed Cutter

fc_L <- dat |>
  filter(pitch_type == "FC",
         p_throws == "L") |>
  dplyr::select(release_speed, horz, vert) |>
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

# 4

fc_L_clust <- as.data.frame(kmeans(fc_L, centers = 4)$centers)

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
  mutate(cluster = kmeans(fc_L, centers=4)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Right-handed Sweeper

st_R <- dat |>
  filter(pitch_type == "ST",
         p_throws == "R") |>
  dplyr::select(release_speed, horz, vert) |>
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
  dplyr::select(release_speed, horz, vert) |>
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
  dplyr::select(release_speed, horz, vert) |>
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
  dplyr::select(release_speed, horz, vert) |>
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

# 4, but there is only 58 pitches, so this is not going to be used

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
  filter(game_year == 2023,
         role_key == "SP",
         p_throws == "L",
         pitch_type == "FS") |>
  group_by(player_name) |>
  summarize(count = n())

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
  dplyr::select(release_speed, horz, vert) |>
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

# 4

kc_R_clust <- as.data.frame(kmeans(kc_R, centers = 4)$centers)

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
  mutate(cluster = kmeans(kc_R, centers=4)$cluster,
         pitch_subtype = paste0(pitch_type,cluster))



# Left-handed Knuckle-curve

kc_L <- dat |>
  filter(pitch_type == "KC",
         p_throws == "L") |>
  dplyr::select(release_speed, horz, vert) |>
  scale()

sse_kc_L <- numeric(length(clust))

for (i in clust) {
  set.seed(740)
  k_means <- kmeans(kc_L, centers=i, iter.max=200, algorithm="MacQueen")
  sse_kc_L[i] <- k_means$tot.withinss
}

par(mar = c(6, 7, 5, 2))
plot(1:20, sse_kc_L, type="b",
     xlab = "",
     ylab = "")
mtext("Number of Clusters", side = 1, line =4, cex = 3)
mtext("Within groups sum of squares", side = 2, line = 4, cex = 3)
title(main = "Elbow Plot for LHP KC", line = 2, cex.main = 4)

# 3

kc_L_clust <- as.data.frame(kmeans(kc_L, centers = 3)$centers)

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
  dplyr::select(release_speed, horz, vert) |>
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
  dplyr::select(release_speed, horz, vert) |>
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
clusters <- c(5, 6, 5, 5, 5, 5, 5, 5, 4, 5, 4, 4, 5, 4, 4, 0, 4, 3)

clust_viz <- data.frame(pitch, handed, clusters)

clust_viz |>
  ggplot(
    aes(x=pitch, y=clusters, fill=handed)
  ) + 
  geom_col(position="dodge") + 
  geom_text(
    aes(label=clusters), 
    position=position_dodge(width=0.9), 
    vjust=-0.5,
    size = 10
  ) + 
  labs(
    title = "Pitch Subtypes Identified by K-Means",
    x = "Primary Pitch Type",
    y = "Number of Clusters",
    fill = "Pitcher"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  facet_wrap(~handed) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),text = element_text(size = rel(5)), legend.text = element_text(size = rel(3)),legend.position = "bottom",legend.key.width = unit(0.6,"in"), strip.text = element_text(size = rel(5)), plot.title = element_text(size = rel(9.5), hjust = 0.5), axis.title.x = element_text(margin = margin(t = 25)), axis.title.y = element_text(margin = margin(r = 25, l=10)))
  

# Cluster center physical characteristics

## 4-seam

RFF_C <- cbind(data.frame("Subtype" = paste0("FF", 1:5)), rep(attr(ff_R, "scaled:scale"), each=5) * ff_R_clust + rep(attr(ff_R, "scaled:center"), each=5))
LFF_C <- cbind(data.frame("Subtype" = paste0("FF", 1:6)), rep(attr(ff_L, "scaled:scale"), each=6) * ff_L_clust + rep(attr(ff_L, "scaled:center"), each=6))

## Changeup

RCH_C <- cbind(data.frame("Subtype" = paste0("CH", 1:5)), rep(attr(ch_R, "scaled:scale"), each=5) * ch_R_clust + rep(attr(ch_R, "scaled:center"), each=5))
LCH_C <- cbind(data.frame("Subtype" = paste0("CH", 1:5)), rep(attr(ch_L, "scaled:scale"), each=5) * ch_L_clust + rep(attr(ch_L, "scaled:center"), each=5))

## Curveball

RCU_C <- cbind(data.frame("Subtype" = paste0("CU", 1:4)), rep(attr(cu_R, "scaled:scale"), each=4) * cu_R_clust + rep(attr(cu_R, "scaled:center"), each=4))
LCU_C <- cbind(data.frame("Subtype" = paste0("CU", 1:5)), rep(attr(cu_L, "scaled:scale"), each=5) * cu_L_clust + rep(attr(cu_L, "scaled:center"), each=5))

## Cutter

RFC_C <- cbind(data.frame("Subtype" = paste0("FC", 1:4)), rep(attr(fc_R, "scaled:scale"), each=4) * fc_R_clust + rep(attr(fc_R, "scaled:center"), each=4))
LFC_C <- cbind(data.frame("Subtype" = paste0("FC", 1:4)), rep(attr(fc_L, "scaled:scale"), each=4) * fc_L_clust + rep(attr(fc_L, "scaled:center"), each=4))

## Knuckle-Curve

RKC_C <- cbind(data.frame("Subtype" = paste0("KC", 1:4)), rep(attr(kc_R, "scaled:scale"), each=4) * kc_R_clust + rep(attr(kc_R, "scaled:center"), each=4))
LKC_C <- cbind(data.frame("Subtype" = paste0("KC", 1:3)), rep(attr(kc_L, "scaled:scale"), each=3) * kc_L_clust + rep(attr(kc_L, "scaled:center"), each=3))

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

load("~/Saberseminar24/CountOutValue.rda")

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



#### Start model building for RHP 

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

r_mod1 <- lmer(out_value ~  pitch_type + prev_type + pitch_type*prev_type + stand + on_base + count_impact + (1|pitcher) + (1|batter) + (1|home_team) + (1|fielder_2), data=RHP)

summary(r_mod1)


# Build a new data set to find the predicted out values for each permutation of pitch type

pitch_types <- c("FF", "CH", "CU", "FS", "KC", "SI", "SL", "FC", "ST")

r_newdata <- data.frame(crossing(prev_type = pitch_types, pitch_type = pitch_types, stand=c("R", "L"), on_base = F, count_impact = "even", batter = "", pitcher = "", home_team = "", fielder_2 = ""))
#This is assuming a 0-0 count with no one on base for the sake of predicted value

res <- predictInterval(merMod = r_mod1, newdata=r_newdata, 
                level = 0.8, n.sims = 10000, stat = "mean",
                type = "linear.prediction", which = "fixed",
                include.resid.var = F)

r_pred_mod1 <- cbind(r_newdata, res)

r_pred_mod1 |>
  filter(stand == "R") |>
  mutate(fit_sig = 
    ifelse(lwr < 0  & upr > 0, 0, fit)
  ) |>
  ggplot(aes(x=pitch_type, y=prev_type, fill = fit_sig)
         ) +
  geom_tile(color="grey90") +
  scale_fill_gradient2(low = "red", high = "darkgreen", limits = c(-0.045, 0.045), breaks = c(-0.04, -0.02, 0, 0.02, 0.04)) +
  theme_minimal() +
  coord_equal() +
  labs(
    title = "RHP Pitch Type Permutations versus RHB",
    x = "Pitch Type",
    y = "Previous Pitch Type",
    fill = "Predicted Out-Value    "
  ) +
  theme(text = element_text(size = rel(5)), legend.text = element_text(size = rel(3)),legend.position = "bottom",legend.key.width = unit(0.6,"in"), strip.text = element_text(margin = margin(b=10)), plot.title = element_text(size = rel(9.5), hjust = 0.5), axis.title.x = element_text(margin = margin(t = 25)), axis.title.y = element_text(margin = margin(r = 25, l=10)))

r_pred_mod1 |>
  filter(stand == "L") |>
  mutate(fit_sig = 
           ifelse(lwr < 0  & upr > 0, 0, fit)
  ) |>
  ggplot(aes(x=pitch_type, y=prev_type, fill = fit_sig)
  ) +
  geom_tile(color="grey90") +
  scale_fill_gradient2(low = "red", high = "darkgreen", limits = c(-0.045, 0.045), breaks = c(-0.04, -0.02, 0, 0.02, 0.04)) +
  theme_minimal() +
  coord_equal() +
  labs(
    title = "RHP Pitch Type Permutations versus LHB",
    x = "Pitch Type",
    y = "Previous Pitch Type",
    fill = "Predicted Out-Value    "
  ) +
  theme(text = element_text(size = rel(5)), legend.text = element_text(size = rel(3)),legend.position = "bottom",legend.key.width = unit(0.6,"in"), strip.text = element_text(margin = margin(b=10)), plot.title = element_text(size = rel(9.5), hjust = 0.5), axis.title.x = element_text(margin = margin(t = 25)), axis.title.y = element_text(margin = margin(r = 25, l=10)))

# Now going to make a higher level to ensure which combinations are successful

res2 <- predictInterval(merMod = r_mod1, newdata=r_newdata, 
                       level = 0.995, n.sims = 10000, stat = "mean",
                       type = "linear.prediction", which = "fixed",
                       include.resid.var = F)

r_pred2_mod1 <- cbind(r_newdata, res2)

r_pred2_mod1 |>
  filter(lwr>0 & upr > 0)

# Best combination for this is a 4-seam setting up a Cutter

# Building a model with the pitch subtypes to figure out which combination of FF (pre) and FC is the best in terms of out value

dat6 |>
  filter(p_throws == "R",
         prev_type == "FF",
         pitch_type == "FC") |>
  group_by(prev_subtype, pitch_subtype) |>
  summarize(mean_out_value = mean(out_value),
             count = n())|>
  arrange(-count)

# Filter the data down to curveball setting up slider and find the worst performing subtype to make the baseline

ff_fc <- dat6 |>
  filter(p_throws == "R",
         prev_type == "FF" & pitch_type == "FC") |>
  mutate(
    prev_subtype = relevel(prev_subtype, ref = "FF2"),
    pitch_subtype = relevel(pitch_subtype, ref = "FC1")
  )

r_mod2 <- lmer(out_value ~ pitch_subtype + prev_subtype + pitch_subtype*prev_subtype + stand + on_base + count_impact + (1|home_team), data=ff_fc)

summary(r_mod2)

ff <- c("FF1", "FF2", "FF3", "FF4", "FF5")
fc <- c("FC1", "FC2", "FC3", "FC4")

ff_fc_r_newdata <- crossing(prev_subtype = ff, pitch_subtype = fc, stand="R", on_base = F, count_impact = "even", batter = "", pitcher = "", home_team = "", fielder_2 = "")


ff_fc_res <- predictInterval(merMod = r_mod2, newdata = ff_fc_r_newdata, seed = 740,
                       level = 0.8, n.sims = 10000, stat = "mean",
                       type = "linear.prediction", which = "fixed",
                       include.resid.var = F)

ff_fc_r_pred_mod2 <- cbind(ff_fc_r_newdata, ff_fc_res)

ff_fc_r_pred_mod2 |>
  mutate(fit_sig = 
           round(ifelse(lwr < 0  & upr > 0, 0, fit), 2)
  ) |>
  ggplot(aes(x=prev_subtype, y=pitch_subtype, fill = fit_sig)
  ) +
  geom_tile(color="grey90") +
  geom_text(aes(label = fit_sig), color= "black", size = 5) +
  scale_fill_gradient2(low = "red", high = "darkgreen", limits = c(-0.04, 0.04)) +
  theme_minimal() +
  coord_equal() +
  labs(
    x = "Pitch Type",
    y = "Previous Pitch Type",
    fill = "Predicted Out-Value"
  ) +
  theme(text = element_text(size = rel(5)), legend.text = element_text(size = rel(3)),legend.position = "bottom",legend.key.width = unit(0.4,"in"), , axis.title.x = element_text(margin = margin(t = 25)), axis.title.y = element_text(margin = margin(r = 25, l=10)))

dat6 |>
  filter(p_throws == "R",
    pitch_subtype == "FC3" & prev_subtype == "FF5") |>
  group_by(player_name) |>
  summarize(count = n()) |>
  arrange(-count)

dat6 |>
  filter(p_throws == "R") |>
  group_by(player_name) |>
  summarize(
    p_ff5 = round(sum(pitch_subtype == "FF5") / sum(pitch_type == "FF"), 2),
    p_fc3 = round(sum(pitch_subtype == "FC3") / sum(pitch_type == "FC"), 2)) |>
  filter(p_ff5 > .25 & p_fc3 > .25)

# Kodai Senga, Dustin May, Grayson Rodriguez, and Yu Darvish (much better than Lance Lynn)



#### Left-Handed Pitchers

LHP <- dat6 |>
  filter(p_throws == "L") |>
  mutate(prev_type = droplevels(prev_type),
         pitch_type = droplevels(pitch_type))

l_mod1 <- lmer(out_value ~  pitch_type + prev_type + pitch_type*prev_type + stand + on_base + count_impact + (1|pitcher) + (1|batter) + (1|home_team) + (1|fielder_2), data=LHP)

summary(l_mod1)

pitch_types <- c("FF", "CH", "CU", "KC", "SI", "SL", "FC", "ST")

l_newdata <- data.frame(crossing(prev_type = pitch_types, pitch_type = pitch_types, stand=c("R", "L"), on_base = F, count_impact = "even", batter = "", pitcher = "", home_team = "", fielder_2 = ""))

res <- predictInterval(merMod = l_mod1, newdata=l_newdata, 
                       level = 0.8, n.sims = 10000, stat = "mean",
                       type = "linear.prediction", which = "fixed",
                       include.resid.var = F)

l_pred_mod1 <- cbind(l_newdata, res)

l_pred_mod1 |>
  filter(stand == "R") |>
  mutate(fit_sig = 
           ifelse(lwr < 0  & upr > 0, 0, fit)
  ) |>
  ggplot(aes(x=pitch_type, y=prev_type, fill = fit_sig)
  ) +
  geom_tile(color="grey90") +
  scale_fill_gradient2(low = "red", high = "darkgreen", limits = c(-0.045, 0.045), breaks = c(-0.04, -0.02, 0, 0.02, 0.04)) +
  theme_minimal() +
  coord_equal() +
  labs(
    title = "LHP Pitch Type Permutations versus RHB",
    x = "Pitch Type",
    y = "Previous Pitch Type",
    fill = "Predicted Out-Value    "
  ) +
  theme(text = element_text(size = rel(5)), legend.text = element_text(size = rel(3)),legend.position = "bottom",legend.key.width = unit(0.6,"in"), strip.text = element_text(margin = margin(b=10)), plot.title = element_text(size = rel(9.5), hjust = 0.5), axis.title.x = element_text(margin = margin(t = 25)), axis.title.y = element_text(margin = margin(r = 25, l=10)))

l_pred_mod1 |>
  filter(stand == "L") |>
  mutate(fit_sig = 
           ifelse(lwr < 0  & upr > 0, 0, fit)
  ) |>
  ggplot(aes(x=pitch_type, y=prev_type, fill = fit_sig)
  ) +
  geom_tile(color="grey90") +
  scale_fill_gradient2(low = "red", high = "darkgreen", limits = c(-0.045, 0.045), breaks = c(-0.04, -0.02, 0, 0.02, 0.04)) +
  theme_minimal() +
  coord_equal() +
  labs(
    title = "LHP Pitch Type Permutations versus LHB",
    x = "Pitch Type",
    y = "Previous Pitch Type",
    fill = "Predicted Out-Value    "
  ) +
  theme(text = element_text(size = rel(5)), legend.text = element_text(size = rel(3)),legend.position = "bottom",legend.key.width = unit(0.6,"in"), strip.text = element_text(margin = margin(b=10)), plot.title = element_text(size = rel(9.5), hjust = 0.5), axis.title.x = element_text(margin = margin(t = 25)), axis.title.y = element_text(margin = margin(r = 25, l=10)))


res2 <- predictInterval(merMod = l_mod1, newdata=l_newdata, seed = 740,
                        level = 0.995, n.sims = 10000, stat = "mean",
                        type = "linear.prediction", which = "fixed",
                        include.resid.var = F)

l_pred2_mod1 <- cbind(l_newdata, res2)

l_pred2_mod1 |>
  filter(lwr>0 & upr > 0)


dat6 |>
  filter(p_throws == "L",
         prev_type == "SL",
         pitch_type == "SL") |>
  group_by(prev_subtype, pitch_subtype) |>
  summarize(mean_out_value = mean(out_value),
            count = n())|>
  arrange(-count)

# Filter the data down to curveball setting up slider and find the worst performing subtype to make the baseline

sl_sl <- dat6 |>
  filter(p_throws == "L",
         prev_type == "SL" & pitch_type == "SL") |>
  mutate(
    prev_subtype = relevel(prev_subtype, ref = "SL1"),
    pitch_subtype = relevel(pitch_subtype, ref = "SL1")
  )

l_mod2 <- lmer(out_value ~ pitch_subtype + prev_subtype + pitch_subtype*prev_subtype + stand + on_base + count_impact + (1|home_team), data=sl_sl)

summary(l_mod2)

sl <- c("SL1", "SL2", "SL3", "SL4", "SL5")

sl_sl_l_newdata <- crossing(prev_subtype = sl, pitch_subtype = sl, stand=c("R","L"), on_base = F, count_impact = "even", batter = "", pitcher = "", home_team = "", fielder_2 = "")


sl_sl_res <- predictInterval(merMod = l_mod2, newdata = sl_sl_l_newdata, seed = 740,
                             level = 0.8, n.sims = 10000, stat = "mean",
                             type = "linear.prediction", which = "fixed",
                             include.resid.var = F)

sl_sl_l_pred_mod2 <- cbind(sl_sl_l_newdata, sl_sl_res)

sl_sl_l_pred_mod2 |>
  mutate(fit_sig = 
           round(ifelse(lwr < 0  & upr > 0, 0, fit), 2),
         batter = paste0(stand,"HB")
  ) |>
  ggplot(aes(x=prev_subtype, y=pitch_subtype, fill = fit_sig)
  ) +
  geom_tile(color="grey90") +
  geom_text(aes(label = fit_sig), color= "black", size = 5) +
  scale_fill_gradient2(low = "red", high = "darkgreen", limits = c(-0.33, 0.33), breaks = c(-.3, -.2, -.1, 0, .1, .2, .3)) +
  theme_minimal() +
  coord_equal() +
  labs(
    title = "LHP Performance of Slider-Slider Combination",
    x = "Pitch Type",
    y = "Previous Pitch Type",
    fill = "Predicted Out-Value   "   
  ) +
  facet_wrap(~batter) +
  theme(text = element_text(size = rel(5)), legend.text = element_text(size = rel(3)),legend.position = "bottom",legend.key.width = unit(0.6,"in"), strip.text = element_text(size = rel(5)), plot.title = element_text(size = rel(9.5), hjust = 0.5), , axis.title.x = element_text(margin = margin(t = 25)), axis.title.y = element_text(margin = margin(r = 25, l=10)))
  

dat6 |>
  filter(p_throws == "L",
         pitch_subtype == "SL4" & prev_subtype == "SL1") |>
  group_by(player_name) |>
  summarize(count = n()) |>
  arrange(-count)

dat6 |>
  filter(p_throws == "L") |>
  group_by(player_name) |>
  summarize(
    p_sl1 = round(sum(pitch_subtype == "SL1") / sum(pitch_type == "SL"), 2),
    p_sl4 = round(sum(pitch_subtype == "SL4") / sum(pitch_type == "SL"), 2)) |>
  filter(p_sl1 > 0.25 & p_sl4 > 0.25)

# Clayton Kershaw, Yusei Kikuchi, Patrick Sandoval, Jose Suarez

dat6 |>
  filter(p_throws == "L",
         pitch_subtype == "SL4" & prev_subtype == "SL2") |>
  group_by(player_name) |>
  summarize(count = n()) |>
  arrange(-count)

dat6 |>
  filter(p_throws == "L") |>
  group_by(player_name) |>
  summarize(
    p_sl1 = round(sum(pitch_subtype == "SL2") / sum(pitch_type == "SL"), 2),
    p_sl4 = round(sum(pitch_subtype == "SL4") / sum(pitch_type == "SL"), 2)) |>
  filter(p_sl1 > 0.25 & p_sl4 > 0.25)

# Ranger Suarez

dat6 |>
  filter(p_throws == "L",
         pitch_subtype == "SL3" & prev_subtype == "SL5") |>
  group_by(player_name) |>
  summarize(count = n()) |>
  arrange(-count)

dat6 |>
  filter(p_throws == "L") |>
  group_by(player_name) |>
  summarize(
    p_sl5 = round(sum(pitch_subtype == "SL5") / sum(pitch_type == "SL"), 2),
    p_sl3 = round(sum(pitch_subtype == "SL3") / sum(pitch_type == "SL"), 2)) |>
  filter(p_sl5 > 0.25 & p_sl3 > 0.25)

# Andrew Heaney




# Cluster Viz

dat_clusts_r <- dat6 |>
  filter(p_throws == "R") |>
  group_by(pitch_subtype) |> 
  summarize(
    release_speed = mean(release_speed),
    vert = mean(pfx_z) * 12,
    horz = mean(pfx_x) * 12
  ) |>
  ungroup() |>
  mutate(pitch_type = substr(pitch_subtype,1,2)) 

dat_clusts_r |>
  mutate(point_size = scales::rescale(release_speed, to = c(6,20), from = range(dat_clusts_r$release_speed))) |> 
  ggplot(aes(x = horz, y = vert, color = release_speed, size = point_size)) +
  geom_point(alpha = 0.8) +
  facet_wrap(~pitch_type) +
  scale_color_gradient2(low = "gray", mid = "yellow", high = "red", midpoint = 82, limits = c(67,97), breaks=c(70,75,80,85,90,95)) +
  scale_size_identity() +
  scale_x_continuous(breaks = seq(-20, 20, by = 10), limits = c(-25, 25)) +
  scale_y_continuous(breaks = seq(-20, 20, by = 10), limits = c(-25, 25)) +
  theme_bw() +
  labs(title = "Physical Characteristics of RHP Pitch Subtypes",
       x = "Horitzonal Break",
       y = "Vertical Break",
       color = "Release Speed    ") +
  theme(text = element_text(size = rel(5)), legend.text = element_text(size = rel(3)),legend.position = "bottom",legend.key.width = unit(1,"in"), strip.text = element_text(size = rel(5)), plot.title = element_text(size = rel(9.5), hjust = 0.5), axis.title.x = element_text(margin = margin(t = 25)), axis.title.y = element_text(margin = margin(r = 25, l=10)))
  
dat_clusts_r |>
  filter(pitch_type == "FF") |>
  dplyr::select(1,2,3,4) |>
  rename(
    "Pitch Subtype" = pitch_subtype,
    "Release Speed" = release_speed,
    "Vertical Break" = vert,
    "Horizontal Break" = horz
  ) |>
  kable()

dat_clusts_r |>
  filter(pitch_type == "FC") |>
  dplyr::select(1,2,3,4) |>
  rename(
    "Pitch Subtype" = pitch_subtype,
    "Release Speed" = release_speed,
    "Vertical Break" = vert,
    "Horizontal Break" = horz
  ) |>
  kable()



dat_clusts_l <- dat6 |>
  filter(p_throws == "L") |>
  group_by(pitch_subtype) |> 
  summarize(
    release_speed = mean(release_speed),
    vert = mean(pfx_z) * 12,
    horz = mean(pfx_x) * 12
  ) |>
  ungroup() |>
  mutate(pitch_type = substr(pitch_subtype,1,2),
         point_size = scales::rescale(release_speed, to = c(6,20), from = range(dat_clusts_l$release_speed)))

dummy <- data.frame(pitch_subtype = "FS1", release_speed = 0, vert = 0, horz = 0, pitch_type = "FS", point_size = 0)

dat_clusts_l1 <- rbind(dat_clusts_l, dummy)

dat_clusts_l1 |>
  ggplot(aes(x = horz, y = vert, color = release_speed, size = point_size)) +
  geom_point(alpha = 0.8) +
  facet_wrap(~factor(pitch_type,levels = c("CH", "CU", "FC", 
                                           "FF", "FS", "KC", 
                                           "SI", "SL", "ST"))) +
  scale_color_gradient2(low = "gray", mid = "yellow", high = "red", midpoint = 82, limits = c(67,97), breaks=c(70,75,80,85,90,95)) +
  scale_size_identity() +
  scale_x_continuous(breaks = seq(-20, 20, by = 10), limits = c(-25, 25)) +
  scale_y_continuous(breaks = seq(-20, 20, by = 10), limits = c(-25, 25)) +
  theme_bw() + labs(title = "Physical Characteristics of LHP Pitch Subtypes",
                    x = "Horitzonal Break",
                    y = "Vertical Break",
                    color = "Release Speed    ") +
  theme(text = element_text(size = rel(5)), legend.text = element_text(size = rel(3)),legend.position = "bottom",legend.key.width = unit(1,"in"), strip.text = element_text(size = rel(5)), plot.title = element_text(size = rel(9.5), hjust = 0.5), axis.title.x = element_text(margin = margin(t = 25)), axis.title.y = element_text(margin = margin(r = 25, l=10)))


dat_clusts_l |>
  filter(pitch_type == "SL") |>
  dplyr::select(1,2,3,4) |>
  rename(
    "Pitch Subtype" = pitch_subtype,
    "Release Speed" = release_speed,
    "Vertical Break" = vert,
    "Horizontal Break" = horz
  ) |>
  kable()
