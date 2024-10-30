library(tidyverse)
library(gt)
library(lme4)
library(merTools)

pitches <- read_csv("savant_pitch_level.csv")

#EDA

qualified_pitchers <- pitches |>
  filter(game_year == 2023,
         role_key == "SP") |>
  group_by(player_name) |>
  reframe(n = n()) |>
  filter(n > 250)

dat <- pitches |>
  filter(player_name %in% qualified_pitchers$player_name,
         game_year == 2023) |>
  dplyr::select(pitch_type,p_throws,release_speed,pfx_x,pfx_z,release_spin_rate,spin_axis) |>
  na.omit() |>
  mutate(horz = pfx_x * 12,
         vert = pfx_z * 12)

eda1 <- dat |>
  group_by(pitch_type, p_throws) |>
  summarize(
    count = n(),
    velo = mean(release_speed),
    spin = mean(release_spin_rate),
    horz = mean(pfx_x) * 12,
    vert = mean(pfx_z) * 12
  ) |>
  ungroup()

eda1|>
  filter(p_throws == "R",
         count > 1000) |>
  dplyr::select(1,4,5,6,7,3) |>
  rename(
    "Pitch Type" = 1,
    "Velocity" = 2,
    "Spin Rate" = 3,
    "Horizontal Movement" = 4,
    "Vertical Movement" = 5,
    "Pitches Thrown" = 6
  ) |>
  gt() |>
  tab_header(
    title = "2023 RHP SP Average Characterstics"
  ) |>
  fmt_number(
    column = 2:5,
    decimals = 2
  )

eda1|>
  filter(p_throws == "L",
         count > 50) |>
  dplyr::select(1,4,5,6,7,3) |>
  rename(
    "Pitch Type" = 1,
    "Velocity" = 2,
    "Spin Rate" = 3,
    "Horizontal Movement" = 4,
    "Vertical Movement" = 5,
    "Pitches Thrown" = 6
  ) |>
  gt() |>
  tab_header(
    title = "2023 LHP SP Average Characterstics"
  ) |>
  fmt_number(
    column = 2:5,
    decimals = 2
  )

p <- dat |>
  mutate(
    pitch_type = case_when(
      pitch_type == "FS" ~ "CH",
      pitch_type == "KC" ~ "CU",
      pitch_type == "ST" ~ "SL",
      pitch_type == "SV" ~ "SL",
      .default = pitch_type
    )
  )

q <- pitches |>
  filter(
    player_name %in% qualified_pitchers$player_name,
    game_year == 2023,
    !is.na(release_speed),
    !is.na(pfx_x),
    !is.na(pfx_z),
    !is.na(release_spin_rate),
    !is.na(spin_axis)
  ) |>
  mutate(
    horz = pfx_x * 12,
    vert = pfx_z * 12,
    pitch_type = case_when(
      pitch_type == "FS" ~ "CH",
      pitch_type == "KC" ~ "CU",
      pitch_type == "ST" ~ "SL",
      pitch_type == "SV" ~ "SL",
      .default = pitch_type
    )
  )

table(p$pitch_type)



# Clustering

clust <- 1:20

## RHP FF

rhp_ff <- q |>
  filter(pitch_type == "FF",
         p_throws == "R") |>
  dplyr::select(release_speed, horz, vert, release_spin_rate) |>
  scale()

sse_ff_R <- numeric(length(clust))

set.seed(740)
for (i in clust) {
  k_means <- kmeans(rhp_ff, centers=i, iter.max = 50, algorithm = "Hartigan-Wong") # 5 clusters
  sse_ff_R[i] <- k_means$tot.withinss
}

plot(1:20, sse_ff_R, type="b",
     main = "RHP FF",
     xlab = "Number of Clusters",
     ylab = "Within groups sum of squares")

R_FF <- q |>  
   filter(pitch_type == "FF",
          p_throws == "R") |>
   mutate(cluster = kmeans(rhp_ff, centers=5, iter.max = 50, algorithm = "Hartigan-Wong")$cluster,
          pitch_subtype = paste0(pitch_type,cluster))
   
## LHP FF
 
 lhp_ff <- q |>
   filter(pitch_type == "FF",
          p_throws == "L") |>
   dplyr::select(release_speed, horz, vert, release_spin_rate) |>
   scale()
 
 sse_ff_L <- numeric(length(clust))
 
 set.seed(740)
 for (i in clust) {
   k_means <- kmeans(lhp_ff, centers=i, iter.max = 50, algorithm = "Hartigan-Wong") # 5 clusters
   sse_ff_L[i] <- k_means$tot.withinss
 }
 
 plot(1:20, sse_ff_L, type="b",
      main = "LHP FF",
      xlab = "Number of Clusters",
      ylab = "Within groups sum of squares")
 
 L_FF <- q |>  
   filter(pitch_type == "FF",
          p_throws == "L") |>
   mutate(cluster = kmeans(lhp_ff, centers=5, iter.max = 50, algorithm = "Hartigan-Wong")$cluster,
          pitch_subtype = paste0(pitch_type,cluster))

## RHP SL
 
 rhp_sl <- q |>
   filter(pitch_type == "SL",
          p_throws == "R") |>
   dplyr::select(release_speed, horz, vert, release_spin_rate) |>
   scale()
 
 sse_sl_R <- numeric(length(clust))
 
 set.seed(740)
 for (i in clust) {
   k_means <- kmeans(rhp_sl, centers=i, iter.max = 50, algorithm = "Hartigan-Wong") # 5 clusters
   sse_sl_R[i] <- k_means$tot.withinss
 }
 
 plot(1:20, sse_sl_R, type="b",
      main = "RHP SL",
      xlab = "Number of Clusters",
      ylab = "Within groups sum of squares")
 
 R_SL <- q |>  
   filter(pitch_type == "SL",
          p_throws == "R") |>
   mutate(cluster = kmeans(rhp_sl, centers=5, iter.max = 50, algorithm = "Hartigan-Wong")$cluster,
          pitch_subtype = paste0(pitch_type,cluster))
 
## LHP SL
 
 lhp_sl <- q |>
   filter(pitch_type == "SL",
          p_throws == "L") |>
   dplyr::select(release_speed, horz, vert, release_spin_rate) |>
   scale()
 
 sse_sl_L <- numeric(length(clust))
 
 set.seed(740)
 for (i in clust) {
   k_means <- kmeans(lhp_sl, centers=i, iter.max = 50, algorithm = "Hartigan-Wong") # 5 clusters
   sse_sl_L[i] <- k_means$tot.withinss
 }
 
 plot(1:20, sse_sl_L, type="b",
      main = "LHP SL",
      xlab = "Number of Clusters",
      ylab = "Within groups sum of squares")
 
 L_SL <- q |>  
   filter(pitch_type == "SL",
          p_throws == "L") |>
   mutate(cluster = kmeans(lhp_sl, centers=5, iter.max = 50, algorithm = "Hartigan-Wong")$cluster,
          pitch_subtype = paste0(pitch_type,cluster))

 
## RHP SI
 
 rhp_si <- q |>
   filter(pitch_type == "SI",
          p_throws == "R") |>
   dplyr::select(release_speed, horz, vert, release_spin_rate) |>
   scale()
 
 sse_si_R <- numeric(length(clust))
 
 set.seed(740)
 for (i in clust) {
   k_means <- kmeans(rhp_si, centers=i, iter.max = 50, algorithm = "Hartigan-Wong") # 5 clusters
   sse_si_R[i] <- k_means$tot.withinss
 }
 
 plot(1:20, sse_si_R, type="b",
      main = "RHP SI",
      xlab = "Number of Clusters",
      ylab = "Within groups sum of squares")
 
 R_SI <- q |>  
   filter(pitch_type == "SI",
          p_throws == "R") |>
   mutate(cluster = kmeans(rhp_si, centers=5, iter.max = 50, algorithm = "Hartigan-Wong")$cluster,
          pitch_subtype = paste0(pitch_type,cluster))
 
## LHP SI
 
 lhp_si <- q |>
   filter(pitch_type == "SI",
          p_throws == "L") |>
   dplyr::select(release_speed, horz, vert, release_spin_rate) |>
   scale()
 
 sse_si_L <- numeric(length(clust))
 
 set.seed(740)
 for (i in clust) {
   k_means <- kmeans(lhp_si, centers=i, iter.max = 50, algorithm = "Hartigan-Wong") # 5 clusters
   sse_si_L[i] <- k_means$tot.withinss
 }
 
 plot(1:20, sse_si_L, type="b",
      main = "LHP SI",
      xlab = "Number of Clusters",
      ylab = "Within groups sum of squares")
 
 L_SI <- q |>  
   filter(pitch_type == "SI",
          p_throws == "L") |>
   mutate(cluster = kmeans(lhp_si, centers=5, iter.max = 50, algorithm = "Hartigan-Wong")$cluster,
          pitch_subtype = paste0(pitch_type,cluster))

## RHP CH
 
 rhp_ch <- q |>
   filter(pitch_type == "CH",
          p_throws == "R") |>
   dplyr::select(release_speed, horz, vert, release_spin_rate) |>
   scale()
 
 sse_ch_R <- numeric(length(clust))
 
 set.seed(740)
 for (i in clust) {
   k_means <- kmeans(rhp_ch, centers=i, iter.max = 50, algorithm = "Hartigan-Wong") # 6 clusters
   sse_ch_R[i] <- k_means$tot.withinss
 }
 
 plot(1:20, sse_ch_R, type="b",
      main = "RHP CH",
      xlab = "Number of Clusters",
      ylab = "Within groups sum of squares")
 
 R_CH <- q |>  
   filter(pitch_type == "CH",
          p_throws == "R") |>
   mutate(cluster = kmeans(rhp_ch, centers=6, iter.max = 50, algorithm = "Hartigan-Wong")$cluster,
          pitch_subtype = paste0(pitch_type,cluster))

## LHP CH
 
 lhp_ch <- q |>
   filter(pitch_type == "CH",
          p_throws == "L") |>
   dplyr::select(release_speed, horz, vert, release_spin_rate) |>
   scale()
 
 sse_ch_L <- numeric(length(clust))
 
 set.seed(740)
 for (i in clust) {
   k_means <- kmeans(lhp_ch, centers=i, iter.max = 50, algorithm = "Hartigan-Wong") # 6 clusters
   sse_ch_L[i] <- k_means$tot.withinss
 }
 
 plot(1:20, sse_ch_L, type="b",
      main = "LHP CH",
      xlab = "Number of Clusters",
      ylab = "Within groups sum of squares")
 
 L_CH <- q |>  
   filter(pitch_type == "CH",
          p_throws == "L") |>
   mutate(cluster = kmeans(lhp_ch, centers=6, iter.max = 50, algorithm = "Hartigan-Wong")$cluster,
          pitch_subtype = paste0(pitch_type,cluster))

## RHP CU
 
 rhp_cu <- q |>
   filter(pitch_type == "CU",
          p_throws == "R") |>
   dplyr::select(release_speed, horz, vert, release_spin_rate) |>
   scale()
 
 sse_cu_R <- numeric(length(clust))
 
 set.seed(740)
 for (i in clust) {
   k_means <- kmeans(rhp_cu, centers=i, iter.max = 50, algorithm = "Hartigan-Wong") # 5 clusters
   sse_cu_R[i] <- k_means$tot.withinss
 }
 
 plot(1:20, sse_cu_R, type="b",
      main = "RHP CU",
      xlab = "Number of Clusters",
      ylab = "Within groups sum of squares")
 
 R_CU <- q |>  
   filter(pitch_type == "CU",
          p_throws == "R") |>
   mutate(cluster = kmeans(rhp_cu, centers=6, iter.max = 50, algorithm = "Hartigan-Wong")$cluster,
          pitch_subtype = paste0(pitch_type,cluster))
 
## LHP CU
 
 lhp_cu <- q |>
   filter(pitch_type == "CU",
          p_throws == "L") |>
   dplyr::select(release_speed, horz, vert, release_spin_rate) |>
   scale()
 
 sse_cu_L <- numeric(length(clust))
 
 set.seed(740)
 for (i in clust) {
   k_means <- kmeans(lhp_cu, centers=i, iter.max = 50, algorithm = "Hartigan-Wong") # 5 clusters
   sse_cu_L[i] <- k_means$tot.withinss
 }
 
 plot(1:20, sse_cu_L, type="b",
      main = "LHP CU",
      xlab = "Number of Clusters",
      ylab = "Within groups sum of squares")
 
 L_CU <- q |>  
   filter(pitch_type == "CU",
          p_throws == "L") |>
   mutate(cluster = kmeans(lhp_cu, centers=6, iter.max = 50, algorithm = "Hartigan-Wong")$cluster,
          pitch_subtype = paste0(pitch_type,cluster))

## RHP FC
 
 rhp_fc <- q |>
   filter(pitch_type == "FC",
          p_throws == "R") |>
   dplyr::select(release_speed, horz, vert, release_spin_rate) |>
   scale()
 
 sse_fc_R <- numeric(length(clust))
 
 set.seed(740)
 for (i in clust) {
   k_means <- kmeans(rhp_fc, centers=i, iter.max = 50, algorithm = "Hartigan-Wong") # 5 clusters
   sse_fc_R[i] <- k_means$tot.withinss
 }
 
 plot(1:20, sse_fc_R, type="b",
      main = "RHP FC",
      xlab = "Number of Clusters",
      ylab = "Within groups sum of squares")
 
 R_FC <- q |>  
   filter(pitch_type == "FC",
          p_throws == "R") |>
   mutate(cluster = kmeans(rhp_fc, centers=5, iter.max = 50, algorithm = "Hartigan-Wong")$cluster,
          pitch_subtype = paste0(pitch_type,cluster))

## LHP FC
 
 lhp_fc <- q |>
   filter(pitch_type == "FC",
          p_throws == "L") |>
   dplyr::select(release_speed, horz, vert, release_spin_rate) |>
   scale()
 
 sse_fc_L <- numeric(length(clust))
 
 set.seed(740)
 for (i in clust) {
   k_means <- kmeans(lhp_fc, centers=i, iter.max = 50, algorithm = "Hartigan-Wong") # 5 clusters
   sse_fc_L[i] <- k_means$tot.withinss
 }
 
 plot(1:20, sse_fc_L, type="b",
      main = "LHP FC",
      xlab = "Number of Clusters",
      ylab = "Within groups sum of squares")
 
 L_FC <- q |>  
   filter(pitch_type == "FC",
          p_throws == "L") |>
   mutate(cluster = kmeans(lhp_fc, centers=5, iter.max = 50, algorithm = "Hartigan-Wong")$cluster,
          pitch_subtype = paste0(pitch_type,cluster))

 
# Some Viz
 
 pitch <- rep(c("4-Seam", "Slider", "Sinker", "Changeup", "Curveball", "Cutter"), each=2)
 handed <- rep(c("RHP", "LHP"), 6)
 clusters <- c(5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 5, 5)
 
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
     title = "Pitch Subtypes Identified by K-Means for 2023 SP",
     x = "Primary Pitch Type",
     y = "Number of Clusters",
     fill = "Pitcher"
   ) +
   scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
   theme_minimal() +
   facet_wrap(~handed) +
   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),text = element_text(size = rel(5)), legend.text = element_text(size = rel(3)),legend.position = "bottom",legend.key.width = unit(0.6,"in"), strip.text = element_text(size = rel(5)), plot.title = element_text(size = rel(9.5), hjust = 0.5), axis.title.x = element_text(margin = margin(t = 25)), axis.title.y = element_text(margin = margin(r = 25, l=10)))



dat1 <- rbind(R_FF, R_SL, R_SI, R_CH, R_CU, R_FC,
               L_FF, L_SL, L_SI, L_CH, L_CU, L_FC)
 
dat2 <- dat1 |>
  group_by(game_pk, at_bat_number) |>
  mutate(at_bat_id = cur_group_id()) |>
  ungroup()

dat3 <- dat2 |>
  arrange(at_bat_id, pitch_number) |>
  group_by(at_bat_id) |> 
  mutate(prev_type = lag(pitch_type),
         prev_subtype = lag(pitch_subtype)) |>
  ungroup()

load("~/Saberseminar24/CountOutValue.rda")

pre_pitch_prob <- p3 |>
  dplyr::select(balls, strikes, percent_out) |>
  rename(pre_prob = percent_out)

dat4 <- dat3 |>
  left_join(pre_pitch_prob, by = c("balls", "strikes"))

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
         batter = as.factor(batter),
         home_team = as.factor(home_team),
  ) |>
  mutate(
    prev_type = relevel(prev_type, ref= "FF"),
    pitch_type = relevel(pitch_type, ref = "FF"),
    count_impact = relevel(count_impact, ref = "even")
  )

#save(dat6, file="PitchData2.rda")

load("~/Saberseminar24/PitchData2.rda")



# Modeling

## RHP

RHP <- dat6 |>
  filter(p_throws == "R")

r_mod1 <- lmer(out_value ~  pitch_type + prev_type + pitch_type*prev_type + stand + on_base + count_impact + (1|pitcher) + (1|batter) + (1|home_team), data=RHP)

pitch_types <- c("FF", "CH", "CU", "SI", "SL", "FC")

r_newdata <- data.frame(crossing(prev_type = pitch_types, pitch_type = pitch_types, stand=c("R", "L"), on_base = FALSE, count_impact = "even", batter = "", pitcher = "", home_team = ""))

res <- predictInterval(merMod = r_mod1, newdata=r_newdata, 
                       level = 0.8, n.sims = 10000, stat = "mean",
                       type = "linear.prediction", which = "fixed",
                       include.resid.var = F)

r_pred_mod1 <- cbind(r_newdata, res)

r_pred_mod1 |>
  filter(stand == "R") |>
  mutate(fit_sig =
           ifelse(lwr < 0  & upr > 0, 0, fit)) |>
  ggplot(aes(x = pitch_type, y = prev_type, fill = fit_sig)) +
  geom_tile(color = "grey90") +
  scale_fill_gradient2(
    low = "red",
    high = "darkgreen",
    limits = c(-0.045, 0.045),
    breaks = c(-0.04, -0.02, 0, 0.02, 0.04)
  ) +
  theme_minimal() +
  coord_equal() +
  labs(title = "RHP versus RHB",
       x = "Pitch Type",
       y = "Previous Pitch Type",
       fill = "Out-Value    ") +
  theme(
    text = element_text(size = rel(5)),
    legend.text = element_text(size = rel(3)),
    legend.position = "bottom",
    legend.key.width = unit(0.6, "in"),
    strip.text = element_text(margin = margin(b = 10)),
    plot.title = element_text(size = rel(9.5), hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 25)),
    axis.title.y = element_text(margin = margin(r = 25, l = 10))
  )

r_pred_mod1 |>
  filter(stand == "L") |>
  mutate(fit_sig =
           ifelse(lwr < 0  & upr > 0, 0, fit)) |>
  ggplot(aes(x = pitch_type, y = prev_type, fill = fit_sig)) +
  geom_tile(color = "grey90") +
  scale_fill_gradient2(
    low = "red",
    high = "darkgreen",
    limits = c(-0.045, 0.045),
    breaks = c(-0.04, -0.02, 0, 0.02, 0.04)
  ) +
  theme_minimal() +
  coord_equal() +
  labs(title = "RHP versus LHB",
       x = "Pitch Type",
       y = "Previous Pitch Type",
       fill = "Out-Value    ") +
  theme(
    text = element_text(size = rel(5)),
    legend.text = element_text(size = rel(3)),
    legend.position = "bottom",
    legend.key.width = unit(0.6, "in"),
    strip.text = element_text(margin = margin(b = 10)),
    plot.title = element_text(size = rel(9.5), hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 25)),
    axis.title.y = element_text(margin = margin(r = 25, l = 10))
  )

res2 <- predictInterval(merMod = r_mod1, newdata=r_newdata, 
                        level = 0.99, n.sims = 10000, stat = "mean",
                        type = "linear.prediction", which = "fixed",
                        include.resid.var = F)

r_pred2_mod1 <- cbind(r_newdata, res2)

r_pred2_mod1 |>
  filter(lwr > 0 | upr < 0)

dat6 |>
  filter(p_throws == "R",
         prev_type == "FF",
         pitch_type == "FC") |>
  group_by(prev_subtype, pitch_subtype) |>
  summarize(mean_out_value = mean(out_value),
            count = n())|>
  arrange(mean_out_value)

ff_fc <- dat6 |>
  filter(p_throws == "R",
         prev_type == "FF" & pitch_type == "FC") |>
  mutate(
    prev_subtype = relevel(prev_subtype, ref = "FF3"),
    pitch_subtype = relevel(pitch_subtype, ref = "FC1")
  )

r_mod2 <- lmer(out_value ~ pitch_subtype + prev_subtype + pitch_subtype*prev_subtype + stand + on_base + count_impact + (1|home_team), data=ff_fc)

ff <- c("FF1", "FF2", "FF3", "FF4", "FF5")
fc <- c("FC1", "FC2", "FC3", "FC4", "FC5")

ff_fc_r_newdata <- crossing(prev_subtype = ff, pitch_subtype = fc, stand="R", on_base = F, count_impact = "even", batter = "", pitcher = "", home_team = "")


ff_fc_res <- predictInterval(merMod = r_mod2, newdata = ff_fc_r_newdata, seed = 740,
                             level = 0.8, n.sims = 10000, stat = "mean",
                             type = "linear.prediction", which = "fixed",
                             include.resid.var = F)

ff_fc_r_pred_mod2 <- cbind(ff_fc_r_newdata, ff_fc_res)

ff_fc_r_pred_mod2 |>
  mutate(fit_sig =
           round(ifelse(lwr < 0  & upr > 0, 0, fit), 2)) |>
  ggplot(aes(x = pitch_subtype, y = prev_subtype, fill = fit_sig)) +
  geom_tile(color = "grey90") +
  geom_text(aes(label = fit_sig), color = "black", size = 5) +
  scale_fill_gradient2(
    low = "red",
    high = "darkgreen",
    limits = c(-0.105, 0.105),
    breaks = c(-0.08, -0.04, 0, 0.04, 0.08)
  ) +
  theme_minimal() +
  coord_equal() +
  labs(x = "Subtype", y = "Previous Subtype", fill = "Out-Value    ") +
  theme(
    text = element_text(size = rel(5)),
    legend.text = element_text(size = rel(3)),
    legend.position = "bottom",
    legend.key.width = unit(0.6, "in"),
    ,
    axis.title.x = element_text(margin = margin(t = 25)),
    axis.title.y = element_text(margin = margin(r = 25, l = 10))
  )

dat6 |>
  filter(p_throws == "R",
         pitch_subtype == "FC1" & prev_subtype == "FF1") |>
  group_by(player_name) |>
  summarize(count = n()) |>
  arrange(-count)

dat6 |>
  filter(p_throws == "R") |>
  group_by(player_name) |>
  summarize(
    p_ff1 = round(sum(pitch_subtype == "FF1") / sum(pitch_type == "FF"), 2),
    p_fc1 = round(sum(pitch_subtype == "FC1") / sum(pitch_type == "FC"), 2)) |>
  filter(p_ff1 > .33 & p_fc1 > .33)

# Jack Flaherty

## LHP

LHP <- dat6 |>
  filter(p_throws == "L")

l_mod1 <- lmer(out_value ~  pitch_type + prev_type + pitch_type*prev_type + stand + on_base + count_impact + (1|pitcher) + (1|batter) + (1|home_team), data=LHP)

l_newdata <- data.frame(crossing(prev_type = pitch_types, pitch_type = pitch_types, stand=c("R", "L"), on_base = F, count_impact = "even", batter = "", pitcher = "", home_team = ""))

res <- predictInterval(merMod = l_mod1, newdata=l_newdata, seed=740,
                       level = 0.8, n.sims = 10000, stat = "mean",
                       type = "linear.prediction", which = "fixed",
                       include.resid.var = F)

l_pred_mod1 <- cbind(l_newdata, res)

l_pred_mod1 |>
  filter(stand == "R") |>
  mutate(fit_sig =
           ifelse(lwr < 0  & upr > 0, 0, fit)) |>
  ggplot(aes(x = pitch_type, y = prev_type, fill = fit_sig)) +
  geom_tile(color = "grey90") +
  scale_fill_gradient2(
    low = "red",
    high = "darkgreen",
    limits = c(-0.045, 0.045),
    breaks = c(-0.04, -0.02, 0, 0.02, 0.04)
  ) +
  theme_minimal() +
  coord_equal() +
  labs(title = "LHP versus RHB",
       x = "Pitch Type",
       y = "Previous Pitch Type",
       fill = "Out-Value    ") +
  theme(
    text = element_text(size = rel(5)),
    legend.text = element_text(size = rel(3)),
    legend.position = "bottom",
    legend.key.width = unit(0.6, "in"),
    strip.text = element_text(margin = margin(b = 10)),
    plot.title = element_text(size = rel(9.5), hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 25)),
    axis.title.y = element_text(margin = margin(r = 25, l = 10))
  )

l_pred_mod1 |>
  filter(stand == "L") |>
  mutate(fit_sig =
           ifelse(lwr < 0  & upr > 0, 0, fit)) |>
  ggplot(aes(x = pitch_type, y = prev_type, fill = fit_sig)) +
  geom_tile(color = "grey90") +
  scale_fill_gradient2(
    low = "red",
    high = "darkgreen",
    limits = c(-0.045, 0.045),
    breaks = c(-0.04, -0.02, 0, 0.02, 0.04)
  ) +
  theme_minimal() +
  coord_equal() +
  labs(title = "LHP versus LHB",
       x = "Pitch Type",
       y = "Previous Pitch Type",
       fill = "Out-Value    ") +
  theme(
    text = element_text(size = rel(5)),
    legend.text = element_text(size = rel(3)),
    legend.position = "bottom",
    legend.key.width = unit(0.6, "in"),
    strip.text = element_text(margin = margin(b = 10)),
    plot.title = element_text(size = rel(9.5), hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 25)),
    axis.title.y = element_text(margin = margin(r = 25, l = 10))
  )

res2 <- predictInterval(merMod = l_mod1, newdata=l_newdata, seed = 740,
                        level = 0.99, n.sims = 10000, stat = "mean",
                        type = "linear.prediction", which = "fixed",
                        include.resid.var = F)

l_pred2_mod1 <- cbind(l_newdata, res2)

l_pred2_mod1 |>
  filter(lwr > 0 | upr < 0)

dat6 |>
  filter(p_throws == "L",
         prev_type == "SL",
         pitch_type == "SL") |>
  group_by(prev_subtype, pitch_subtype) |>
  summarize(mean_out_value = mean(out_value),
            count = n())|>
  arrange(mean_out_value)

sl_sl <- dat6 |>
  filter(p_throws == "L",
         prev_type == "SL" & pitch_type == "SL") |>
  mutate(
    prev_subtype = relevel(prev_subtype, ref = "SL1"),
    pitch_subtype = relevel(pitch_subtype, ref = "SL4")
  )

l_mod2 <- lmer(out_value ~ pitch_subtype + prev_subtype + pitch_subtype*prev_subtype + stand + on_base + count_impact + (1|home_team), data=sl_sl)

sl <- c("SL1", "SL2", "SL3", "SL4", "SL5")

sl_sl_l_newdata <- crossing(prev_subtype = sl, pitch_subtype = sl, stand=c("R","L"), on_base = F, count_impact = "even", batter = "", pitcher = "", home_team = "")

sl_sl_res <- predictInterval(merMod = l_mod2, newdata = sl_sl_l_newdata, seed = 740,
                             level = 0.8, n.sims = 10000, stat = "mean",
                             type = "linear.prediction", which = "fixed",
                             include.resid.var = F)

sl_sl_l_pred_mod2 <- cbind(sl_sl_l_newdata, sl_sl_res)

sl_sl_l_pred_mod2 |>
  filter(stand == "L") |>
  mutate(
    fit_sig =
      round(ifelse(lwr < 0  & upr > 0, 0, fit), 2),
    fit_sig =
      round(
        ifelse(prev_subtype == "SL1" &
                 pitch_subtype == "SL3", 0, fit_sig),
        2
      ),
    batter = paste0(stand, "HB")
  ) |>
  ggplot(aes(x = pitch_subtype, y = prev_subtype, fill = fit_sig)) +
  geom_tile(color = "grey90") +
  geom_text(aes(label = fit_sig), color = "black", size = 5) +
  scale_fill_gradient2(
    low = "red",
    high = "darkgreen",
    limits = c(-0.105, 0.105),
    breaks = c(-0.08, -0.04, 0, 0.04, 0.08)
  ) +
  theme_minimal() +
  coord_equal() +
  labs(
    #title = "LHP Slider-Slider",
       x = "Subtype",
       y = "Previous Subtype",
       fill = "Out-Value   ") +
  theme(
    text = element_text(size = rel(5)),
    legend.text = element_text(size = rel(3)),
    legend.position = "bottom",
    legend.key.width = unit(0.6, "in"),
    strip.text = element_text(size = rel(5)),
    plot.title = element_text(size = rel(9.5), hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 25)),
    axis.title.y = element_text(margin = margin(r = 25, l = 10))
  )

dat6 |>
  filter(p_throws == "L",
         pitch_subtype == "SL4" & prev_subtype == "SL5") |>
  group_by(player_name) |>
  summarize(count = n()) |>
  arrange(-count)

dat6 |>
  filter(p_throws == "L") |>
  group_by(player_name) |>
  summarize(
    p_sl5 = round(sum(pitch_subtype == "SL5") / sum(pitch_type == "SL"), 2),
    p_sl4 = round(sum(pitch_subtype == "SL4") / sum(pitch_type == "SL"), 2)) |>
  filter(p_sl5 > 0.2 & p_sl4 > 0.2)

# Sean Manaea

dat6 |>
  filter(!is.na(prev_type)) |>
  group_by(pitch_type, prev_type) |>
  summarize(count = n()) |>
  ggplot(aes(x=pitch_type, y = prev_type, fill = count)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "white", high = "#800020") +
  labs(
    title = "Frequency of Pitch Type Combinations",
    x = "Pitch Type",
    y = "Previous Pitch Type",
    fill = "Count     "
  ) +
  theme(
    text = element_text(size = rel(5)),
    legend.text = element_text(size = rel(3)),
    legend.key.height = unit(0.8, "in"),
    strip.text = element_text(size = rel(5)),
    plot.title = element_text(size = rel(9.5), hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 25)),
    axis.title.y = element_text(margin = margin(r = 25, l = 10))
  )
  

dat_clusts_r <- dat6 |>
  filter(p_throws == "R") |>
  group_by(pitch_subtype) |> 
  summarize(
    release_speed = mean(release_speed),
    vert = mean(pfx_z) * 12,
    horz = mean(pfx_x) * 12,
    spin = mean(release_spin_rate),
    count = n()
  ) |>
  ungroup() |>
  mutate(pitch_type = substr(pitch_subtype,1,2))


dat_clusts_r |>
  ggplot(aes(
    x = horz,
    y = vert,
    label= pitch_subtype
  )) +
  geom_point(size = 7,
             alpha = 0.5,
             color = "#800020") +
  facet_wrap( ~ pitch_type) +
  labs(
    title = "Movement Characteristics of RHP Pitch Subtypes",
    x = "Horitzonal Break",
    y = "Vertical Break"
  ) +
  theme( 
    text = element_text(size = rel(5)),
    strip.text = element_text(size = rel(5)),
    plot.title = element_text(size = rel(9.5), hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 25)),
    axis.title.y = element_text(margin = margin(r = 25, l = 10))
  )

dat_clusts_r |>
  ggplot(aes(
    x = spin,
    y = release_speed,
    label= pitch_subtype
  )) +
  geom_point(size = 7,
             alpha = 0.5,
             color = "#800020") +
  facet_wrap( ~ pitch_type) +
  labs(
    title = "Pitch Dynamics of RHP Pitch Subtypes",
    x = "Spin Rate",
    y = "Velocity"
  ) +
  theme( 
    text = element_text(size = rel(5)),
    strip.text = element_text(size = rel(5)),
    plot.title = element_text(size = rel(9.5), hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 25)),
    axis.title.y = element_text(margin = margin(r = 25, l = 10))
  )

dat_clusts_l <- dat6 |>
  filter(p_throws == "L") |>
  group_by(pitch_subtype) |> 
  summarize(
    release_speed = mean(release_speed),
    vert = mean(pfx_z) * 12,
    horz = mean(pfx_x) * 12,
    spin = mean(release_spin_rate),
    count = n()
  ) |>
  ungroup() |>
  mutate(pitch_type = substr(pitch_subtype,1,2))


dat_clusts_l |>
  ggplot(aes(
    x = horz,
    y = vert,
    label= pitch_subtype
  )) +
  geom_point(size = 7,
             alpha = 0.5,
             color = "#800020") +
  facet_wrap( ~ pitch_type) +
  labs(
    title = "Movement Characteristics of LHP Pitch Subtypes",
    x = "Horitzonal Break",
    y = "Vertical Break"
  ) +
  theme( 
    text = element_text(size = rel(5)),
    strip.text = element_text(size = rel(5)),
    plot.title = element_text(size = rel(9.5), hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 25)),
    axis.title.y = element_text(margin = margin(r = 25, l = 10))
  )

dat_clusts_l |>
  ggplot(aes(
    x = spin,
    y = release_speed,
    label= pitch_subtype
  )) +
  geom_point(size = 7,
             alpha = 0.5,
             color = "#800020") +
  facet_wrap( ~ pitch_type) +
  labs(
    title = "Pitch Dynamics of LHP Pitch Subtypes",
    x = "Spin Rate",
    y = "Velocity"
  ) +
  theme( 
    text = element_text(size = rel(5)),
    strip.text = element_text(size = rel(5)),
    plot.title = element_text(size = rel(9.5), hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 25)),
    axis.title.y = element_text(margin = margin(r = 25, l = 10))
  )
