library(tidyverse)

pitches <- read_csv("savant_pitch_level.csv")

# Assign Unique at-bat id's for all pitches in the dataset

abid <- pitches |>
  group_by(game_pk, at_bat_number) |>
  reframe(
    at_bat_id = cur_group_id()
  )

# Join these id's to each individual pitch

p1 <- left_join(pitches, abid, by = c("game_pk", "at_bat_number"))

# Pull out the last pitch of each at-bat and what the result of that at-bat was

outcome <- p1 |>
  group_by(at_bat_id) |>
  slice_max(order_by = pitch_number, n = 1) |> # Any blanks left are either intentional walks or a runner being caught stealing to end the inning, neither of which are batter-induced outs
  dplyr::select(at_bat_id, ending_event = events)

table(outcome$ending_event)

# Join back on the ending result of the entire at-bat to each pitch within the at-bat

p2 <- left_join(p1, outcome, by = "at_bat_id")

# Defining which events are batter outs

outs <- c("double_play", "field_out", "fielders_choice", "fielders_choice_out", "force_out", "grounded_into_double_play", "strikeout", "strikeout_double_play", "triple_play", "sac_bunt_double_play", "sac_fly_double_play", "sac_bunt", "sac_fly")
# Not sure if errors should count (I think yes) and not sure about sacrifices (sac double plays for sure but idk about normal sac bunt/fly)

p3 <- p2 |>
  filter(!str_detect(ending_event, "sac")) |>
  distinct(at_bat_id, balls, strikes, .keep_all=T) |>
  group_by(balls, strikes) |>
  reframe(
    count = n(),
    outs = sum(ending_event %in% outs), 
    percent_out = outs/count
  ) |>
  filter(balls <4,
         strikes<3,
  ) # After double checking, the 8 cases where the pre-pitch count had 3 strikes or 4 balls are not data errors, but umpire errors (I think)

save(p3, file="CountOutValue.rda")

load("~/Saberseminar24/CountOutValue.rda")

# At 0-0, we start at a .676 probability of an out

p3 |>
  ggplot(
    aes(x=balls, y=percent_out, color=as.factor(strikes))) +
  geom_path(linewidth = 2) +
  geom_point(size=5) +
  labs(
    title = "Pitcher-Induced Out Percentage by Count",
    x = "Balls",
    y = "Percent of PAs ending in an out",
    color = "Strikes"
  ) +
  theme_minimal() +
  theme(text = element_text(size = rel(5)), legend.text = element_text(size = rel(3)),legend.key.height = unit(0.6,"in"), strip.text = element_text(margin = margin(b=10)), plot.title = element_text(size = rel(9.5), hjust = 0.5), axis.title.x = element_text(margin = margin(t = 25)), axis.title.y = element_text(margin = margin(r = 25, l=10)))

p3 |>
  ggplot(
    aes(x=strikes, y=percent_out, color=as.factor(balls))) +
  geom_path() +
  geom_point() +
  labs(
    title = "Pitcher-caused Out Percentage by Count",
    x = "Strikes",
    y = "Percent of PAs ending in an out",
    color = "Balls"
  ) +
  theme_minimal()
  

# Building a out-value table to merge on for the pitches

strikeout <- data.frame(balls = c(0:3), strikes = 3, percent_out = 1)
walk <- data.frame(balls = 4, strikes = 0:2, percent_out = 0)

p4 <- p3 |>
  dplyr::select(1,2,5) |>
  rbind(strikeout, walk)

p4

out_values <- cross_join(p4,p4) |>
  rename(pre_balls = balls.x,
         pre_strikes = strikes.x ,
         pre_prob = percent_out.x,
         post_balls = balls.y,
         post_strikes = strikes.y,
         post_prob = percent_out.y
  ) |>
  filter(pre_balls <= 3,
         pre_strikes <= 2,
         pre_balls == post_balls & pre_strikes + 1 == post_strikes |
           pre_balls + 1 == post_balls & pre_strikes == post_strikes) |>
  mutate(out_value = post_prob - pre_prob)

view(out_values)