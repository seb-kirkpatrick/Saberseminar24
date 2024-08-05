library(tidyverse)
library(readr)

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
  select(at_bat_id, ending_event = events)

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

p3 |>
  ggplot(
    aes(x=balls, y=percent_out, color=as.factor(strikes))) +
  geom_path() +
  geom_point() +
  labs(
    title = "Pitcher-caused Out Percentage by Count",
    x = "Balls",
    y = "Percent of PAs ending in an out",
    color = "Strikes"
  )
