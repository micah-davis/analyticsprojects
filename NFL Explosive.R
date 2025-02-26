library(tidyverse)
library(ggrepel)
library(nflreadr)
library(nflplotR)

pbp <- load_pbp(2024)

explosive <- pbp |> 
  filter(!is.na(posteam) &
           !is.na(yards_gained)
         & fixed_drive_result == "Touchdown") |> 
  filter(special == 0 & fumble == 0 & interception == 0) |> 
  group_by(posteam, game_id, drive) |> 
  summarize(max_yards = max(yards_gained)) |> 
  mutate(explosive_play = if_else(max_yards >= 20, 1, 0)) |> 
  ungroup() |> 
  group_by(posteam) |> 
  summarize(tds_no_explosive = sum(explosive_play == 0),
            tds_explosive = sum(explosive_play == 1),
            total_drives = sum(tds_no_explosive + tds_explosive),
            percent_no_exp = tds_no_explosive / total_drives,
            percent_w_exp = tds_explosive / total_drives) |> 
  select(posteam, percent_w_exp, percent_no_exp)

ggplot(explosive, aes(percent_w_exp, y = reorder(posteam, percent_w_exp))) +
  geom_col(aes(color = posteam, fill = posteam), width = 0.5) +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl(alpha = 0.5) +
  theme(axis.text.y = element_nfl_logo(size = .65)) +
  scale_x_continuous(expand = c(0,0), 
                     breaks = scales::pretty_breaks(n = 5),
                     label = scales::percent_format()) +
  labs(title = "Which NFL Team Has The Highest % of
       Explosive Plays per TD Drive?",
       subtitle = "2024 Season",
       caption = "From An Introduction to NFL Analytics with R
       by Brad Congelio") +
  xlab("Percent of TD Drives with an Explosive Play (20+ Yards)") +
  ylab("")
