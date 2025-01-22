library(tidyverse)
library(gganimate)

tracking <- read_csv("tracking_week_1.csv")

#change ids/colors/names depending on play, motion type, team, and players involved


#the code below filters out the plays to identify a specific play with a specific motion
ex1 <- tracking |> 
  filter(gameId == 2022091102, playId == 145) |> 
  mutate(first_frame = frameId[which(event == "line_set")][1],
         last_frame = frameId[which(event == "tackle")][1]) |> 
  filter(frameId >= first_frame & frameId <= last_frame) |> 
  mutate(
    pt_size = ifelse(club == "football", 2.7, 5.4),
    pt_fill_order = case_when(
      club == "CHI" ~ "2",
      club == "SF" & nflId == "52433" ~ "3",
      club == "SF" & nflId != "52433" ~ "1",
      club == "football" ~ "4"
    )
  )

#identifies team 1
club1 <- ex1 |> 
  filter(club != "football")
#identifies team 2
football1 <- ex1 |>
  filter(club == "football")

#maps out the path of the motioned player
path1 <- ex1 |>
  filter(nflId == 52433) |>
  mutate(motion_frame = frameId[which(event == "man_in_motion")][1],
         snap_frame = frameId[which(event == "ball_snap")][1]) |> 
  mutate(motion_color = "lightgray",
         motion_linetype = "dashed",
         motion_linewidth = 0.4)
motion1 <- path1 |>
  filter(frameId >= motion_frame & frameId <= snap_frame) |> 
  mutate(motion_color = "gold",
         motion_linetype = "solid",
         motion_linewidth = 1.2)

#generates animated plot of the play and generates a football field grid
anim_u <- ggplot() +
  annotate("rect",
           xmin = 160/3,
           xmax = 0,
           ymin = 15,
           ymax = 65,
           fill = scales::alpha("#21ae5f", 0.9)) +
  annotate("text", 
           y = seq(20, 60, 10),
           x = 10,
           color = "white",
           family = "Chivo",
           label = seq(10, 50, 10),
           size = 6,
           angle = 90) +
  annotate("text", 
           y = seq(20, 60, 10),
           x = 40,
           color = "white",
           family = "Chivo",
           label = seq(10, 50, 10),
           size = 6,
           angle = 270) +
  annotate("text", 
           y = setdiff(seq(15, 65, 1), seq(15, 65, 5)),
           x = 160/3,
           color = "white",
           label = "—") +
  annotate("text", 
           y = setdiff(seq(15, 65, 1), seq(15, 65, 5)),
           x = 0,
           color = "white",
           label = "—") +
  annotate("text", 
           y = setdiff(seq(15, 65, 1), seq(15, 65, 5)),
           x = 23.36667,
           color = "white",
           size = 3,
           label = "—") +
  annotate("text", 
           y = setdiff(seq(15, 65, 1), seq(15, 65, 5)),
           x = 29.96667,
           color = "white",
           size = 3,
           label = "—") +
  annotate("segment", 
           y = 15,
           yend = 65,
           x = c(160/3, 0),
           xend = c(160/3, 0),
           color = "white") +
  geom_hline(yintercept = seq(15, 65, 5), color = "white") +
  annotate("segment", 
           y = 60,
           yend = 60,
           x = 0,
           xend = 160/3,
           size = 1.5,
           color = "#FDE725") +
  annotate("segment", 
           y = 53,
           yend = 53,
           x = 0,
           xend = 160/3,
           size = 1.5,
           #000aa0
           color = "midnightblue") +
  geom_point(data = club1, 
             aes(y, x, fill = pt_fill_order, size = pt_size, group = nflId), shape = 21) +
  geom_point(data = football1, 
             aes(y, x, size = pt_size, group = nflId), fill = "#654321", shape = 21) +
  geom_line(data = motion1,
            aes(y, x, color = motion_color, linetype = motion_linetype, linewidth = motion_linewidth)) +
  geom_line(data = path1, 
            aes(y, x, color = motion_color, linetype = motion_linetype, linewidth = motion_linewidth)) +
  scale_fill_manual(values = c("red", "#002C5F", "gold"),
                    labels = c("SF", "CHI", "Motion (B. Aiyuk)")) +
  scale_size_identity() +
  scale_color_identity() +
  scale_linetype_identity() +
  scale_linewidth_identity() +
  scale_x_reverse() +
  transition_time(frameId) +
  transition_reveal(frameId) +
  ease_aes("linear") +
  guides(fill = guide_legend(override.aes = list(size = 5.4))) +
  labs(title = "San Francisco 49ers @ Chicago Bears",
       fill = NULL) +
  theme_minimal() 
anim_u

#saves the animation
anim_save("anim_u.gif", anim_u)
