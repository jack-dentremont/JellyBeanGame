rm(list = ls())
library(dplyr)

bids <- read.csv("The Jellybean Game - Descriptive Analytics - Public Scoreboard (I do this).csv")
real <- read.csv("The Jellybean Game - Descriptive Analytics - Final Results (we do this).csv")

final_rounds <- bids %>%
  group_by(Date, Jar..) %>%
  filter(Bidding.Opportunity.. == max(Bidding.Opportunity..)) %>%
  filter(Bid.Amount == max(Bid.Amount))

final_rounds

joined <- final_rounds %>%
  left_join(real, by = c("Date", "Jar.."))

joined

scoreboard <- joined %>%
  mutate(
    Game.Score = Actual.Jellybeans - Bid.Amount
  ) %>%
  ungroup() %>%
  select(Player, Date, Game.Score) %>%
  tidyr::pivot_wider(
    names_from = Date,
    values_from = Game.Score,
    values_fill = 0
  ) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(-Player), na.rm = TRUE)) %>%
  ungroup()

scoreboard

exclusions <- setdiff(bids$Player, scoreboard$Player)
exclusions
additions <- data.frame(Player = exclusions)
additions

complete_scoreboard <- scoreboard %>%
  full_join(additions, by = "Player") %>%
  mutate(across(-Player, ~ ifelse(is.na(.), 0, .)))

complete_scoreboard

leaderboard <- complete_scoreboard[order(desc(complete_scoreboard$Total)), ]
leaderboard
