# COSC 421 Project - Download the data
# Run once. Saves matches.rds and events.rds (not in the repo because they are too large).

# install.packages("remotes")
# remotes::install_github("statsbomb/StatsBombR")
library(StatsBombR)

# Find the 2015/16 Premier League
comps <- FreeCompetitions()
pl <- comps[comps$competition_name == "Premier League" & comps$season_name == "2015/2016", ]

# All matches (should be 380)
matches <- FreeMatches(pl)
nrow(matches)

# All events from every match (takes 10-20 minutes)
events <- free_allevents(MatchesDF = matches, Parallel = TRUE)
events <- allclean(events)
nrow(events)

saveRDS(matches, "matches.rds")
saveRDS(events, "events.rds")