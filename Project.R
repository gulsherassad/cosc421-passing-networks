# COSC 421 Project: Football Passing Networks and Tactical Adaptation
# Saksham Joshi, Sarabroop Aulakh, Gulsher Assad
# Data: StatsBomb open data, 2015/16 Premier League
#
# Before running:
# 1. Run 00_download_data.R once to create matches.rds and events.rds
# 2. Set the working directory to this folder (Session > Set Working Directory)

library(igraph)

# Part 1: Loading the Data

matches <- readRDS("matches.rds")
events <- readRDS("events.rds")

# Part 2: Score at Each Point in the Match

# Sort events in match order
events <- events[order(events$match_id, events$index), ]

# Home team for each event
events$home_team <- matches$home_team.home_team_name[match(events$match_id, matches$match_id)]

# Goals (including own goals)
goal <- (events$type.name == "Shot" & events$shot.outcome.name %in% "Goal") |
  events$type.name == "Own Goal For"
events$home_goal <- goal & events$team.name == events$home_team
events$away_goal <- goal & events$team.name != events$home_team

# Score before each event
events$home_before <- ave(as.numeric(events$home_goal), events$match_id, FUN = cumsum) - events$home_goal
events$away_before <- ave(as.numeric(events$away_goal), events$match_id, FUN = cumsum) - events$away_goal

# Check our goals match the final scores (should be 380)
hg <- tapply(events$home_goal, events$match_id, sum)
ag <- tapply(events$away_goal, events$match_id, sum)
id <- as.character(matches$match_id)
sum(hg[id] == matches$home_score & ag[id] == matches$away_score)

# Part 3: Completed Passes and Score State

passes <- events[events$type.name == "Pass" & is.na(events$pass.outcome.name) &
                   !is.na(events$pass.recipient.name), ]
nrow(passes)

# Winning, drawing or losing when the pass was made
is_home <- passes$team.name == passes$home_team
gf <- ifelse(is_home, passes$home_before, passes$away_before)
ga <- ifelse(is_home, passes$away_before, passes$home_before)
passes$state <- ifelse(gf > ga, "winning", ifelse(gf < ga, "losing", "drawing"))
table(passes$state)

# Save the passes we need
pass_data <- data.frame(match_id = passes$match_id, team = passes$team.name,
                        from = passes$player.name, to = passes$pass.recipient.name,
                        minute = passes$minute, state = passes$state, gf = gf, ga = ga)
write.csv(pass_data, "passes.csv", row.names = FALSE)

# Part 4: Leicester City Network

p <- read.csv("passes.csv")
lei <- p[p$team == "Leicester City", ]

# Edge list: number of passes from each player to each player
edges <- aggregate(minute ~ from + to, data = lei, FUN = length)
names(edges)[3] <- "weight"

# Only keep regular players (300+ passes made or received)
involve <- table(c(lei$from, lei$to))
regulars <- names(involve[involve >= 300])
edges <- edges[edges$from %in% regulars & edges$to %in% regulars, ]

# Directed, weighted network
g <- graph_from_data_frame(edges, directed = TRUE)

# Network size, density and average degree
vcount(g)
ecount(g)
edge_density(g)
mean(degree(g, mode = "all"))

# Weighted degree (passes made and received) and betweenness
# (1 / weight so players who pass more are closer)
out_w <- strength(g, mode = "out")
in_w <- strength(g, mode = "in")
btw <- betweenness(g, weights = 1 / E(g)$weight)
players <- data.frame(out_w, in_w, btw)

# Top 5 players
head(players[order(players$out_w, decreasing = TRUE), ], 5)
head(players[order(players$btw, decreasing = TRUE), ], 5)

# Figure 1 (only links with 20+ passes, surnames as labels)
g_plot <- delete_edges(g, E(g)[weight < 20])
V(g_plot)$label <- sub(".* ", "", V(g_plot)$name)
V(g_plot)$label[V(g_plot)$label == "Laet"] <- "De Laet"
plot(g_plot, layout = layout_in_circle(g_plot), edge.width = E(g_plot)$weight / 40,
     edge.arrow.size = 0.2, main = "Figure 1: Leicester City passing network, 2015/16")

# Part 5: Networks for Each Team, Match and Score

# One segment = one team in one match at one scoreline
p$key <- paste(p$match_id, p$team, p$gf, p$ga)
groups <- split(p, p$key)

seg <- data.frame()
for (sub in groups) {
  # Skip segments shorter than 15 minutes
  mins <- max(sub$minute) - min(sub$minute)
  if (mins < 15) next
  
  # Network for this segment
  e <- aggregate(minute ~ from + to, data = sub, FUN = length)
  names(e)[3] <- "weight"
  g2 <- graph_from_data_frame(e, directed = TRUE)
  
  # Final result of the match for this team
  m <- matches[matches$match_id == sub$match_id[1], ]
  home <- sub$team[1] == m$home_team.home_team_name
  goals_for <- ifelse(home, m$home_score, m$away_score)
  goals_against <- ifelse(home, m$away_score, m$home_score)
  result <- ifelse(goals_for > goals_against, "win", ifelse(goals_for < goals_against, "loss", "draw"))
  
  seg <- rbind(seg, data.frame(team = sub$team[1], state = sub$state[1], result = result,
                               minutes = mins, passes_per_min = nrow(sub) / mins,
                               density = edge_density(g2),
                               centralization = centr_degree(g2, mode = "all")$centralization))
}

# Number of networks
nrow(seg)
table(seg$state)
write.csv(seg, "segments.csv", row.names = FALSE)

# Part 6: RQ1 - Do networks change with the score?

# Average of each measure when winning, drawing and losing
aggregate(cbind(passes_per_min, density, centralization) ~ state, data = seg, FUN = mean)

# Drawing segments are longer (every match starts 0-0)
aggregate(minutes ~ state, data = seg, FUN = mean)

# Compare again using only segments of a similar length (30 to 45 minutes)
seg_mid <- seg[seg$minutes >= 30 & seg$minutes <= 45, ]
table(seg_mid$state)
aggregate(cbind(passes_per_min, density, centralization) ~ state, data = seg_mid, FUN = mean)

# Figures 2 and 3
boxplot(centralization ~ state, data = seg, main = "Figure 2: Degree centralization by score state",
        ylab = "Degree centralization")
boxplot(passes_per_min ~ state, data = seg, main = "Figure 3: Passes per minute by score state",
        ylab = "Passes per minute")

# Part 7: RQ2 - Which players change role with the score?

# Leicester
table(lei$state)

# Winning
lw <- lei[lei$state == "winning", ]
ew <- aggregate(minute ~ from + to, data = lw, FUN = length)
names(ew)[3] <- "weight"
ew <- ew[ew$from %in% regulars & ew$to %in% regulars, ]
gw <- graph_from_data_frame(ew, directed = TRUE)

# Drawing
ld <- lei[lei$state == "drawing", ]
ed <- aggregate(minute ~ from + to, data = ld, FUN = length)
names(ed)[3] <- "weight"
ed <- ed[ed$from %in% regulars & ed$to %in% regulars, ]
gd <- graph_from_data_frame(ed, directed = TRUE)

# Losing
ll <- lei[lei$state == "losing", ]
el <- aggregate(minute ~ from + to, data = ll, FUN = length)
names(el)[3] <- "weight"
el <- el[el$from %in% regulars & el$to %in% regulars, ]
gl <- graph_from_data_frame(el, directed = TRUE)

# Share of the team's passes each player was involved in
# (each pass has two players, so divide by 2 x total passes)
share_win <- strength(gw, mode = "all")[regulars] / (2 * sum(E(gw)$weight))
share_draw <- strength(gd, mode = "all")[regulars] / (2 * sum(E(gd)$weight))
share_lose <- strength(gl, mode = "all")[regulars] / (2 * sum(E(gl)$weight))

# Betweenness in each score state
btw_win <- betweenness(gw, weights = 1 / E(gw)$weight)[regulars]
btw_draw <- betweenness(gd, weights = 1 / E(gd)$weight)[regulars]
btw_lose <- betweenness(gl, weights = 1 / E(gl)$weight)[regulars]

roles <- data.frame(share_win, share_draw, share_lose, btw_win, btw_draw, btw_lose)
roles$change <- pmax(share_win, share_draw, share_lose) - pmin(share_win, share_draw, share_lose)
round(roles[order(roles$change, decreasing = TRUE), ], 3)

# Part 8: RQ3 - Do communities match positions? (Leicester)

# Most common position for each Leicester player
lei_ev <- events[events$team.name == "Leicester City" & events$player.name %in% regulars, ]
pos <- as.data.frame(table(player = lei_ev$player.name, position = lei_ev$position.name))
pos <- pos[order(pos$Freq, decreasing = TRUE), ]
pos <- pos[!duplicated(pos$player), ]

# Position groups: lines (defense, midfield, attack) and flanks (left, right, base)
defense <- c("Goalkeeper", "Left Back", "Right Back", "Left Center Back", "Right Center Back")
attack <- c("Left Center Forward", "Right Center Forward")
base <- c("Goalkeeper", "Left Center Back", "Right Center Back")
pos$line <- ifelse(pos$position %in% defense, "Defense",
                   ifelse(pos$position %in% attack, "Attack", "Midfield"))
pos$flank <- ifelse(pos$position %in% base, "Base",
                    ifelse(substr(pos$position, 1, 4) == "Left", "Left", "Right"))
pos[, c("player", "position", "line", "flank")]

# Undirected network (passes A->B and B->A added together) for Louvain
gu <- as_undirected(g, mode = "collapse", edge.attr.comb = "sum")
set.seed(421)
comm <- cluster_louvain(gu)
length(comm)
sizes(comm)
modularity(comm)

# Community membership by line and by flank
line <- pos$line[match(V(gu)$name, pos$player)]
flank <- pos$flank[match(V(gu)$name, pos$player)]
table(membership(comm), line)
table(membership(comm), flank)

# Figure 4: Leicester communities
V(gu)$label <- sub(".* ", "", V(gu)$name)
set.seed(421)
plot(comm, gu, main = "Figure 4: Leicester City passing communities")

# Communities in each score state
for (s in c("winning", "drawing", "losing")) {
  sub <- lei[lei$state == s, ]
  e <- aggregate(minute ~ from + to, data = sub, FUN = length)
  names(e)[3] <- "weight"
  e <- e[e$from %in% regulars & e$to %in% regulars, ]
  gs <- as_undirected(graph_from_data_frame(e, directed = TRUE), mode = "collapse", edge.attr.comb = "sum")
  set.seed(421)
  cs <- cluster_louvain(gs)
  print(s)
  print(modularity(cs))
  print(split(V(gs)$name, membership(cs)))
}

# Part 9: RQ2 and RQ3 for All Teams

# Most common position for every player in the league
ev <- events[!is.na(events$player.name), ]
pos_all <- as.data.frame(table(player = ev$player.name, position = ev$position.name))
pos_all <- pos_all[pos_all$Freq > 0, ]
pos_all <- pos_all[order(pos_all$Freq, decreasing = TRUE), ]
pos_all <- pos_all[!duplicated(pos_all$player), ]

# Position groups
cb <- c("Right Center Back", "Center Back", "Left Center Back")
fb <- c("Right Back", "Left Back", "Right Wing Back", "Left Wing Back")
att <- c("Right Wing", "Left Wing", "Right Center Forward", "Left Center Forward",
         "Center Forward", "Striker", "Secondary Striker")
pos_all$group <- ifelse(pos_all$position == "Goalkeeper", "Goalkeeper",
                        ifelse(pos_all$position %in% cb, "Centre back",
                               ifelse(pos_all$position %in% fb, "Full back",
                                      ifelse(pos_all$position %in% att, "Attack", "Midfield"))))
pos_all$line <- ifelse(pos_all$group %in% c("Goalkeeper", "Centre back", "Full back"), "Defense",
                       ifelse(pos_all$group == "Attack", "Attack", "Midfield"))
pos_all$flank <- ifelse(pos_all$group %in% c("Goalkeeper", "Centre back"), "Base",
                        ifelse(substr(pos_all$position, 1, 4) == "Left", "Left",
                               ifelse(substr(pos_all$position, 1, 5) == "Right", "Right", "Center")))

role_all <- data.frame()
comm_teams <- data.frame()

for (t in unique(p$team)) {
  # Regular players for this team (300+ passes made or received)
  tp <- p[p$team == t, ]
  inv <- table(c(tp$from, tp$to))
  reg <- names(inv[inv >= 300])
  tp <- tp[tp$from %in% reg & tp$to %in% reg, ]
  
  # RQ2: share of the team's passing in each score state
  w <- tp[tp$state == "winning", ]
  d <- tp[tp$state == "drawing", ]
  l <- tp[tp$state == "losing", ]
  if (nrow(w) >= 500 & nrow(l) >= 500) {
    # Count how many passes each player made or received, divided by 2 x total passes
    share_win <- table(factor(c(w$from, w$to), levels = reg)) / (2 * nrow(w))
    share_draw <- table(factor(c(d$from, d$to), levels = reg)) / (2 * nrow(d))
    share_lose <- table(factor(c(l$from, l$to), levels = reg)) / (2 * nrow(l))
    role_all <- rbind(role_all, data.frame(team = t, player = reg,
                                           share_win = as.numeric(share_win),
                                           share_draw = as.numeric(share_draw),
                                           share_lose = as.numeric(share_lose)))
  }
  
  # RQ3: communities in the season network
  e <- aggregate(minute ~ from + to, data = tp, FUN = length)
  names(e)[3] <- "weight"
  gt <- as_undirected(graph_from_data_frame(e, directed = TRUE), mode = "collapse", edge.attr.comb = "sum")
  set.seed(421)
  ct <- cluster_louvain(gt)
  line <- pos_all$line[match(V(gt)$name, pos_all$player)]
  flank <- pos_all$flank[match(V(gt)$name, pos_all$player)]
  
  # Match score: share of players in their community's most common group
  # (for each community, take the biggest group, add them up, divide by number of players)
  comm_teams <- rbind(comm_teams, data.frame(team = t, communities = length(ct),
                                             modularity = round(modularity(ct), 3),
                                             line_match = round(sum(apply(table(membership(ct), line), 1, max)) / vcount(gt), 2),
                                             flank_match = round(sum(apply(table(membership(ct), flank), 1, max)) / vcount(gt), 2)))
}

# RQ2 results (all teams)
length(unique(role_all$team))
role_all$group <- pos_all$group[match(role_all$player, pos_all$player)]
aggregate(cbind(share_win, share_draw, share_lose) ~ group, data = role_all, FUN = mean)
role_all$change <- pmax(role_all$share_win, role_all$share_draw, role_all$share_lose) -
  pmin(role_all$share_win, role_all$share_draw, role_all$share_lose)
head(role_all[order(role_all$change, decreasing = TRUE), ], 10)

# RQ3 results (all teams, Table 9)
comm_teams[order(comm_teams$flank_match, decreasing = TRUE), ]
mean(comm_teams$modularity)
mean(comm_teams$line_match)
mean(comm_teams$flank_match)
sum(comm_teams$flank_match > comm_teams$line_match)

# Part 10: RQ4 - At the same score, do networks differ by final result?

table(seg$state, seg$result)

# Average measures for each score state and final result
aggregate(cbind(passes_per_min, density, centralization) ~ state + result, data = seg, FUN = mean)

# Figure 5: drawing segments only (every match starts at 0-0, so this is the fairest comparison)
boxplot(passes_per_min ~ result, data = seg[seg$state == "drawing", ],
        main = "Figure 5: Passes per minute while drawing, by final result",
        ylab = "Passes per minute")
