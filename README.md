# Football Passing Networks and Tactical Adaptation
COSC 421 (Network Science) final project, UBC Okanagan.
Gulsher Assad, Saksham Joshi, Sarabroop Aulakh

This project models football passing as directed, weighted networks (players are nodes,
completed passes are edges) and compares how teams' passing networks change when they
are winning, drawing or losing, using every match of the 2015/16 Premier League.

## Data
Data provided by StatsBomb: https://github.com/statsbomb/open-data
Accessed in R with the StatsBombR package.
- passes.csv: all 282,379 completed passes with team, passer, receiver, minute and score state
- segments.csv: the 1,566 score-state networks with their density, centralization and passes per minute

The raw files (matches.rds, events.rds) are too large for GitHub. Run 00_download_data.R to download them.

## Code
- 00_download_data.R: downloads the 2015/16 Premier League data (run once, 10-20 minutes)
- project.R: score-state tagging, network construction and all analysis (RQ1 to RQ4)

## How to run
1. Install packages: igraph, remotes, then remotes::install_github("statsbomb/StatsBombR")
2. Set the working directory to this folder
3. Run 00_download_data.R
4. Run project.R
