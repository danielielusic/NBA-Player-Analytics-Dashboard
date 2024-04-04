# NBA-Player-Analytics-Dashboard
## Project Overview
- Created an NBA Player Analytics Dashboard which presents both advanced and player tracking stats in a visual and easily interpretable manner.
- Scraped data from the NBA Stats API to retrieve detailed information on shot types, defensive activity and advanced stats.
- Built an R Shiny App to allow users to easily search for players and view up-to-date statistics.

## Code and Files
**R Version:** 4.2.3

**webscraping.R:** code to scrape data from the NBA Stats API.

**nbadata.rda:** contains the data used in the Shiny App (updated weekly).

**app.R:** script to build visualizations and run the Shiny App.

## Web Scraping
Scraped data from [stats.nba.com](stats.nba.com) to retrieve the following tracking stats:
- Shot types
- Shooting breakdown by distance of closest defender
- Defensive stats
- Hustle stats
- Advanced stats

Additionally, I used the `nbastatR` package to load play-by-play data, which was used to create the shooting location chart.

The data does not update automatically - I have to run the web scraping script and push the data online. I will try to do this weekly, but please note that data from the most recent games might not always be available.

## Data Preparation
The NBA does a great job providing a wealth of data, but falls short in performing necessary transformations to help interpret this information. I took the following steps before building visualizations to make the data more digestible:
- Standardized stats on a per-36 minute basis to account for differences in playing time.
- Calculated percentiles of these stats broken down by position to easily compare similar players.

## Overview of Dashboard
*All screenshots are as of April 4, 2024.*
### Offensive Overview
This page provides insight into how the player is performing relative to similar-usage players (within 5% of the player's usage percentile) at his position. The left side of the page displays traditional box score stats as well as the player's usage percentile. The right side shows how that player compares to his most similar competitors in shooting efficiency, passing ability and limiting turnovers.

![Offensive Overview Page]((https://github.com/danielielusic/NBA-Player-Analytics-Dashboard/blob/main/readme%20images/offensive_overview.png))
### Shooting Breakdown
The shooting breakdown page makes use of player tracking and play-by-play data to dissect a player's strengths and weaknesses as a scorer. The table at the top shows how often and how well he shoots contested and uncontested shots. One can also see where on the court he shoots most often. Lastly, the dumbbell plot indicates how frequently and efficiently the player scores out of different play types.

## Defense and Rebounding
The final page of statistics indicates how active the player is as a defender and rebounder. The 'Hustle Stats' table uses tracking data to demonstrate impact beyond typical box score stats with metrics such as deflections, charges and boxouts. The scatter plots show how frequently the player records steals, blocks, and rebounds relative to other players at his position.

## Acknowledgements
This project would not have been possible without my Sports Analytics professor, Martin Barron, who helped me brainstorm ideas and troubleshoot code, and got me started with my first Shiny App. Additionally, I'd like to thank Owen Phillips for his template of web scraping code, Dom Samangy for his tutorial on building a shot chart and the developers of `nbastatR` for making it easy to scrape play-by-play data.
