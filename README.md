# NBA-Player-Analytics-Dashboard
## Project Overview
- Created an NBA Player Analytics Dashboard which presents both advanced and player tracking stats in a visual and easily interpretable manner.
- Scraped data from the NBA Stats API to get detailed information on shot types, defensive activity and advanced stats.
- Built an R Shiny App to allow users to easily search for players and view up-to-date statistics.

## Code and Files
**R Version:** 4.2.3

**webscraping.R:** code to scrape data from the NBA Stats API.

**nbadata.rda:** contains the data used in the Shiny App (updated weekly).

**app.R:** script to build visualizations and run the Shiny App.

## Web Scraping
Scraped data from stats.nba.com to get the following tracking stats:
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
