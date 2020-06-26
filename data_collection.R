library(tidyverse)
library(rvest)
library(stringr)
library(rebus)
library(data.table)
library(janitor)

## First get per game stats
playerurls <- paste("https://www.basketball-reference.com/leagues/NBA_",
              as.character(seq(1989,2020,1)),"_per_game.html",
              sep = "")

yearlist <- as.list(seq(1989,2020,1))


playerlist <- list()
for(x in playerurls){
  playerlist[[x]] <- html_session(x) %>%
    html_node("table#per_game_stats") %>%
    html_table()
}

playerlist <- Map(cbind, playerlist, year = yearlist)

player_pg_df <- rbindlist(playerlist)

write_csv(player_pg_df,file.path("C:/Users/96dav/Documents/Data Analysis/R/WD/allnba_proj/data_collection","player_pg_df.csv"))

# Now get team stats

teamurls <- paste("https://www.basketball-reference.com/leagues/NBA_",
              as.character(seq(1989,2020,1)),".html",
              sep = "")

teamlist <- list()
for(x in teamurls){
  teamlist[[x]] <- read_html(x) %>% # Read in the raw webpage
    xml_find_all('//comment()') %>% # Use xpath to find all comment nodes
    xml_text() %>% # convert to raw strings 
    paste0(collapse = "") %>% # flatten into a character vector
    read_html %>% # re-read as html content 
    #xml_find_all("//table")
    html_node("table#misc_stats") %>% html_table 
}

teamlist <- Map(cbind, teamlist, year = yearlist)

team_df <- rbindlist(teamlist)

write_csv(team_df, file.path("C:/Users/96dav/Documents/Data Analysis/R/WD/allnba_proj/data_collection", "team_df.csv"))

## Function for getting page numbers
bballref_pages <- function(extra_pages) {
  page_vector <- as.character(seq(from = 0, to = extra_pages*100, by = 100))
}

## Vector of url for each page
allnba_urls <- paste("https://www.basketball-reference.com/play-index/psl_finder.cgi?request=1&match=single&type=per_game&per_minute_base=36&per_poss_base=100&lg_id=NBA&is_playoffs=N&year_min=1989&year_max=&franch_id=&season_start=1&season_end=-1&age_min=0&age_max=99&shoot_hand=&height_min=0&height_max=99&birth_country_is=Y&birth_country=&birth_state=&college_id=&draft_year=&is_active=&debut_yr_nba_start=&debut_yr_nba_end=&is_hof=&is_as=&as_comp=gt&as_val=0&award=all-nba-n&pos_is_g=Y&pos_is_gf=Y&pos_is_f=Y&pos_is_fg=Y&pos_is_fc=Y&pos_is_c=Y&pos_is_cf=Y&qual=&c1stat=&c1comp=&c1val=&c2stat=&c2comp=&c2val=&c3stat=&c3comp=&c3val=&c4stat=&c4comp=&c4val=&c5stat=&c5comp=&c6mult=&c6stat=&order_by=ws&order_by_asc=&offset=",bballref_pages(4), sep = "")

## For loop to scrape table from each url
allnbalist <- list()
for(x in allnba_urls){
  allnbalist[[x]] <- html_session(x) %>%
    html_node("table#stats") %>%
    html_table()
}

## Append all separate dataframes in output
allnba_df <- rbindlist(allnbalist) %>% 
  row_to_names(1) %>%
  arrange(Season) %>%
  filter(Player != "Player") %>%
  mutate(year = rep(c(1989:2019), each = 15))

write_csv(allnba_df, file.path("C:/Users/96dav/Documents/Data Analysis/R/WD/allnba_proj/data_collection", "allnba_df.csv"))

## Lastly we need advanced stats
playerurls2 <- paste("https://www.basketball-reference.com/leagues/NBA_",
                    as.character(seq(1989,2020,1)),"_advanced.html",
                    sep = "")

playerlist2 <- list()
for(x in playerurls2){
  playerlist2[[x]] <- html_session(x) %>%
    html_node("table#advanced_stats") %>%
    html_table()
}

playerlist2 <- Map(cbind, playerlist2, year = yearlist)

player_adv_df <- rbindlist(playerlist2)

write_csv(player_adv_df,file.path("C:/Users/96dav/Documents/Data Analysis/R/WD/allnba_proj/data_collection","player_adv_df.csv"))

