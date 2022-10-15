#install.packages("devtools")
#devtools::install_github(repo = "maksimhorowitz/nflscrapR")

#library(nflscrapR)
#packageurl <- "http://cran.r-project.org/src/contrib/Archive/nflscrapR/nflscrapR_1.8.3.tar.gz"
#install.packages(packageurl, contriburl=NULL, type="source")

#install.packages("remotes")
#remotes::install_github("jthomasmock/espnscrapeR")
run_libs = function(){
  library(espnscrapeR)
  library(tidyverse)
  library(ggplot2)
  library(ggpubr)
  library(readxl)
}



lsf.str("package:espnscrapeR")
a = get_nfl_qbr("2022", season_type = "Regular", week = 1)


nfl_qbr <- function(year, season_type, week, qb_name){
  temp = data.frame(get_nfl_qbr(season = year, season_type = season_type, week = week))
  temp = temp[temp$name_display == qb_name,]
  temp
  } 


team_id_list = data.frame(get_nfl_teams())$team_id
team_id_list = strtoi(team_id_list)

##### Data Frame of All Team Depth Charts ######
a = get_depth_chart(team = 1)
for (i in team_id_list){
  b = get_depth_chart(team = i)
  a = bind_rows(a, b)
}

##### ------  List of Depth Charts with Player ID -------- ###########
all_players = data.frame(distinct(a))
player_id_list = all_players$athlete_id

############# Need to Pull a List of Player Names to Map to Player/Athlete ID #########

get_athlete_1 <- function(athlete_id){
  
  season <- Sys.Date() %>% substr(1, 4)
  
  base_url <- "https://sports.core.api.espn.com/v2/sports/football/leagues/nfl/seasons/{season}/athletes/{athlete_id}"
  
  raw_get  <- base_url %>%
    glue::glue() %>%
    httr::GET()
  
  httr::stop_for_status(raw_get)
  
  raw_json <- content(raw_get)
  
  athlete_df <- raw_json %>%
    tibble::enframe() %>%
    dplyr::filter(name != "$ref") %>%
    tidyr::pivot_wider(names_from = name, values_from = value) %>%
    janitor::clean_names() %>%
    tidyr::hoist(position, pos = "abbreviation") %>%
    #tidyr::hoist(headshot, headshot_url = "href") %>%
    #tidyr::hoist(
    #  draft,
    #  draft_txt = "displayText",
    #  draft_year = "year",
    #  draft_round = "round",
    #  draft_slot = "selection",
    #  draft_team_id = list("team", "$ref")
    #) %>%
    tidyr::hoist(experience, nfl_exp = "years") %>%
    tidyr::hoist(team, team_id = list(1, "$ref")) %>%
    dplyr::select(
      player_id = id,
      player_guid = guid,
      team_id,
      pos,
      player_first_name = first_name,
      player_last_name = last_name,
      player_full_name = full_name,
      player_short_name = short_name,
      weight,
      height,
      age,
      dob = date_of_birth,
      #headshot_url,
      #jersey,
      nfl_exp,
      dplyr::contains("draft"),
      #-draft
    ) %>%
    dplyr::mutate(
      #draft_team_id = stringr::str_remove(draft_team_id, "http://sports.core.api.espn.com/v2/sports/football/leagues/nfl/seasons/[:digit:]+/teams/"),
      #draft_team_id = stringr::str_remove(draft_team_id, "\\?lang=en&region=us"),
      team_id = stringr::str_remove(team_id, "http://sports.core.api.espn.com/v2/sports/football/leagues/nfl/seasons/[:digit:]+/teams/"),
      team_id = stringr::str_remove(team_id, "\\?lang=en&region=us")
    ) %>%
    tidyr::unchop(tidyselect:::where(is.list))
  
  athlete_df
  
}
get_athlete(17196)

fn_pid = data.frame(fn = "Test", pID = "Test")
for(i in player_id_list){
  try({
    get_athlete_1(i)
    fn = get_athlete_1(i)$player_full_name
    pID = get_athlete_1(i)$player_id
    temp = data.frame(fn = fn, pID = pID)
    fn_pid = rbind(fn_pid, temp)
  })
}

## Data Table Cleanup ##
names(fn_pid)[names(fn_pid) == 'pID'] <- 'athlete_id'

all_table = merge(all_players, fn_pid, by='athlete_id', all = TRUE)
teams = data.frame(get_nfl_teams())
all_table = merge(all_table, teams, by = 'team_id', all=TRUE)
all_table = all_table[,-c(16:18)]

all_table = distinct(all_table)

library(writexl)
write_xlsx(all_table, "D:/Projects/Sports Data/Name Table.xlsx")

## -- Get Player Info Based on Player Name -- ##

player_find = function(player_name){
  na.omit(all_table[all_table$fn == player_name, ])
}

player_find("Travis Etienne Jr.")


### --- Use User Input to find Player Data based on their name --- ###
player_data <- function(){
  x = 0
  while (x < 1){
    player_name = readline(prompt="Enter Player Name: ")
    if(player_name == "Exit"){
      x = 1
    }
    else{
      if(nrow(player_find((player_name))) == 0){
        print("Player does not exist. Please try again.")
      }
      else{
        print(player_find(player_name))
      }
    }
  }
}

nrow(player_find("Josh Allen"))

### ------- Create a data frame to find Game IDs to pull boxscore info ------- ######

game_id = get_nfl_schedule(season="2021")$game_id
season = get_nfl_schedule(2021)$season
test = data.frame(get_nfl_schedule(2021))
test$game_date

nfl_years = as.character(c(2015:2022))
season_info = data.frame(get_nfl_schedule(2022))
season_info[,c(-1)]
colnames(season_info)
season_bs = season_info[, c("game_id", "matchup_short", "season", "slug", "game_date", "indoor", "home_team_name",
                "home_team_id", "away_team_name", "away_team_id", "home_score", "away_score", "home_win",
                "home_record", "away_record")]

season_columns = c("game_id", "matchup_short", "season", "slug", "game_date", "indoor", "home_team_name",
                   "home_team_id", "away_team_name", "away_team_id", "home_score", "away_score", "home_win",
                   "home_record", "away_record")
season_bs %>% filter(slug != "preseason")

season_scores = data.frame(get_nfl_schedule("2014"))
season_scores = season_scores[, season_columns]
for(i in nfl_years){
  tryCatch({
    season_temp = data.frame(get_nfl_schedule(i))
    season_temp = season_temp[, season_columns]
    season_scores = rbind(season_scores, season_temp)
    print(paste("Successfully joined Season: ", i))
  },
  error = function(e){
    print(paste("Error on Season: " , i))
    print(e)
  }
  )
}




boxscore_players_colnames = c("game_id", "season", "season_type", "date", "week", 
"home_away", "winner", "team_id", "team_city", "team_name", 
"team_abb", "team_full", "team_score", "team_record", 
"player_id", "first_name", "last_name", "full_name", 
"pass_att", "pass_cmp", "pass_yds", "pass_avg", "pass_td", 
"pass_int", "pass_sacks", "pass_sack_yds", "pass_rtg",
"rush_att", "rush_yds", "rush_avg", "rush_td", "rush_long", 
"rec_total", "rec_yds", "rec_avg", "rec_td", "rec_long", "rec_tgts",
"fum_total")


df2 = data.frame(get_nfl_boxscore_players(400554677))
df1 = df1[, boxscore_players_colnames]

desired_columns_boxscore = colnames(df1)
desired_columns_boxscore
paste(desired_columns_boxscore, sep = ",", collapse = ", ")


colnames(df2)
get_nfl_boxscore_players("340104011")

get_nfl_boxscore_players_1 <- function(game_id) {
  
  game_url <- glue::glue("http://site.api.espn.com/apis/site/v2/sports/football/nfl/summary")
  
  raw_get <- httr::GET(game_url, query = list(event = game_id, enable = "ranks,odds,linescores,logos"))
  
  httr::stop_for_status(raw_get)
  
  raw_json <- httr::content(raw_get)
  
  wide_game_raw <- raw_json[["boxscore"]][["players"]] %>%
    tibble(data = .) %>%
    unnest_wider(data)
  
  if ("statistics" %in% names(wide_game_raw)) {
    wide_game_summary <- raw_json[["boxscore"]][["players"]] %>%
      tibble(data = .) %>%
      unnest_wider(data) %>%
      unnest_longer(statistics) %>%
      unnest_wider(statistics) %>%
      #unnest_longer(athletes) %>%
      #unnest_wider(athletes) %>%
      #unnest_wider(athlete) %>%
      unchop(cols = c(labels, descriptions, totals, stats)) %>%
      unchop(cols = c(labels, descriptions, totals, stats)) %>%
      select(-links) %>%
      mutate(
        name = case_when(
          name == "passing" ~ "pass",
          name == "rushing" ~ "rush",
          name == "receiving" ~ "rec",
          name == "fumbles" ~ "fum",
          name == "defensive" ~ "def",
          name == "interceptions" ~ "int",
          name == "kickReturns" ~ "kick_ret",
          name == "puntReturns" ~ "punt_ret",
          name == "kicking" ~ "kick",
          name == "punting" ~ "punt",
          TRUE ~ name
        )
      ) %>%
      unite(stat_labels, sep = "_", name, labels) %>%
      mutate(
        stat_labels = tolower(stat_labels) %>% gsub(x = ., pattern = "/", replacement = "p"),
        stat_labels = gsub(x = stat_labels, pattern = " ", replacement = "_"),
      ) %>%
      pivot_wider(names_from = stat_labels, values_from = stats, id_cols = c(text, id:displayName)) %>%
      rename(
        player_id = id,
        player_uid = uid,
        player_guid = guid,
        first_name = firstName,
        last_name = lastName,
        full_name = displayName,
        rush_att = rush_car,
        def_solo_tkl = def_solo,
        def_tkl = def_tot,
        rec_total = rec_rec,
        fum_total = fum_fum,
        int_total = int_int
      ) %>%
      separate(pass_cpatt, into = c("pass_att", "pass_cmp"), sep = "/", convert = TRUE) %>%
      separate(pass_sacks, into = c("pass_sacks", "pass_sack_yds"), sep = "-", convert = TRUE) %>%
      separate(kick_fg, into = c("kick_fg_att", "kick_fg_made"), sep = "/", convert = TRUE) %>%
      separate(kick_xp, into = c("kick_xp_att", "kick_xp_made"), sep = "/", convert = TRUE) %>%
      mutate(across(c(pass_yds:punt_long), ~ suppressWarnings(as.double(.x))))
    
    player_id_df <- raw_json[["boxscore"]][["players"]] %>%
      tibble(data = .) %>%
      unnest_wider(data) %>%
      unnest_wider(team) %>%
      select(
        team_id = id,
        team_uid = uid,
        team_city = location,
        team_name = name,
        team_abb = abbreviation,
        team_full = displayName,
        team_color = color,
        team_color_alt = alternateColor,
        team_logo = logo,
        statistics
      ) %>%
      unnest_longer(statistics) %>%
      unnest_wider(statistics) %>%
      #unnest_longer(athletes) %>%
      #unnest_wider(athletes) %>%
      #unnest_wider(athlete) %>%
      select(team_id:team_logo, player_id = id) %>%
      distinct(player_id, .keep_all = TRUE)
  } else {
    wide_game_summary <- wide_game_raw %>%
      unnest_wider(team)
  }
  
  game_header <- raw_json %>%
    keep(names(raw_json) %in% "header") %>%
    tibble(data = .) %>%
    unnest_wider(data) %>%
    unchop(competitions) %>%
    rename(game_id = id, game_uid = uid) %>%
    unnest_wider(competitions) %>%
    unnest_wider(season) %>%
    select(-id, -uid) %>%
    rename(season = year, season_type = type) %>%
    select(
      !any_of(
        c(
          "neutralSite",
          "conferenceCompetition",
          "boxscoreAvailable",
          "commentaryAvailable",
          "liveAvailable",
          "onWatchESPN",
          "recent",
          "boxscoreSource",
          "playByPlaySource"
        )
      )
    ) %>%
    unchop(competitors) %>%
    unnest_wider(competitors) %>%
    rename(team_id = id, team_uid = uid, team_order = order, home_away = homeAway) %>%
    select(-links, -timeValid) %>%
    unnest_wider(team) %>%
    rename(
      team_name = name,
      team_location = location,
      team_full = displayName,
      team_abb = abbreviation,
      team_color = color,
      team_color_alt = alternateColor,
      team_score = score
    ) %>%
    hoist(logos, team_logo = list(1, "href")) %>%
    hoist(record, team_record = list(1, "displayValue")) %>%
    select(
      !any_of(c(
        "links",
        "logos",
        "possession",
        "broadcasts",
        "league",
        "linescores",
        "record",
        "status",
        "rank",
        "team_uid",
        "team_name",
        "team_abb",
        "team_full",
        "team_color",
        "team_color_alt",
        "team_logo",
        "nickname",
        "id",
        "uid"
      ))
    ) %>%
    select(game_id:date, any_of("week"), team_id:last_col())
  
  if ("statistics" %in% names(wide_game_raw)) {
    left_join(wide_game_summary, player_id_df, by = "player_id") %>%
      left_join(game_header, by = "team_id") %>%
      select(
        contains("game"),
        contains("season"),
        date,
        week,
        home_away,
        winner,
        contains("team"),
        everything(),
        -team_location
      ) %>%
      mutate(
        winner = as.integer(winner),
        team_score = as.integer(team_score)
      )
  } else {
    game_header %>%
      select(
        contains("game"),
        contains("season"),
        date,
        any_of("week"),
        home_away,
        winner,
        contains("team"),
        everything(),
        -team_location
      ) %>%
      mutate(
        winner = as.integer(winner),
        team_score = as.integer(team_score)
      )
  }
}
get_nfl_boxscore_players_1(340126035)


game_id_list = season_scores$game_id
df1 = data.frame(get_nfl_boxscore_players(season_scores$game_id[1]))
df1 = df1[, boxscore_players_colnames]
count_success = 0
count_error = 0
for(i in game_id_list){
  tryCatch({
    box_score_temp = data.frame(get_nfl_boxscore_players(i))
    box_score_temp = box_score_temp[, boxscore_players_colnames]
    df1 = rbind(df1, box_score_temp)
    print(paste("Successfully joined on Game ID: ", i))
    count_success = count_success + 1
  },
  error = function(e){
    print(paste("Error on Game ID: " , i))
    print(e)
    count_error = count_error + 1
  }
  )
}
print(paste("Successfully Joined: ", count_success))
print(paste("Unsuccessfully Joined: ", count_error))

season_scores$new_date = substr(season_scores$game_date, 0, 10)

more_bs = function(date = date){
  game_ids = season_scores[season_scores$new_date == date, ]
  game_ids$game_id
}


season_scores[season_scores$new_date == "2022-09-20", ]

########################## FUNCTION TO UPDATE BOX SCORES ##################
create_bs_df = function(list_of_ids = list_of_ids){
  temp_bs_df = data.frame(get_nfl_boxscore_players(todays_bs[1]))
  temp_bs_df = temp_bs_df[, boxscore_players_colnames]
  count_success = 0
  count_error = 0
  for(i in list_of_ids){
    tryCatch({
      box_score_temp = data.frame(get_nfl_boxscore_players(i))
      box_score_temp = box_score_temp[, boxscore_players_colnames]
      temp_bs_df = rbind(temp_bs_df, box_score_temp)
      print(paste("Successfully joined on Game ID: ", i))
      count_success = count_success + 1
    },
    error = function(e){
      print(paste("Error on Game ID: " , i))
      print(e)
      count_error = count_error + 1
    }
    )
  }
  temp_bs_df
}

todays_bs = more_bs(Sys.Date()-5)

today_df = unique(create_bs_df(todays_bs))
df1 = rbind(df1, today_df)
df1 = unique(df1)
tail(df1)
####### --------------------------------------------------------------####

jt = df1 %>% filter(full_name == "Jonathan Taylor", rush_att != "NA")


### ----- Use game ids to pull boxscores -------- ####


#boxscore_players_colnames = c("game_id", "season", "season_type", "date", "week", "home_away", "winner",
 #                             "team_id", "team_name", "team_abb", "team_record", "player_id", "full_name")
test_run = season_bs$game_id[0:20]
temp_df = data.frame(get_nfl_boxscore_players(401220114))
temp_df = temp_df[, boxscore_players_colnames]
for (i in test_run){
  tryCatch({
    misc = get_nfl_boxscore_players(i)
    misc = misc[, boxscore_players_colnames]
    temp_df = rbind(temp_df, misc)
    print(paste("Successfully joined Game ID: ", i))
  },
  error = function(e){
    print(paste("Error on Game_ID: " , i))
  }
  )
}


### ---- Function to show stats by player name ---- ####
player_stats = function(name){
  ps = df1 %>% filter(full_name == name)
  ps[is.na(ps)] = 0
  ps = ps %>% group_by(game_id, full_name, season, season_type, date, week, winner) %>% 
    summarize("Rush Attempts" = sum(rush_att), "Rush Yards" = sum(rush_yds), "Rush Avg" = sum(rush_avg),
              "Rush TDs" = sum(rush_td),
              "Pass Attempts" = sum(pass_att), "Pass Completed" = sum(pass_cmp), "Pass Yards" = sum(pass_yds),
              "Pass TDs" = sum(pass_td), "Pass INTs" = sum(pass_int),
              "Rec Yards" = sum(rec_yds), "Rec Targets" = sum(rec_tgts), "Rec Catches" = sum(rec_total),
              "Rec TDs" = sum(rec_td), "Reg Long" = sum(rec_long)
    )
  ps$date = as.Date(ps$date)
  ps$week2 = ifelse(ps$week < 10, paste("0", ps$week, sep = ""), ps$week)
  ps$date_id = paste(ps$season, ps$season_type, ps$week2, sep = "")

  ps
}


ps_data = player_stats("Tua Tagovailoa")
ps_data = ps_data[ps_data$season_type != 1, ]

ggplot(data.frame(ps_data), 
       aes(x=ps_data$date_id, y=ps_data$`Pass TDs`, group = 1)) + geom_bar(stat = "Identity") + 
  labs(title="Player Data", x= "Date") + theme(axis.text.x = element_text(angle = 90)) +
  geom_text(aes(label = ps_data$`Pass TDs`), vjust=-0.2) + geom_line(aes(y = 3))

ps_data
colnames(ps_data)

passing_tds <- function(name){
  ps_data = player_stats(name)
  ps_data = ps_data[ps_data$season_type != 1, ]
  
  ggplot(data.frame(ps_data), 
         aes(x=ps_data$date_id, y=ps_data$`Pass TDs`, group = 1)) + geom_bar(stat = "Identity") + 
    labs(title=paste("Passing TDs for",  name), x= "Date") + theme(axis.text.x = element_text(angle = 90)) +
    geom_text(aes(label = ps_data$`Pass TDs`), vjust=-0.2)
}

passing_yards <- function(name){
  ps_data = player_stats(name)
  ps_data = ps_data[ps_data$season_type != 1, ]
  
  ggplot(data.frame(ps_data), 
         aes(x=ps_data$date_id, y=ps_data$`Pass Yards`, group = 1)) + geom_bar(stat = "Identity") + 
    labs(title=paste("Passing Yards for",  name), x= "Date") + theme(axis.text.x = element_text(angle = 90)) +
    geom_text(aes(label = ps_data$`Pass Yards`), vjust=-0.2)
}

rushing_yards <- function(name){
  ps_data = player_stats(name)
  ps_data = ps_data[ps_data$season_type != 1, ]
  
  ggplot(data.frame(ps_data), 
         aes(x=ps_data$date_id, y=ps_data$`Rush Yards`, group = 1)) + geom_bar(stat = "Identity") + 
    labs(title=paste("Rushing Yards for",  name), x= "Date") + theme(axis.text.x = element_text(angle = 90)) +
    geom_text(aes(label = ps_data$`Rush Yards`), vjust=-0.2)
}

rushing_tds <- function(name){
  ps_data = player_stats(name)
  ps_data = ps_data[ps_data$season_type != 1, ]
  
  ggplot(data.frame(ps_data), 
         aes(x=ps_data$date_id, y=ps_data$`Rush TDs`, group = 1)) + geom_bar(stat = "Identity") + 
    labs(title=paste("Rushing TDs for",  name), x= "Date") + theme(axis.text.x = element_text(angle = 90)) +
    geom_text(aes(label = ps_data$`Rush TDs`), vjust=-0.2)
}

receiving_yards <- function(name){
  ps_data = player_stats(name)
  ps_data = ps_data[ps_data$season_type != 1, ]
  
  ggplot(data.frame(ps_data), 
         aes(x=ps_data$date_id, y=ps_data$`Rec Yards`, group = 1)) + geom_bar(stat = "Identity") + 
    labs(title=paste("Receiving Yards for",  name), x= "Date") + theme(axis.text.x = element_text(angle = 90)) +
    geom_text(aes(label = ps_data$`Rec Yards`), vjust=-0.2)
}

receiving_targets <- function(name){
  ps_data = player_stats(name)
  ps_data = ps_data[ps_data$season_type != 1, ]
  
  ggplot(data.frame(ps_data), 
         aes(x=ps_data$date_id, y=ps_data$`Rec Targets`, group = 1)) + geom_bar(stat = "Identity") + 
    labs(title=paste("Receiving Targets for",  name), x= "Date") + theme(axis.text.x = element_text(angle = 90)) +
    geom_text(aes(label = ps_data$`Rec Targets`), vjust=-0.2)
}

receiving_tds <- function(name){
  ps_data = player_stats(name)
  ps_data = ps_data[ps_data$season_type != 1, ]
  
  ggplot(data.frame(ps_data), 
         aes(x=ps_data$date_id, y=ps_data$`Rec TDs`, group = 1)) + geom_bar(stat = "Identity") + 
    labs(title=paste("Receiving TDs for",  name), x= "Date") + theme(axis.text.x = element_text(angle = 90)) +
    geom_text(aes(label = ps_data$`Rec TDs`), vjust=-0.2)
}

all_stats <- function(name){
  ps_data = player_stats(name)
  ps_data = ps_data[ps_data$season_type != 1, ]
  ps_data
}


ps
name_fill = "Justin Jefferson"

passing_yards(name_fill)
passing_tds(name_fill)
rushing_yards(name_fill)
rushing_tds(name_fill)
a = mean_stats(name_fill)
t = mean(a$`Rec Yards`)
ryds = receiving_yards(name_fill) + geom_line(aes(y = mean(all_stats(name_fill)$`Rec Yards`)))
rtds = receiving_tds(name_fill) + geom_line(aes(y = mean(all_stats(name_fill)$`Rec TDs`)))

ggarrange(ryds, rtds, nrow = 2)

player_history <- function(){
  x = 0
  while (x < 1){
    player_name = readline(prompt="Enter Player Name: ")
    if(player_name == "Exit"){
      x = 1
    }
    else{
      if(nrow(player_stats((player_name))) == 0){
        print("Player does not exist. Please try again.")
      }
      else{
        print(player_name)
        ps_data = player_stats(player_name)
        ps_data = ps_data[ps_data$season_type != 1, ]
        print(ggplot(data.frame(ps_data), 
                     aes(x=ps_data$date_id, y=ps_data$`Pass Yards`, group = 1)) + geom_bar(stat = "Identity") + 
                labs(title="Player Data", x= "Date") + theme(axis.text.x = element_text(angle = 90)) +
                geom_text(aes(label = ps_data$`Pass Yards`), vjust=-0.2))
      }
    }
  }
}




############### Get a list of 2022 Receiver stats with avg and variance on YDS ###########
data_22 = df1
data_22[is.na(data_22)] = 0


rec_test = data_22 %>% group_by(game_id, full_name, season, season_type, date, week, winner) %>% 
  summarize("Rush Attempts" = sum(rush_att), "Rush Yards" = sum(rush_yds), "Rush Avg" = sum(rush_avg),
            "Rush TDs" = sum(rush_td),
            "Pass Attempts" = sum(pass_att), "Pass Completed" = sum(pass_cmp), "Pass Yards" = sum(pass_yds),
            "Pass TDs" = sum(pass_td), "Pass INTs" = sum(pass_int),
            "Rec Yards" = sum(rec_yds), "Rec Targets" = sum(rec_tgts), "Rec Catches" = sum(rec_total),
            "Rec TDs" = sum(rec_td), "Reg Long" = sum(rec_long))

rec_yards = rec_test[rec_test$season == 2022 & rec_test$season_type == 2, ]

rec_yards[rec_yards$full_name == name_fill, ]


current_ry = rec_yards %>% group_by(full_name) %>% 
  summarise("Avg Rec Yards" = mean(`Rec Yards`), "STD" = sd(`Rec Yards`)) %>% 
  arrange(desc(`Avg Rec Yards`))
print(current_ry, n = 50)

current_ry[current_ry$full_name == "Alec Pierce", ]

data_22[data_22$full_name == "Amon-Ra St. Brown", ]
rec_test[rec_test$full_name == "Amon-Ra St. Brown", ]
a = rec_test[rec_test$full_name == "Amon-Ra St. Brown", ] 

receiving_yards("Terry McLaurin")


##### Add in Team Name and Import Offense + Defense DVOA #####
# Import DVOA Files #

colnames(all_table)
roster = all_table
colnames(roster)[11] = "full_name"
team_name = roster[, c("full_name", "team_abb")]

ry_table = unique(merge(current_ry, team_name, by="full_name") %>% arrange(desc(`Avg Rec Yards`)))

season_info = data.frame(get_nfl_schedule(2022))
season_info$new_date = substr(season_info$startDate, 0, 10)
season_info %>% arrange(desc(new_date))
start_date = "2022-10-13"
end_date = "2022-10-18"
current_week_schedule = season_info[season_info$new_date >= start_date & season_info$new_date <= end_date, ]
current_week_schedule =current_week_schedule[, c("matchup_short", "venue_city", "indoor", "home_team_name", "home_team_abb",
                          "home_team_id", "home_record",
                          "away_team_name", "away_team_abb", "away_team_id", "away_record",
                          "new_date")]


########## List of Schedule with Team Abbreviations ########
colnames(current_week_schedule)
matchup = "remove"
team1 = "remove"
team2 = "remove"
t1_home_away = "remove"
list_of_games = data.frame(matchup, team1, team2, t1_home_away)
rownames(current_week_schedule) = 1:nrow(current_week_schedule)

for (i in current_week_schedule){
  home_df = data.frame(current_week_schedule$matchup_short,
                       current_week_schedule$home_team_abb,
                       current_week_schedule$away_team_abb,
                       "Home")
  away_df = data.frame(current_week_schedule$matchup_short,
                       current_week_schedule$away_team_abb,
                       current_week_schedule$home_team_abb,
                       "Away")
  colnames(home_df) = c("matchup", "team1", "team2", "t1_home_away")
  colnames(away_df) = c("matchup", "team1", "team2", "t1_home_away")
  list_of_games = rbind(list_of_games, home_df)
  list_of_games = rbind(list_of_games, away_df)
}
list_of_games = unique(list_of_games)
schedule_abbr = list_of_games[-1, ]
colnames(schedule_abbr)[2] = "Team"
head(schedule_abbr)



########### DVOA File Joining ##########
week = 6


setwd(paste("D:/Projects/Sports Data/DVOA FIles/Week", week))
def_dvoa = data.frame(read_csv("2022 Team DVOA Ratings Defense.csv"))
off_dvoa = data.frame(read_csv("2022 Team DVOA Ratings Offense.csv"))
qb_dvoa = data.frame(read_csv("2022 Quarterbacks.csv"))
wr_dvoa = data.frame(read_csv("2022 Wide Receivers.csv"))
rb_dvoa = data.frame(read_csv("2022 Running Backs.csv"))
te_dvoa = data.frame(read_csv("2022 Tight Ends.csv"))

overall_dvoa = data.frame(read_csv(paste("2022 Team DVOA Ratings Overall (Week ", week, ")",  ".csv", sep = "")))
colnames(def_dvoa)[1] = "Team2"


ry_table = unique(merge(current_ry, team_name, by="full_name") %>% arrange(desc(`Avg Rec Yards`)))
colnames(ry_table)[4] = "Team"
head(ry_table)
ry_table_merge = merge(ry_table, off_dvoa, by="Team") %>% arrange(desc(`Avg Rec Yards`))
head(ry_table_merge)

ry_table_dvoa = merge(ry_table_merge, schedule_abbr, by="Team") %>% arrange(desc(`Avg Rec Yards`))
colnames(ry_table_dvoa)[27] = "Team2"

ry_table_all_dvoa = merge(ry_table_dvoa, def_dvoa, by = "Team2") %>% arrange(desc(`Avg Rec Yards`))

colnames(ry_table_all_dvoa)
ry_table_final = ry_table_all_dvoa[, c("Team", "full_name", "Avg Rec Yards", "STD", "Pass.DVOA.Rank.x", "Pass.DVOA.x",
                      "Team2", "Pass.DVOA.Rank.y" ,"Pass.DVOA.y", "t1_home_away")]
ry_table_final
############## DVOA Final Table Complete ###############

##### Compare average yards bewtween each week and flag for upward trends (e.g. Alec Pierce) ######
data_22 = df1
data_22[is.na(data_22)] = 0
data_22 = data_22[data_22$season == 2022 & data_22$season_type != 1, ]


weeks = data_22 %>% group_by(game_id, full_name, season, season_type, date, week, winner) %>% 
  summarize("Rush Attempts" = sum(rush_att), "Rush Yards" = sum(rush_yds), "Rush Avg" = sum(rush_avg),
            "Rush TDs" = sum(rush_td),
            "Pass Attempts" = sum(pass_att), "Pass Completed" = sum(pass_cmp), "Pass Yards" = sum(pass_yds),
            "Pass TDs" = sum(pass_td), "Pass INTs" = sum(pass_int),
            "Rec Yards" = sum(rec_yds), "Rec Targets" = sum(rec_tgts), "Rec Catches" = sum(rec_total),
            "Rec TDs" = sum(rec_td), "Reg Long" = sum(rec_long))

week1 = weeks[weeks$week == 1, ]
week1 = week1 %>% group_by(full_name, season) %>% summarize("Week 1 Avg" = mean(`Rec Yards`)) %>%
  arrange(desc(`Week 1 Avg`))


for (i in 2:week){
  weekx = weeks[weeks$week <= i, ]
  string_temp = paste("Week", i,"Avg")
  weekx = weekx %>% group_by(full_name, season) %>% 
    summarize(string_temp = mean(`Rec Yards`))
  print(string_temp)
  week1 = merge(week1, weekx, by = "full_name")
}
a = seq(2, 2*week, 2)
trends = week1[, -a]
head(trends)

weekavg = c("Week 1 Avg")
for (i in 2:week){
  string_temp = paste("Week", i,"Avg")
  weekavg = append(weekavg, string_temp)
}

colnames(trends)[-1] = weekavg
head(trends)

trends = trends[rowSums(trends[, c(2:5)]) != 0, ]  
w2_pos = trends[trends$`Week 2 Avg` >= trends$`Week 1 Avg`, ] 
w3_pos = w2_pos[w2_pos$`Week 3 Avg` >= w2_pos$`Week 2 Avg`, ] 
w4_pos = w3_pos[w3_pos$`Week 4 Avg` >= w3_pos$`Week 3 Avg`, ]
w5_pos = w4_pos[w4_pos$`Week 5 Avg` >= w4_pos$`Week 4 Avg`, ]
head(w5_pos)

week5 = weeks[weeks$week <= 5, ]

trends[trends$full_name == "J.D. McKissic", ]
receiving_yards("Darnell Mooney")

######### Flag players with the top 10 highest change ###########


### AVG Yards, AVG Yards L3, Receptions, Targets, QB DVOA, WR DVOA, Pass DVOA, Oppenent, Openent Pass DVOA ###
data_22 = df1
data_22[is.na(data_22)] = 0
data_22 = data_22[data_22$season == 2022 & data_22$season_type != 1, ]


data_analysis = data_22 %>% group_by(game_id, full_name, season, season_type, date, week, winner) %>% 
  summarize("Rush Attempts" = sum(rush_att), "Rush Yards" = sum(rush_yds), "Rush Avg" = sum(rush_avg),
            "Rush TDs" = sum(rush_td),
            "Pass Attempts" = sum(pass_att), "Pass Completed" = sum(pass_cmp), "Pass Yards" = sum(pass_yds),
            "Pass TDs" = sum(pass_td), "Pass INTs" = sum(pass_int),
            "Rec Yards" = sum(rec_yds), "Rec Targets" = sum(rec_tgts), "Rec Catches" = sum(rec_total),
            "Rec TDs" = sum(rec_td), "Reg Long" = sum(rec_long))

wr_analysis = data_analysis %>% group_by(full_name, season) %>% 
  summarize("Avg Rec Yards" = mean(`Rec Yards`), "Avg Rec Targets" = mean(`Rec Targets`),
            "Avg Rec Catches" = mean(`Rec Catches`))

l1 = week - 1
l1_data = data_analysis[data_analysis$week >= l1, ]

wr_l1 = l1_data %>% group_by(full_name, season) %>% 
  summarize("L1 Avg Rec Yards" = mean(`Rec Yards`), "L1 Avg Rec Targets" = mean(`Rec Targets`),
            "L1 Avg Rec Catches" = mean(`Rec Catches`))

l3 = week - 3
l3_data = data_analysis[data_analysis$week >= l3, ]

wr_l3 = l3_data %>% group_by(full_name, season) %>% 
  summarize("L3 Avg Rec Yards" = mean(`Rec Yards`), "L3 Avg Rec Targets" = mean(`Rec Targets`),
            "L3 Avg Rec Catches" = mean(`Rec Catches`))

unique(l3_data$game_id)
receiving_yards("Mark Andrews")


head(team_name)
colnames(team_name)[2] = "Team"
matchup = schedule_abbr[2:4]

wr_analysis = merge(wr_analysis, team_name, by="full_name")
wr_analysis1 = merge(wr_analysis, matchup, by = "Team") %>% arrange(desc(`Avg Rec Yards`), desc("Team"))

wr_analysis1 = merge(wr_analysis1, wr_l3, by = "full_name")
wr_analysis1 = merge(wr_analysis1, wr_l1, by = "full_name")
wr_analysis1 = unique(wr_analysis1) %>% arrange(desc(`L3 Avg Rec Yards`))
colnames(wr_analysis1)[7] = "Opponent"

### AVG, L3, L1 Stats for WRs ###
head(wr_analysis1)
head(off_dvoa)
colnames(off_dvoa)[c(1,9, 11)]
off_ranks = off_dvoa[,c(1,9, 11) ]

colnames(def_dvoa)[c(1,9, 11)]
def_ranks = def_dvoa[,c(1,9, 11) ]
colnames(def_ranks) = c("Opponent", "Def.Pass.DVOA.Rank", "Def.Rush.DVOA.Rank")
wr_analysis1 = merge(wr_analysis1, off_ranks, by="Team")
wr_analysis1 = merge(wr_analysis1, def_ranks, by="Opponent") %>% arrange(desc(`L3 Avg Rec Yards`))
colnames(wr_analysis1)
list = c("Team", "full_name", "season.x", "Avg Rec Yards", "L3 Avg Rec Yards", "L1 Avg Rec Yards",
         "Avg Rec Catches", "Avg Rec Targets",
  "L3 Avg Rec Catches", "L3 Avg Rec Targets",
  "L1 Avg Rec Catches", "L1 Avg Rec Targets",
  "Opponent", "Pass.DVOA.Rank", "Def.Pass.DVOA.Rank", "Rush.DVOA.Rank", "Def.Rush.DVOA.Rank")
wr_table = wr_analysis1[, list]
trending_up = wr_table[wr_table$`L3 Avg Rec Yards` > wr_table$`Avg Rec Yards`, ]
trending_down = wr_table[wr_table$`L3 Avg Rec Yards` < wr_table$`Avg Rec Yards`, ]
good_matchups = wr_table[wr_table$Def.Pass.DVOA.Rank > 25 & wr_table$Pass.DVOA.Rank < 10, ]
bad_matchups = wr_table[wr_table$Def.Pass.DVOA.Rank < 10 & wr_table$Pass.DVOA.Rank > 22, ]

head(wr_table)

receiving_yards("Travis Kelce")
wr_analysis

######################### ---- Passing Stats -------- #########################
colnames(data_analysis)
data_analysis = data_analysis[!(data_analysis$full_name == "Josh Allen" & data_analysis$`Pass Yards` == 0), ]
qb_analysis = data_analysis %>% group_by(full_name, season) %>% 
  summarize("Avg Pass Yards" = mean(`Pass Yards`), "Avg Pass Att" = mean(`Pass Attempts`),
            "Avg Pass Comp" = mean(`Pass Completed`), "Avg Pass TD" = mean(`Pass TDs`),
                                   "Avg Pass INTs" = mean(`Pass INTs`))
l1_data = data_analysis[data_analysis$week >= l1, ]
qb_l1 = l1_data %>% group_by(full_name, season) %>% 
  summarize("L1 Avg Pass Yards" = mean(`Pass Yards`), "L1 Avg Pass Att" = mean(`Pass Attempts`),
            "L1 Avg Pass Comp" = mean(`Pass Completed`), "L1 Avg Pass TD" = mean(`Pass TDs`),
            "L1 Avg Pass INTs" = mean(`Pass INTs`))

l3_data = data_analysis[data_analysis$week >= l3, ]
qb_l3 = l3_data %>% group_by(full_name, season) %>% 
  summarize("L3 Avg Pass Yards" = mean(`Pass Yards`), "L3 Avg Pass Att" = mean(`Pass Attempts`),
            "L3 Avg Pass Comp" = mean(`Pass Completed`), "L3 Avg Pass TD" = mean(`Pass TDs`),
            "L3 Avg Pass INTs" = mean(`Pass INTs`))


qb_analysis = merge(qb_analysis, team_name, by="full_name")
qb_analysis1 = merge(qb_analysis, matchup, by = "Team")

qb_analysis1 = merge(qb_analysis1, qb_l3, by = "full_name")
qb_analysis1 = merge(qb_analysis1, qb_l1, by = "full_name")
qb_analysis1 = unique(qb_analysis1)

head(qb_analysis1)


### AVG, L3, L1 Stats for WRs ###
head(qb_analysis1)

colnames(def_dvoa)[c(1,9, 11)]
def_ranks = def_dvoa[,c(1,9, 11) ]
colnames(def_ranks) = c("Opponent", "Def.Pass.DVOA.Rank", "Def.Rush.DVOA.Rank")
qb_analysis1 = merge(qb_analysis1, off_ranks, by="Team")
colnames(qb_analysis1)
colnames(qb_analysis1)[9] = "Opponent"
qb_analysis1 = merge(qb_analysis1, def_ranks, by="Opponent")
colnames(qb_analysis1)
list = c("Team", "full_name", "season", "Avg Pass Yards", "L3 Avg Pass Yards", "L1 Avg Pass Yards",
         "Avg Pass Att", "Avg Pass Comp",
         "L3 Avg Pass Att", "L3 Avg Pass Comp",
         "L1 Avg Pass Att", "L1 Avg Pass Comp",
         "Opponent", "Pass.DVOA.Rank", "Def.Pass.DVOA.Rank", "Rush.DVOA.Rank", "Def.Rush.DVOA.Rank")
qb_table = qb_analysis1[, list]
qb_trending_up = qb_table[qb_table$`L3 Avg Pass Yards` > qb_table$`Avg Pass Yards`, ]
qb_trending_down = qb_table[qb_table$`L3 Avg Pass Yards` < qb_table$`Avg Pass Yards`, ]
qb_good_matchups = qb_table[qb_table$Def.Pass.DVOA.Rank > 25 & qb_table$Pass.DVOA.Rank < 10, ]
qb_bad_matchups = qb_table[qb_table$Def.Pass.DVOA.Rank < 10 & qb_table$Pass.DVOA.Rank > 22, ]

qb_table[qb_table$full_name == "Josh Allen", ]
wr_analysis

######################### ---- Rushing Stats -------- #########################
colnames(data_analysis)
data_analysis = data_analysis[!(data_analysis$full_name == "Josh Allen" & data_analysis$`Pass Yards` == 0), ]
rb_analysis = data_analysis %>% group_by(full_name, season) %>% 
  summarize("Avg Rush Yards" = mean(`Rush Yards`), "Avg Rush Att" = mean(`Rush Attempts`),
            "Avg Rush TD" = mean(`Rush TDs`))
l1_data = data_analysis[data_analysis$week >= l1, ]
rb_l1 = l1_data %>% group_by(full_name, season) %>% 
  summarize("L1 Avg Rush Yards" = mean(`Rush Yards`), "L1 Avg Rush Att" = mean(`Rush Attempts`),
            "L1 Avg Rush TD" = mean(`Rush TDs`))

l3_data = data_analysis[data_analysis$week >= l3, ]
rb_l3 = l3_data %>% group_by(full_name, season) %>% 
  summarize("L3 Avg Rush Yards" = mean(`Rush Yards`), "L3 Avg Rush Att" = mean(`Rush Attempts`),
            "L3 Avg Rush TD" = mean(`Rush TDs`))


rb_analysis = merge(rb_analysis, team_name, by="full_name")
rb_analysis1 = merge(rb_analysis, matchup, by = "Team")

rb_analysis1 = merge(rb_analysis1, rb_l3, by = "full_name")
rb_analysis1 = merge(rb_analysis1, rb_l1, by = "full_name")
rb_analysis1 = unique(rb_analysis1)

head(rb_analysis1)


### AVG, L3, L1 Stats for WRs ###
head(rb_analysis1)

colnames(def_dvoa)[c(1,9, 11)]
def_ranks = def_dvoa[,c(1,9, 11) ]
colnames(def_ranks) = c("Opponent", "Def.Pass.DVOA.Rank", "Def.Rush.DVOA.Rank")
rb_analysis1 = merge(rb_analysis1, off_ranks, by="Team")
colnames(rb_analysis1)
colnames(rb_analysis1)[7] = "Opponent"
rb_analysis1 = merge(rb_analysis1, def_ranks, by="Opponent")
colnames(rb_analysis1)
list = c("Team", "full_name", "season", "Avg Rush Yards", "L3 Avg Rush Yards", "L1 Avg Rush Yards",
         "Avg Rush Att", "Avg Rush TD",
         "L3 Avg Rush Att", "L3 Avg Rush TD",
         "L1 Avg Rush Att", "L1 Avg Rush TD",
         "Opponent", "Pass.DVOA.Rank", "Def.Pass.DVOA.Rank", "Rush.DVOA.Rank", "Def.Rush.DVOA.Rank")

rb_table = rb_analysis1[, list]
rb_trending_up = rb_table[rb_table$`L3 Avg Rush Yards` > rb_table$`Avg Rush Yards`, ]
rb_trending_down = rb_table[rb_table$`L3 Avg Rush Yards` < rb_table$`Avg Rush Yards`, ]
rb_good_matchups = rb_table[rb_table$Rush.DVOA.Rank > 25 & qb_table$Pass.DVOA.Rank < 10, ]
rb_bad_matchups = rb_table[rb_table$Def.Pass.DVOA.Rank < 10 & qb_table$Pass.DVOA.Rank > 22, ]

qb_table[qb_table$full_name == "Josh Allen", ]
wr_analysis

