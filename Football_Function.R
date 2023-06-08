library(tidyverse)
library(httr)
library(rvest)
library(dplyr)

#-------------------------------------------------------------------------
scores2021 <- function(x) {

URL <- "https://www.fleaflicker.com/nfl/scores?season=2021&week="
WEEK <- c(1:x)
all_data <- c()
for (i in WEEK) {
  
  combined <- paste(URL,WEEK[i], sep = "")
  score_html <- read_html(combined)
  week_score <- score_html %>%
    html_nodes(".right.text-right span , .left+ td") %>%
    html_text()
  
  df <- matrix(week_score, ncol = 4, byrow = TRUE) %>% as.tibble(df)
  df2 <- transform(df, V2 = as.numeric(V2), V4 = as.numeric(V4))
  df2 <- df2 %>% mutate(team_1_Tot = V2 - V4, team_2_Tot = V4 - V2)
  
  all_data <- rbind(all_data, df2)
  
}

col1 <- c(all_data$V1,all_data$V3)
col2 <- c(all_data$team_1_Tot,all_data$team_2_Tot)

team_tots <- cbind(col1,col2)
team_tots <- as.tibble(team_tots)
team_tots <- transform(team_tots, col2 = as.numeric(col2))

team_tots2 <- team_tots %>%
  group_by(col1) %>% 
  summarise(total = sum(col2)) %>%
  arrange(desc(total))

  return(team_tots2)

}

#---------------------------------------------------------------------------

scores2022 <- function(x) {
  
  URL <- "https://www.fleaflicker.com/nfl/scores?season=2022&week="
  WEEK <- c(1:x)
  all_data <- c()
  for (i in WEEK) {
    
    combined <- paste(URL,WEEK[i], sep = "")
    score_html <- read_html(combined)
    week_score <- score_html %>%
      html_nodes(".right.text-right span , .left+ td") %>%
      html_text()
    
    df <- matrix(week_score, ncol = 4, byrow = TRUE) %>% as.tibble(df)
    df2 <- transform(df, V2 = as.numeric(V2), V4 = as.numeric(V4))
    df2 <- df2 %>% mutate(team_1_Tot = V2 - V4, team_2_Tot = V4 - V2)
    
    all_data <- rbind(all_data, df2)
    
  }
  
  col1 <- c(all_data$V1,all_data$V3)
  col2 <- c(all_data$team_1_Tot,all_data$team_2_Tot)
  
  team_tots <- cbind(col1,col2)
  team_tots <- as.tibble(team_tots)
  team_tots <- transform(team_tots, col2 = as.numeric(col2))
  
  team_tots2 <- team_tots %>%
    group_by(col1) %>% 
    summarise(total = sum(col2)) %>%
    arrange(desc(total))
  
  return(team_tots2)
  
}

#---------------------------------------------------------------------------

week_scores2021 <- scores2021(18)
week_scores2022 <- scores2022(9)

week_scores3 <- rbind(week_scores2021, week_scores2022) %>%
  group_by(col1) %>%
  summarise(total = sum(total)) %>%
  arrange(desc(total))
