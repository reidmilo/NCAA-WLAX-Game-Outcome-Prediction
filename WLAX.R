library(dplyr)
library(tidyr)
setwd('/Users/oliverreidmiller/Desktop/Applied Econ Data/Project/Lax Pro Data/Womens Data ')
gamesdf <- read.csv('Games_D1.csv')
teamsdf <- read.csv('Teams_D1.csv')
shotsdf <- read.csv('Shots_D1.csv')
goalsdf <- read.csv('Goals_D1.csv')
faceOff <- read.csv('Faceoffs_D1.csv')
players <-read.csv('Players_D1.csv')
possessions <- read.csv('Possessions_D1.csv')


gamesdf2 <- read.csv('Games_D2.csv')
teamsdf2 <- read.csv('Teams_D2.csv')
shotsdf2 <- read.csv('Shots_D2.csv')
goalsdf2 <- read.csv('Goals_D2.csv')
faceOff2 <- read.csv('Faceoffs_D2.csv')
players2 <-read.csv('Players_D2.csv')
possessions2 <- read.csv('Possessions_D2.csv')

gamesdf3 <- read.csv('Games_D3.csv')
teamsdf3 <- read.csv('Teams_D3.csv')
shotsdf3 <- read.csv('Shots_D3.csv')
goalsdf3 <- read.csv('Goals_D3.csv')
faceOff3 <- read.csv('Faceoffs_D3.csv')
players3 <-read.csv('Players_D3.csv')
possessions3 <- read.csv('Possessions_D3.csv')

addDivision <- function(df, division){
  df$Division <- division
  return(df)
}

gamesdf <- addDivision(gamesdf,1)
teamsdf<- addDivision(teamsdf,1)
shotsdf<- addDivision(shotsdf,1)
goalsdf<- addDivision(goalsdf,1)
faceOff<- addDivision(faceOff,1)
players<- addDivision(players,1)
possessions<- addDivision(possessions,1)

gamesdf2 <- addDivision(gamesdf2,2)
teamsdf2<- addDivision(teamsdf2,2)
shotsdf2<- addDivision(shotsdf2,2)
goalsdf2<- addDivision(goalsdf2,2)
faceOff2<- addDivision(faceOff2,2)
players2<- addDivision(players2,2)
possessions2<- addDivision(possessions2,2)

gamesdf3 <- addDivision(gamesdf3,3)
teamsdf3<- addDivision(teamsdf3,3)
shotsdf3<- addDivision(shotsdf3,3)
goalsdf3<- addDivision(goalsdf3,3)
faceOff3<- addDivision(faceOff3,3)
players3<- addDivision(players3,3)
possessions3<- addDivision(possessions3,3)

gamesdf<- rbind(gamesdf,gamesdf2,gamesdf3)
teamsdf<- rbind(teamsdf,teamsdf2,teamsdf3)
write.csv(teamsdf, 'allTeams.csv')
shotsdf <- rbind(shotsdf,shotsdf2,shotsdf3)
goalsdf <- rbind(goalsdf,goalsdf2,goalsdf3)
faceOff <- rbind (faceOff,faceOff2,faceOff3)
players <- rbind (players, players2, players3)
possessions <- rbind(possessions, possessions2, possessions3)



# AGGREGATE SHOTS AND GOALS --> GAME & TEAM

shots_game_data <- shotsdf %>%
  group_by(game_ID, team_ID) %>%
  summarize(total_goals = sum(goal),
            total_shots = n())  # Count the number of rows for total shots


# -FACEOFFS ** AGGREGATING AND AVERAGE **
getFaceOffWinPct<- function(faceOff){
  faceOff <- faceOff %>% select(game_ID,winner_ID,team_ID,opp_team_ID)
  faceOff$winner_Team_ID <- NA
  # AGGREGATE FACEOFFS
  
  #ADD TEAM ID FOR FACE OFF WINNER
  for (i in 1:nrow(faceOff)) {
    winnerID <- faceOff[i, 2]
    playerRow <- which(players$ID == winnerID)
    
    # Check if winnerID exists in players data frame
    if (length(playerRow) > 0) {
      faceOff[i, 5] <- players[playerRow, 3]
    } 
    else {
      faceOff[i, 5] <- NA  
    }
    
  }
  na.omit(faceOff$winner_Team_ID)
  
  #ADD GET WIN PCT
  
  #Group by Total Per Team
  faceOffs_game_data <- faceOff %>%
    group_by(game_ID, winner_Team_ID) %>%
    summarize(totalFaceOffs = n())
  
  # Group by total per Game
  game_totals <- faceOffs_game_data %>%
    group_by(game_ID) %>%
    summarize(gameFaceOffs = sum(totalFaceOffs))
  
  # Merge dataframes to have total per team and total per game
  faceOffs_game_data <- left_join(faceOffs_game_data, game_totals, by = "game_ID")
  
  # Divide totalFaceOffs by gameFaceOffs to get PCT
  faceOffs_game_data$faceOff_Win_Pct <- faceOffs_game_data$totalFaceOffs / faceOffs_game_data$gameFaceOffs
  
  return(faceOffs_game_data)
}

faceOff_Pct<- getFaceOffWinPct(faceOff)

#POSSESIONS  AGGREGATION
possessions_agg <- possessions %>%
  group_by(game_ID, team_ID) %>%
  summarise(
    num_shots = sum(num_shots),
    duration = sum(duration),
    num_possessions = n())

# GET GAME INFO -- DATE, SCORES, IDS
gamesdf1 <- gamesdf %>% select('ID','game_date','home_ID','away_ID','home_score','away_score','Division')

homeTeams <- gamesdf1 %>%
  select(ID, game_date,home_ID, home_score) %>%
  rename(team_ID = home_ID, score = home_score)

awayTeams <- gamesdf1 %>%
  select(ID, game_date,away_ID, away_score) %>%
  rename(team_ID = away_ID, score = away_score)

gamesdf2<- rbind(homeTeams, awayTeams)


#Merged Shots
merged_df <- merge(gamesdf2, shots_game_data, by.x = c("ID", "team_ID"), by.y = c("game_ID", "team_ID"))

#Merge Faceoffs
merged_df <- merge(merged_df,faceOff_Pct,by.x = c("ID", "team_ID"),by.y = c("game_ID", "winner_Team_ID"))

merged_df <- merge(merged_df,possessions_agg,by.x = c("ID", "team_ID"),by.y = c("game_ID", "team_ID"))


#Merged

merged_df$game_date <- as.Date(merged_df$game_date)

merged_df <- merged_df %>% arrange(game_date)

#ADD OFF EFF
merged_df$off_eff <- merged_df$total_goals/merged_df$num_possessions


getAvgShots <- function(merged_df,avgShotsCol,avgGoalsCol){
  
  merged_df$avgShots <- NA
  merged_df$avgGoals <- NA
  
  avgShots_Col <- which(names(merged_df) == "avgShots")
  avgGoals_Col <- which(names(merged_df) == "avgGoals")
  
  
  
  for (i in 1:nrow(merged_df)){
    
    team <- merged_df[i, 2]
    date <- merged_df[i, 3]
    
    teamdf <- merged_df %>% filter(team_ID == team & game_date < date)
    if (nrow(teamdf)>0){ 
      totalGames <- nrow(teamdf)
      totalshots <- sum(teamdf$total_shots)
      totalGoals <- sum(teamdf$total_goals)
      
      #column 7 = avgShots
      merged_df[i, avgShots_Col] <- totalshots / totalGames
      #column 8 = avgGoals
      merged_df[i, avgGoals_Col] <- totalGoals / totalGames
    }
    else
    {
      merged_df[i, avgShots_Col] <- NA
      merged_df[i, avgGoals_Col] <-NA
    }
  }
  return(merged_df)
}


addOppShots <- function(merged_df){
  
  merged_df$opp_avg_shots <-NA
  merged_df$opp_avg_goals <-NA
  
  avgShots_opp_Col <- which(names(merged_df) == "opp_avg_shots")
  avgGoals_opp_col <- which(names(merged_df) == "opp_avg_goals")
  
  
  for (i in 1:nrow(merged_df)){
    print (i)
    game = merged_df[i,1]
    team = merged_df[i,2]
    
    oppRow <- which(merged_df$team_ID != team & merged_df$ID == game)
    
    #column 9 =opp avg shots
    merged_df[i, avgShots_opp_Col] <- merged_df[oppRow,7]
    #column 10 = opp avg goals
    merged_df[i, avgGoals_opp_col] <- merged_df[oppRow,7]
  }
  return(merged_df)
}

addWin <- function(merged_df){
  merged_df$Win <- NA
  
  winCol <- which(names(merged_df) == "Win")
  
  for (i in 1:nrow(merged_df)){
    
    game <- merged_df[i, "ID"]
    team <- merged_df[i, "team_ID"]
    
    oppRow <- which(merged_df$team_ID != team & merged_df$ID == game)
    print(paste("game:", game, "team:", team, "oppRow:", oppRow))
    
    if (length(oppRow) == 0) {
      print("Opponent row not found!")
      next
    }
    
    score <- merged_df[i, "score"]
    oppscore <- merged_df[oppRow, "score"]
    
    print(paste("score:", score, "oppscore:", oppscore))
    
    if (score > oppscore) {
      merged_df[i, winCol] <- 1
    } else if (score < oppscore) {
      merged_df[i, winCol] <- 0
    }
    
  }
  return(merged_df)
}


getAvgFaceOff <- function(merged_df){
  merged_df$avgFaceOffPct <- NA
  avgFaceOffPct_col <- which(names(merged_df) == "avgFaceOffPct")
  
  for (i in 1:nrow(merged_df)){
    
    team <- merged_df[i, 2]
    date <- merged_df[i, 3]
    
    teamdf <- merged_df %>% filter(team_ID == team & game_date < date)
    if (nrow(teamdf)>0){ 
      totalGames <- nrow(teamdf)
      totalFaceOffs <- sum(teamdf$faceOff_Win_Pct)
      
      #column 7 = avgShots
      merged_df[i, avgFaceOffPct_col] <- totalFaceOffs / totalGames
    }
    else
    {
      merged_df[i, avgFaceOffPct_col] <- NA
    }
  }
  return(merged_df)
}

getAvgShotPct <- function(df){
  df$Avg_shot_Pct <- df$avgGoals/df$avgShots
  
  return(df)
}

getRecord <- function(teamID, Date) {
  
  
  teamGames <- gamesdf %>% filter((home_ID == teamID | away_ID == teamID) & game_date < Date)
  
  if (nrow(teamGames) > 0) {
    
    teamGames$Win <- NA
    
    teamGames$Win[teamGames$home_ID == teamID & teamGames$home_score > teamGames$away_score] <- 1  # Home win
    teamGames$Win[teamGames$home_ID == teamID & teamGames$home_score <= teamGames$away_score] <- 0  # Home loss
    
    teamGames$Win[teamGames$away_ID == teamID & teamGames$away_score > teamGames$home_score] <- 1  # Away win
    teamGames$Win[teamGames$away_ID == teamID & teamGames$away_score <= teamGames$home_score] <- 0  # Away loss
    
    wins <- sum(teamGames$Win, na.rm = TRUE)
    games <- nrow(teamGames)
    
    winPct <- wins / games
    return(winPct)
  }
  else {
    return(NA)
  }
}

addRecord <- function (df){
  
  df$record <- NA
  recordCol <- which(names(df) == "record")
  
  for (i in 1:nrow(df)){
    team <- df[i,2]
    date <- df[i,3]
    df[i,recordCol]<- as.numeric(getRecord(team,date))
  }
  
  return(df)
}

getDefEff <- function(df){
  df$def_eff <- NA
  
  offEffCol <- which(names(df) == "off_eff")
  def_eff_col <- which(names(df) == "def_eff")
  
  for (i in 1:nrow(df)){
    
    gameId <- df[i, "ID"]
    teamid <- df[i, "team_ID"]
    
    otherTeamRow <- which(df$ID == gameId & df$team_ID != teamid)
    
    print(paste("gameId:", gameId, "teamid:", teamid, "otherTeamRow:", otherTeamRow))
    
    if (length(otherTeamRow) == 0) {
      print("Other team row not found!")
      next
    }
    
    oppOffEff <- df[otherTeamRow, offEffCol]
    
    print(paste("oppOffEff:", oppOffEff))
    
    df[i, def_eff_col] <- oppOffEff
  }
  
  return(df)
}

 
getGamesPlayed <- function(df){
  df$gamesPlayed <- NA
  gamesPlayedCol <- which(names(df) == "gamesPlayed")
  
  for (i in 1:nrow(df)){
    
    Date <- df [i,3]
    teamID <- df[i,2]
    #find all games the team played in the season 
    teamGames <- gamesdf %>% filter((home_ID == teamID | away_ID == teamID) & game_date < Date)
    
    df[i,gamesPlayedCol] <- nrow(teamGames)
  }
   return(df)
}



df<-getAvgShots(merged_df)
#df <-addOppShots(df)
df <- addWin(df)
df<-getAvgFaceOff(df)
df<- getAvgShotPct(df)

df<- addRecord(df)
df <- getDefEff (df)

df <- getGamesPlayed(df)
df1 <- na.omit(df)


write.csv(df1,'All_divisions_df.csv')

df <- read.csv('All_divisions_df.csv')
df <- df[-c(1)]
df$game_date <- as.Date(df$game_date)

getAvgOff_def_eff <- function(df){
  
  df$avg_Off_Eff <- NA
  df$avg_Def_Eff <- NA
  
  avg_off_eff_col <- which(names(df) == "avg_Off_Eff")
  avg_def_eff_col <- which(names(df) == "avg_Def_Eff")
  
  
  for (i in 1:nrow(df)){
    
    team <- df[i, 2]
    date <- df[i, 3]
    
    teamdf <- df %>% filter(team_ID == team & game_date < date)
    
    if (nrow(teamdf)>0){ 
      
      totalGames <- nrow(teamdf)
      total_Off_Eff <- sum(teamdf$off_eff)
      total_Def_eff <- sum(teamdf$def_eff)
      
      df[i, avg_off_eff_col] <- total_Off_Eff / totalGames
      df[i,avg_def_eff_col] <- total_Def_eff/totalGames
    }
    else
    {
      df[i, avg_off_eff_col] <- NA
      df[i,avg_def_eff_col] <- NA
      
    }
  }
  return(df)
}

df2 <-getAvgOff_def_eff(df)
df2 <- na.omit(df2)

write.csv(df2, 'WLAXD1-3.csv')
