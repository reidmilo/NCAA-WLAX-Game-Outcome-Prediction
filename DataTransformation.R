#Logistic Model 
setwd('/Users/oliverreidmiller/Desktop/Applied Econ Data/Project/Lax Pro Data/Womens Data ')
library(dplyr)

df <- read.csv('WLAXD1-3.csv')
teams <- read.csv('allTeams.csv')

#addDivision <- function(df, teams){
#  df$Division <- NA
  
#  for (i in 1:nrow(df)){
    
#    team <- df[i, "team_ID"]  # Assuming "Team" is the column name in df containing team names
#    division <- teams$Division[teams$ID == team]  # Assuming "Team" and "Division" are columns in the teams dataframe
#    if (length(division) > 0) {
#      df[i, "Division"] <- division
#    } else {
#      df[i, "Division"] <- NA
 #   }
 # }
  
#  return(df)
#}

#df <- addDivision(df,teams)
df <- df %>% arrange(ID)

df <- df[-1,-1]

df <- df[, !(names(df) %in% c('total_goals', 'total_shots', 'faceOff_Win_Pct', 'duration', 'num_shots', 'num_possessions', 'off_eff', 'def_eff',
                              'totalFaceOffs','gameFaceOffs'))]

opp_col_names <- c("opp_ID", "opp_team_ID", "opp_game_date", "opp_score", "opp_avgShots", "opp_avgGoals", "opp_Win", "opp_avgFaceOffPct", 
                   "opp_Avg_shot_Pct", "opp_record", 'opp_games_played',"opp_avg_Off_Eff", "opp_avg_Def_Eff",'opp_Home')

df$Home <- NA

for (i in 1:nrow(df)){
  game_id <- df[i, "ID"]  
  
  game_df <- df %>% filter(ID == game_id)
  
  if (nrow(game_df) < 2) {
    df[i, "Home"] <- NA  
  }
  else if (is.na(df[i, "Home"])) {
    df[i, "Home"] <- 1
    df[i+1, "Home"] <- 0
  }
}
#library(jtools)

# Filter data for home and away teams
df_home <- df %>% filter(Home == 1)
df_opp <- df %>% filter(Home == 0)

# Rename columns of df_opp
names(df_opp) <- opp_col_names

# Merge the data frames based on respective IDs
final_df <- merge(df_home, df_opp, by.x = 'ID', by.y = 'opp_ID')
#final_df$DivisionDiff <- final_df$Division-final_df$opp_Division

final_df <- final_df[-c(1:4,14:17,20,27)]

final_df <- na.omit(final_df)

write.csv(final_df, 'finalWLAXdf.csv')

sample <- sample(c(TRUE, FALSE), nrow(final_df), replace=TRUE, prob=c(0.75,0.25))
train  <- final_df[sample, ]
test   <- final_df[!sample, ]

logitModel <-glm(Win ~.,family=binomial(link='logit'),data=train)
summ(logitModel)
predict_logit <- predict(logitModel, newdata = test,type = "response")

# Get predictive accuracy
predicted_probs <- predict(logitModel, newdata = test, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, TRUE, FALSE)  # Convert probabilities to classes

actual_classes <- test$Win  # Actual classes from the test dataset

# Calculate accuracy
accuracy <- mean(predicted_classes == actual_classes)

library(corrplot)

correlation_matrix <- cor(final_df)

# Plot correlation matrix
corrplot(correlation_matrix, method = "circle", type = "upper", tl.col = "black")




