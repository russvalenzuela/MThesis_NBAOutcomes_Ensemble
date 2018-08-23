rm(list=ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(zoo)

#Import Raw Datasets
X2008_2009_NBA_Box_Score_Team_Stats <- read.csv("X2008_2009_NBA_Box_Score_Team_Stats.csv",stringsAsFactors = FALSE)
X2006_to_2017_NBA_Historical_Schedule <- read.csv("X2006_to_2017_NBA_Historical_Schedule.csv",stringsAsFactors = FALSE)

#Find and Drop Unneeded Columns
unneed_cols <- grep('STARTING LINEUPS|X__|MAIN.REFEREE|CREW.REFEREES|OPENING.ODDS|TOTAL|MOVEMENTS|CLOSING|MONEYLINE|HALFTIME|BOX.SCORE|ODDS',
                    colnames(X2008_2009_NBA_Box_Score_Team_Stats),value=TRUE)
X2008_2009_NBA_Box_Score_Team_Stats_dropped <- select(X2008_2009_NBA_Box_Score_Team_Stats,-one_of(unneed_cols))
X2006_to_2017_NBA_Historical_Schedule_dropped <- select(X2006_to_2017_NBA_Historical_Schedule,-`BOX.SCORE.URL`)

#Filter 2008-2009 Regular Season
X2008_2009_NBA_Box_Score_Team_Stats_season <- filter(X2008_2009_NBA_Box_Score_Team_Stats_dropped,DATASET=="2008-2009 Regular Season")
X2008_2009_season_games <- filter(X2006_to_2017_NBA_Historical_Schedule_dropped,DATASET=="2008-2009 Regular Season")

#Margin of Victory For HOME.TEAMs Using Season Games File
X2008_2009_season_games$`HOME MOV` <- X2008_2009_season_games$`HOME.TEAM.FINAL.SCORE` - X2008_2009_season_games$`ROAD.TEAM.FINAL.SCORE`

#Add WIN Markers
X2008_2009_season_games$HOME_WIN <- X2008_2009_season_games$`HOME MOV` > 0
X2008_2009_season_games$ROAD_WIN <- !X2008_2009_season_games$`HOME_WIN`

#Overall Cummalative In-Game Stats
X2008_2009_NBA_Box_Score_Team_Stats_season<-arrange(X2008_2009_NBA_Box_Score_Team_Stats_season,TEAMS,DATE)

X2008_2009_NBA_Box_Score_Team_Stats_season$'cum_FG_overall' <-ave(X2008_2009_NBA_Box_Score_Team_Stats_season$FG,
                                                                  X2008_2009_NBA_Box_Score_Team_Stats_season$TEAMS,
                                                                  FUN=cumsum)
X2008_2009_NBA_Box_Score_Team_Stats_season$'cum_FGA_overall' <-ave(X2008_2009_NBA_Box_Score_Team_Stats_season$FGA,
                                                                   X2008_2009_NBA_Box_Score_Team_Stats_season$TEAMS,
                                                                   FUN=cumsum)
X2008_2009_NBA_Box_Score_Team_Stats_season$'cum_3P_overall' <-ave(X2008_2009_NBA_Box_Score_Team_Stats_season$'X3P',
                                                                  X2008_2009_NBA_Box_Score_Team_Stats_season$TEAMS,
                                                                  FUN=cumsum)
X2008_2009_NBA_Box_Score_Team_Stats_season$'cum_3PA_overall' <-ave(X2008_2009_NBA_Box_Score_Team_Stats_season$'X3PA',
                                                                   X2008_2009_NBA_Box_Score_Team_Stats_season$TEAMS,
                                                                   FUN=cumsum)
X2008_2009_NBA_Box_Score_Team_Stats_season$'cum_FT_overall' <-ave(X2008_2009_NBA_Box_Score_Team_Stats_season$FT,
                                                                  X2008_2009_NBA_Box_Score_Team_Stats_season$TEAMS,
                                                                  FUN=cumsum)
X2008_2009_NBA_Box_Score_Team_Stats_season$'cum_FTA_overall' <-ave(X2008_2009_NBA_Box_Score_Team_Stats_season$FTA,
                                                                   X2008_2009_NBA_Box_Score_Team_Stats_season$TEAMS,
                                                                   FUN=cumsum)
X2008_2009_NBA_Box_Score_Team_Stats_season$'cum_OR_overall' <-ave(X2008_2009_NBA_Box_Score_Team_Stats_season$OR,
                                                                  X2008_2009_NBA_Box_Score_Team_Stats_season$TEAMS,
                                                                  FUN=cumsum)
X2008_2009_NBA_Box_Score_Team_Stats_season$'cum_DR_overall' <-ave(X2008_2009_NBA_Box_Score_Team_Stats_season$DR,
                                                                  X2008_2009_NBA_Box_Score_Team_Stats_season$TEAMS,
                                                                  FUN=cumsum)
X2008_2009_NBA_Box_Score_Team_Stats_season$'cum_TOT_overall' <-ave(X2008_2009_NBA_Box_Score_Team_Stats_season$TOT,
                                                                   X2008_2009_NBA_Box_Score_Team_Stats_season$TEAMS,
                                                                   FUN=cumsum)
X2008_2009_NBA_Box_Score_Team_Stats_season$'cum_A_overall' <-ave(X2008_2009_NBA_Box_Score_Team_Stats_season$A,
                                                                 X2008_2009_NBA_Box_Score_Team_Stats_season$TEAMS,
                                                                 FUN=cumsum)
X2008_2009_NBA_Box_Score_Team_Stats_season$'cum_PF_overall' <-ave(X2008_2009_NBA_Box_Score_Team_Stats_season$PF,
                                                                  X2008_2009_NBA_Box_Score_Team_Stats_season$TEAMS,
                                                                  FUN=cumsum)
X2008_2009_NBA_Box_Score_Team_Stats_season$'cum_ST_overall' <-ave(X2008_2009_NBA_Box_Score_Team_Stats_season$ST,
                                                                  X2008_2009_NBA_Box_Score_Team_Stats_season$TEAMS,
                                                                  FUN=cumsum)
X2008_2009_NBA_Box_Score_Team_Stats_season$'cum_TO_overall' <-ave(X2008_2009_NBA_Box_Score_Team_Stats_season$TO,
                                                                  X2008_2009_NBA_Box_Score_Team_Stats_season$TEAMS,
                                                                  FUN=cumsum)
X2008_2009_NBA_Box_Score_Team_Stats_season$'cum_BL_overall' <-ave(X2008_2009_NBA_Box_Score_Team_Stats_season$BL,
                                                                  X2008_2009_NBA_Box_Score_Team_Stats_season$TEAMS,
                                                                  FUN=cumsum)
X2008_2009_NBA_Box_Score_Team_Stats_season$'cum_PTS_overall' <-ave(X2008_2009_NBA_Box_Score_Team_Stats_season$PTS,
                                                                   X2008_2009_NBA_Box_Score_Team_Stats_season$TEAMS,
                                                                   FUN=cumsum)


#Split into Home and Road
X2008_2009_NBA_Box_Score_Team_Stats_season_HOME <- X2008_2009_NBA_Box_Score_Team_Stats_season[which(X2008_2009_NBA_Box_Score_Team_Stats_season$VENUE=='Home'),]
X2008_2009_NBA_Box_Score_Team_Stats_season_ROAD <- X2008_2009_NBA_Box_Score_Team_Stats_season[which(X2008_2009_NBA_Box_Score_Team_Stats_season$VENUE=='Road'),]

#Add GAME.ID to HOME and ROAD Box Scores
#Sort Box Scores by Team and Date
X2008_2009_NBA_Box_Score_Team_Stats_season_HOME<-arrange(X2008_2009_NBA_Box_Score_Team_Stats_season_HOME,TEAMS,DATE)
X2008_2009_NBA_Box_Score_Team_Stats_season_ROAD<-arrange(X2008_2009_NBA_Box_Score_Team_Stats_season_ROAD,TEAMS,DATE)

#Sort Schedule by ROAD.TEAM and Date, Add "GAME.ID" to Road Box Scores
X2008_2009_season_games<-arrange(X2008_2009_season_games,`ROAD.TEAM`,DATE)
X2008_2009_NBA_Box_Score_Team_Stats_season_ROAD$`GAME.ID` <- X2008_2009_season_games$`GAME.ID`

#Sort Schedule by HOME.TEAM and Date, Add "GAME.ID" to Home Box Scores
X2008_2009_season_games<-arrange(X2008_2009_season_games,`HOME.TEAM`,DATE)
X2008_2009_NBA_Box_Score_Team_Stats_season_HOME$`GAME.ID` <- X2008_2009_season_games$`GAME.ID`

#Prepare For Merge
#Drop DATASET and DATE from Box Scores (Merging on GAME.ID)
X2008_2009_NBA_Box_Score_Team_Stats_season_HOME<-select(X2008_2009_NBA_Box_Score_Team_Stats_season_HOME,
                                                        -DATASET,-DATE)
X2008_2009_NBA_Box_Score_Team_Stats_season_ROAD<-select(X2008_2009_NBA_Box_Score_Team_Stats_season_ROAD,
                                                        -DATASET,-DATE)

#Add HOME, ROAD Markets
colnames(X2008_2009_NBA_Box_Score_Team_Stats_season_HOME)<-paste(colnames(X2008_2009_NBA_Box_Score_Team_Stats_season_HOME),"HOME",sep="_")
colnames(X2008_2009_NBA_Box_Score_Team_Stats_season_ROAD)<-paste(colnames(X2008_2009_NBA_Box_Score_Team_Stats_season_ROAD),"ROAD",sep="_")
X2008_2009_NBA_Box_Score_Team_Stats_season_HOME<-rename(X2008_2009_NBA_Box_Score_Team_Stats_season_HOME,'GAME.ID'='GAME.ID_HOME')
X2008_2009_NBA_Box_Score_Team_Stats_season_ROAD<-rename(X2008_2009_NBA_Box_Score_Team_Stats_season_ROAD,'GAME.ID'='GAME.ID_ROAD')

#Merge
X2008_2009_season_games<-merge(X2008_2009_season_games,X2008_2009_NBA_Box_Score_Team_Stats_season_HOME,by="GAME.ID")
X2008_2009_season_games<-merge(X2008_2009_season_games,X2008_2009_NBA_Box_Score_Team_Stats_season_ROAD,by="GAME.ID")


#Get Running Totals
#Home/Road Wins-Losses 
X2008_2009_season_games<-arrange(X2008_2009_season_games,`HOME.TEAM`,`GAME.ID`)
X2008_2009_season_games$'HOME cWINS'<-ave(X2008_2009_season_games$`HOME_WIN`,
                                          X2008_2009_season_games$`HOME.TEAM`,
                                          FUN=cumsum)
X2008_2009_season_games$'HOME cLOSSES'<-ave(!X2008_2009_season_games$`HOME_WIN`,
                                            X2008_2009_season_games$`HOME.TEAM`,
                                            FUN=cumsum)

X2008_2009_season_games<-arrange(X2008_2009_season_games,`ROAD.TEAM`,`GAME.ID`)
X2008_2009_season_games$'ROAD cWINS'<-ave(X2008_2009_season_games$`ROAD_WIN`,
                                          X2008_2009_season_games$`ROAD.TEAM`,
                                          FUN=cumsum)
X2008_2009_season_games$'ROAD cLOSSES'<-ave(!X2008_2009_season_games$`ROAD_WIN`,
                                            X2008_2009_season_games$`ROAD.TEAM`,
                                            FUN=cumsum)

#Home In-Game Stats
X2008_2009_season_games<-arrange(X2008_2009_season_games,`HOME.TEAM`,`GAME.ID`)


X2008_2009_season_games$cum_FG_HOME <- ave(X2008_2009_season_games$FG_HOME,
                                           X2008_2009_season_games$'HOME.TEAM',
                                           FUN=cumsum)
X2008_2009_season_games$cum_FGA_HOME <- ave(X2008_2009_season_games$FGA_HOME,
                                            X2008_2009_season_games$'HOME.TEAM',
                                            FUN=cumsum)
X2008_2009_season_games$cum_3P_HOME <-ave(X2008_2009_season_games$'X3P_HOME',
                                          X2008_2009_season_games$'HOME.TEAM',
                                          FUN=cumsum)
X2008_2009_season_games$cum_3PA_HOME <-ave(X2008_2009_season_games$'X3PA_HOME',
                                           X2008_2009_season_games$'HOME.TEAM',
                                           FUN=cumsum)
X2008_2009_season_games$cum_FT_HOME <- ave(X2008_2009_season_games$FT_HOME,
                                           X2008_2009_season_games$'HOME.TEAM',
                                           FUN=cumsum)
X2008_2009_season_games$cum_FTA_HOME <- ave(X2008_2009_season_games$FTA_HOME,
                                            X2008_2009_season_games$'HOME.TEAM',
                                            FUN=cumsum)
X2008_2009_season_games$cum_OR_HOME <-ave(X2008_2009_season_games$OR_HOME,
                                          X2008_2009_season_games$'HOME.TEAM',
                                          FUN=cumsum)
X2008_2009_season_games$cum_DR_HOME <-ave(X2008_2009_season_games$DR_HOME,
                                          X2008_2009_season_games$'HOME.TEAM',
                                          FUN=cumsum)
X2008_2009_season_games$cum_TOT_HOME <- ave(X2008_2009_season_games$TOT_HOME,
                                            X2008_2009_season_games$'HOME.TEAM',
                                            FUN=cumsum)
X2008_2009_season_games$cum_A_HOME <- ave(X2008_2009_season_games$A_HOME,
                                          X2008_2009_season_games$'HOME.TEAM',
                                          FUN=cumsum)
X2008_2009_season_games$cum_PF_HOME <-ave(X2008_2009_season_games$PF_HOME,
                                          X2008_2009_season_games$'HOME.TEAM',
                                          FUN=cumsum)
X2008_2009_season_games$cum_ST_HOME <-ave(X2008_2009_season_games$ST_HOME,
                                          X2008_2009_season_games$'HOME.TEAM',
                                          FUN=cumsum)
X2008_2009_season_games$cum_TO_HOME <-ave(X2008_2009_season_games$TO_HOME,
                                          X2008_2009_season_games$'HOME.TEAM',
                                          FUN=cumsum)
X2008_2009_season_games$cum_BL_HOME <-ave(X2008_2009_season_games$BL_HOME,
                                          X2008_2009_season_games$'HOME.TEAM',
                                          FUN=cumsum)
X2008_2009_season_games$cum_PTS_HOME <-ave(X2008_2009_season_games$PTS_HOME,
                                           X2008_2009_season_games$'HOME.TEAM',
                                           FUN=cumsum)

#Road In-Game Stats
X2008_2009_season_games<-arrange(X2008_2009_season_games,`ROAD.TEAM`,`GAME.ID`)


X2008_2009_season_games$cum_FG_ROAD <- ave(X2008_2009_season_games$FG_ROAD,
                                           X2008_2009_season_games$'ROAD.TEAM',
                                           FUN=cumsum)
X2008_2009_season_games$cum_FGA_ROAD <- ave(X2008_2009_season_games$FGA_ROAD,
                                            X2008_2009_season_games$'ROAD.TEAM',
                                            FUN=cumsum)
X2008_2009_season_games$cum_3P_ROAD <-ave(X2008_2009_season_games$'X3P_ROAD',
                                          X2008_2009_season_games$'ROAD.TEAM',
                                          FUN=cumsum)
X2008_2009_season_games$cum_3PA_ROAD <-ave(X2008_2009_season_games$'X3PA_ROAD',
                                           X2008_2009_season_games$'ROAD.TEAM',
                                           FUN=cumsum)
X2008_2009_season_games$cum_FT_ROAD <- ave(X2008_2009_season_games$FT_ROAD,
                                           X2008_2009_season_games$'ROAD.TEAM',
                                           FUN=cumsum)
X2008_2009_season_games$cum_FTA_ROAD <- ave(X2008_2009_season_games$FTA_ROAD,
                                            X2008_2009_season_games$'ROAD.TEAM',
                                            FUN=cumsum)
X2008_2009_season_games$cum_OR_ROAD <-ave(X2008_2009_season_games$OR_ROAD,
                                          X2008_2009_season_games$'ROAD.TEAM',
                                          FUN=cumsum)
X2008_2009_season_games$cum_DR_ROAD <-ave(X2008_2009_season_games$DR_ROAD,
                                          X2008_2009_season_games$'ROAD.TEAM',
                                          FUN=cumsum)
X2008_2009_season_games$cum_TOT_ROAD <- ave(X2008_2009_season_games$TOT_ROAD,
                                            X2008_2009_season_games$'ROAD.TEAM',
                                            FUN=cumsum)
X2008_2009_season_games$cum_A_ROAD <- ave(X2008_2009_season_games$A_ROAD,
                                          X2008_2009_season_games$'ROAD.TEAM',
                                          FUN=cumsum)
X2008_2009_season_games$cum_PF_ROAD <-ave(X2008_2009_season_games$PF_ROAD,
                                          X2008_2009_season_games$'ROAD.TEAM',
                                          FUN=cumsum)
X2008_2009_season_games$cum_ST_ROAD <-ave(X2008_2009_season_games$ST_ROAD,
                                          X2008_2009_season_games$'ROAD.TEAM',
                                          FUN=cumsum)
X2008_2009_season_games$cum_TO_ROAD <-ave(X2008_2009_season_games$TO_ROAD,
                                          X2008_2009_season_games$'ROAD.TEAM',
                                          FUN=cumsum)
X2008_2009_season_games$cum_BL_ROAD <-ave(X2008_2009_season_games$BL_ROAD,
                                          X2008_2009_season_games$'ROAD.TEAM',
                                          FUN=cumsum)
X2008_2009_season_games$cum_PTS_ROAD <-ave(X2008_2009_season_games$PTS_ROAD,
                                           X2008_2009_season_games$'ROAD.TEAM',
                                           FUN=cumsum)


#Lagged Running Totals By Team

#ROAD TOTALS

X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_FG_ROAD=lag(cum_FG_ROAD, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_FGA_ROAD=lag(cum_FGA_ROAD, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_3P_ROAD=lag(cum_3P_ROAD, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_3PA_ROAD=lag(cum_3PA_ROAD, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_FT_ROAD=lag(cum_FT_ROAD, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_FTA_ROAD=lag(cum_FTA_ROAD, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_OR_ROAD=lag(cum_OR_ROAD, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_DR_ROAD=lag(cum_DR_ROAD, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_TOT_ROAD=lag(cum_TOT_ROAD, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_A_ROAD=lag(cum_A_ROAD, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_PF_ROAD=lag(cum_PF_ROAD, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_ST_ROAD=lag(cum_ST_ROAD, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_TO_ROAD=lag(cum_TO_ROAD, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_BL_ROAD=lag(cum_BL_ROAD, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_PTS_ROAD=lag(cum_PTS_ROAD, default=0))

#ROAD OVERALL TOTALS

X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_FG_overall_ROAD=lag(cum_FG_overall_ROAD, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_FGA_overall_ROAD=lag(cum_FGA_overall_ROAD, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_3P_overall_ROAD=lag(cum_3P_overall_ROAD, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_3PA_overall_ROAD=lag(cum_3PA_overall_ROAD, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_FT_overall_ROAD=lag(cum_FT_overall_ROAD, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_FTA_overall_ROAD=lag(cum_FTA_overall_ROAD, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_OR_overall_ROAD=lag(cum_OR_overall_ROAD, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_DR_overall_ROAD=lag(cum_DR_overall_ROAD, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_TOT_overall_ROAD=lag(cum_TOT_overall_ROAD, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_A_overall_ROAD=lag(cum_A_overall_ROAD, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_PF_overall_ROAD=lag(cum_PF_overall_ROAD, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_ST_overall_ROAD=lag(cum_ST_overall_ROAD, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_TO_overall_ROAD=lag(cum_TO_overall_ROAD, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_BL_overall_ROAD=lag(cum_BL_overall_ROAD, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(cum_PTS_overall_ROAD=lag(cum_PTS_overall_ROAD, default=0))

#RESET
X2008_2009_season_games<-arrange(X2008_2009_season_games,`GAME.ID`)

#HOME TOTALS

X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_FG_HOME=lag(cum_FG_HOME, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_FGA_HOME=lag(cum_FGA_HOME, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_3P_HOME=lag(cum_3P_HOME, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_3PA_HOME=lag(cum_3PA_HOME, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_FT_HOME=lag(cum_FT_HOME, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_FTA_HOME=lag(cum_FTA_HOME, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_OR_HOME=lag(cum_OR_HOME, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_DR_HOME=lag(cum_DR_HOME, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_TOT_HOME=lag(cum_TOT_HOME, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_A_HOME=lag(cum_A_HOME, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_PF_HOME=lag(cum_PF_HOME, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_ST_HOME=lag(cum_ST_HOME, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_TO_HOME=lag(cum_TO_HOME, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_BL_HOME=lag(cum_BL_HOME, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_PTS_HOME=lag(cum_PTS_HOME, default=0))

#HOME OVERALL TOTALS

X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_FG_overall_HOME=lag(cum_FG_overall_HOME, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_FGA_overall_HOME=lag(cum_FGA_overall_HOME, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_3P_overall_HOME=lag(cum_3P_overall_HOME, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_3PA_overall_HOME=lag(cum_3PA_overall_HOME, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_FT_overall_HOME=lag(cum_FT_overall_HOME, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_FTA_overall_HOME=lag(cum_FTA_overall_HOME, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_OR_overall_HOME=lag(cum_OR_overall_HOME, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_DR_overall_HOME=lag(cum_DR_overall_HOME, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_TOT_overall_HOME=lag(cum_TOT_overall_HOME, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_A_overall_HOME=lag(cum_A_overall_HOME, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_PF_overall_HOME=lag(cum_PF_overall_HOME, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_ST_overall_HOME=lag(cum_ST_overall_HOME, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_TO_overall_HOME=lag(cum_TO_overall_HOME, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_BL_overall_HOME=lag(cum_BL_overall_HOME, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(cum_PTS_overall_HOME=lag(cum_PTS_overall_HOME, default=0))

X2008_2009_season_games<-arrange(X2008_2009_season_games,`GAME.ID`)


#Get Overall Win-Loss Record
#Split and Sort Home/Road Win-Loss Records
wl_h<-select(X2008_2009_season_games,`GAME.ID`,TEAMS_HOME,VENUE_HOME,`HOME cWINS`,`HOME cLOSSES`)
wl_h<-arrange(wl_h,TEAMS_HOME,`GAME.ID`)
wl_r<-select(X2008_2009_season_games,`GAME.ID`,TEAMS_ROAD,VENUE_ROAD,`ROAD cWINS`,`ROAD cLOSSES`)
wl_r<-arrange(wl_r,TEAMS_ROAD,`GAME.ID`)

#Remove Home/Road Markers for TEAM
wl_h<-rename(wl_h,TEAMS=TEAMS_HOME,VENUE=VENUE_HOME)
wl_r<-rename(wl_r,TEAMS=TEAMS_ROAD,VENUE=VENUE_ROAD)

#Vertical Join Records into one df
wl_overall<-bind_rows(wl_h,wl_r)
wl_overall<-arrange(wl_overall,TEAMS,`GAME.ID`)

#Fill NA with previous value by TEAM
wl_overall<-wl_overall %>% group_by(TEAMS) %>% fill(`HOME cWINS`,`HOME cLOSSES`,
                                                    `ROAD cWINS`,`ROAD cLOSSES`)
wl_overall[is.na(wl_overall)] <- 0


#SHIFT Win-Loss Records

wl_overall<-arrange(wl_overall,TEAMS,`GAME.ID`)

wl_overall<- wl_overall %>% group_by(TEAMS) %>% mutate(`ROAD cWINS`=lag(`ROAD cWINS`, default=0))
wl_overall<- wl_overall %>% group_by(TEAMS) %>% mutate(`ROAD cLOSSES`=lag(`ROAD cLOSSES`, default=0))


wl_overall<- wl_overall %>% group_by(TEAMS) %>% mutate(`HOME cWINS`=lag(`HOME cWINS`, default=0))
wl_overall<- wl_overall %>% group_by(TEAMS) %>% mutate(`HOME cLOSSES`=lag(`HOME cLOSSES`, default=0))

#Sum Total Wins-Losses
wl_overall$`OVERALL cWINS`<-wl_overall$`HOME cWINS` + wl_overall$`ROAD cWINS`
wl_overall$`OVERALL cLOSSES`<-wl_overall$`HOME cLOSSES` + wl_overall$`ROAD cLOSSES`

#Re-split Home/Road
wl_h<-wl_overall[which(wl_overall$VENUE=='Home'),]
wl_h<-select(wl_h,`GAME.ID`,TEAMS,`OVERALL cWINS`,`OVERALL cLOSSES`)
wl_h<-rename(wl_h,`HOME OVERALL cWINS`=`OVERALL cWINS`,`HOME OVERALL cLOSSES`=`OVERALL cLOSSES`)
wl_r<-wl_overall[which(wl_overall$VENUE=='Road'),]
wl_r<-select(wl_r,`GAME.ID`,TEAMS,`OVERALL cWINS`,`OVERALL cLOSSES`)
wl_r<-rename(wl_r,`ROAD OVERALL cWINS`=`OVERALL cWINS`,`ROAD OVERALL cLOSSES`=`OVERALL cLOSSES`)

#Add Overall Games Played Column
wl_h$'HOME_OVERALL_GP'<-wl_h$`HOME OVERALL cWINS` + wl_h$`HOME OVERALL cLOSSES`
wl_r$'ROAD_OVERALL_GP'<-wl_r$`ROAD OVERALL cWINS` + wl_r$`ROAD OVERALL cLOSSES`


#Add Overall Win-Loss Records to main df
X2008_2009_season_games<-merge(X2008_2009_season_games,wl_h,by='GAME.ID')
X2008_2009_season_games<-merge(X2008_2009_season_games,wl_r,by='GAME.ID')
X2008_2009_season_games<-select(X2008_2009_season_games,-TEAMS.x,-TEAMS.y)


#Shift Home/Road Win-Loss Records
X2008_2009_season_games<-arrange(X2008_2009_season_games,`ROAD.TEAM`,`GAME.ID`)

X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(`ROAD cWINS`=lag(`ROAD cWINS`, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`ROAD.TEAM`) %>% mutate(`ROAD cLOSSES`=lag(`ROAD cLOSSES`, default=0))

#Get/Add GP on Road
X2008_2009_season_games$'ROAD_GP' <- X2008_2009_season_games$`ROAD cWINS` + X2008_2009_season_games$`ROAD cLOSSES`


#Reset for HOME.TEAMs
X2008_2009_season_games<-arrange(X2008_2009_season_games,`HOME.TEAM`,`GAME.ID`)

X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(`HOME cWINS`=lag(`HOME cWINS`, default=0))
X2008_2009_season_games<- X2008_2009_season_games %>% group_by(`HOME.TEAM`) %>% mutate(`HOME cLOSSES`=lag(`HOME cLOSSES`, default=0))

#Get/Add GP on Home
X2008_2009_season_games$'HOME_GP' <- X2008_2009_season_games$`HOME cWINS` + X2008_2009_season_games$`HOME cLOSSES`

#Reset
X2008_2009_season_games<- arrange(X2008_2009_season_games,`GAME.ID`)


#Calculate Per Game Averages Entering Games

#WinPCTs
X2008_2009_season_games$HOME_WinPCT<- X2008_2009_season_games$`HOME cWINS` / X2008_2009_season_games$HOME_GP
X2008_2009_season_games$HOME_overall_WinPCT<- X2008_2009_season_games$`HOME OVERALL cWINS` / X2008_2009_season_games$HOME_OVERALL_GP

X2008_2009_season_games$ROAD_WinPCT<- X2008_2009_season_games$`ROAD cWINS` / X2008_2009_season_games$ROAD_GP
X2008_2009_season_games$ROAD_overall_WinPCT<- X2008_2009_season_games$`ROAD OVERALL cWINS` / X2008_2009_season_games$ROAD_OVERALL_GP

#FGPCT
X2008_2009_season_games$HOME_FGPCT<- X2008_2009_season_games$cum_FG_HOME / X2008_2009_season_games$cum_FGA_HOME
X2008_2009_season_games$HOME_overall_FGPCT<- X2008_2009_season_games$cum_FG_overall_HOME / X2008_2009_season_games$cum_FGA_overall_HOME

X2008_2009_season_games$ROAD_FGPCT<- X2008_2009_season_games$cum_FG_ROAD / X2008_2009_season_games$cum_FGA_ROAD
X2008_2009_season_games$ROAD_overall_FGPCT<- X2008_2009_season_games$cum_FG_overall_ROAD / X2008_2009_season_games$cum_FGA_overall_ROAD

#3PPCT
X2008_2009_season_games$HOME_3PPCT<- X2008_2009_season_games$cum_3P_HOME / X2008_2009_season_games$cum_3PA_HOME
X2008_2009_season_games$HOME_overall_3PPCT<- X2008_2009_season_games$cum_3P_overall_HOME / X2008_2009_season_games$cum_3PA_overall_HOME

X2008_2009_season_games$ROAD_3PPCT<- X2008_2009_season_games$cum_3P_ROAD / X2008_2009_season_games$cum_3PA_ROAD
X2008_2009_season_games$ROAD_overall_3PPCT<- X2008_2009_season_games$cum_3P_overall_ROAD / X2008_2009_season_games$cum_3PA_overall_ROAD


#FTPCT
X2008_2009_season_games$HOME_FTPCT<- X2008_2009_season_games$cum_FT_HOME / X2008_2009_season_games$cum_FTA_HOME
X2008_2009_season_games$HOME_overall_FTPCT<- X2008_2009_season_games$cum_FT_overall_HOME / X2008_2009_season_games$cum_FTA_overall_HOME

X2008_2009_season_games$ROAD_FTPCT<- X2008_2009_season_games$cum_FT_ROAD / X2008_2009_season_games$cum_FTA_ROAD
X2008_2009_season_games$ROAD_overall_FTPCT<- X2008_2009_season_games$cum_FT_overall_ROAD / X2008_2009_season_games$cum_FTA_overall_ROAD



#REBOUNDS PER GAME
#OREB
X2008_2009_season_games$HOME_ORPG<- X2008_2009_season_games$cum_OR_HOME / X2008_2009_season_games$HOME_GP
X2008_2009_season_games$HOME_overall_ORPG<- X2008_2009_season_games$cum_OR_overall_HOME / X2008_2009_season_games$HOME_OVERALL_GP

X2008_2009_season_games$ROAD_ORPG<- X2008_2009_season_games$cum_OR_ROAD / X2008_2009_season_games$ROAD_GP
X2008_2009_season_games$ROAD_overall_ORPG<- X2008_2009_season_games$cum_OR_overall_ROAD / X2008_2009_season_games$ROAD_OVERALL_GP

#DREB
X2008_2009_season_games$HOME_DRPG<- X2008_2009_season_games$cum_DR_HOME / X2008_2009_season_games$HOME_GP
X2008_2009_season_games$HOME_overall_DRPG<- X2008_2009_season_games$cum_DR_overall_HOME / X2008_2009_season_games$HOME_OVERALL_GP

X2008_2009_season_games$ROAD_DRPG<- X2008_2009_season_games$cum_DR_ROAD / X2008_2009_season_games$ROAD_GP
X2008_2009_season_games$ROAD_overall_DRPG<- X2008_2009_season_games$cum_DR_overall_ROAD / X2008_2009_season_games$ROAD_OVERALL_GP

#TREB
X2008_2009_season_games$HOME_TRPG<- X2008_2009_season_games$cum_TOT_HOME / X2008_2009_season_games$HOME_GP
X2008_2009_season_games$HOME_overall_TRPG<- X2008_2009_season_games$cum_TOT_overall_HOME / X2008_2009_season_games$HOME_OVERALL_GP

X2008_2009_season_games$ROAD_TRPG<- X2008_2009_season_games$cum_TOT_ROAD / X2008_2009_season_games$ROAD_GP
X2008_2009_season_games$ROAD_overall_TRPG<- X2008_2009_season_games$cum_TOT_overall_ROAD / X2008_2009_season_games$ROAD_OVERALL_GP

#ASSISTS PER GAME

X2008_2009_season_games$HOME_APG<- X2008_2009_season_games$cum_A_HOME / X2008_2009_season_games$HOME_GP
X2008_2009_season_games$HOME_overall_APG<- X2008_2009_season_games$cum_A_overall_HOME / X2008_2009_season_games$HOME_OVERALL_GP

X2008_2009_season_games$ROAD_APG<- X2008_2009_season_games$cum_A_ROAD / X2008_2009_season_games$ROAD_GP
X2008_2009_season_games$ROAD_overall_APG<- X2008_2009_season_games$cum_A_overall_ROAD / X2008_2009_season_games$ROAD_OVERALL_GP

#FOULS PER GAME

X2008_2009_season_games$HOME_PFPG<- X2008_2009_season_games$cum_PF_HOME / X2008_2009_season_games$HOME_GP
X2008_2009_season_games$HOME_overall_PFPG<- X2008_2009_season_games$cum_PF_overall_HOME / X2008_2009_season_games$HOME_OVERALL_GP

X2008_2009_season_games$ROAD_PFPG<- X2008_2009_season_games$cum_PF_ROAD / X2008_2009_season_games$ROAD_GP
X2008_2009_season_games$ROAD_overall_PFPG<- X2008_2009_season_games$cum_PF_overall_ROAD / X2008_2009_season_games$ROAD_OVERALL_GP

#STEALS PER GAME

X2008_2009_season_games$HOME_SPG<- X2008_2009_season_games$cum_ST_HOME / X2008_2009_season_games$HOME_GP
X2008_2009_season_games$HOME_overall_SPG<- X2008_2009_season_games$cum_ST_overall_HOME / X2008_2009_season_games$HOME_OVERALL_GP

X2008_2009_season_games$ROAD_SPG<- X2008_2009_season_games$cum_ST_ROAD / X2008_2009_season_games$ROAD_GP
X2008_2009_season_games$ROAD_overall_SPG<- X2008_2009_season_games$cum_ST_overall_ROAD / X2008_2009_season_games$ROAD_OVERALL_GP

#TURNOVERS PER GAME

X2008_2009_season_games$HOME_TOPG<- X2008_2009_season_games$cum_TO_HOME / X2008_2009_season_games$HOME_GP
X2008_2009_season_games$HOME_overall_TOPG<- X2008_2009_season_games$cum_TO_overall_HOME / X2008_2009_season_games$HOME_OVERALL_GP

X2008_2009_season_games$ROAD_TOPG<- X2008_2009_season_games$cum_TO_ROAD / X2008_2009_season_games$ROAD_GP
X2008_2009_season_games$ROAD_overall_TOPG<- X2008_2009_season_games$cum_TO_overall_ROAD / X2008_2009_season_games$ROAD_OVERALL_GP

#BLOCKS PER GAME

X2008_2009_season_games$HOME_BPG<- X2008_2009_season_games$cum_BL_HOME / X2008_2009_season_games$HOME_GP
X2008_2009_season_games$HOME_overall_BPG<- X2008_2009_season_games$cum_BL_overall_HOME / X2008_2009_season_games$HOME_OVERALL_GP

X2008_2009_season_games$ROAD_BPG<- X2008_2009_season_games$cum_BL_ROAD / X2008_2009_season_games$ROAD_GP
X2008_2009_season_games$ROAD_overall_BPG<- X2008_2009_season_games$cum_BL_overall_ROAD / X2008_2009_season_games$ROAD_OVERALL_GP

#POINTS PER GAME

X2008_2009_season_games$HOME_PPG<- X2008_2009_season_games$cum_PTS_HOME / X2008_2009_season_games$HOME_GP
X2008_2009_season_games$HOME_overall_PPG<- X2008_2009_season_games$cum_PTS_overall_HOME / X2008_2009_season_games$HOME_OVERALL_GP

X2008_2009_season_games$ROAD_PPG<- X2008_2009_season_games$cum_PTS_ROAD / X2008_2009_season_games$ROAD_GP
X2008_2009_season_games$ROAD_overall_PPG<- X2008_2009_season_games$cum_PTS_overall_ROAD / X2008_2009_season_games$ROAD_OVERALL_GP

#Ungroup
X2008_2009_season_games <- X2008_2009_season_games %>% ungroup()

#KEEP Selected Columns

X0809_season_OAHR<- select(X2008_2009_season_games, SPREAD_HOME, matches("HOME_|ROAD_"))
X0809_season_OAHR<- select(X0809_season_OAHR, -ROAD_WIN)
X0809_season_OAHR<- select(X0809_season_OAHR, -HOME_GP, -HOME_OVERALL_GP, -ROAD_GP, -ROAD_OVERALL_GP)
X0809_season_OAHR<- X0809_season_OAHR[complete.cases(X0809_season_OAHR),]