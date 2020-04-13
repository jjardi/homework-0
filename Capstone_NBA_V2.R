if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(measurements)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(janitor)) install.packages("janitor", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(factoextra)) install.packages("factoextra", repos = "http://cran.us.r-project.org")

if(!require(Mclust)) install.packages("fMclust", repos = "http://cran.us.r-project.org")
if(!require(fpc)) install.packages("fpc", repos = "http://cran.us.r-project.org")
if(!require(cluster)) install.packages("cluster", repos = "http://cran.us.r-project.org")

library(data.table)
library(lubridate)
library(tidyverse)
library(readr)
library(dplyr)
library(measurements)
library(janitor)  # for data cleaning)
library(rvest)      # for web scraping
library(corrplot)   # correlation plots
library(caret)
library(rpart)
library("factoextra")



set.seed(1)


setwd("C:\\Users\\Utilisateur\\Documents\\projects\\Capstone2-NBa")
#
#Download and extracting the files.
#Dataset from: https://www.kaggle.com/drgilermo/nba-players-stats/version/2
#Hide
#fileURL <- "https://www.kaggle.com/drgilermo/nba-players-stats/downloads/nba-players-stats.zip/2"
#filename <- "NBASeason1950-2017.zip"
#
# Checking if archieve already exists.
#if (!file.exists(filename)){
#  download.file(fileURL, filename, method="curl")
#}  
#
# Checking if folder exists
#if (!file.exists("NBA Season Dataset")) { 
#  unzip(filename)
#}
#




#players <- read.csv("players.csv", header=TRUE)
#player_data <- read.csv("player_data.csv", header=TRUE)
#season <- read.csv("Seasons_Stats.csv", header=TRUE)
#player_of_week<-read.csv("NBA_player_of_the_week.csv", header=TRUE)
#Coach<-read.csv("NBA_head_coaches.csv", header=TRUE)

# -----------------------------------------------------------------------------------
#
# The below function will extract players data from basketball reference.com by season
# -----------------------------------------------------------------------------------
#
scrape_stats <- function(season){
  #total stats
  #scrape
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_",season,"_totals.html")
  stats_tot <- url %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]]
  #
  #stats_tot<-stats_tot%>%mutate(Y=season)
  #
  #cleaninig
  #
  player_stats_tot <- stats_tot %>% 
    remove_empty("cols") %>%
    clean_names() %>% 
    dplyr::filter(!player=="Player") %>%
    mutate_at(vars(-c(player,tm,pos)),as.numeric) %>% 
    mutate_at(vars(-c(player,tm,pos)), funs(replace(., is.na(.), 0))) %>% 
    as_tibble() %>% 
    group_by(player) %>% 
    slice(1) %>% 
    ungroup() %>% 
    select(-rk)
  
  #per minute
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_",season,"_per_minute.html")
  stats_pm <- url %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]]
  
  
  #stats_pm<-stats_pm%>%mutate(Y=season)
  
  player_stats_pm <- stats_pm %>% 
    remove_empty("cols") %>%
    clean_names() %>% 
    dplyr::filter(!player=="Player") %>%
    mutate_at(vars(-c(player,tm,pos)),as.numeric) %>% 
    mutate_at(vars(-c(player,tm,pos)), funs(replace(., is.na(.), 0))) %>% 
    as_tibble() %>% 
    group_by(player) %>% 
    slice(1) %>% 
    ungroup() %>% 
    rename_at(vars(9:29),funs(paste0(.,"_pm"))) %>% 
    select(-rk)
  
  #advanced
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_",season,"_advanced.html")
  stats_adv <- url %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]]
  
  #stats_adv<-stats_adv%>%mutate(Y=season)
  
  player_stats_adv <- stats_adv %>% 
    remove_empty("cols") %>%
    clean_names() %>% 
    dplyr::filter(!player=="Player") %>%
    mutate_at(vars(-c(player,tm,pos)),as.numeric) %>% 
    mutate_at(vars(-c(player,tm,pos)), funs(replace(., is.na(.), 0))) %>% 
    as_tibble() %>% 
    mutate(year=season)%>%
    group_by(player) %>% 
    slice(1) %>% 
    ungroup() %>% 
    select(-rk)
  
  player_stats <- full_join(player_stats_tot,player_stats_pm,
                            by = c("player", "pos", "age", "tm", "g", "gs", "mp")) %>% 
    full_join(player_stats_adv,
              by = c("player", "pos", "age", "tm", "g", "mp"))
  
  
  
  return(player_stats)
}

#
# url <- paste0("https://www.basketball-reference.com/players/a/#players::none")
#
# ---------------------------------------------------------------------------------------
scrape_player <- function(alpha){
  # player stats
  # scrape
  lien <- paste0("https://www.basketball-reference.com/players/" ,alpha, "/#players::none",sep="",collapse=NULL)
  url<-lien
  #
  p_stats <- url %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]]
  
  #clean
  p_stats <- p_stats %>% 
    remove_empty() %>%
    clean_names() %>% 
    dplyr::filter(!player=="Player") %>%
    as_tibble() %>% 
    group_by(player) %>% 
    slice(1) %>% 
    ungroup() 
  
  return(p_stats)
  
}
#
#
#size conversion 
# 
c_height<-function(x){
  x<-as.character(x)
  split<-strsplit(x,"-")
  feet<-as.numeric(split[[1]][1])
  inch<-as.numeric(split[[1]][2])
  x<-round(conv_unit(feet,"ft","cm")+conv_unit(inch,"inch","cm"),0)
}


c_weight<-function(x){
  x<-as.numeric(as.character(x))
  round(conv_unit(x,"lbs","kg"),0)
}
#




theme_set(theme_minimal()+
            theme(legend.position = "bottom",
                  text=element_text(size = 12)))


#
# Baseball player , if csv exist use them, if not download
#
if (file.exists("player_stats_1980.csv")){
  player_stats_1980 <- read.csv("player_stats_1980.csv", header=TRUE)
} 
if (file.exists("player_stats_1990.csv")){
  player_stats_1990 <- read.csv("player_stats_1990.csv", header=TRUE)
} 
if (file.exists("player_stats_2000.csv")){
  player_stats_2000 <- read.csv("player_stats_2000.csv", header=TRUE)
}  
  if (file.exists("player_stats_last.csv")){
    player_stats_last <- read.csv("player_stats_last.csv", header=TRUE)
  } 


#
#  If no csv saved, reload data from website 
#
setwd('C:\\Users\\Utilisateur\\Documents\\projects\\Capstone2-NBa')
getwd()
if (!file.exists("player_stats_last.csv")){
player_stats_last <- map_dfr(2018:2019,scrape_stats)
player_stats_1980 <-map_dfr(1980:1989,scrape_stats)
player_stats_1990 <- map_dfr(1990:1999,scrape_stats)
player_stats_2000 <- map_dfr(2000:2017,scrape_stats)
#
write.csv(player_stats_last,"player_stats_last.csv")
write.csv(player_stats_1980,"player_stats_1980.csv")
write.csv(player_stats_1990,"player_stats_1990.csv")
write.csv(player_stats_2000,"player_stats_2000.csv")
}
#
#  merge data
#
player_stats_all<-player_stats_last
player_stats_all<-merge(player_stats_all,player_stats_1990,all=TRUE)
player_stats_all<-merge(player_stats_all,player_stats_2000,all=TRUE)
player_stats_all<-merge(player_stats_all,player_stats_1980,all=TRUE)

#filter by player with more than 500  mn played
player_stats <- player_stats_all%>%select(-X,-age) %>% 
  dplyr::filter(mp>=500)

write.csv(player_stats,"player_stats.csv", row.names = FALSE)



#
# research all players in alphabetical; no x in databse
#
#
alphabet<-letters[seq( from = 1, to = 26 )]  
alphabet<-alphabet[-24]
alphabet


if (file.exists("player_data.csv")){
  player_data <- read.csv("player_data.csv", header=TRUE)
}
#
if (!file.exists("player_data.csv")){
player_data <-map_dfr(alphabet,scrape_player)
#
write.csv(player_data,"player_data.csv")
#
}
#
#
#  i keep the rw data in player_stats and create a working set NBA
#
#
player_data$Player <- gsub("\\*$", "", player_data$player)
-----------------------------------------------------------------------------------
  #
  # Player statitics - convert height and weight
  #
  #------------------------------------------------------------------------------------
player_data<-player_data%>%rowwise()%>%mutate(height=c_height(ht),weight=c_weight(wt))
player_data<-player_data%>%filter(!is.na(wt) &!is.na(ht))%>%rowwise()%>%rowwise()%>%mutate(p_cm=(c_height(ht)),p_kg=(c_weight(wt)))%>%mutate(height=c_height(ht),weight=c_weight(wt))
#

#
print(player_data)
p_stat<-player_data
p_stat%>%group_by(from) %>% summarise(avg_h=mean(p_cm), avg_p=mean(p_kg))

player_d<-player_data%>%select(player,p_cm,p_kg)
#
NBA <- left_join(player_stats,player_d, by=c("player"))

#
# --------------------             DATA Cleaning ------------------------------------------------
# Remove NA rows
NBA <- NBA %>% filter(!is.na(year), !is.na(player))
# Remove Team = TOT (which indicates total, when player played in more than 1 team in a season)
NBA <- NBA[NBA$tm != "TOT",] 
# Remove of "*" which indicates a player is a member of NBA Hall of Fame
#NBA$Player <- gsub("\\*$", "", NBA$Player)
# Fix player data
#PlayerData[2143, 4] = as.factor("6-2")
#PlayerData[2143, 5] = 190
#NBA[21304, 3] = "SG"
#
str(NBA)
head(NBA)
dim(NBA)
#
str(player_data)
head(player_data)
dim(player_data)
#
#str(season)
#head(season)
#dim(season)
#season$Player
#season$year
#
# removing na rows
#
#season <- season %>% filter(!is.na(Player), !is.na(Year))

#
NBA %>% 
       ggplot(aes(year)) + 
       geom_histogram(binwidth=0.2, color="darkblue", fill="lightblue") + 
       ggtitle("Nb Players by season")

Team_stat<-NBA %>% group_by(year) %>% summarise(nb_player=n_distinct(player), nb_teams=n_distinct(tm),nb_game=max(g), play_by_team=round(nb_player/nb_teams))


Team_stat %>% 
  ggplot()+geom_line(aes(year,nb_player)) + 
  ggtitle("Number of Players by Year")

Team_stat %>% 
  ggplot()+geom_line(aes(year,nb_teams)) + 
  ggtitle("Number of Teams by year")

Team_stat %>% 
  ggplot()+geom_line(aes(year,nb_game)) + 
  ggtitle("Number of game by year")



# Players Height distribution for all players
p_stat %>% 
  ggplot(aes(p_cm,fill=TRUE),color="BLUE") + 
  geom_density() +
  ggtitle("Height Distribution of all Player")
#
# Evolution of players heights by year
#
j<-p_stat  %>% group_by(from)%>%summarize(avg_h=mean(p_cm),avg_p=mean(p_kg))
j %>%
     ggplot(aes(x=as.numeric(from),y=as.numeric(avg_h))) + 
     geom_line()+geom_smooth()+ggtitle("Players Heights by Starting Years")

# Evolution of players weight by year 

j %>%
  ggplot(aes(x=as.numeric(from),y=as.numeric(avg_p))) + 
  geom_line()+geom_smooth()+ggtitle("Players Weights by Starting Years")

# compare weight and height

j %>%
  ggplot(aes(x=as.numeric(from)))+geom_line(aes(y=as.numeric(avg_p))) + 
  geom_line(aes(y=as.numeric(avg_h)))
#
#
#
#                                                  
#
# now analyse players by position
#
D_by_pos<-p_stat%>%group_by(from,pos)%>%summarise (Nb=n(),start=mean(from),avg_w=mean(p_kg),avg_h=mean(p_cm))
#
#
#
print(D_by_pos)
#
D_by_pos %>%  group_by(pos) %>%summarise(pos_h=mean(avg_h),pos_w=mean(avg_w))
print(D_by_pos)
#
D_by_pos%>%ggplot(aes(x=(pos)))+geom_line(aes(y=as.numeric(avg_w))) 
D_by_pos%>%ggplot(aes(x=(pos)))+geom_line(aes(y=as.numeric(avg_h))) 

#
#  Big one by position short and tall
#
player_data %>%
     group_by(p_kg, player) %>%
     summarise(pos,YearActive = paste(mean(from), "-", mean(to)))%>%
     head()

player_data %>%
  group_by(p_cm, player) %>%
  summarise(pos,YearActive = paste(mean(from), "-", mean(to)))%>%
  tail()
#
# Stats by position
#

p_stat %>%
  group_by(pos) %>%
  summarise(MinHeight = min(p_cm),
            MaxHeight = max(p_cm),
            MedianHeight = median(p_cm),
            MeanHeight = round(mean(`p_cm`), 2)) 

p_stat %>%
  ggplot(aes(pos, p_cm, color=pos)) +
  geom_violin() +
  ggtitle("Height distribution by position") +
  stat_summary(fun.y=mean, geom="point", shape=8, size=6) +
  geom_point() +
  geom_hline(aes(yintercept = mean(p_stat$p_cm, na.rm=T), linetype = "Average NBA players"),
             col = "red",
             alpha = 0.5) +
  geom_hline(aes(yintercept = 179, linetype = "Average American male"),
             col = "blue",
             alpha = 0.5) +
  theme(legend.position="bottom")


p_stat %>%
  ggplot(aes(pos, p_kg, color=pos)) +
  geom_violin() +
  ggtitle("Weight distribution by position") +
  stat_summary(fun.y=mean, geom="point", shape=8, size=6) +
  geom_point() +
  geom_hline(aes(yintercept = mean(p_stat$p_kg, na.rm=T), linetype = "Average NBA players"),
             col = "red",
             alpha = 0.5) +
  geom_hline(aes(yintercept = 80, linetype = "Average American male"),
             col = "blue",
             alpha = 0.5) +
  theme(legend.position="bottom")
# Players last year
#  Points by position
#
NBA20<-NBA%>% filter(year>2017)
NBA20%>%group_by(pos) %>%
  summarise(Games=mean(g),FieldGoal=mean(fg),Attemps=mean(fga))
#
#   Player by Field Goal Assist / games 2018
#
NBA20%>%group_by(player,pos,year) %>%
  summarise(Games=mean(g),FieldGoal=mean(fg),Attemps=mean(fga))%>%arrange(desc(FieldGoal))%>%head()

pairs(~fg+x3p+x2p+ft+orb+drb+ast+stl+blk,data = NBA20,
      main = "Positon Matrix")
#
# filtering players with more than 50 games a year
#
NBA20 %>% filter(g>50)%>%
  ggplot(aes(pos, fg, color=pos)) +
  geom_violin() +
  ggtitle("Field Goal by Position") +
  stat_summary(fun.y=mean, geom="point", shape=8, size=6) +
  geom_point() +
  geom_hline(aes(yintercept = mean(NBA20$fg, na.rm=T), linetype = "Average FG by players"),
             col = "red",
             alpha = 0.5)  +
  theme(legend.position="bottom")
#
# 3points by position
#
NBA20 %>% filter(g>50)%>%
  ggplot(aes(pos, x3p, color=pos)) +
  geom_violin() +
  ggtitle("3 Points by Position") +
  stat_summary(fun.y=mean, geom="point", shape=8, size=6) +
  geom_point() +
  geom_hline(aes(yintercept = mean(NBA20$x3p, na.rm=T), linetype = "Average 3Pts by players"),
             col = "red",
             alpha = 0.5)  +
  theme(legend.position="bottom")
#
# 2 points by positions
#
NBA20 %>% filter(g>50)%>%
  ggplot(aes(pos, x2p, color=pos)) +
  geom_violin() +
  ggtitle("2 Points by Position (50 games played)") +
  stat_summary(fun.y=mean, geom="point", shape=8, size=6) +
  geom_point() +
  geom_hline(aes(yintercept = mean(NBA20$x2p, na.rm=T), linetype = "Average 3Pts by players"),
             col = "red",
             alpha = 0.5)  +
  theme(legend.position="bottom")
#
# shotters
#
NBA20 %>% filter(g>50)%>%
  ggplot(aes(pos, x2p+x3p, color=pos)) +
  geom_violin() +
  ggtitle("Points by Position(>50 gmas played)") +
  stat_summary(fun.y=mean, geom="point", shape=8, size=6) +
  geom_point() +
  geom_hline(aes(yintercept = mean(NBA20$x2p+NBA20$x3p, na.rm=T), linetype = "Average Pts by players"),
             col = "red",
             alpha = 0.5)  +
  theme(legend.position="bottom")

#
# REbound by player
#
NBA20 %>% filter(g>50 )%>%
  ggplot(aes(pos, orb+drb, color=pos)) +
  geom_violin() +
  ggtitle("Rebounds by Position (>50 games played") +
  stat_summary(fun.y=mean, geom="point", shape=8, size=6) +
  geom_point() +
  geom_hline(aes(yintercept = mean(NBA20$orb+NBA20$drb, na.rm=T), linetype = "Average Rebound by players"),
             col = "red",
             alpha = 0.5)  +
  theme(legend.position="bottom")
#
#  DEfense
#
NBA20 %>% filter(g>50)%>%
  ggplot(aes(pos, stl+blk, color=pos)) +
  geom_violin() +
  ggtitle("Steal/Block by Position (50 Games played)") +
  stat_summary(fun.y=mean, geom="point", shape=8, size=6) +
  geom_point() +
  geom_hline(aes(yintercept = mean(NBA20$stl+NBA20$blk, na.rm=T), linetype = "AverageBlock+Steal by players"),
             col = "red",
             alpha = 0.5)  +
  theme(legend.position="bottom")
#
#  assist + turnover
#
NBA20 %>% filter(g>50)%>%
  ggplot(aes(pos, ast+tov, color=pos)) +
  geom_violin() +
  ggtitle("Assist/Turnover by Position (>50 games played)") +
  stat_summary(fun.y=mean, geom="point", shape=8, size=6) +
  geom_point() +
  geom_hline(aes(yintercept = mean(NBA20$ast+NBA20$tov, na.rm=T), linetype = "Average Assist+Turnover by players"),
             col = "red",
             alpha = 0.5)  +
  theme(legend.position="bottom")
#
#  compare with stats by minutes players
#
NBA20 %>% filter(g>50 & mp>1500)%>%
  ggplot(aes(pos, x3p_pm+x2p_pm, color=pos)) +
  geom_violin() +
  ggtitle("Points per Minutes by Position") +
  stat_summary(fun.y=mean, geom="point", shape=8, size=6) +
  geom_point() +
  geom_hline(aes(yintercept = mean(NBA20$x3p_pm+NBA20$x2p_pm, na.rm=T), linetype = "Average Pts per Min by players"),
             col = "red",
             alpha = 0.5)  +
  theme(legend.position="bottom")

NBA20 %>% filter(g>50 & mp>1500)%>%
  ggplot(aes(pos, ast_pm+tov_pm, color=pos)) +
  geom_violin() +
  ggtitle("Assist/Turnover played >1500 mn") +
  stat_summary(fun.y=mean, geom="point", shape=8, size=6) +
  geom_point() +
  geom_hline(aes(yintercept = mean(NBA20$ast_pm+NBA20$tov_pm, na.rm=T), linetype = "Average Assist+Turnover by players"),
             col = "red",
             alpha = 0.5)  +
  theme(legend.position="bottom")
#
#Player info dataset
#
#ORpG: (Offensive Rebounds per Game): Average offensive rebounds a player made in a game. (ORB/G) 
#DRpG: (Defensive Rebounds per Game): Average defensive rebounds a player made in a game. (DRB/G) 
#RpG (Rebounds per Game): Average rebounds a player made in a game. (TRB/G) 
#ApG (Assists per Game): Average assists a player made in a game. (AST/G) 
#SPG (Steals per Game): Average rebounds a player made in a game. (STL/G) 
#BPG (Blocks Per Game): Average rebounds a player made in a game. (BLK/G) 
#TPG (Turnovers Per Game): Average turnovers a player made in a game. (TOV/G) 
#PpG (Points per Game): Average points a player made in a game. (PTS/G) 
#Pos Position

P_info<-NBA20%>%filter(g>50)%>%
  rowwise() %>%
  mutate(ORpG = orb / g,
         DRpG = drb / g,
         RpG = trb / g,
         ApG = ast / g,
         SpG = stl / g,
         BpG = blk / g,
         TpG = tov / g,
         PpG = pts / g)%>% select(player,pos,ORpG,DRpG,RpG,ApG,SpG,BpG,TpG,PpG,p_cm,p_kg)
#
pairs(~ORpG+DRpG+RpG+ApG+SpG+BpG+TpG,data = P_info,
      main = "Positon Matrix")
#
# Computing the Principal Components (PC) 
# I will use NBA dataset with 8 components for the demonstration. The data contain 8 continuous variables 
# which corresponds to ability and a categorical variable describing the player position.
#  I will not use age, height and weight, as they impact the 8 datas

# data transform for the selected dataset on P_info
log.player <-(P_info[, 3:12])
pl.pos <- P_info[, 2]
pl.name<-P_info[,1]
print(log.player)
#
sum(is.na (log.player))
log.player<-na.omit(log.player)
#
# search for correlation
#
c<-cor(log.player)
print(c)
corrplot.mixed(cor(log.player), order="hclust", tl.col="black")
#
# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
#
player.pca <- prcomp(log.player,
                 center = TRUE,
                 scale. = TRUE)
print(player.pca)
#
plot(player.pca)

player_var <- get_pca_var(player.pca)
pc_s <- player_var$contrib[,1:8]
colnames(pc_s) <- paste0("PC",1:8)

as_tibble(pc_s   ,rownames = "stat") %>%
  gather(pc,contrib,PC1:PC8) %>%
  mutate(pc=factor(pc,levels=paste0("PC",1:10))) %>% 
  group_by(pc) %>% 
  top_n(5,contrib) %>% 
  ggplot(aes(x=stat,y=contrib))+
  geom_col()+
  coord_flip()+
  facet_wrap(~pc,scales = "free",ncol=5)+
  labs(x="",y="")
#
# PC1: (Turnovers Per Game)-RpG(Rebounds per Game)-(Defensive Rebounds per Game)-(Points per Game)
# Overall player with good Rebounds capabilities
#
# PC2: (Offensive Rebounds per Game)-(Assists per Game)
# Offensive player with good assist and offensive rebound capabilities
#
# PC3: (Steals per Game)
# Defensive player able to steal the ball
#
# PC4: (Blocks Per Game)
# Big defensive player focus on blocking
#
# PC5: (Points per Game)-(Assists per Game)
# The offensive top scorer, focus on Points and assist
#
# PC6: (Offensive Rebounds per Game)-(Defensive Rebounds per Game)
# The overall rebounder, focus on offensive and defensive rebound
#
# PC7: ((Turnovers Per Game)-(Assists per Game))
# Offensive Support player
# 
#PC8 : (Rebounds per Game) (Defensive Rebounds per Game)
# Defensive support player
#
#ORpG: (Offensive Rebounds per Game): Average offensive rebounds a player made in a game. (ORB/G) 
#DRpG: (Defensive Rebounds per Game): Average defensive rebounds a player made in a game. (DRB/G) 
#RpG (Rebounds per Game): Average rebounds a player made in a game. (TRB/G) 
#ApG (Assists per Game): Average assists a player made in a game. (AST/G) 
#SPG (Steals per Game): Average steal a player made in a game. (STL/G) 
#BPG (Blocks Per Game): Average rebounds a player made in a game. (BLK/G) 
#TPG (Turnovers Per Game): Average turnovers a player made in a game. (TOV/G) 
#PpG (Points per Game): Average points a player made in a game. (PTS/G) 
#

#
# The summary method describe the importance of the PCs. 
# The first row describe again the standard deviation associated with each PC. 
# The second row shows the proportion of the variance in the data explained by each component 
# while the third row describe the cumulative proportion of explained variance. 
# We can see there that the first five PCs accounts for more than 95% of the variance of the data.
#
summary(player.pca)
#
# Predict Principal Components PCs
#
predict(player.pca, 
        newdata=tail(log.player, 2))

player_stats_ld <- player.pca$x[,1:8]

P_info<-na.omit(P_info)


player_clus <- kmeans(player_stats_ld,centers = 5,iter.max = 150)
summary(player_clus)

aggregate(player_stats_ld,by=list(player_clus$cluster),mean)

#The following code is used to visualizes the cluster centers
as_tibble(player_clus$centers) %>% 
  gather(component,value,PC1:PC8) %>% 
  mutate(clust = rep(1:5,8)) %>% 
  ggplot(aes(x=factor(component,levels = paste0("PC",10:1)),y=value))+
  geom_col()+
  coord_flip()+
  facet_wrap(~clust)+
  labs(x="",y="")

print(pc_s)



X <- sapply(1:7, function(i){
  kmeans(pc_s,i,nstart=50,iter.max=15)$tot.withinss
  
})

print(X)
plot(1:7, X,type="b",pch=19,frame=FALSE,xlab="NB Clustr", ylab="Total within cluster")
#
# decide to choose Nb cluster = 5 to tke winthin >4000
#
print(player.pca)
player_stats_ld <- player.pca$x[,1:10]

aggregate(player_stats_ld,by=list(player_clus$cluster),mean)


player_clus <- kmeans(player_stats_ld,centers = 5,iter.max = 150)
summary(player_clus)

#The following code is used to visualizes the cluster centers
as_tibble(player_clus$centers) %>% 
  gather(component,value,PC1:PC8) %>% 
  mutate(clust = rep(1:5,8)) %>% 
  ggplot(aes(x=factor(component,levels = paste0("PC",10:1)),y=value))+
  geom_col()+
  coord_flip()+
  facet_wrap(~clust)+
  labs(x="",y="")

#
#  Pc1 to PC6 define the clusters
# what do they represent ?
#Traditionally, basketball has 5 specific positions on the court.
#Two guards, two forwards, and a center.
#1. Point guard
#2. Shooting guard
#3. Small forward
#4. Power forward
#5. Center
# 


class.players = cbind(P_info, player.pca$x)

class.players$km.cluster = player.pca$cluster

summary(class.players)








set.seed(5)

num.clusters = 12

cluster_data = P_info %>% 
  
  select(-player,-pos) %>%
  
  scale()

km.mod = kmeans(cluster_data, centers=num.clusters, iter.max=50)

km.mod





# -------------------------------------------------------------------------

# plot results by PCs

#---------------------------------------------------------------------------

class.players = cbind(P_info, player.pca$x)

class.players$km.cluster = km.mod$cluster


class.players %>% filter(km.cluster == 8)

summary(class.players)


my.cols = c('black', 'blue', 'yellow', 'lightgreen', 'cadetblue2', 
            'darkorange', 'forestgreen', 'darkorchid', 'goldenrod', 'red', 'green2', 'lightpink3')



palette(my.cols)



plot(PC2 ~ PC1, data=class.players, col=km.cluster, pch=16, main = "Player clusters by first two principal components") 

pt.labels = ifelse(class.players$PpG > 23,class.players$player, "") 

text(class.players$PC1, class.players$PC2, pt.labels, pos=2, cex=0.5) 

cluster.labels = c("1: ",
                   
                   "2: ",
                   
                   "3: ",
                   
                   "4: ",
                   
                   "5: ",
                   
                   "6: ",
                   
                   "7: ",
                   
                   "8: ",
                   
                   "9: ",
                   
                   "10: ",
                   
                   "11: ",
                   
                   "12: r") 

legend(5.4, -1.2, legend=cluster.labels, col=my.cols, pch=16, cex=0.6)
legend("topright", legend=cluster.labels, col=1:num.clusters, pch=16, cex=0.45)
palette("default")

#
#
#  PC1 size / physical
#  PC2 Quicknes/Ball handling
#  PC3 Ball Catcher
# ---------------------------------- GLOBAL VIEW -------------------------------------------------------

plot(PC2 ~ PC1, data=class.players, col=km.cluster, pch=16, main = "Position clusters by first two principal components",
     xlab="PC1: 'size/physicality'", ylab="PC2: 'quickness/ballhandling'") 

pt.labels = class.players$pos 

text(class.players$PC1, class.players$PC2, pt.labels, pos=2, cex=0.5) 

cluster.labels = class.players$pos

legend(5.4, -1.2, legend=cluster.labels, col=my.cols, pch=16, cex=0.6)
palette("default")



plot(PC3 ~ PC2, data=class.players, col=km.cluster, pch=16, main = "Position clusters by PC2 vs PC3 components",
     xlab="PC1: 'Quick/BallHandling'", ylab="PC3: 'PickPocket'") 

pt.labels = class.players$pos 

text(class.players$PC1, class.players$PC2, pt.labels, pos=2, cex=0.5) 

cluster.labels = class.players$pos

legend(5.4, -1.2, legend=cluster.labels, col=my.cols, pch=16, cex=0.6)
palette("default")

# -------------------------------------1 PF Only --------------------------------------------------


d<-class.players%>%filter(class.players$pos=="PF")


plot(PC2 ~ PC1, data=d, col=km.cluster, pch=16, main = " PF by first two principal components",
     xlab="PC1: '  '", ylab="PC2: ' '") 

pt.labels = d$player
cluster.labels = d$player

text(d$PC1, d$PC2, pt.labels, pos=2, cex=0.5) 
palette("default")

# ---------------------------------------2 CEnter only -------------------------------------------

d<-class.players%>%filter(class.players$pos=="C")


plot(PC2 ~ PC1, data=d, col=km.cluster, pch=16, main = " C by first two principal components",
     xlab="PC1: '  '", ylab="PC2: ' '") 

pt.labels = d$player
cluster.labels = d$player
text(d$PC1, d$PC2, pt.labels, pos=2, cex=0.5) 
palette("default")

# -------------------------------------- 3 PG Only ------------------------------------------------

d<-class.players%>%filter(class.players$pos=="PG")


plot(PC2 ~ PC1, data=d, col=km.cluster, pch=16, main = " PG by first two principal components",
     xlab="PC1: ' '", ylab="PC2: ' '") 

pt.labels = d$player
cluster.labels = d$player
text(d$PC1, d$PC2, pt.labels, pos=2, cex=0.5) 
palette("default")



# ---------------------------------------- 4 SG only -------------------------------------------

d<-class.players%>%filter(class.players$pos=="SG")


plot(PC2 ~ PC1, data=d, col=km.cluster, pch=16, main = " SG by first two principal components",
     xlab="PC1: ' '", ylab="PC2: ' '") 

pt.labels = d$player
cluster.labels = d$player
text(d$PC1, d$PC2, pt.labels, pos=2, cex=0.5) 
palette("default")

# -------------------------------- 5 SF only ---------------------------------------------------

d<-class.players%>%filter(class.players$pos=="SF")


plot(PC2 ~ PC1, data=d, col=km.cluster, pch=16, main = " SF by first two principal components",
     xlab="PC1: ' '", ylab="PC2: ' '") 

pt.labels = d$player
cluster.labels = d$player
text(d$PC1, d$PC2, pt.labels, pos=2, cex=0.5) 
palette("default")


# ---------------------------------------------------------------------------------------------7


plot(PC3 ~ PC1, data=class.players, col=km.cluster, pch=16, main = "Player clusters by first two principal components",
     xlab="PC1: 'size/physicality'", ylab="PC3: 'pickpockets'") 

pt.labels = ifelse(class.players$PpG > 22, class.players$player, "") 

text(class.players$PC1, class.players$PC3, pt.labels, pos=2, cex=0.5) 

cluster.labels = c("1: ",
                   
                   "2: ",
                   
                   "3: ",
                   
                   "4: ",
                   
                   "5: ",
                   
                   "6: ",
                   
                   "7: ",
                   
                   "8: ",
                   
                   "9: ",
                   
                   "10: ",
                   
                   "11: ",
                   
                   "12: ") 

legend(5.4, -1.2, legend=cluster.labels, col=my.cols, pch=16, cex=0.6)
legend("topright", legend=cluster.labels, col=1:num.clusters, pch=16, cex=0.45)
palette("default")













# --------------------------------------------------------------------------------------------
#
#                  using more players  data
#
# --------------------------------------------------------------------------------------------

# ---------------------------------FULL set 17------------------------------------------------

# ------------------------------------------------------------------------------
#  Rebuild P_info_all
#  same with full set of data
#  for players with more the 500 minutes played
#-------------------------------------------------------------------------------
P_info_all<-NBA %>% filter(mp>500) %>% group_by(player)%>%
  select(pos,p_cm,p_kg,fg,fga,x3p,x3pa,x2p,x2pa,ft,fta,orb,drb,trb,ast,stl,blk,tov,pf,pts)
P_info_all[is.na(P_info_all)]<- 0
sum(is.na (P_info_all))
P_info_all<-na.omit(P_info_all)
head(P_info_all)
P_info_all<-aggregate(P_info_all,by=list(P_info_all$player,P_info_all$pos),mean)
head(P_info_all)
P_info_all<-P_info_all%>%select(-player,-pos)
P_info_all=rename(P_info_all,player=Group.1,pos=Group.2)
head(P_info_all)

#
# classification by stats colums from fg to pts
#

log.player <-(P_info_all[, 4:20])
pl.pos <- P_info[, 2]
pl.name<-P_info[,1]
print(log.player)
#
sum(is.na (log.player))
log.player<-na.omit(log.player)
#
# search for correlation in caracteristics
#
correlation<-cor(log.player)
print(correlation)
corrplot.mixed(cor(log.player), order="hclust", tl.col="black")
#
#  
#
#-------------------------------------------------- Dimension reduction by PCA ---------------------------------------------------------
#
# select data from fg to pts
#
nba.pca <- P_info_all %>% select(fg:pts)%>%
  as.matrix() %>% 
  prcomp(center = TRUE,scale = TRUE,retx = TRUE)

print (nba.pca)

#
# plot variance to show K
#
fviz_eig(nba.pca,ncp=17)
##



nb<-17

player_stats_ld <- nba.pca$x[,1:nb]

print(nba.pca)
#
plot(nba.pca)
summary(nba.pca)
#
# let' s try with nb vars
#

nba_var <- get_pca_var(nba.pca)
pcs <- nba_var$contrib[,1:nb]
colnames(pcs) <- paste0("PC",1:nb)

print(pcs)
summary(pcs)

as_tibble(pcs,rownames = "stat") %>%
  gather(pc,contrib,PC1:nb) %>%
  mutate(pc=factor(pc,levels=paste0("PC",1:nb))) %>% 
  group_by(pc) %>% 
  top_n(5,contrib) %>% 
  ggplot(aes(x=stat,y=contrib))+
  geom_col()+
  coord_flip()+
  facet_wrap(~pc,scales = "free",ncol=5)+
  labs(x="",y="")

#number of K using silhouette

fviz_nbclust(player_stats_ld,kmeans,method="silhouette")


# -----------------------------------------------------------------------------
# data definition
# -----------------------------------------------------------------------------
#Season -- If listed as single number, the year the season ended.
#★ - Indicates All-Star for league.
#Only on regular season tables.
#Age -- Player's age on February 1 of the season
#Tm -- Team
#Lg -- League
#Pos -- Position
#G -- Games
#GS -- Games Started
#MP -- Minutes Played Per Game
#FG -- Field Goals Per Game
#FGA -- Field Goal Attempts Per Game
#FG% -- Field Goal Percentage
#3P -- 3-Point Field Goals Per Game
#3PA -- 3-Point Field Goal Attempts Per Game
#3P% -- 3-Point Field Goal Percentage
#2P -- 2-Point Field Goals Per Game
#2PA -- 2-Point Field Goal Attempts Per Game
#2P% -- 2-Point Field Goal Percentage
#eFG% -- Effective Field Goal Percentage
#
#This statistic adjusts for the fact that a 3-point field goal is worth one more point than a 2-point field goal.
#
#FT -- Free Throws Per Game
#FTA -- Free Throw Attempts Per Game
#FT% -- Free Throw Percentage
#ORB -- Offensive Rebounds Per Game
#DRB -- Defensive Rebounds Per Game
#TRB -- Total Rebounds Per Game
#AST -- Assists Per Game
#STL -- Steals Per Game
#BLK -- Blocks Per Game
#TOV -- Turnovers Per Game
#PF -- Personal Fouls Per Game
#PTS -- Points Per Game
# ------------------------------------------------------------
# PC1,PC11: 2pts Shooter,fied goal attemps
# PC2: 3pts shooter,offensive rebonds, block
# PC3,PC13:  3 pts shooter
# PC4,PC8: steal and assist (mixt)
# PC5,6: pure blocker (def)
# PC7: aggresive faulty player
# PC9,PC14: Rebound expert
# PC10: support player
# PC12: passing 
#

#corrplot(p_info_all ,type="upper",method="number")
#
#  select value of  K means
#
#
X <- sapply(1:15, function(i){
  kmeans(pcs,i,nstart=50,iter.max=15)$tot.withinss
  
})
#
#  Only 2 clusters , waiting for at least 5 ????
#
print(X)
plot(1:15, X,type="b",pch=19,frame=FALSE,xlab="NB Clustr", ylab="Total within cluster")
#
# decide to choose Nb cluster = 5 to take within >12000
#
player_stats_ld <- nba.pca$x[,1:10]
player_clus <- kmeans(player_stats_ld,centers = 5,iter.max = 150)
print(player_clus)
str(player_clus)
summary(player_clus)
#
# generate the cluster means
#
aggregate(player_stats_ld,by=list(player_clus$cluster),mean)

#
#The following code is used to visualizes the cluster centers
#
as_tibble(player_clus$centers) %>% 
  gather(component,value,PC1:PC10) %>% 
  mutate(clust = rep(1:5,10)) %>% 
  ggplot(aes(x=factor(component,levels = paste0("PC",10:1)),y=value))+
  geom_col()+
  coord_flip()+
  facet_wrap(~clust)+
  labs(x="",y="")
#----------------------------------------------------------------------------------
# hierachical dendogram - all players
#---------------------------------------------------------------------------------
mydata<-data.frame(player_stats_ld,player_clus$cluster)
d<-dist(mydata,method="euclidean")
fit<-hclust(d,method="ward.D2")
plot(fit,hang=-1)
groups<-cutree(fit,k=5)
rect.hclust(fit,k=5,border="blue")





#
#  Pc1 to PC17 define the clusters
# what do they represent ?
#Traditionally, basketball has 5 specific positions on the court.
#Two guards, two forwards, and a center.
#1. Point guard
#2. Shooting guard
#3. Small forward
#4. Power forward
#5. Center
# 
  

class.players = cbind(P_info_all, nba.pca$x)

class.players$km.cluster = nba.pca$cluster

head(class.players)


# -----------------------------------------------------------
#   trying 10 clusters
#------------------------------------------------------------
set.seed(5)
num.clusters = 10
#cluster_data = P_info_all %>% select(-player) 
cluster_data<-player_stats_ld
km.mod = kmeans(cluster_data, centers=num.clusters, iter.max=150)

km.mod







###################################

### results ALL PLAYERS  STATS ####

####################################

class.players = cbind(P_info_all, nba.pca$x)

class.players$km.cluster = km.mod$cluster


#class.players<-class.players%>% filter(km.cluster == 8)

summary(class.players)


my.cols = c('black', 'blue', 'yellow', 'lightgreen', 'cadetblue2', 
            'darkorange', 'forestgreen', 'darkorchid', 'goldenrod', 'red', 'green2', 'lightpink3')
palette(my.cols)

plot(PC2 ~ PC1, data=class.players, col=km.cluster, pch=16, main = "Player clusters by first two principal components") 

pt.labels = ifelse(class.players$pts > 1500,class.players$player," ")

text(class.players$PC1, class.players$PC2, pt.labels, pos=2, cex=0.5) 

cluster.labels = c("1: Shooter",
                   
                   "2: exterior distributor",
                   
                   "3: defensive stopper",
                   
                   "4: offensive hub",
                   
                   "5: size and distance",
                   
                   "6: under the basket",
                   
                   "7: stretch big",
                   
                   "8: attacking shooter",
                   
                   "9: inside / outside",
                   
                   "10: 3-point specialist",
                   
                   "11: attacking distributor",
                   
                   "12: exterior shooter") 

#legend(5.4, -1.2, legend=cluster.labels, col=my.cols, pch=16, cex=0.6)
legend("topright", legend=cluster.labels, col=1:num.clusters, pch=16, cex=0.45)
palette("default")

#
#
#  PC1 
#  PC2 
#  PC3 
# ---------------------------------- GLOBAL VIEW -------------------------------------------------------

plot(PC2 ~ PC1, data=class.players, col=km.cluster, pch=16, main = "Position clusters by first two principal components",
     xlab="PC1: 'size/physicality'", ylab="PC2: 'quickness/ballhandling'") 

pt.labels = ifelse(class.players$pts > 1500,class.players$player,"") 

text(class.players$PC1, class.players$PC2, pt.labels, pos=2, cex=0.5) 

cluster.labels = class.players$pos

legend("topright", legend=cluster.labels, col=1:num.clusters, pch=16, cex=0.45)
palette("default")



plot(PC3 ~ PC2, data=class.players, col=km.cluster, pch=16, main = "Position clusters by PC2 vs PC3 components",
     xlab="PC1: 'Quick/BallHandling'", ylab="PC3: 'PickPocket'") 

pt.labels = ifelse(class.players$pts > 1500,class.players$player,"")  

text(class.players$PC1, class.players$PC2, pt.labels, pos=2, cex=0.5) 

cluster.labels = class.players$player

#legend(5.4, -1.2, legend=cluster.labels, col=my.cols, pch=16, cex=0.6)
legend("topright", legend=cluster.labels, col=1:num.clusters, pch=16, cex=0.45)
palette("default")

# -------------------------------------1 PF Only --------------------------------------------------


d<-class.players%>%filter(class.players$pos=="PF")


plot(PC2 ~ PC1, data=d, col=km.cluster, pch=16, main = " PF by first two principal components",
     xlab="PC1: 'size/physicality'", ylab="PC2: 'quickness/ballhandling'") 

pt.labels = ifelse(d$pts > 1500,d$player,"")
cluster.labels = d$player

text(d$PC1, d$PC2, pt.labels, pos=2, cex=0.5) 
palette("default")

# ---------------------------------------2 CEnter only -------------------------------------------

d<-class.players%>%filter(class.players$pos=="C")


plot(PC2 ~ PC1, data=d, col=km.cluster, pch=16, main = " Centers by first two principal components showing Player>1500 pts",
     xlab="PC1: 'size/physicality'", ylab="PC2: 'quickness/ballhandling'") 

pt.labels = ifelse(d$pts > 1500,d$player,"")
cluster.labels = d$player
text(d$PC1, d$PC2, pt.labels, pos=2, cex=0.5) 
palette("default")

# -------------------------------------- 3 PG Only ------------------------------------------------

d<-class.players%>%filter(class.players$pos=="PG")


plot(PC2 ~ PC1, data=d, col=km.cluster, pch=16, main = " PG by first two principal components",
     xlab="PC1: 'PC1'", ylab="PC2: 'PC2'") 

pt.labels = ifelse(d$drb > 200,d$player,"")

text(d$PC1, d$PC2, pt.labels, pos=2, cex=0.5) 
palette("default")



# ---------------------------------------- 4 SG only -------------------------------------------

d<-class.players%>%filter(class.players$pos=="SG")


plot(PC2 ~ PC1, data=d, col=km.cluster, pch=16, main = " SG by first two principal components",
     xlab="PC1: '1'", ylab="PC2: '2'") 

pt.labels = ifelse(d$drb > 200,d$player,"")
cluster.labels = d$player
text(d$PC1, d$PC2, pt.labels, pos=2, cex=0.5) 
palette("default")

# -------------------------------- 5 SF only ---------------------------------------------------

d<-class.players%>%filter(class.players$pos=="SF")


plot(PC2 ~ PC1, data=d, col=km.cluster, pch=16, main = " SF by first two principal components",
     xlab="PC1: '1'", ylab="PC2: '2'") 

pt.labels = ifelse(d$pts > 200,d$player,"")
cluster.labels = d$player
text(d$PC1, d$PC2, pt.labels, pos=2, cex=0.5) 
palette("default")


# ---------------------------------------------------------------------------------------------7


plot(PC3 ~ PC1, data=class.players, col=km.cluster, pch=16, main = "Player clusters by first two principal components",
     xlab="PC1: 'size/physicality'", ylab="PC3: 'pickpockets'") 

pt.labels = ifelse(class.players$pts > 1500, class.players$player, "") 

text(class.players$PC1, class.players$PC2, pt.labels, pos=2, cex=0.5) 

cluster.labels = c("1: Shooter",
                   
                   "2: exterior distributor",
                   
                   "3: defensive stopper",
                   
                   "4: offensive hub",
                   
                   "5: size and distance",
                   
                   "6: under the basket",
                   
                   "7: stretch big",
                   
                   "8: attacking shooter",
                   
                   "9: inside / outside",
                   
                   "10: 3-point specialist",
                   
                   "11: attacking distributor",
                   
                   "12: exterior shooter") 

#legend(5.4, -1.2, legend=cluster.labels, col=my.cols, pch=16, cex=0.6)
legend("topright", legend=cluster.labels, col=1:num.clusters, pch=16, cex=0.45)
palette("default")

# --------------------------------------------------------------------------------------------------------------------------------
#   MODEL BAYESIAN  - testing with FPC, CLUSTER , PCLUST and MCLUST packages
#---------------------------------------------------------------------------------------------------------------------------------

player_stats_b <- P_info %>% select(ORpG:p_kg)

library(pvclust)
fit<-pvclust(player_stats_b,method.hclust="ward.D2",method.dist="euclidian")
plot(fit)


#library(mclust)
#fit<-Mclust(player_stats_b)
#plot(fit)
#summary(fit)

library(cluster)
d<-dist(player_stats_b,method="euclidean")
fit<-kmeans(player_stats_b,5)
clusplot(player_stats_b,fit$cluster,color=TRUE,shade=TRUE,labels=2,lines=0)

d<-dist(player_stats_b,method="euclidean")
fit<-kmeans(player_stats_b,8)
clusplot(player_stats_b,fit$cluster,color=TRUE,shade=TRUE,labels=2,lines=0)



library(fpc)
plotcluster(player_stats_b,fit$cluster)
fit_stats<-cluster.stats(d,fit$cluster,player_clus$cluster)
fit_stats

# ---   all dataset mean by years ---

player_stats_b <- P_info_all %>% select(fg:pts)

library(pvclust)
fit<-pvclust(player_stats_b,method.hclust="ward.D2",method.dist="euclidian")
plot(fit)


library(mclust)
fit<-Mclust(player_stats_b)
plot(fit)
summary(fit)

library(cluster)
d<-dist(player_stats_b,method="euclidean")
fit<-kmeans(player_stats_b,5)
clusplot(player_stats_b,fit$cluster,color=TRUE,shade=TRUE,labels=2,lines=0)
library(fpc)
plotcluster(player_stats_b,fit$cluster)
fit_stats<-cluster.stats(d,fit$cluster,player_clus$cluster)
fit_stats






# --------------------------------------------------------------------------------------------------------------------------------
#   Decision tree
# --------------------------------------------------------------------------------------------------------------------------------


log.player <-(P_info[, 2:12])
pl.pos <- P_info[, 2]
pl.name<-P_info[,1]
head(log.player)
head(P_info_all)
player_all<-NBA

tree <- rpart(log.player ,data=P_info,method="class")
summary(tree)
plot(tree, uniform = TRUE, main="Position Tree with 10 caracteristics") 
text(tree, all=TRUE) 
#----------------------------------------------------------------------
# Tree without Height and Weight
# driven by Offensive REbound
#----------------------------------------------------------------------
log.player <-(P_info[, 2:10])
pl.pos <- P_info[, 2]
pl.name<-P_info[,1]
head(log.player)
head(P_info)
player_all<-NBA

tree <- rpart(log.player ,data=P_info,method="class")
summary(tree)
plot(tree, uniform = TRUE, main="Position Tree without Height and Weight") 
text(tree, all=TRUE) 
# --------------------------------------------------------------------------
#  NBA 2019 dataset Decisopn Tree out of  Position
# -------------------------------------------------------------------------
d <-NBA20%>%select (-tm)%>%select (player,pos:pts)
log.player <-(d[, 2:20])
pl.pos <- NBA20[, 2]
pl.name<-NBA20[,1]
head(log.player)
head(d)
tree <- rpart(log.player ,data=d,method="class")
summary(tree)
plot(tree, uniform = TRUE, main="Position Tree by Caracteristics only") 
text(tree, all=TRUE) 









log.player <-P_info_all%>%select(-player,-pos)
head(log.player)
log.player<-log.player[, 2:20]
head(log.player)
head(P_info_all)


tree <- rpart(log.player ,data=P_info_all,method="class")
summary(tree)
plot(tree, uniform = TRUE, main="Position Tree") 
text(tree, all=TRUE) 
#
# The classification tree show 7 positions
# based on weight, then Block, 3 points,assit,Offensive rebond and steal
#
#206 : >103 KG, Blocking rate <30 or >85 kg with blocking rate >31.63
#190 : >85.5 Kg, assist <177.8
#185 : <85.5 Kg
#196 : 85.5< Weight <97.5, assis<177, blocking<15.8
#198 : 97.5 <Weight<103.5, 3Pts>70583,Offensive rebond<93
#203 : >97.5<W<103.5, offensive rebond>93.85
#211 : 97.5<W>103.5, blocking >3.16, steal > 42.27
#213: w>103.5,blocking >30.16, steal <42.27
#
#
# Finding the players by those  7 classification
# according to the tree
# using year 2019
#
#P_info_all<-P_info_all%>%select(-player,-pos)%>%mutate(Name=P_info_all$Group.1)
#
P_206<-P_info_all%>% filter(p_kg>=103.5 & blk<30.16)%>%mutate(pos=206)
head(P_206)
P_206_2<-P_info_all%>%filter(p_kg>97.5 & p_kg<103.5 & x3pa<7.583)%>%mutate(pos=206)
head(P_206_2)
P_206_3<-P_info_all%>%filter(p_kg<=97.5 & p_kg>=85.5 & blk>=31.63)%>%mutate(pos=206)
head(P_206_3)

P_206%>%group_by(player) %>%
  summarise(Weight=mean(p_kg),Points=mean(pts),Block=mean(blk))%>%arrange(desc(Block))%>%head()

P_206_2%>%group_by(player) %>%
  summarise(Weight=mean(p_kg),Points=mean(pts),Block=mean(blk))%>%arrange(desc(Block))%>%head()

P_206_3%>%group_by(player) %>%
  summarise(Weight=mean(p_kg),Points=mean(pts),Block=mean(blk))%>%arrange(desc(Block))%>%head()

P_206%>%rbind(P_206,P_206_2)
P_206%>%rbind(P_206,P_206_3)
#
# Group 2
#

P_190<-P_info_all%>% filter(p_kg>=85.5 & ast<177.8)%>%mutate(pos=190)
P_190%>%group_by(player) %>%
  summarise(Weight=mean(p_kg),Assists=mean(ast),tov=mean(tov))%>%arrange(desc(Assists))%>%head()

#185 : <85.5 Kg
P_185<-P_info_all%>% filter(p_kg<=85.5)%>%mutate(pos=185)
P_185%>%group_by(player) %>%
  summarise(Weight=mean(p_kg),Points=mean(pts),tov=mean(tov))%>%arrange(desc(Points))%>%head()


#
#196 : 85.5< Weight <97.5, assis<177, blocking<15.8
#

P_196%>%group_by(player) %>%
  summarise(Weight=mean(p_kg),Assists=mean(ast),tov=mean(tov))%>%arrange(desc(Assists))%>%head()

#198 : 97.5 <Weight<103.5, 3Pts>70583,Offensive rebond<93

P_198<-P_info_all%>% filter(p_kg>=97.5 & p_kg<103.5 & x3pa>7.583 & orb<93)%>%mutate(pos=198)
P_198%>%group_by(player) %>%
  summarise(Weight=mean(p_kg),ThreePts=mean(x3pa),OffRebound=mean(orb))%>%arrange(desc(ThreePts))%>%head()

#203 : >97.5<W<103.5, offensive rebond>93.85

P_203<-P_info_all%>% filter(p_kg>=97.5 & p_kg<103.5 & orb>93.85)%>%mutate(pos=203)
P_203%>%group_by(player) %>%
  summarise(Weight=mean(p_kg),OffRebounds=mean(orb),Points=mean(pts))%>%arrange(desc(OffRebounds))%>%head()


#211 : 97.5<W>103.5, blocking >3.16, steal > 42.27


P_211<-P_info_all%>% filter(p_kg>=97.5 & p_kg<103.5 & blk>3.16 & stl>42.27)%>%mutate(pos=211)
P_211%>%group_by(player) %>%
  summarise(Weight=mean(p_kg),Block=mean(blk),Steal=mean(stl))%>%arrange(desc(Block))%>%head()

#213: w>103.5,blocking >3.16, steal <42.27

P_213<-P_info_all%>% filter(p_kg>103.5 & blk>3.16 & stl<42.27)%>%mutate(pos=213)
P_213%>%group_by(Name) %>%
  summarise(Weight=mean(p_kg),Block=mean(blk),Steal=mean(stl))%>%arrange(desc(Block))%>%head()

New_pos<-rbind(P_206,P_190)
New_pos<-rbind(New_pos,P_185)
New_pos<-rbind(New_pos,P_211)
New_pos<-rbind(New_pos,P_213)
New_pos<-rbind(New_pos,P_196)
New_pos<-rbind(New_pos,P_198)
New_pos<-rbind(New_pos,P_203)

# The classification tree show 7 positions
# based on weight, then Block, 3 points,assit,Offensive rebond and steal
#
#206 : >103 KG, Blocking rate <30 or >85 kg with blocking rate >31.63
#190 : >85.5 Kg, assist <177.8
#185 : <85.5 Kg
#196 : 85.5< Weight <97.5, assist<177, blocking<15.8
#198 : 97.5 <Weight<103.5, 3Pts>70583,Offensive rebond<93
#203 : >97.5<W<103.5, offensive rebond>93.85
#211 : 97.5<W>103.5, blocking >3.16, steal > 42.27
#213: w>103.5,blocking >30.16, steal <42.27

New_pos<-New_pos%>%mutate(posname=ifelse(pos==185,"Wing",""))
New_pos<-New_pos%>%mutate(posname=ifelse(pos==190,"Assist",posname))
New_pos<-New_pos%>%mutate(posname=ifelse(pos==196,"Light",posname))
New_pos<-New_pos%>%mutate(posname=ifelse(pos==198,"3PTS_Shooter",posname))
New_pos<-New_pos%>%mutate(posname=ifelse(pos==203,"OffRebound",posname))
New_pos<-New_pos%>%mutate(posname=ifelse(pos==206,"Big or Block",posname))
New_pos<-New_pos%>%mutate(posname=ifelse(pos==211,"Stealer",posname))
New_pos<-New_pos%>%mutate(posname=ifelse(pos==213,"Blocker",posname))
colnames(New_pos)[colnames(New_pos)=="Group.1"] <- "Player_name"
sum(is.na (New_pos))



New_pos %>%
  ggplot(aes(posname, p_kg, color=pos)) +
  geom_violin() +
  ggtitle("Weight distribution by position") +
  stat_summary(fun.y=mean, geom="point", shape=8, size=6) +
  geom_point() +
  geom_hline(aes(yintercept = mean(New_pos$p_kg, na.rm=T), linetype = "Average Weight NBA players"),
             col = "red",
             alpha = 0.5) +
  geom_hline(aes(yintercept = 90, linetype = "Average American male"),
             col = "blue",
             alpha = 0.5) +
  theme(legend.position="bottom")

New_pos %>%
  ggplot(aes(posname, p_cm, color=pos)) +
  geom_violin() +
  ggtitle("Height distribution by position") +
  stat_summary(fun.y=mean, geom="point", shape=8, size=6) +
  geom_point() +
  geom_hline(aes(yintercept = mean(New_pos$p_cm, na.rm=T), linetype = "Average Height NBA players"),
             col = "red",
             alpha = 0.5) +
  geom_hline(aes(yintercept = 90, linetype = "Average American male"),
             col = "blue",
             alpha = 0.5) +
  theme(legend.position="bottom")
#
New_pos %>%
  ggplot(aes(posname, blk, color=pos)) +
  geom_violin() +
  ggtitle("Block distribution by position") +
  stat_summary(fun.y=mean, geom="point", shape=8, size=6) +
  geom_point() +
  geom_hline(aes(yintercept = mean(New_pos$blk, na.rm=T), linetype = "Average Block NBA players"),
             col = "red",
             alpha = 0.5)  +
  theme(legend.position="bottom")
#

#
New_pos %>%
  ggplot(aes(posname, pts, color=pos)) +
  geom_violin() +
  ggtitle("Pts distribution by position") +
  stat_summary(fun.y=mean, geom="point", shape=8, size=6) +
  geom_point() +
  geom_hline(aes(yintercept = mean(New_pos$pts, na.rm=T), linetype = "Average PTS NBA players"),
             col = "red",
             alpha = 0.5)  +
  theme(legend.position="bottom")

New_pos %>%
  ggplot(aes(posname, x3p, color=pos)) +
  geom_violin() +
  ggtitle("3Pts distribution by position") +
  stat_summary(fun.y=mean, geom="point", shape=8, size=6) +
  geom_point() +
  geom_hline(aes(yintercept = mean(New_pos$x3p, na.rm=T), linetype = "Average 3PTS NBA players"),
             col = "red",
             alpha = 0.5)  +
  theme(legend.position="bottom")

New_pos %>%
  ggplot(aes(posname, x2p, color=pos)) +
  geom_violin() +
  ggtitle("2Pts distribution by position") +
  stat_summary(fun.y=mean, geom="point", shape=8, size=6) +
  geom_point() +
  geom_hline(aes(yintercept = mean(New_pos$x2p, na.rm=T), linetype = "Average 2PTS NBA players"),
             col = "red",
             alpha = 0.5)  +
  theme(legend.position="bottom")

New_pos %>%
  ggplot(aes(posname, stl, color=pos)) +
  geom_violin() +
  ggtitle("Steal distribution by position") +
  stat_summary(fun.y=mean, geom="point", shape=8, size=6) +
  geom_point() +
  geom_hline(aes(yintercept = mean(New_pos$stl, na.rm=T), linetype = "Average Steal NBA players"),
             col = "red",
             alpha = 0.5)  +
  theme(legend.position="bottom")

# ----------------------------------------------------------------------------------------------------
#
#  Machine Learning Prediction
#
# ----------------------------------------------------------------------------------------------------
#


#
New_pos %>%
  ggplot(aes(posname, pts, color=pos)) +
  geom_violin() +
  ggtitle("Pts distribution by position") +
  stat_summary(fun.y=mean, geom="point", shape=8, size=6) +
  geom_point() +
  geom_hline(aes(yintercept = mean(New_pos$pts, na.rm=T), linetype = "Average PTS NBA players"),
             col = "red",
             alpha = 0.5)  +
  theme(legend.position="bottom")




# One variable with 3 others and total 4 variables.

pairs(~fg+fga+x3p+x2p+fta+orb+ast+stl+blk,data = New_pos,
      main = "Positon Matrix")

#
#   Average by New Position  group
#

P1<-aggregate(New_pos,by=list(New_pos$posname),mean)
#P1<-P1%>%select(-Name,-posname,-Player_name)
print(P1)

#
#  Assist 
#

P<-New_pos%>%filter(pos==185)
#
print(P)
boxplot(P$fga,P$x3pa,P$x2p,P$fta,P$orb,P$ast,P$stl,P$blk,names=c("fga","3pts","2pts","fta","orb","ast","stl","blk"),main='WingMan')

# One variable with 3 others and total 4 variables.

pairs(~fg+fga+x3p+x2p+fta+orb+ast+stl+blk,data = P,
      main = "Assist Positon Matrix")



P<-New_pos%>%filter(pos==190)
#
print(P)
boxplot(P$fga,P$x3pa,P$x2p,P$fta,P$orb,P$ast,P$stl,P$blk,names=c("fga","3pts","2pts","fta","orb","ast","stl","blk"),main='Assist')
pairs(~fg+fga+x3p+x2p+fta+orb+ast+stl+blk,data = P,
      main = "Positon Matrix")

P<-New_pos%>%filter(pos==196)
#
print(P)
boxplot(P$fga,P$x3pa,P$x2p,P$fta,P$orb,P$ast,P$stl,P$blk,names=c("fga","3pts","2pts","fta","orb","ast","stl","blk"),main='Light')
pairs(~fg+fga+x3p+x2p+fta+orb+ast+stl+blk,data = P,
      main = "Positon Matrix")
P<-New_pos%>%filter(pos==198)
#
print(P)
boxplot(P$fga,P$x3pa,P$x2p,P$fta,P$orb,P$ast,P$stl,P$blk,names=c("fga","3pts","2pts","fta","orb","ast","stl","blk"),main='Group Shooter')
pairs(~fg+fga+x3p+x2p+fta+orb+ast+stl+blk,data = P,
      main = "Positon Matrix")
P<-New_pos%>%filter(pos==203)
#
print(P)
boxplot(P$fga,P$x3pa,P$x2p,P$fta,P$orb,P$ast,P$stl,P$blk,names=c("fga","3pts","2pts","fta","orb","ast","stl","blk"),main='OffRebound')
pairs(~fg+fga+x3p+x2p+fta+orb+ast+stl+blk,data = P,
      main = "Positon Matrix")
P<-New_pos%>%filter(pos==206)
#
print(P)
boxplot(P$fga,P$x3pa,P$x2p,P$fta,P$orb,P$ast,P$stl,P$blk,names=c("fga","3pts","2pts","fta","orb","ast","stl","blk"),main='Big Block')
pairs(~fg+fga+x3p+x2p+fta+orb+ast+stl+blk,data = P,
      main = "Positon Matrix")
P<-New_pos%>%filter(pos==211)
#
print(P)
boxplot(P$fga,P$x3pa,P$x2p,P$fta,P$orb,P$ast,P$stl,P$blk,names=c("fga","3pts","2pts","fta","orb","ast","stl","blk"),main='Stealer')
pairs(~fg+fga+x3p+x2p+fta+orb+ast+stl+blk,data = P,
      main = "Positon Matrix")

P<-New_pos%>%filter(pos==213)
#
print(P)
#
boxplot(P$fga,P$x3pa,P$x2p,P$fta,P$orb,P$ast,P$stl,P$blk,names=c("fga","3pts","2pts","fta","orb","ast","stl","blk"),main='Blocker')
pairs(~fg+fga+x3p+x2p+fta+orb+ast+stl+blk,data = P,
      main = "Positon Matrix")
#
#  staring a new selection using minutes played by 36 mn
#
nba<-NBA %>% group_by(player)%>%
  select(player,fg,fga,x3p,x3pa,x2p,x2pa,ft,fta,orb,drb,trb,ast,stl,blk,tov,pf,pts,mp)
sum(is.na (nba))
nba<-na.omit(nba)
print(nba)


nba<-nba%>%
  rowwise() %>%
  mutate(ORpG = orb / (mp/36),
         DRpG = drb / (mp/36),
         RpG = trb / (mp/36),
         ApG = ast / (mp/36),
         SpG = stl / (mp/36),
         BpG = blk / (mp/36),
         TpG = tov / (mp/36),
         PpG = pts / (mp/36))%>%
        select(player,ORpG,DRpG,RpG,ApG,SpG,BpG,TpG,PpG)


nba<-nba%>%group_by(player)%>%summarise(ORP=mean(ORpG),DRP=mean(DRpG),RPG=mean(RpG),APG=mean(ApG),SPG=mean(SpG),BPG=mean(BpG),TPG=mean(TpG),PPG=mean(PpG))%>%
select(player,ORP,DRP,RPG,APG,SPG,BPG,TPG,PPG)
#
sum(is.na (nba))
nba<-na.omit(nba)
print(nba)



stat.36 <- nba[, grep("PPG", names(nba))]
head(stat.36)
x36 <- data.frame("name" = nba$player, "stat.36" = nba$PPG)
pct <- ecdf(x36$stat.36)
pctile <- round(pct(10)*100, 1)
print(paste(pctile, "th Percentile"))
simplyr <- x36[which.min(abs(10-x36$stat.36)),]
print(paste(simplyr$name, "-", round(simplyr$stat.36,1)))

ggplot(x36, aes(stat.36)) + stat_ecdf(geom = "step")+
labs(title="Empirical Cumulative \n Density Function",
    y = "F(PPM)", x="Points per Game")+
   theme_classic()

ggplot(nba, aes(PPG)) + stat_ecdf(geom = "step")+
  labs(title="Empirical Cumulative \n Density Function",
       y = "F(PPM)", x="Points per Game")+
  theme_classic()

ggplot(nba, aes(APG)) + stat_ecdf(geom = "step")+
  labs(title="Empirical Cumulative \n Density Function",
       y = "F(Assist)", x="Assists per Game")+
  theme_classic()
simplyr <- nba[which.min(abs(3-nba$APG)),]
print(paste(simplyr$player, "-", round(simplyr$APG,1)))


# -------------------------------------------------------------------------------------------
#                            PER prediction
#  without PER Formula, average PER =15
# -------------------------------------------------------------------------------------------
#

nba_test<- NBA20%>%group_by(per)%>%filter(pm>1500)
# Validation set will be 10% of Player data
set.seed(1)
test_index <- createDataPartition(y = nba_test$per,times=1, p = 0.1, list = FALSE)
edx <- nba_test[-test_index,]
temp <- nba_test[test_index,]
# Make sure Player and Pos in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "player") %>%
  semi_join(edx, by = "pos")

validation <- temp %>% 
  semi_join(edx, by = "pos")
# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)


RMSE <- function(true_per, predicted_per){
  sqrt(mean((true_per-predicted_per)^2,na.rm=T))
}

head(edx) 
summary(edx)

vec_per <- as.vector(edx$per)
unique(vec_per) 
#
# PER distribution by player
#
vec_per <- vec_per[vec_per != 0]
vec_per <- factor(vec_per)
qplot(vec_per) +
  ggtitle("PER Ratings' Distribution")
#
# distribution PER by Position
#

edx %>% count(pos) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Players")

#♠
# average PER of the testing set , NBA average=15
#
mu <- mean(edx$per)  
mu
#
#
# baseline Model: just the mean 
#
baseline_rmse <- RMSE(validation$per,mu)
## Test results based on simple prediction
baseline_rmse
#
rmse_results <- data_frame(method = "Using PER mean only", RMSE = baseline_rmse)
rmse_results
#
#  Field Goal per minutes effect only
#
fg_avgs_norm <- edx %>% 
  group_by(fg_pm) %>% 
  summarize(b_fg = mean(fg_pm - mu))
fg_avgs_norm %>% qplot(b_fg, geom ="histogram", bins = 20, data = ., color = I("black"))
mu_fg <- mean(edx$fg_pm) 
FG_rmse <- RMSE(validation$per,mu_fg)
rmse_results <- data_frame(method = "Using FG mean only", RMSE = FG_rmse)
rmse_results
#

#
#  pts/mn effect only
#
pts_avgs_norm <- edx %>% 
  group_by(pts_pm) %>% 
  summarize(b_pts = mean(per-pts_pm))
pts_avgs_norm %>% qplot(b_pts, geom ="histogram", bins = 20, data = ., color = I("black"))
mu_pts<- mean(edx$pts_pm)
PTS_rmse <- RMSE(validation$per,mu_pts)
rmse_results <- data_frame(method = "Using PTS mean only", RMSE = PTS_rmse)
rmse_results

#
#   orb effect only
#
orb_avgs_norm <- edx %>% 
  group_by(orb_pm) %>% 
  summarize(b_orb = mean(per-orb_pm))
orb_avgs_norm %>% qplot(b_orb, geom ="histogram", bins = 20, data = ., color = I("black"))
pts_avgs_norm %>% qplot(b_pts, geom ="histogram", bins = 20, data = ., color = I("black"))
mu_orb<- mean(edx$orb_pm)
ORB_rmse <- RMSE(validation$per,mu_orb)
rmse_results <- data_frame(method = "Using  ORB mean only", RMSE = ORB_rmse)
rmse_results
#
#   drb effect only
#
drb_avgs_norm <- edx %>% 
  group_by(drb_pm) %>% 
  summarize(b_drb = mean(per-drb_pm))
drb_avgs_norm %>% qplot(b_drb, geom ="histogram", bins = 20, data = ., color = I("black"))
mu_drb<- mean(edx$drb_pm)
DRB_rmse <- RMSE(validation$per,mu_drb)
rmse_results <- data_frame(method = "Using  DRB mean only", RMSE = DRB_rmse)
rmse_results

#
#   trb effect only
#
trb_avgs_norm <- edx %>% 
  group_by(trb_pm) %>% 
  summarize(b_trb = mean(per - trb_pm))
trb_avgs_norm %>% qplot(b_trb, geom ="histogram", bins = 20, data = ., color = I("black"))
mu_trb<- mean(edx$trb_pm)
TRB_rmse <- RMSE(validation$per,mu_trb)
rmse_results <- data_frame(method = "Using  TRB mean only", RMSE = TRB_rmse)
rmse_results

#
#   ast effect only
#
ast_avgs_norm <- edx %>% 
  group_by(ast_pm) %>% 
  summarize(b_ast = mean(per-ast_pm))
ast_avgs_norm %>% qplot(b_ast, geom ="histogram", bins = 20, data = ., color = I("black"))
mu_ast<- mean(edx$ast_pm)
AST_rmse <- RMSE(validation$per,mu_ast)
rmse_results <- data_frame(method = "Using  TRB mean only", RMSE = AST_rmse)
rmse_results

#
#   stl effect only
#
stl_avgs_norm <- edx %>% 
  group_by(stl_pm) %>% 
  summarize(b_stl = mean(per-stl_pm))
stl_avgs_norm %>% qplot(b_stl, geom ="histogram", bins = 20, data = ., color = I("black"))
mu_stl<- mean(edx$stl_pm)
STL_rmse <- RMSE(validation$per,mu_stl)
rmse_results <- data_frame(method = "Using STL mean only", RMSE = STL_rmse)
rmse_results

#
#   blk effect only
#
blk_avgs_norm <- edx %>% 
  group_by(blk_pm) %>% 
  summarize(b_blk = mean(per-blk_pm))
blk_avgs_norm %>% qplot(b_blk, geom ="histogram", bins = 20, data = ., color = I("black"))
mu_blk<- mean(edx$blk_pm)
BLK_rmse <- RMSE(validation$per,mu_blk)
rmse_results <- data_frame(method = "Using BLK mean only", RMSE = BLK_rmse)
rmse_results

#
#   tov effect only
#
tov_avgs_norm <- edx %>% 
  group_by(tov_pm) %>% 
  summarize(b_tov = mean(per-tov_pm))
tov_avgs_norm %>% qplot(b_tov, geom ="histogram", bins = 20, data = ., color = I("black"))
mu_tov<- mean(edx$stl_pm)
TOV_rmse <- RMSE(validation$per,mu_tov)
rmse_results <- data_frame(method = "Using TOV mean only", RMSE = TOV_rmse)
rmse_results



#
#   x3p effect only
#
x3p_avgs_norm <- edx %>% 
  group_by(x3p_pm) %>% 
  summarize(b_x3p = mean(per-x3p_pm ))
x3p_avgs_norm %>% qplot(b_x3p, geom ="histogram", bins = 20, data = ., color = I("black"))
mu_x3p<- mean(edx$x3p_pm)
X3P_rmse <- RMSE(validation$per,mu_stl)
rmse_results <- data_frame(method = "Using X3P mean only", RMSE = X3P_rmse)
rmse_results

#
#   x2p effect only
#
x2p_avgs_norm <- edx %>% 
  group_by(x2p_pm) %>% 
  summarize(b_x2p = mean(per - x2p_pm))
x2p_avgs_norm %>% qplot(b_x2p, geom ="histogram", bins = 20, data = ., color = I("black"))
mu_x2p<- mean(edx$stl_pm)
X2P_rmse <- RMSE(validation$per,mu_x2p)
rmse_results <- data_frame(method = "Using X2P mean only", RMSE = X2P_rmse)
rmse_results





# ---------------------------------------------------------------------------------
#              cumulative
#
# FG + Pts effect
#
pts_avgs_norm <- edx %>% 
  left_join(fg_avgs_norm, by='fg_pm') %>%
  group_by(pts_pm) %>%
  summarize(b_pts = mean(per - mu ))
pts_avgs_norm %>% qplot(b_pts, geom ="histogram", bins = 30, data = ., color = I("black"))

#
#  FG+PTS+Orb effect
#
orb_avgs_norm <- edx %>% 
  left_join(fg_avgs_norm, by='fg_pm') %>%
  left_join(pts_avgs_norm, by='pts_pm') %>%
  group_by(orb_pm) %>%
  summarize(b_orb = mean(per - mu - b_pts))
orb_avgs_norm %>% qplot(b_orb, geom ="histogram", bins = 30, data = ., color = I("black"))


# "---------------------------------------"

# FG effects only 
predicted_per_fg_norm <- validation %>% 
  left_join(fg_avgs_norm, by='fg_pm') %>%
  mutate(pred = mu + b_fg) 
model_1_rmse <- RMSE(validation$per,predicted_per_fg_norm$pred)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="FG Effect Model",  
                                     RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()


#F Orb effects only 
predicted_per_orb_norm <- validation %>% 
  left_join(orb_avgs_norm, by='orb_pm') %>%
  mutate(pred = mu + b_orb) 
model_2_rmse <- RMSE(validation$per,predicted_per_fg_norm$pred)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Orb Effect Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()

#
# pts effect
# Use test set,join movie averages & user averages
# Prediction equals the mean with user effect b_u & movie effect b_i
#
predicted_per_orb_norm <- validation %>% 
  left_join(fg_avgs_norm, by='fg') %>%
  left_join(pts_avgs_norm, by='pts') %>%
  mutate(pred = mu + b_i + b_u) 
# test and save rmse results 
model_3_rmse <- RMSE(validation$per,predicted_per_orb_norm$pred)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Fg and Pts Effect Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()


#
#
#

# lambda is a tuning parameter
# Use cross-validation to choose it.
lambdas <- seq(4, 12, 0.5)
# For each lambda,find b_orb & b_fg, followed by PER prediction & testing
# note:the below code could take some time 
#

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$per)
  
  b_fg <- edx %>% 
    group_by(fg_pm) %>%
    summarize(b_fg = sum(per - mu)/(n()+l))
  
  b_pts <- edx %>% 
    left_join(b_fg, by="fg_pm") %>%
    group_by(pts_pm) %>%
    summarize(b_pts = sum(per - b_fg - mu)/(n()+l))
  
  
  b_orb <- edx %>% 
    left_join(b_fg, by="fg_pm") %>%
    left_join(b_pts, by = "pts_pm") %>%
    group_by(orb_pm) %>%
    summarize(b_orb = sum(per - b_fg -b_pts - mu)/(n()+l))
  
  
  b_drb <- edx %>% 
    left_join(b_fg, by="fg_pm") %>%
    left_join(b_pts, by = "pts_pm") %>%
    left_join(b_orb, by = "orb_pm") %>%
    group_by(drb_pm) %>%
    summarize(b_drb = sum(per - b_fg -b_pts-b_orb - mu)/(n()+l))
  
  
  b_stl <- edx %>% 
    left_join(b_fg, by="fg_pm") %>%
    left_join(b_pts, by = "pts_pm") %>%
    left_join(b_orb, by = "orb_pm") %>%
    left_join(b_drb, by = "drb_pm") %>%
    group_by(stl_pm) %>%
    summarize(b_stl = sum(per - b_fg -b_pts-b_orb-b_drb - mu)/(n()+l))
  
  b_blk <- edx %>% 
    left_join(b_fg, by="fg_pm") %>%
    left_join(b_pts, by = "pts_pm") %>%
    left_join(b_orb, by = "orb_pm") %>%
    left_join(b_drb, by = "drb_pm") %>%
    left_join(b_stl, by = "stl_pm") %>%
    group_by(blk_pm) %>%
    summarize(b_blk = sum(per - b_fg -b_pts-b_orb-b_drb - mu-b_stl)/(n()+l))
  
  
  b_tov <- edx %>% 
    left_join(b_fg, by="fg_pm") %>%
    left_join(b_pts, by = "pts_pm") %>%
    left_join(b_orb, by = "orb_pm") %>%
    left_join(b_drb, by = "drb_pm") %>%
    left_join(b_stl, by = "stl_pm") %>%
    left_join(b_blk, by = "blk_pm") %>%
    group_by(tov_pm) %>%
    summarize(b_tov = sum(per - b_fg -b_pts-b_orb-b_drb - mu-b_stl-b_blk)/(n()+l))
  
  b_pf <- edx %>% 
    left_join(b_fg, by="fg_pm") %>%
    left_join(b_pts, by = "pts_pm") %>%
    left_join(b_orb, by = "orb_pm") %>%
    left_join(b_drb, by = "drb_pm") %>%
    left_join(b_stl, by = "stl_pm") %>%
    left_join(b_blk, by = "blk_pm") %>%
    left_join(b_tov, by = "tov_pm") %>%
    group_by(x3p_pm) %>%
    summarize(b_pf = sum(per - b_fg -b_pts-b_orb-b_drb - mu-b_stl-b_blk-b_tov)/(n()+l))
  
  
  b_pd <- edx %>% 
    left_join(b_fg, by="fg_pm") %>%
    left_join(b_pts, by = "pts_pm") %>%
    left_join(b_orb, by = "orb_pm") %>%
    left_join(b_drb, by = "drb_pm") %>%
    left_join(b_stl, by = "stl_pm") %>%
    left_join(b_blk, by = "blk_pm") %>%
    left_join(b_tov, by = "tov_pm") %>%
    left_join(b_pf, by = "x3p_pm") %>%
    group_by(x2p_pm) %>%
    summarize(b_pd = sum(per - b_fg -b_pts-b_orb-b_drb - mu-b_stl-b_blk-b_tov-b_pf)/(n()+l))
  
  predicted_per <- validation %>% 
    left_join(b_fg, by = "fg_pm") %>%
    left_join(b_pts, by = "pts_pm") %>%
    left_join(b_orb, by = "orb_pm") %>%
    left_join(b_drb, by = "drb_pm") %>%
    left_join(b_stl, by = "stl_pm") %>%
    left_join(b_blk, by = "blk_pm") %>%
    left_join(b_tov, by = "tov_pm") %>%
    left_join(b_pf, by = "x3p_pm") %>%
    left_join(b_pd, by = "x2p_pm") %>%
    mutate(pred = mu + b_fg + b_pts+b_orb+b_drb+b_stl+b_blk+b_tov+b_pf+b_pd) %>%
    .$pred
  
  return(RMSE(validation$per,predicted_per))
})
# Plot rmses vs lambdas to select the optimal lambda
qplot(lambdas, rmses)  
lambda <- lambdas[which.min(rmses)]
lambda

#
# Compute regularized estimates of b_fg using lambda
#
r_fg <- edx %>% 
  group_by(fg_pm) %>% 
  summarize(r_fg = sum(per - mu)/(n()+lambda), n_fg = n())


# Compute regularized estimates of b_u using lambda
r_pts <- edx %>% 
  left_join(r_fg, by='fg_pm') %>%
  group_by(pts_pm) %>%
  summarize(r_pts = sum(per - mu - r_fg)/(n()+lambda), n_pts = n())

r_orb <- edx %>% 
  left_join(r_fg, by="fg_pm") %>%
  left_join(r_pts, by = "pts_pm") %>%
  group_by(orb_pm) %>%
  summarize(r_orb = sum(per - r_fg -r_pts - mu)/(n()+lambda), n_orb = n())


r_drb <- edx %>% 
  left_join(r_fg, by="fg_pm") %>%
  left_join(r_pts, by = "pts_pm") %>%
  left_join(r_orb, by = "orb_pm") %>%
  group_by(drb_pm) %>%
  summarize(r_drb = sum(per - r_fg -r_pts -r_orb - mu)/(n()+lambda), n_drb = n())

r_stl <- edx %>% 
  left_join(r_fg, by="fg_pm") %>%
  left_join(r_pts, by = "pts_pm") %>%
  left_join(r_orb, by = "orb_pm") %>%
  left_join(r_drb, by = "drb_pm") %>%
  group_by(stl_pm) %>%
  summarize(r_stl = sum(per - r_fg -r_pts-r_orb-r_drb - mu)/(n()+lambda), n_stl = n())

r_blk <- edx %>% 
  left_join(r_fg, by="fg_pm") %>%
  left_join(r_pts, by = "pts_pm") %>%
  left_join(r_orb, by = "orb_pm") %>%
  left_join(r_drb, by = "drb_pm") %>%
  left_join(r_stl, by = "stl_pm") %>%
  group_by(blk_pm) %>%
  summarize(r_blk = sum(per - r_fg -r_pts-r_orb-r_drb - mu-r_stl)/(n()+lambda), n_blk = n())


r_tov <- edx %>% 
  left_join(r_fg, by="fg_pm") %>%
  left_join(r_pts, by = "pts_pm") %>%
  left_join(r_orb, by = "orb_pm") %>%
  left_join(r_drb, by = "drb_pm") %>%
  left_join(r_stl, by = "stl_pm") %>%
  left_join(r_blk, by = "blk_pm") %>%
  group_by(tov_pm) %>%
  summarize(r_tov = sum(per - r_fg -r_pts-r_orb-r_drb - mu-r_stl-r_blk)/(n()+lambda), n_tov = n())

r_pf <- edx %>% 
  left_join(r_fg, by="fg_pm") %>%
  left_join(r_pts, by = "pts_pm") %>%
  left_join(r_orb, by = "orb_pm") %>%
  left_join(r_drb, by = "drb_pm") %>%
  left_join(r_stl, by = "stl_pm") %>%
  left_join(r_blk, by = "blk_pm") %>%
  left_join(r_tov, by = "tov_pm") %>%
  group_by(x3p_pm) %>%
  summarize(r_pf = sum(per - r_fg -r_pts-r_orb-r_drb - mu-r_stl-r_blk-r_tov)/(n()+lambda), n_pf = n())


r_pd <- edx %>% 
  left_join(r_fg, by="fg_pm") %>%
  left_join(r_pts, by = "pts_pm") %>%
  left_join(r_orb, by = "orb_pm") %>%
  left_join(r_drb, by = "drb_pm") %>%
  left_join(r_stl, by = "stl_pm") %>%
  left_join(r_blk, by = "blk_pm") %>%
  left_join(r_tov, by = "tov_pm") %>%
  left_join(r_pf, by = "x3p_pm") %>%
  group_by(x2p_pm) %>%
  summarize(r_pd = sum(per - r_fg -r_pts-r_orb-r_drb - mu-r_stl-r_blk-r_tov-r_pf)/(n()+lambda), n_pd = n())

predicted_per_reg <- validation %>% 
  left_join(r_fg, by = "fg_pm") %>%
  left_join(r_pts, by = "pts_pm") %>%
  left_join(r_orb, by = "orb_pm") %>%
  left_join(r_drb, by = "drb_pm") %>%
  left_join(r_stl, by = "stl_pm") %>%
  left_join(r_blk, by = "blk_pm") %>%
  left_join(r_tov, by = "tov_pm") %>%
  left_join(r_pf, by = "x3p_pm") %>%
  left_join(r_pd, by = "x2p_pm") %>%
  mutate(pred = mu + r_fg + r_pts + r_drb + r_stl + r_blk + r_tov + r_pf + r_pd) %>% .$pred


# Test and save results
model_R_rmse <- RMSE(validation$per,predicted_per_reg)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Model",  
                                     RMSE = model_R_rmse ))
rmse_results %>% knitr::kable()
#
#
#  apply coefficient 
#  
#



predicted_per_reg <- validation %>% 
  left_join(r_fg, by = "fg_pm") %>%
  left_join(r_pts, by = "pts_pm") %>%
  left_join(r_orb, by = "orb_pm") %>%
  left_join(r_drb, by = "drb_pm") %>%
  left_join(r_stl, by = "stl_pm") %>%
  left_join(r_blk, by = "blk_pm") %>%
  left_join(r_tov, by = "tov_pm") %>%
  left_join(r_pf, by = "x3p_pm") %>%
  left_join(r_pd, by = "x2p_pm") %>%
  mutate(pred = mu + r_fg + r_pts + r_drb + r_stl + r_blk + r_tov + r_pf + r_pd) %>% .$pred


model_R_rmse <- RMSE(validation$per,predicted_per_reg)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Model",  
                                     RMSE = model_R_rmse ))
rmse_results %>% knitr::kable()



# ---------------------------------------------------- SCALED ----------------------------------------------------------
# edx scaled
#  same with scaling
# -----------------------------------------------------SCALED ----------------------------------------------------------


nba_test<- NBA20%>%group_by(per)%>%filter(mp>1500)
nba_test[, -c(1:3)] <- scale(nba_test[, -c(1:3)])

# Validation set will be 10% of Player data
set.seed(1)
test_index <- createDataPartition(y = nba_test$per,times=1, p = 0.1, list = FALSE)
edx <- nba_test[-test_index,]
temp <- nba_test[test_index,]
# Make sure Player and Pos in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "player") %>%
  semi_join(edx, by = "pos")

validation <- temp %>% 
  semi_join(edx, by = "pos")
# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)


# average PER of the testing set , NBA average=15
#
mu <- mean(edx$per)  
mu
#
#
# baseline Model: just the mean 
#
baseline_rmse <- RMSE(validation$per,mu)
## Test results based on simple prediction
baseline_rmse
#
rmse_results <- data_frame(method = "Using PER mean only", RMSE = baseline_rmse)
rmse_results
#
#  Field Goal per minutes effect only
#
fg_avgs_norm <- edx %>% 
  group_by(fg_pm) %>% 
  summarize(b_fg = mean(fg_pm - mu))
fg_avgs_norm %>% qplot(b_fg, geom ="histogram", bins = 20, data = ., color = I("black"))
mu_fg <- mean(edx$fg_pm) 
FG_rmse <- RMSE(validation$per,mu_fg)
rmse_results <- data_frame(method = "Using FG mean only", RMSE = FG_rmse)
rmse_results
#

#
#  pts/mn effect only
#
pts_avgs_norm <- edx %>% 
  group_by(pts_pm) %>% 
  summarize(b_pts = mean(per-pts_pm))
pts_avgs_norm %>% qplot(b_pts, geom ="histogram", bins = 20, data = ., color = I("black"))
mu_pts<- mean(edx$pts_pm)
PTS_rmse <- RMSE(validation$per,mu_pts)
rmse_results <- data_frame(method = "Using PTS mean only", RMSE = PTS_rmse)
rmse_results

#
#   orb effect only
#
orb_avgs_norm <- edx %>% 
  group_by(orb_pm) %>% 
  summarize(b_orb = mean(per-orb_pm))
orb_avgs_norm %>% qplot(b_orb, geom ="histogram", bins = 20, data = ., color = I("black"))
pts_avgs_norm %>% qplot(b_pts, geom ="histogram", bins = 20, data = ., color = I("black"))
mu_orb<- mean(edx$orb_pm)
ORB_rmse <- RMSE(validation$per,mu_orb)
rmse_results <- data_frame(method = "Using  ORB mean only", RMSE = ORB_rmse)
rmse_results
#
#   drb effect only
#
drb_avgs_norm <- edx %>% 
  group_by(drb_pm) %>% 
  summarize(b_drb = mean(per-drb_pm))
drb_avgs_norm %>% qplot(b_drb, geom ="histogram", bins = 20, data = ., color = I("black"))
mu_drb<- mean(edx$drb_pm)
DRB_rmse <- RMSE(validation$per,mu_drb)
rmse_results <- data_frame(method = "Using  DRB mean only", RMSE = DRB_rmse)
rmse_results

#
#   trb effect only
#
trb_avgs_norm <- edx %>% 
  group_by(trb_pm) %>% 
  summarize(b_trb = mean(per - trb_pm))
trb_avgs_norm %>% qplot(b_trb, geom ="histogram", bins = 20, data = ., color = I("black"))
mu_trb<- mean(edx$trb_pm)
TRB_rmse <- RMSE(validation$per,mu_trb)
rmse_results <- data_frame(method = "Using  TRB mean only", RMSE = TRB_rmse)
rmse_results

#
#   ast effect only
#
ast_avgs_norm <- edx %>% 
  group_by(ast_pm) %>% 
  summarize(b_ast = mean(per-ast_pm))
ast_avgs_norm %>% qplot(b_ast, geom ="histogram", bins = 20, data = ., color = I("black"))
mu_ast<- mean(edx$ast_pm)
AST_rmse <- RMSE(validation$per,mu_ast)
rmse_results <- data_frame(method = "Using  TRB mean only", RMSE = AST_rmse)
rmse_results

#
#   stl effect only
#
stl_avgs_norm <- edx %>% 
  group_by(stl_pm) %>% 
  summarize(b_stl = mean(per-stl_pm))
stl_avgs_norm %>% qplot(b_stl, geom ="histogram", bins = 20, data = ., color = I("black"))
mu_stl<- mean(edx$stl_pm)
STL_rmse <- RMSE(validation$per,mu_stl)
rmse_results <- data_frame(method = "Using STL mean only", RMSE = STL_rmse)
rmse_results

#
#   blk effect only
#
blk_avgs_norm <- edx %>% 
  group_by(blk_pm) %>% 
  summarize(b_blk = mean(per-blk_pm))
blk_avgs_norm %>% qplot(b_blk, geom ="histogram", bins = 20, data = ., color = I("black"))
mu_blk<- mean(edx$blk_pm)
BLK_rmse <- RMSE(validation$per,mu_blk)
rmse_results <- data_frame(method = "Using BLK mean only", RMSE = BLK_rmse)
rmse_results

#
#   tov effect only
#
tov_avgs_norm <- edx %>% 
  group_by(tov_pm) %>% 
  summarize(b_tov = mean(per-tov_pm))
tov_avgs_norm %>% qplot(b_tov, geom ="histogram", bins = 20, data = ., color = I("black"))
mu_tov<- mean(edx$stl_pm)
TOV_rmse <- RMSE(validation$per,mu_tov)
rmse_results <- data_frame(method = "Using TOV mean only", RMSE = TOV_rmse)
rmse_results



#
#   x3p effect only
#
x3p_avgs_norm <- edx %>% 
  group_by(x3p_pm) %>% 
  summarize(b_x3p = mean(per-x3p_pm ))
x3p_avgs_norm %>% qplot(b_x3p, geom ="histogram", bins = 20, data = ., color = I("black"))
mu_x3p<- mean(edx$x3p_pm)
X3P_rmse <- RMSE(validation$per,mu_stl)
rmse_results <- data_frame(method = "Using X3P mean only", RMSE = X3P_rmse)
rmse_results

#
#   x2p effect only
#
x2p_avgs_norm <- edx %>% 
  group_by(x2p_pm) %>% 
  summarize(b_x2p = mean(per - x2p_pm))
x2p_avgs_norm %>% qplot(b_x2p, geom ="histogram", bins = 20, data = ., color = I("black"))
mu_x2p<- mean(edx$stl_pm)
X2P_rmse <- RMSE(validation$per,mu_x2p)
rmse_results <- data_frame(method = "Using X2P mean only", RMSE = X2P_rmse)
rmse_results





# ---------------------------------------------------------------------------------
#              cumulative
#
# FG + Pts effect
#
pts_avgs_norm <- edx %>% 
  left_join(fg_avgs_norm, by='fg_pm') %>%
  group_by(pts_pm) %>%
  summarize(b_pts = mean(per - mu ))
pts_avgs_norm %>% qplot(b_pts, geom ="histogram", bins = 30, data = ., color = I("black"))

#
#  FG+PTS+Orb effect
#
orb_avgs_norm <- edx %>% 
  left_join(fg_avgs_norm, by='fg_pm') %>%
  left_join(pts_avgs_norm, by='pts_pm') %>%
  group_by(orb_pm) %>%
  summarize(b_orb = mean(per - mu - b_pts))
orb_avgs_norm %>% qplot(b_orb, geom ="histogram", bins = 30, data = ., color = I("black"))


# "---------------------------------------"

# FG effects only 
predicted_per_fg_norm <- validation %>% 
  left_join(fg_avgs_norm, by='fg_pm') %>%
  mutate(pred = mu + b_fg) 
model_1_rmse <- RMSE(validation$per,predicted_per_fg_norm$pred)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="FG Effect Model",  
                                     RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()


#F Orb effects only 
predicted_per_orb_norm <- validation %>% 
  left_join(orb_avgs_norm, by='orb_pm') %>%
  mutate(pred = mu + b_orb) 
model_2_rmse <- RMSE(validation$per,predicted_per_fg_norm$pred)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Orb Effect Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()

#
# pts effect
# Use test set,join movie averages & user averages
# Prediction equals the mean with user effect b_u & movie effect b_i
#
predicted_per_orb_norm <- validation %>% 
  left_join(fg_avgs_norm, by='fg') %>%
  left_join(pts_avgs_norm, by='pts') %>%
  mutate(pred = mu + b_i + b_u) 
# test and save rmse results 
model_3_rmse <- RMSE(validation$per,predicted_per_orb_norm$pred)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Fg and Pts Effect Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()


#
#
#

# lambda is a tuning parameter
# Use cross-validation to choose it.
lambdas <- seq(4, 12, 0.5)
# For each lambda,find b_orb & b_fg, followed by PER prediction & testing
# note:the below code could take some time 
#

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$per)
  
  b_fg <- edx %>% 
    group_by(fg_pm) %>%
    summarize(b_fg = sum(per - mu)/(n()+l))
  
  b_pts <- edx %>% 
    left_join(b_fg, by="fg_pm") %>%
    group_by(pts_pm) %>%
    summarize(b_pts = sum(per - b_fg - mu)/(n()+l))
  
  
  b_orb <- edx %>% 
    left_join(b_fg, by="fg_pm") %>%
    left_join(b_pts, by = "pts_pm") %>%
    group_by(orb_pm) %>%
    summarize(b_orb = sum(per - b_fg -b_pts - mu)/(n()+l))
  
  
  b_drb <- edx %>% 
    left_join(b_fg, by="fg_pm") %>%
    left_join(b_pts, by = "pts_pm") %>%
    left_join(b_orb, by = "orb_pm") %>%
    group_by(drb_pm) %>%
    summarize(b_drb = sum(per - b_fg -b_pts-b_orb - mu)/(n()+l))
  
  
  b_stl <- edx %>% 
    left_join(b_fg, by="fg_pm") %>%
    left_join(b_pts, by = "pts_pm") %>%
    left_join(b_orb, by = "orb_pm") %>%
    left_join(b_drb, by = "drb_pm") %>%
    group_by(stl_pm) %>%
    summarize(b_stl = sum(per - b_fg -b_pts-b_orb-b_drb - mu)/(n()+l))
  
  b_blk <- edx %>% 
    left_join(b_fg, by="fg_pm") %>%
    left_join(b_pts, by = "pts_pm") %>%
    left_join(b_orb, by = "orb_pm") %>%
    left_join(b_drb, by = "drb_pm") %>%
    left_join(b_stl, by = "stl_pm") %>%
    group_by(blk_pm) %>%
    summarize(b_blk = sum(per - b_fg -b_pts-b_orb-b_drb - mu-b_stl)/(n()+l))
  
  
  b_tov <- edx %>% 
    left_join(b_fg, by="fg_pm") %>%
    left_join(b_pts, by = "pts_pm") %>%
    left_join(b_orb, by = "orb_pm") %>%
    left_join(b_drb, by = "drb_pm") %>%
    left_join(b_stl, by = "stl_pm") %>%
    left_join(b_blk, by = "blk_pm") %>%
    group_by(tov_pm) %>%
    summarize(b_tov = sum(per - b_fg -b_pts-b_orb-b_drb - mu-b_stl-b_blk)/(n()+l))
  
  b_pf <- edx %>% 
    left_join(b_fg, by="fg_pm") %>%
    left_join(b_pts, by = "pts_pm") %>%
    left_join(b_orb, by = "orb_pm") %>%
    left_join(b_drb, by = "drb_pm") %>%
    left_join(b_stl, by = "stl_pm") %>%
    left_join(b_blk, by = "blk_pm") %>%
    left_join(b_tov, by = "tov_pm") %>%
    group_by(x3p_pm) %>%
    summarize(b_pf = sum(per - b_fg -b_pts-b_orb-b_drb - mu-b_stl-b_blk-b_tov)/(n()+l))
  
  
  b_pd <- edx %>% 
    left_join(b_fg, by="fg_pm") %>%
    left_join(b_pts, by = "pts_pm") %>%
    left_join(b_orb, by = "orb_pm") %>%
    left_join(b_drb, by = "drb_pm") %>%
    left_join(b_stl, by = "stl_pm") %>%
    left_join(b_blk, by = "blk_pm") %>%
    left_join(b_tov, by = "tov_pm") %>%
    left_join(b_pf, by = "x3p_pm") %>%
    group_by(x2p_pm) %>%
    summarize(b_pd = sum(per - b_fg -b_pts-b_orb-b_drb - mu-b_stl-b_blk-b_tov-b_pf)/(n()+l))
  
  predicted_per <- validation %>% 
    left_join(b_fg, by = "fg_pm") %>%
    left_join(b_pts, by = "pts_pm") %>%
    left_join(b_orb, by = "orb_pm") %>%
    left_join(b_drb, by = "drb_pm") %>%
    left_join(b_stl, by = "stl_pm") %>%
    left_join(b_blk, by = "blk_pm") %>%
    left_join(b_tov, by = "tov_pm") %>%
    left_join(b_pf, by = "x3p_pm") %>%
    left_join(b_pd, by = "x2p_pm") %>%
    mutate(pred = mu + b_fg + b_pts+b_orb+b_drb+b_stl+b_blk+b_tov+b_pf+b_pd) %>%
    .$pred
  
  return(RMSE(validation$per,predicted_per))
})
# Plot rmses vs lambdas to select the optimal lambda
qplot(lambdas, rmses)  
lambda <- lambdas[which.min(rmses)]
lambda

#
# Compute regularized estimates of b_fg using lambda
#
r_fg <- edx %>% 
  group_by(fg_pm) %>% 
  summarize(r_fg = sum(per - mu)/(n()+lambda), n_fg = n())


# Compute regularized estimates of b_u using lambda
r_pts <- edx %>% 
  left_join(r_fg, by='fg_pm') %>%
  group_by(pts_pm) %>%
  summarize(r_pts = sum(per - mu - r_fg)/(n()+lambda), n_pts = n())

r_orb <- edx %>% 
  left_join(r_fg, by="fg_pm") %>%
  left_join(r_pts, by = "pts_pm") %>%
  group_by(orb_pm) %>%
  summarize(r_orb = sum(per - r_fg -r_pts - mu)/(n()+lambda), n_orb = n())


r_drb <- edx %>% 
  left_join(r_fg, by="fg_pm") %>%
  left_join(r_pts, by = "pts_pm") %>%
  left_join(r_orb, by = "orb_pm") %>%
  group_by(drb_pm) %>%
  summarize(r_drb = sum(per - r_fg -r_pts -r_orb - mu)/(n()+lambda), n_drb = n())

r_stl <- edx %>% 
  left_join(r_fg, by="fg_pm") %>%
  left_join(r_pts, by = "pts_pm") %>%
  left_join(r_orb, by = "orb_pm") %>%
  left_join(r_drb, by = "drb_pm") %>%
  group_by(stl_pm) %>%
  summarize(r_stl = sum(per - r_fg -r_pts-r_orb-r_drb - mu)/(n()+lambda), n_stl = n())

r_blk <- edx %>% 
  left_join(r_fg, by="fg_pm") %>%
  left_join(r_pts, by = "pts_pm") %>%
  left_join(r_orb, by = "orb_pm") %>%
  left_join(r_drb, by = "drb_pm") %>%
  left_join(r_stl, by = "stl_pm") %>%
  group_by(blk_pm) %>%
  summarize(r_blk = sum(per - r_fg -r_pts-r_orb-r_drb - mu-r_stl)/(n()+lambda), n_blk = n())


r_tov <- edx %>% 
  left_join(r_fg, by="fg_pm") %>%
  left_join(r_pts, by = "pts_pm") %>%
  left_join(r_orb, by = "orb_pm") %>%
  left_join(r_drb, by = "drb_pm") %>%
  left_join(r_stl, by = "stl_pm") %>%
  left_join(r_blk, by = "blk_pm") %>%
  group_by(tov_pm) %>%
  summarize(r_tov = sum(per - r_fg -r_pts-r_orb-r_drb - mu-r_stl-r_blk)/(n()+lambda), n_tov = n())

r_pf <- edx %>% 
  left_join(r_fg, by="fg_pm") %>%
  left_join(r_pts, by = "pts_pm") %>%
  left_join(r_orb, by = "orb_pm") %>%
  left_join(r_drb, by = "drb_pm") %>%
  left_join(r_stl, by = "stl_pm") %>%
  left_join(r_blk, by = "blk_pm") %>%
  left_join(r_tov, by = "tov_pm") %>%
  group_by(x3p_pm) %>%
  summarize(r_pf = sum(per - r_fg -r_pts-r_orb-r_drb - mu-r_stl-r_blk-r_tov)/(n()+lambda), n_pf = n())


r_pd <- edx %>% 
  left_join(r_fg, by="fg_pm") %>%
  left_join(r_pts, by = "pts_pm") %>%
  left_join(r_orb, by = "orb_pm") %>%
  left_join(r_drb, by = "drb_pm") %>%
  left_join(r_stl, by = "stl_pm") %>%
  left_join(r_blk, by = "blk_pm") %>%
  left_join(r_tov, by = "tov_pm") %>%
  left_join(r_pf, by = "x3p_pm") %>%
  group_by(x2p_pm) %>%
  summarize(r_pd = sum(per - r_fg -r_pts-r_orb-r_drb - mu-r_stl-r_blk-r_tov-r_pf)/(n()+lambda), n_pd = n())

predicted_per_reg <- validation %>% 
  left_join(r_fg, by = "fg_pm") %>%
  left_join(r_pts, by = "pts_pm") %>%
  left_join(r_orb, by = "orb_pm") %>%
  left_join(r_drb, by = "drb_pm") %>%
  left_join(r_stl, by = "stl_pm") %>%
  left_join(r_blk, by = "blk_pm") %>%
  left_join(r_tov, by = "tov_pm") %>%
  left_join(r_pf, by = "x3p_pm") %>%
  left_join(r_pd, by = "x2p_pm") %>%
  mutate(pred = mu + r_fg + r_pts + r_drb + r_stl + r_blk + r_tov + r_pf + r_pd) %>% .$pred


# Test and save results
model_R_rmse <- RMSE(validation$per,predicted_per_reg)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Scaled Model",  
                                     RMSE = model_R_rmse ))
rmse_results %>% knitr::kable()
#
#
#  apply coefficient 
#  
#



predicted_per_reg <- validation %>% 
  left_join(r_fg, by = "fg_pm") %>%
  left_join(r_pts, by = "pts_pm") %>%
  left_join(r_orb, by = "orb_pm") %>%
  left_join(r_drb, by = "drb_pm") %>%
  left_join(r_stl, by = "stl_pm") %>%
  left_join(r_blk, by = "blk_pm") %>%
  left_join(r_tov, by = "tov_pm") %>%
  left_join(r_pf, by = "x3p_pm") %>%
  left_join(r_pd, by = "x2p_pm") %>%
  mutate(pred = mu + r_fg + r_pts + r_drb + r_stl*0.5 + r_blk + r_tov*0.5 + r_pf + r_pd) %>% .$pred


model_R_rmse <- RMSE(validation$per,predicted_per_reg)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized SCALED+Coeff Model",  
                                     RMSE = model_R_rmse ))
rmse_results %>% knitr::kable()



