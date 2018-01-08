require(tidyverse)
require(plotly)
require(RColorBrewer)
data <- read.csv("GitHub/NBA_Stats/nba_player_stats_12_13_17.csv",header=T)
data2 <- read.csv("GitHub/NBA_Stats/nba_advanced_stats_12_13_17.csv",header=T)
data <- full_join(data,data2,by="Player")
data2 <- NULL
data$X <- NULL
data$X.1 <- NULL
data <- as_tibble(data)
data %>% 
  ggplot(aes(x=Age,y=MP))+
  geom_point()+
  geom_smooth(method = "lm", formula = y~x)

data %>% 
  ggplot(aes(x=PTS,y=eFG.))+
  geom_point()+
  geom_smooth(method = "lm", formula = y~x)

data %>% 
  ggplot(aes(x=FGA,y=TOV))+
  geom_point()+
  geom_smooth(method = "lm", formula = y~x)

ggplotly(
  data %>% 
    ggplot(aes(x=PTS,y=TOV,label=Player))+
    geom_point()+
    geom_label(aes(label=Player))+
    geom_smooth(method = "lm", formula = y~x)
)

ggplotly(
  data %>% 
    ggplot(aes(x=TOV.,y=VORP,label=Player))+
    geom_point()+
    geom_label(aes(label=Player))+
    geom_smooth(method = "lm", formula = y~x)
)

fit1 <- lm(VORP~.-Player, data=data)
summary(fit1)
fit2 <- lm(VORP~PF, data=data)
summary(fit2)

plot(VORP~PF,data)
abline(fit2, col="red")

ggplotly(
  data %>% 
    ggplot(aes(x=PF,y=VORP,label=Player))+
    geom_point()+
    geom_smooth(method = "lm", formula = y~x)
)

ggplotly(
  data %>% 
    ggplot(aes(x=STL,y=VORP,label=Player))+
    geom_point()+
    geom_smooth(method = "lm", formula = y~x)
)

team_age <- data %>%
  group_by(Tm.x) %>% 
  summarise(mean(Age.x))
team_age <- arrange(team_age,Tm.x)
team_age <- team_age[!team_age$Tm.x == 'TOT',]

team <- read_csv("GitHub/NBA_Stats//nba_team_stats_12_13_17.csv")
team <- as_tibble(team)
team <- arrange(team,Team)
team <- team[c(1:3,5,4,6:nrow(team)),]

data_team <- bind_cols(team,team_age)
colnames(data_team)[17] <- "MeanAge"

# plotting Offensive and Defensive Rating

data_team %>% 
  ggplot(aes(y=`ORtg/A`,x=`DRtg/A`))+
  scale_x_reverse()+
  geom_point(aes(color=MeanAge))+
  scale_colour_gradient2(low = "dark green", mid = "white",
                         high = "red", midpoint = mean(data_team$MeanAge))+
  theme_minimal()+
  geom_text(aes(label=Tm.x),hjust=0, vjust=-1,size=2)+
  xlab("Adjusted Defensive Rating")+
  ylab("Adjusted Offensive Rating")
  

## gonna filter to players with over 100 mins

data <- data %>% filter(MP.x >= 100)
team_age <- data %>%
  group_by(Tm.x) %>% 
  summarise(mean(Age.x))
team_age <- arrange(team_age,Tm.x)
team_age <- team_age[!team_age$Tm.x == 'TOT',]

team <- read_csv("Documents/nba_team_stats_12_13_17.csv")
team <- as_tibble(team)
team <- arrange(team,Team)
team <- team[c(1:3,5,4,6:nrow(team)),]

data_team <- bind_cols(team,team_age)
colnames(data_team)[17] <- "MeanAge"

# plotting Offensive and Defensive Rating

data_team %>% 
  ggplot(aes(y=`ORtg/A`,x=`DRtg/A`,text=MeanAge))+
  scale_x_reverse()+
  geom_point(aes(color=MeanAge), size=4)+
  scale_colour_gradient2(low = "dark green", mid = "white",
                         high = "red", midpoint = mean(data_team$MeanAge))+
  theme_minimal()+
  geom_text(aes(label=Tm.x),hjust=0, vjust=-1,size=4)+
  xlab("Adjusted Defensive Rating")+
  ylab("Adjusted Offensive Rating")

team_names <- team_ratings[,c(2,16)]
