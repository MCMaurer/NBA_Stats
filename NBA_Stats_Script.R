require(tidyverse)
require(ggrepel)
require(ggthemes)
# read in regular and advanced stats csv files, then join them and get rid of redundancy
data <- read.csv("Documents/nba_player_stats_12_13_17.csv",header=T)
data2 <- read.csv("Documents//nba_advanced_stats_12_13_17.csv",header=T)
data <- full_join(data,data2,by="Player")
data2 <- NULL
data$X <- NULL
data$X.1 <- NULL
data <- as_tibble(data)

# let's just use players with over 100 mins
data <- data %>% filter(MP.x >= 100)

# we're gonna find the mean age of each team
team_age <- data %>%
  group_by(Tm.x) %>% 
  summarise(MeanAge=mean(Age.x))
team_age <- arrange(team_age,Tm.x)
team_age <- team_age[!team_age$Tm.x == 'TOT',]

# read in the team-level stats
team <- read_csv("Documents/nba_team_stats_12_13_17.csv")
team <- as_tibble(team)
team <- arrange(team,Team)
team <- team[c(1:3,5,4,6:nrow(team)),]

# put all the team stuff together
data_team <- bind_cols(team,team_age)

# plotting Offensive and Defensive Rating
p <- data_team %>% 
  ggplot(aes(y=`ORtg/A`,x=`DRtg/A`,text=MeanAge))+
  scale_x_reverse()+
  geom_point(aes(color=MeanAge), size=3)+
  scale_colour_gradient2(low="dark green", mid="khaki1",
                         high="red", midpoint=mean(data_team$MeanAge))+
  theme_tufte()+
  theme(text=element_text(family="sans", colour="black"), legend.position=c(0.92,0.75), legend.key.size=unit(0.4,"cm"), legend.background=element_rect(color="black", size=0.2))+
  geom_text_repel(aes(label=Tm.x), size=2.5, direction = "y", point.padding=0.1)+
  xlab("Adjusted Defensive Rating")+
  ylab("Adjusted Offensive Rating")
p

pdf(file = "Documents/NBA_TeamAge_plot.pdf")
p
dev.off()

# now I'm gonna do mean VORP
team_VORP <- data %>%
  group_by(Tm.x) %>% 
  summarise(MeanVORP=mean(VORP))
team_VORP <- arrange(team_VORP,Tm.x)
team_VORP <- team_VORP[!team_VORP$Tm.x == 'TOT',]

data_team <- bind_cols(team,team_VORP)
colnames(data_team)[17] <- "MeanVORP"

# plotting Offensive and Defensive Rating
p <- data_team %>% 
  ggplot(aes(y=`ORtg/A`,x=`DRtg/A`,text=MeanVORP))+
  scale_x_reverse()+
  geom_point(aes(color=MeanVORP), size=3)+
  scale_colour_gradient2(low="dark green", mid="khaki1",
                         high="red", midpoint=mean(data_team$MeanVORP))+
  theme_tufte()+
  theme(text=element_text(family="sans", colour="black"), legend.position=c(0.92,0.75), legend.key.size=unit(0.4,"cm"), legend.background=element_rect(color="black", size=0.2))+
  geom_text_repel(aes(label=Tm.x), size=2.5, direction = "y", point.padding=0.1)+
  xlab("Adjusted Defensive Rating")+
  ylab("Adjusted Offensive Rating")
p

pdf(file = "Documents/NBA_TeamVORP_plot.pdf")
p
dev.off()

# Regression of mean VORP per team vs. net rating
p <- data_team %>% 
  ggplot(aes(x=MeanVORP,y=NRtg,label=Tm.x))+
  geom_point()+
  stat_smooth(geom='line', method = "lm", alpha=0.2, size = 1.2)+
  theme_tufte()+
  theme(text=element_text(family="sans", colour="black"), legend.position=c(0.92,0.75), legend.key.size=unit(0.4,"cm"), legend.background=element_rect(color="black", size=0.2))+
  geom_text_repel(aes(label=Tm.x), size=2.5, point.padding=0.1)

pdf(file = "Documents/NBA_VORP_NRtg_plot.pdf")
p
dev.off()


fit2 <- lm(VORP~AST+STL+BLK+PTS+MP.x+I(MP.x*PTS)+I(MP.x*AST), data)
summary(fit2)


# Ok now I'm gonna make plots of cumulative team points by team ranked scorers
data <- read.csv("Documents/nba_player_stats_12_13_17.csv",header=T)
data2 <- read.csv("Documents//nba_advanced_stats_12_13_17.csv",header=T)
data <- full_join(data,data2,by="Player")
data2 <- NULL
data$X <- NULL
data$X.1 <- NULL
data <- data[!data$Tm.x == 'TOT',]
data <- as_tibble(data)

data <- data %>%
  group_by(Tm.x) %>% 
  mutate(teamPTS=sum(PTS)) %>% 
  mutate(percentPoints=(PTS/teamPTS)*100) %>% 
  ungroup()

d2 <- select(data, Player, Tm.x, percentPoints)

d3 <- d2 %>% 
  group_by(Tm.x) %>% 
  mutate(rank=rank(desc(percentPoints), ties.method = "random")) %>% 
  arrange(rank) %>% 
  mutate(cumulPercent=cumsum(percentPoints)) %>% 
  ungroup()

p <- d3 %>% 
  filter(rank <= 20) %>% 
  ggplot(aes(x=rank, y=cumulPercent, color=Tm.x, facet=Tm.x))+
  geom_line(size=0.8)+
  facet_wrap(~Tm.x)+
  theme_few()+
  theme(legend.position="none")+
  xlab("Scorer Rank")+
  ylab("Cumulative Percentage of Team Pts")+
  scale_colour_manual(values=c("#E03A3E", "#008248", "black", "#CE1141", "#1D1160", "#6F2633", "#007DC5", "#5091CD", "#ED174C", "#243E90", "#CE1141", "#002D62", "#ED174C", "#552583", "#00285E", "#98002E", "#00471B", "#005083", "#002B5C", "#F58426", "#007AC1", "#0B77BD", "#006BB6", "#1D1160", "#E13A3E", "#5A2D81", "#C4CED4", "#CD1141", "#0C2340", "#002B5C"))
p

pdf(file = "Documents/cumulativeScorers.pdf")
p
dev.off()

# ok now the same plot, but rank will be by minutes played, still looking at cumulative points though
data <- data %>%
  group_by(Tm.x) %>% 
  mutate(teamMP=sum(MP.x)) %>% 
  mutate(percentMins=(MP.x/teamMP)*100) %>% 
  ungroup()

d2 <- select(data, Player, Tm.x, percentMins, MP.x)

d3 <- d2 %>% 
  group_by(Tm.x) %>% 
  mutate(rank=rank(desc(percentMins), ties.method = "random")) %>% 
  arrange(rank) %>% 
  mutate(cumulMins=cumsum(percentMins)) %>% 
  ungroup()

p <- d3 %>% 
  filter(rank <= 20) %>% 
  ggplot(aes(x=rank, y=cumulMins, color=Tm.x, facet=Tm.x))+
  geom_line(size=0.8)+
  facet_wrap(~Tm.x)+
  theme_few()+
  theme(legend.position="none")+
  xlab("Minutes Rank")+
  ylab("Cumulative Percentage of Team Mins")+
  scale_colour_manual(values=c("#E03A3E", "#008248", "black", "#CE1141", "#1D1160", "#6F2633", "#007DC5", "#5091CD", "#ED174C", "#243E90", "#CE1141", "#002D62", "#ED174C", "#552583", "#00285E", "#98002E", "#00471B", "#005083", "#002B5C", "#F58426", "#007AC1", "#0B77BD", "#006BB6", "#1D1160", "#E13A3E", "#5A2D81", "#C4CED4", "#CD1141", "#0C2340", "#002B5C"))

pdf(file = "Documents/cumulativeMinutes.pdf")
p
dev.off()

# not that different I guess, let's try looking at cumulative MINUTES against minutes rank


## gonna look at TOV

team_TOV <- data %>%
  group_by(Tm.x) %>% 
  summarise(TotalTOV=sum(TOV))
team_TOV <- arrange(team_TOV,Tm.x)
team_TOV <- team_TOV[!team_TOV$Tm.x == 'TOT',]

# read in the team-level stats
team <- read_csv("Documents/nba_team_stats_12_13_17.csv")
team <- as_tibble(team)
team <- arrange(team,Team)
team <- team[c(1:3,5,4,6:nrow(team)),]

# put all the team stuff together
data_team <- bind_cols(team,team_TOV)

# plotting Offensive and Defensive Rating
p <- data_team %>% 
  ggplot(aes(y=`ORtg/A`,x=`DRtg/A`,text=TotalTOV))+
  scale_x_reverse()+
  geom_point(aes(color=TotalTOV), size=3)+
  scale_colour_gradient2(low="dark green", mid="khaki1",
                         high="red", midpoint=mean(data_team$TotalTOV))+
  theme_tufte()+
  theme(text=element_text(family="sans", colour="black"), legend.position=c(0.92,0.75), legend.key.size=unit(0.4,"cm"), legend.background=element_rect(color="black", size=0.2))+
  geom_text_repel(aes(label=Tm.x), size=2.5, direction = "y", point.padding=0.1)+
  xlab("Adjusted Defensive Rating")+
  ylab("Adjusted Offensive Rating")
p
