require(tidyverse)
require(ggrepel)
require(ggthemes)
NBA_colors <- c("#E03A3E", "#008248", "black", "#CE1141", "#1D1160", "#6F2633", "#007DC5", "#5091CD", "#ED174C", "#243E90", "#CE1141", "#002D62", "#ED174C", "#552583", "#00285E", "#98002E", "#00471B", "#005083", "#002B5C", "#F58426", "#007AC1", "#0B77BD", "#006BB6", "#1D1160", "#E13A3E", "#5A2D81", "#C4CED4", "#CD1141", "#0C2340", "#002B5C")
# read in regular and advanced stats csv files, then join them and get rid of redundancy
data <- read.csv("GitHub/NBA_Stats/nba_player_stats_12_13_17.csv",header=T)
data2 <- read.csv("GitHub/NBA_Stats/nba_advanced_stats_12_13_17.csv",header=T)
data <- full_join(data,data2,by="Player")
data2 <- NULL
data$X <- NULL
data$X.1 <- NULL
data <- data[!data$Tm.x == 'TOT',]
data <- as_tibble(data)
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
  xlab("Within-Team Rank of Players by Mins")+
  ylab("Cumulative Percentage of Team Mins")+
  scale_colour_manual(values=NBA_colors)

pdf(file = "GitHub/NBA_Stats/cumulativeMinutes.pdf")
p
dev.off()

p <- d3 %>% 
  filter(Tm.x == "MIN") %>% 
  ggplot(aes(x=rank, y=cumulMins, color=Tm.x, facet=Tm.x))+
  geom_line(size=0.8)+
  theme_few()+
  theme(legend.position="none")+
  xlab("Within-Team Rank of Players by Mins")+
  ylab("Cumulative Percentage of Team Mins")+
  scale_colour_manual(values=NBA_colors[18])+
  annotate("text", x=3.4, y=25, label="Steep slope \n here = \n relying on \n top players", color="darkgray")+
  annotate("text", x = 10.5, y = 90, label = "Short tail \n here = \n not much \n bench", color="darkgray")+
  annotate("text", x = 13.5, y = 25, label = "Line ends \n when no  \n players with \n minutes remain", color="darkgray", hjust = 1)+
  annotate("segment", x = 13, xend = 13, y = 33, yend = 100,
           colour = "gray", linetype="dashed")
p

pdf(file = "GitHub/NBA_Stats/cumulativeMinutesExplanation.pdf")
p
dev.off()