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

# p <- d3 %>% 
#   filter(rank <= 20) %>% 
#   ggplot(aes(x=rank, y=cumulMins, color=Tm.x, facet=Tm.x))+
#   geom_line(size=0.8)+
#   theme_few()+
#   theme(legend.position="none")+
#   xlab("Within-Team Rank of Players by Mins")+
#   ylab("Cumulative Percentage of Team Mins")+
#   scale_colour_manual(values=NBA_colors)
# p
# 
# pdf(file = "GitHub/NBA_Stats/cumulativeMinutes.pdf")
# p
# dev.off()
# 
# p <- d3 %>% 
#   filter(Tm.x == "MIN") %>% 
#   ggplot(aes(x=rank, y=cumulMins, color=Tm.x, facet=Tm.x))+
#   geom_line(size=0.8)+
#   theme_few()+
#   theme(legend.position="none")+
#   xlab("Within-Team Rank of Players by Mins")+
#   ylab("Cumulative Percentage of Team Mins")+
#   scale_colour_manual(values=NBA_colors[18])+
#   annotate("text", x=3.4, y=25, label="Steep slope \n here = \n relying on \n top players", color="darkgray")+
#   annotate("text", x = 10.5, y = 90, label = "Short tail \n here = \n not much \n bench", color="darkgray")+
#   annotate("text", x = 13.5, y = 25, label = "Line ends \n when no  \n players with \n minutes remain", color="darkgray", hjust = 1)+
#   annotate("segment", x = 13, xend = 13, y = 33, yend = 100,
#            colour = "gray", linetype="dashed")
# p
# 
# pdf(file = "GitHub/NBA_Stats/cumulativeMinutesExplanation.pdf")
# p


### trying to plot them all together but highlight certain standouts

alpha.values <- c(0.1, 0.1, 1, rep(0.1,6), 1, rep(0.1,6), 1, 1, rep(0.1,5), 1, rep(0.1,6))
color_scheme <- cbind(select(team_age,Tm.x),NBA_colors,alpha.values)
color_scheme$NBA_colors <- as.character(color_scheme$NBA_colors)

color_scheme[12,3] <- 1
color_scheme[24,3] <- 0.1
color_scheme[10,2] <- "#FFCD34"
color_scheme[18,2] <- "#7AC143"


p <- d3 %>% 
  mutate(label = ifelse((Tm.x == "MIN"|Tm.x == "IND"|Tm.x == "GSW"|Tm.x == "BRK"|Tm.x == "MIL") & rank == 5, as.character(Tm.x), NA)) %>% 
  filter(rank <= 20) %>% 
  ggplot(aes(x=rank, y=cumulMins, color=Tm.x, alpha=Tm.x,label=label))+
  geom_line(size=0.8)+
  theme_tufte()+
  theme(legend.position="none", text=element_text(family="sans", colour="black"))+
  xlab("Within-Team Rank of Players by Mins")+
  ylab("Cumulative Percentage of Team Mins")+
  scale_colour_manual(values=color_scheme[,2])+
  scale_alpha_manual(values=color_scheme[,3])+
  geom_label_repel(aes(label = label), direction = "both", nudge_x = -1, nudge_y = 0, point.padding = 0.5, na.rm = TRUE)
p

pdf(file = "GitHub/NBA_Stats/cumulativeMinutesSingle.pdf")
p
dev.off()
