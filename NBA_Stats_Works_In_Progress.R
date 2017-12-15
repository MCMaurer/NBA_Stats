require(rvest)
require(tidyverse)
require(ggrepel)
require(ggthemes)

## web-scraping data tables, make a function that just pulls all the data I might be interested in using
url <- 'https://www.basketball-reference.com/leagues/NBA_2018_totals.html'
css_page <- '#totals_stats'
data <- url %>% 
  read_html %>%
  html_nodes(css_page) %>% 
  html_table(header = T) %>% 
  data.frame() %>% 
  as_tibble() %>% 
  filter(!Player == "Player") %>% 
  filter(!Tm == "TOT")

url <- 'https://www.basketball-reference.com/leagues/NBA_2018_advanced.html'
css_page <- '#advanced_stats'
data2 <- url %>% 
  read_html %>%
  html_nodes(css_page) %>% 
  html_table(header = T) %>% 
  data.frame() %>% 
  as_tibble() %>% 
  filter(!Player == "Player") %>% 
  filter(!Tm == "TOT")

data <- full_join(data,data2,by="Player")
data2 <- NULL
data$Var.20 <- NULL
data$Var.25 <- NULL

# putting together a color_scheme table, then a function that lets you select any number of teams and give them labels/make all other teams alpha=0.1

highlight_teams <- function(team_list, alpha_others){
  NBA_colors <- c("#E03A3E", "#008248", "black", "#CE1141", "#1D1160", "#6F2633", "#007DC5", "#5091CD", "#ED174C", "#FFCD34", "#CE1141", "#002D62", "#ED174C", "#552583", "#00285E", "#98002E", "#00471B", "#7AC143", "#002B5C", "#F58426", "#007AC1", "#0B77BD", "#006BB6", "#1D1160", "#E13A3E", "#5A2D81", "#C4CED4", "#CD1141", "#0C2340", "#002B5C")
  alpha_values <- rep(alpha_others,30)
  team_names <- c("ATL", "BOS", "BRK", "CHI", "CHO", "CLE", "DAL", "DEN", "DET", "GSW", "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOP", "NYK", "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS", "TOR", "UTA", "WAS")
  
  color_scheme <- as_tibble(cbind(team_names,NBA_colors,alpha_values))
  color_scheme$alpha_values <- as.numeric(color_scheme$alpha_values)
  color_scheme[color_scheme$team_names %in% team_list,3] <- 1
  return(color_scheme)
}

highlight_teams(c("MIN", "MIL", "HOU", "BOS"), 0.2)

p <- d3 %>% 
  mutate(label = ifelse((Tm.x == "MIN"|Tm.x == "IND"|Tm.x == "GSW"|Tm.x == "BRK"|Tm.x == "MIL") & rank == 5, as.character(Tm.x), NA)) 

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

# non-cumulative minutes

p <- d3 %>% 
  mutate(label = ifelse((Tm.x == "MIN"|Tm.x == "IND"|Tm.x == "GSW"|Tm.x == "BRK"|Tm.x == "MIL") & rank == 5, as.character(Tm.x), NA)) %>% 
  filter(rank <= 20) %>% 
  ggplot(aes(x=rank, y=MP.x, color=Tm.x, alpha=Tm.x,label=label))+
  geom_line(size=0.8)+
  theme_tufte()+
  theme(legend.position="none", text=element_text(family="sans", colour="black"))+
  xlab("Within-Team Rank of Players by Mins")+
  ylab("Minutes Played")+
  scale_colour_manual(values=color_scheme[,2])+
  scale_alpha_manual(values=color_scheme[,3])+
  geom_label_repel(aes(label = label), direction = "both", nudge_x = -1, nudge_y = 0, point.padding = 0.5, na.rm = TRUE)
p

pdf(file = "GitHub/NBA_Stats/MinutesSingle.pdf")
p
dev.off()

# Toronto only
p <- d3 %>% 
  #mutate(label = ifelse((Tm.x == "MIN"|Tm.x == "IND"|Tm.x == "GSW"|Tm.x == "BRK"|Tm.x == "MIL") & rank == 5, as.character(Tm.x), NA)) %>% 
  filter(rank <= 20) %>% 
  ggplot(aes(x=rank, y=cumulMins, color=Tm.x, alpha=Tm.x))+
  geom_line(size=0.8)+
  theme_tufte()+
  theme(legend.position="none", text=element_text(family="sans", colour="black"))+
  xlab("Within-Team Rank of Players by Mins")+
  ylab("Cumulative Percentage of Team Mins")+
  scale_colour_manual(values=color_scheme[,2])+
  scale_alpha_manual(values=c(rep(0.1,27),1,0.1,0.1))
#geom_label_repel(aes(label = label), direction = "both", nudge_x = -1, nudge_y = 0, point.padding = 0.5, na.rm = TRUE)
p

pdf(file = "GitHub/NBA_Stats/cumulativeMinutesSingleToronto.pdf")
p
dev.off()