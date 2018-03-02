require(rvest)
require(tidyverse)
require(ggrepel)
require(ggthemes)

# It would be cool to write all of these up into a package

## web-scraping data tables, make a function that just pulls all the data I might be interested in using

# I should probably break this up into a few functions so I don't have to use global assignments
load_NBA_data <- function(){
  require(rvest)
  require(tidyverse)
  # this function loads individual and team data from basketball reference, tidies it, and saves it to global env
url <- 'https://www.basketball-reference.com/leagues/NBA_2018_totals.html'
css_page <- '#totals_stats'
player_totals <- url %>% 
  read_html %>%
  html_nodes(css_page) %>% 
  html_table(header = T) %>% 
  data.frame() %>% 
  as_tibble() %>% 
  filter(!Player == "Player") %>% 
  filter(!Tm == "TOT")

url <- 'https://www.basketball-reference.com/leagues/NBA_2018_advanced.html'
css_page <- '#advanced_stats'
player_advanced <- url %>% 
  read_html %>%
  html_nodes(css_page) %>% 
  html_table(header = T) %>% 
  data.frame() %>% 
  as_tibble() %>% 
  filter(!Player == "Player") %>% 
  filter(!Tm == "TOT")

# combine all player data, delete some junk columns
player_data <- full_join(player_totals,player_advanced,by="Player")
player_data$Var.20 <- NULL
player_data$Var.25 <- NULL

url <- "https://www.basketball-reference.com/leagues/NBA_2018_ratings.html"
css_page <- "#ratings"
team_ratings <- url %>% 
  read_html %>%
  html_nodes(css_page) %>% 
  html_table() %>% 
  data.frame()
colnames(team_ratings) <- team_ratings[1,]
team_ratings <- team_ratings[-1,]
team_ratings <- team_ratings %>% 
  as_tibble() %>% 
  arrange(Team)

# flip Chicago and Charlotte, then add 3-letter team codes as Tm.x
team_ratings <- team_ratings[c(1:3,5,4,6:nrow(team_ratings)),]
Tm.x <- sort(unique(player_data$Tm.x))
team_ratings <- cbind(team_ratings, Tm.x)

# gotta change lots of data to numeric from characters
player_cols <- c(1,4,6:31,33,35:56)
player_data[,player_cols] <- player_data[,player_cols] %>% 
  lapply(function(x) as.numeric(x))

team_cols <- c(1,5,6:15)
team_ratings[,team_cols] <- team_ratings[,team_cols] %>% 
  lapply(function(x) as.numeric(x))

# save the data, make sure they're both tibbles (for whatever reason, without this, team_ratings would appear as a dataframe when calling this function)
data <- NULL
data$player_data <- as_tibble(player_data)
data$team_ratings <- as_tibble(team_ratings)
return(data)
}

nba_data <- load_NBA_data()

nba_data$player_data
nba_data$team_ratings

# let's just assign separate objects
player_data <- nba_data$player_data
team_ratings <- nba_data$team_ratings

d2 <- player_data %>%
  group_by(Tm.x) %>% 
  mutate(teamMP=sum(MP.x)) %>% 
  mutate(percentMins=(MP.x/teamMP)*100) %>% 
  ungroup()

d3 <- select(d2, Player, Tm.x, percentMins, MP.x)

d4 <- d3 %>% 
  group_by(Tm.x) %>% 
  mutate(rank=rank(desc(percentMins), ties.method = "random")) %>% 
  arrange(rank) %>% 
  mutate(cumulMins=cumsum(percentMins)) %>% 
  mutate(cumulActualMins=cumsum(MP.x)) %>% 
  ungroup()


# putting together a color_scheme table, then a function that lets you select any number of teams and give them labels/make all other teams alpha=0.1

team_list <- c("GSW", "SAC", "MIN", "BRK", "IND")

highlight_teams <- function(team_list, alpha_others){
  # this function will take a list of teams and an alpha value (pick a low value), and return a table with all teams, their colors, and low alpha values for all teams not on your list
  NBA_colors <- c("#E03A3E", "#008248", "black", "#CE1141", "#1D1160", "#6F2633", "#007DC5", "#5091CD", "#ED174C", "#FFCD34", "#CE1141", "#002D62", "#ED174C", "#552583", "#00285E", "#98002E", "#00471B", "#7AC143", "#002B5C", "#F58426", "#007AC1", "#0B77BD", "#006BB6", "#1D1160", "#E13A3E", "#5A2D81", "#C4CED4", "#CD1141", "#0C2340", "#002B5C")
  alpha_values <- rep(alpha_others,30)
  team_names <- c("ATL", "BOS", "BRK", "CHI", "CHO", "CLE", "DAL", "DEN", "DET", "GSW", "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOP", "NYK", "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS", "TOR", "UTA", "WAS")
  
  color_scheme <- as_tibble(cbind(team_names,NBA_colors,alpha_values))
  color_scheme$alpha_values <- as.numeric(color_scheme$alpha_values)
  color_scheme[color_scheme$team_names %in% team_list,3] <- 1
  color_scheme <- as_tibble(color_scheme)
  return(color_scheme)
}

color_scheme <- highlight_teams(team_list = team_list, 0.1)


label_teams <- function(data, team_list, selector_variable, sel_var_value){
  # this function takes an NBA data frame (data), a team list, the value you'll be using for the x-axis on your plot (in the form data$column), and where along that x-axis the label should fall (sel_var_value). It'll then create labels for each team
  data_new <- data %>% 
    mutate(label = ifelse((Tm.x %in% team_list & selector_variable == sel_var_value), as.character(Tm.x), NA))
  return(data_new)
}

d4 <- label_teams(data=d4, team_list = team_list, selector_variable = d4$rank, sel_var_value = 5)


# everything works!
p <- d4 %>% 
  filter(rank <= 20) %>% 
  ggplot(aes(x=rank, y=cumulMins, color=Tm.x, alpha=Tm.x,label=label))+
  geom_line(size=0.8)+
  theme_tufte()+
  theme(legend.position="none", text=element_text(family="sans", colour="black"))+
  xlab("Within-Team Rank of Players by Mins")+
  ylab("Cumulative Percentage of Team Mins")+
  scale_colour_manual(values=color_scheme$NBA_colors)+
  scale_alpha_manual(values=color_scheme$alpha_values)+
  geom_label_repel(aes(label = label), direction = "both", nudge_x = -1, nudge_y = 0, point.padding = 0.5, na.rm = TRUE)
p

p <- d4 %>% 
  filter(rank <= 20) %>% 
  ggplot(aes(x=rank, y=cumulActualMins, color=Tm.x, alpha=Tm.x,label=label))+
  geom_line(size=0.8)+
  theme_tufte()+
  theme(legend.position="none", text=element_text(family="sans", colour="black"))+
  xlab("Within-Team Rank of Players by Mins")+
  ylab("Cumulative Percentage of Team Mins")+
  scale_colour_manual(values=color_scheme$NBA_colors)+
  scale_alpha_manual(values=color_scheme$alpha_values)+
  geom_label_repel(aes(label = label), direction = "both", nudge_x = -1, nudge_y = 0, point.padding = 0.5, na.rm = TRUE)
p



d4 %>% 
  filter(rank <= 20) %>% 
  ggplot(aes(x=rank, y=cumulMins, color=Tm.x))+
  geom_line(size=0.8)+
  theme_tufte()+
  theme(legend.position="none", text=element_text(family="sans", colour="black"))+
  xlab("Within-Team Rank of Players by Mins")+
  ylab("Cumulative Percentage of Team Mins")+
  scale_colour_manual(values=color_scheme$NBA_colors)+
  facet_wrap(~Tm.x)


team_list <- team_ratings %>% 
  group_by(Conf) %>% 
  arrange(desc(W.L.)) %>% 
  mutate(rank = rank(-W.L., ties.method = "random")) %>% 
  ungroup() %>% 
  filter(rank <= 3) %>% 
  select(Tm.x) %>% 
  as_vector()


team_list <- c("MIN", "IND", "HOU")
color_scheme <- highlight_teams(team_list = team_list, 0.05)
d4 <- label_teams(data=d4, team_list = team_list, selector_variable = d4$rank, sel_var_value = 5)

d4$animate <- NA
d4$animate[!d4$Tm.x %in% team_list] <- 1
d4$animate[d4$Tm.x == "MIN"] <- 2
d4$animate[d4$Tm.x == "IND"] <- 3
d4$animate[d4$Tm.x == "HOU"] <- 4

p <- d4 %>% 
  filter(rank <= 20) %>% 
  ggplot(aes(x=rank, y=cumulMins, color=Tm.x, alpha=Tm.x, group=animate))+
  geom_line(aes(frame=animate,cumulative=TRUE),size=1.2)+
  theme_tufte()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        text=element_text(family="Roboto Condensed", colour="black", size=20))+
scale_colour_manual(values=color_scheme$NBA_colors)+
  scale_alpha_manual(values=color_scheme$alpha_values)


#### REALLY advanced stats ####

# gotta check out stats from NBA's tracking data, crazy stuff on individual players
