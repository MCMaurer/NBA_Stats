require(rvest)
require(tidyverse)
require(ggrepel)
require(ggthemes)
require(zoo)
require(skimr)

# web-scraping data on roster continuity
url <- 'https://www.basketball-reference.com/friv/continuity.html'
css_page <- '#continuity'
continuity <- url %>%
  read_html %>%
  html_nodes(css_page) %>%
  html_table(header = T, fill = T) %>%
  data.frame() %>%
  as_tibble()

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

team_list <- c()

color_scheme <- highlight_teams(team_list = team_list, 1)

## need to grep out the % symbol and fill in blanks with NAs
continuity2 <- continuity
continuity2[,-1] <- as.numeric(gsub("\\%","",as.matrix(continuity2[,-1])))

# ok hell yeah this works
# now I think I need to tidy it such that each team x year is an observation, so columns for team, year, and continuity. Then I can try to read in a team's record, playoff loss, some measure of team success, etc.

continuity2
continuity3 <- gather(continuity2, "team", "continuity", 2:31)
continuity3 %>% 
  ggplot(aes(x=Season, y=continuity, color=team))+
  geom_point()+
  scale_colour_manual(values=color_scheme$NBA_colors)+
  theme_tufte()
  
# Need to read html pages for individual seasons, read in team's W/L, net rating, whatever metrics make sense. Do this for every season, then combine them into a tidy data frame with columns for year, team, and whatever metrics. Then I could plot continuity against any number of metrics, and have multiple years of data for each.

# reading in these season pages is gonna take a while, I could use a for loop to go through urls for each season (they're the same except for the year) and then read in the data, but I'm a bit worried that might be considered spidering the site. If that's a problem, I should just do it once and then save the data as one big csv, that way I'll have it on hand, and most of it won't change (except current year's data)

# I think what I need to do is add a column with the year (it'll be all the same for a given year), and then append each new year's worth of data onto the old data frame. That way, I'll build it up tidy, with year and team as separate columns so each row is an observation. I should also see if I can append team name codes... Or if I can just go through the completed data frame and add team name codes based on the team name column, that would probably be faster/easier

# another thing to consider is that I shouldn't re-add the header each time, I should JUST use it the first time and get rid of it the rest of the times. Could try to do this upon reading in each file, or I could just search and destroy the additional headers in the completed data frame

# I think there's a quicker way to implement this, probably not using a for loop, probably using an apply function. Should also look into best way to append these data tables together


#### Creating ratings ####

# Alright, here all the code commented out is just making the ratings file, which I then saved as a csv so I don't have to keep scraping the website and being annoying, since those data aren't changing except the most recent year


# i <- 1986
# url <- paste('https://www.basketball-reference.com/leagues/NBA_',i,'_ratings.html', sep = "")
# css_page <- '#ratings'
# ratings <- url %>% 
#   read_html %>%
#   html_nodes(css_page) %>% 
#   html_table(header = T, fill = T) %>% 
#   data.frame() %>% 
#   as_tibble()
# 
# ratings$Year <- i
# 
# for(i in 1987:2017){
# url <- paste('https://www.basketball-reference.com/leagues/NBA_',i,'_ratings.html', sep = "")
# css_page <- '#ratings'
# ratings_new <- url %>% 
#   read_html %>%
#   html_nodes(css_page) %>% 
#   html_table(header = T, fill = T) %>% 
#   data.frame() %>% 
#   as_tibble()
# 
# ratings_new$Year <- i
# 
# ratings <- bind_rows(ratings, ratings_new)
# }
# 
# write_csv(ratings, "GitHub/NBA_Stats/AllYearsTeamRatings.csv")

ratings <- read_csv("AllYearsTeamRatings.csv")

# ok rad, now I have all the ratings data in here, I just need to clean it up, create a new column with team name abbreviations on it. This will probably mean grepping through a whole list of names, but I might be able to quicken it by making vectors of matching team names and abbreviations. Then basically if continuity$Name == "d$Name[1]", continuity$Abbreviation <- d$Abbreviation[1]. Then I'll clean that up going and looking at any teams that don't exist any more and doing those by hand
  # on this note, I may be able to grab the combinations of team name and abbreviation from one of the team_data sets from the random_NBA_stats_stuff

team_names <- read_csv("team_names_abbreviations.csv")

# ratings$Tm.x <- NA
# 
# 
# for(i in 1:30){ # I can for sure vectorize this, should look into how to do that
# ratings[ratings$Team %in% team_names$Team[i], 17] <- team_names$Tm.x[i]
# }

# this is vectorized and also rad, really should note that this is a great way to create new columns based on matching other stuff. Basically I'm creating a key-pair combo with the team names and abbreviations, then give the abbreviations to a bigger data frame that only has the actual team name
ratings$Tm.x <- team_names$Tm.x[match(ratings$Team, team_names$Team)]

# ok now just to weed out the weirdos
ratings[ratings$Team == "New Jersey Nets", 17] <- "BRK"
ratings[ratings$Team == "New Orleans Hornets", 17] <- "NOP"
ratings[ratings$Team == "Charlotte Bobcats", 17] <- "CHO"
ratings[ratings$Team == "Seattle SuperSonics", 17] <- "OKC"
ratings[ratings$Team == "Washington Bullets", 17] <- "WAS"
ratings[ratings$Team == "Vancouver Grizzlies", 17] <- "MEM"
ratings[ratings$Team == "New Orleans/Oklahoma City Hornets", 17] <- "NOP"

continuity3[continuity3$team == "NJN", 2] <- "BRK"
continuity3[continuity3$team == "CHA", 2] <- "CHO"
continuity3[continuity3$team == "NOH", 2] <- "NOP"

continuity3$Season <- gsub("-.*","", continuity3$Season)
continuity3$Season <- as.numeric(continuity3$Season)
continuity3$Season <- continuity3$Season + 1

cont <- full_join(continuity3,ratings, by = c("team" = "Tm.x", "Season" = "Year"))

# the continuity3 data set includes some of our non-standard teams in it, so we'll need to change their abbreviations too


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

cont <- cont %>% arrange(team, Season)

cont %>% 
  ggplot(aes(x=continuity, y=NRtg.A))+
  geom_point(aes(color=team))+
  theme_tufte()+
  theme(legend.position="none", text=element_text(family="sans", colour="black"))+
  scale_colour_manual(values=color_scheme$NBA_colors)+
  geom_smooth(method = "lm", formula = y~x, color = "gray")

# this looks awesome! now I wanna compute a running three-year or five-year average of continuity to figure out how a team's success is drawn from the previous few years of continuity



fit1 <- lm(NRtg.A~continuity, data=cont)
summary(fit1)


# computing a running average

cont %>% 
  group_by(team) %>% 
  arrange(Season)

test <- runif(100, min = 0, max = 100)
test[1:4] <- NA

rollmean(test, 3, align = "right", fill = NULL)
rollapply(test,3,mean, fill = NA)

cont$roll_cont <- cont %>%
  group_by(team) %>% 
  arrange(Season) %>%
  rollapply(3, mean, fill = NA)

Atlanta <- cont %>% filter(team == "ATL")
Atlanta <- Atlanta %>% arrange(Season)

rollmean(x = Atlanta$continuity, k = 3, fill = NA, align = "right")

# this works!
brooklyn <- cont %>% 
  filter(team == "BRK") %>% 
  arrange(Season) %>% 
  mutate(roll_cont = rollapply(data = continuity, width = 3, FUN = mean, fill = NA, align = "right"))


cont_rolling <- cont %>% 
  group_by(team) %>% 
  arrange(Season) %>% 
  mutate(roll_cont = rollapply(data = continuity, width = 3, FUN = mean, fill = NA, align = "right"))



color_scheme <- highlight_teams(team_list = "", 1)
cont_rolling %>% 
  ggplot(aes(x=roll_cont, y=NRtg.A))+
  geom_point(aes(color=team, alpha=team))+
  theme_tufte()+
  theme(legend.position="none", text=element_text(family="sans", colour="black"))+
  scale_colour_manual(values=color_scheme$NBA_colors)+
  scale_alpha_manual(values=color_scheme$alpha_values)+
  geom_smooth(method = "lm", formula = y~x, color = "gray", alpha=0.2)

# highlight SAS
color_scheme <- highlight_teams(team_list = "SAS", 0.1)
color_scheme[27,2] <- "black"
cont_rolling %>% 
  ggplot(aes(x=roll_cont, y=NRtg.A))+
  geom_point(aes(color=team, alpha=team))+
  theme_tufte()+
  theme(legend.position="none", text=element_text(family="sans", colour="black"))+
  scale_colour_manual(values=color_scheme$NBA_colors)+
  scale_alpha_manual(values=color_scheme$alpha_values)+
  geom_smooth(method = "lm", formula = y~x, color = "gray", alpha=0.2)

fit2 <- lm(NRtg.A~continuity + roll_cont, data=cont_rolling)
summary(fit2)

library(plotly)
# interactive plot
color_scheme <- highlight_teams(team_list = "", 1)
ggplotly(
  cont_rolling %>% 
    filter(Season >= 1986) %>% 
    ggplot(aes(x=roll_cont, y=NRtg.A))+
    geom_point(aes(color=team, alpha=team))+
    theme_tufte()+
    theme(legend.position="none", text=element_text(family="sans", colour="black"))+
    scale_colour_manual(values=color_scheme$NBA_colors)+
    facet_wrap(~Season)+
    geom_smooth(method = "lm", formula = y~x, color = "gray", alpha=0.2)
)

ggplotly(
  cont_rolling %>% 
    filter(Season >= 1986) %>% 
    ggplot(aes(x=roll_cont, y=NRtg.A))+
    geom_point(aes(color=team, alpha=Season))+
    theme_tufte()+
    theme(legend.position="none", text=element_text(family="sans", colour="black"))+
    scale_colour_manual(values=color_scheme$NBA_colors)+
    facet_wrap(~team)+
    geom_smooth(method = "lm", formula = y~x, color = "gray", alpha=0.2)
)

ggplotly(
  cont %>% 
    filter(Season >= 1984) %>% 
    ggplot(aes(x=continuity, y=NRtg.A))+
    geom_point(aes(color=team, alpha=Season))+
    theme_tufte()+
    theme(legend.position="none", text=element_text(family="sans", colour="black"))+
    scale_colour_manual(values=color_scheme$NBA_colors)+
    facet_wrap(~team)+
    geom_smooth(method = "lm", formula = y~x, color = "gray", alpha=0.2)
)


cont_rolling <- as.data.frame(cont_rolling)
cont_rolling$Decade[cont_rolling$Season %in% 1980:1989] <- 1980
cont_rolling$Decade[cont_rolling$Season %in% 1990:1999] <- 1990
cont_rolling$Decade[cont_rolling$Season %in% 2000:2009] <- 2000
cont_rolling$Decade[cont_rolling$Season %in% 2010:2019] <- 2010

ggplotly(
  cont_rolling %>% 
    filter(Season >= 1984) %>% 
    ggplot(aes(x=roll_cont, y=NRtg.A, group=1))+
    geom_point(aes(color=team))+
    theme_tufte()+
    theme(legend.position="none", text=element_text(family="sans", colour="black"))+
    scale_colour_manual(values=color_scheme$NBA_colors)+
    facet_wrap(~Decade)+
    geom_smooth(method = "lm", formula = y~x, color = "gray", alpha=0.2)
)

#### Does success lead to roster continuity? ####

# Let's look at the opposite question: does success in a given year lead to a more continuous roster in the next year?

# create a column for last year's net rating
cont <- cont %>% 
  group_by(team) %>% 
  arrange(Season) %>% 
  mutate(PreviousNRtg.A = lag(NRtg.A))

cont %>% 
  ggplot(aes(x=PreviousNRtg.A, y=continuity, group=1))+
  geom_point(aes(color=team))+
  theme_tufte()+
  theme(legend.position="none", text=element_text(family="sans", colour="black"))+
  scale_colour_manual(values=color_scheme$NBA_colors)+
  geom_smooth(method = "lm", formula = y~x, color = "gray")

# animated plot time
#devtools::install_github("dgrtwo/gganimate")
require(gganimate)

p <- cont_rolling %>% 
  ggplot(aes(x=continuity, y=NRtg.A, group=Decade))+
  geom_point(aes(color=team, frame=Decade))+
  theme_tufte()+
  theme(legend.position="none", text=element_text(family="sans", colour="black"))+
  scale_colour_manual(values=color_scheme$NBA_colors)+
  geom_smooth(aes(frame=Decade),method = "lm", formula = y~x, color = "gray")
gganimate(p)


fit3 <- lm(continuity~PreviousNRtg.A, data=cont)
summary(fit3)

fit4 <- lm(NRtg.A~roll_cont + team:roll_cont, data=cont_rolling)
summary(fit4)
plot(fit4, which = c(1:6))


p <- cont_rolling %>%
  ggplot(aes(x=Season, y=roll_cont))+
  geom_line(aes(group=team, color=team))+
  theme_tufte()+
  scale_colour_manual(values=color_scheme$NBA_colors)+
  theme(legend.position="right", panel.grid = element_blank(), legend.title=element_blank(), text=element_text(family="Roboto Condensed", colour="black"))
ggplotly(p)


cont_rolling %>%
  ggplot(aes(x=Season, y=roll_cont))+
  geom_line(aes(group=team, color=team))+
  theme_tufte()+
  scale_colour_manual(values=color_scheme$NBA_colors)+
  theme(legend.position="none", panel.grid = element_blank(), legend.title=element_blank(), text=element_text(family="Roboto Condensed", colour="black"))+
  facet_wrap(~team)


# scatter plot for each team with 3-year rolling average cont
p <- cont_rolling %>% 
  ggplot(aes(x=roll_cont, y=NRtg.A))+
  xlab("Roster Continuity")+
  ylab("Adjusted Net Rating")+
  geom_point(aes(color=team))+
  geom_line(aes(group=team), stat="smooth", method="lm", size=1.2, formula = y~x, color="gray", alpha=0.5)+
  geom_smooth(aes(group=team), method = "lm", size=0, formula = y~x, color = "gray", alpha=0.2)+
  theme_tufte()+
  scale_colour_manual(values=color_scheme$NBA_colors)+
  theme(legend.position="none", panel.grid = element_blank(), legend.title=element_blank(), text=element_text(family="Roboto Condensed", colour="black"))+
  facet_wrap(~team)
ggsave("team_cont_scatter.tiff", p, width = 8, height = 8)

# let's calculate a 5-year rolling average
cont_rolling <- cont_rolling %>% 
  group_by(team) %>% 
  arrange(Season) %>% 
  mutate(roll_cont5 = rollapply(data = continuity, width = 5, FUN = mean, fill = NA, align = "right"))

cont_rolling <- cont_rolling %>% 
  group_by(Season) %>% 
  mutate(league_mean_rc5 = mean(roll_cont5, na.rm = T))

league_average_roll5 <- mean(cont_rolling$roll_cont5, na.rm = T)

# now plotting the 5-year-rolling
cont_rolling %>% 
  ggplot(aes(x=roll_cont5, y=NRtg.A))+
  geom_point(aes(color=team))+
  geom_smooth(aes(group=team), method = "lm", formula = y~x, color = "gray", alpha=0.2)+
  theme_tufte()+
  scale_colour_manual(values=color_scheme$NBA_colors)+
  theme(legend.position="none", panel.grid = element_blank(), legend.title=element_blank(), text=element_text(family="Roboto Condensed", colour="black"))+
  facet_wrap(~team)

# let's look at 5 year rolling across time, plotting the all-time league average as well
# cont_rolling %>%
#   ggplot(aes(x=Season, y=roll_cont5))+
#   geom_line(aes(group=team, color=team))+
#   geom_hline(yintercept = league_average_roll5, linetype=2)+
#   geom_hline(yintercept = mean(roll_cont5), linetype=2)+
#   theme_tufte()+
#   scale_colour_manual(values=color_scheme$NBA_colors)+
#   theme(legend.position="none", panel.grid = element_blank(), legend.title=element_blank(), text=element_text(family="Roboto Condensed", colour="black"))+
#   facet_wrap(~team)

# let's look at 5 year rolling across time, but plotting the league average across years too
p <- cont_rolling %>%
  ggplot(aes(x=Season, y=roll_cont5))+
  ylab("Roster Continuity (light gray is league average)")+
  geom_line(aes(color=team))+
  geom_line(aes(x=Season, y=league_mean_rc5), linetype=1, alpha=0.1)+
  theme_tufte()+
  scale_colour_manual(values=color_scheme$NBA_colors)+
  theme(legend.position="none", panel.grid = element_blank(), legend.title=element_blank(), text=element_text(family="Roboto Condensed", colour="black"))+
  facet_wrap(~team) 
p
ggsave("team_rc5_time_all.tiff", p, width = 8, height = 8)


p1 <- ggplot_build(p)
ggplot_gtable(p1)

# zoom in on modern era
p <- cont_rolling %>%
  filter(Season > 1999) %>% 
  ggplot(aes(x=Season, y=roll_cont5))+
  ylab("Roster Continuity (light gray is league average)")+
  geom_line(aes(group=team, color=team))+
  geom_line(aes(x=Season, y=league_mean_rc5), linetype=1, alpha=0.1)+
  theme_tufte()+
  scale_colour_manual(values=color_scheme$NBA_colors)+
  theme(legend.position="none", panel.grid = element_blank(), legend.title=element_blank(), text=element_text(family="Roboto Condensed", colour="black"))+
  facet_wrap(~team) 
p
ggsave("team_rc5_time_modern.tiff", p, width = 8, height = 8)

p <- cont_rolling %>% 
  ggplot(aes(x=Season, y=league_mean_rc5))+
  ylab("League Average Roster Continuity")+
  scale_x_continuous(breaks=c(1976, 1983, 1995, 1999, 2011))+
  geom_line()+
  #geom_vline(xintercept = 1972, lty=2)+ 
  geom_vline(xintercept = 1976, lty=2)+ # merger and limited free agency
  annotate("text", x=1974, y=66, label="Merger/Limited FA", angle=90, alpha=0.5)+
  #geom_vline(xintercept = 1980, lty=2)+ # eliminated no-trade clauses
  geom_vline(xintercept = 1983, lty=2)+ # added salary cap and Bird Rights
  annotate("text", x=1981, y=65, label="Cap/Bird Rights", angle=90, alpha=0.5)+
  #geom_vline(xintercept = 1988, lty=2)+ # added unrestricted free agency
  geom_vline(xintercept = 1995, lty=2)+ # added rookie scale contracts
  annotate("text", x=1993, y=66, label="Rookie Scale Contracts", angle=90, alpha=0.5)+
  geom_vline(xintercept = 1999, lty=2)+ # added max salaries, mid-level exception, escrow and luxury tax 
  annotate("text", x=2001, y=73, label="Max Sal, Mid-Level Exc, Escrow/Lux", angle=90, alpha=0.5)+
  #geom_vline(xintercept = 2005, lty=2)+ # luxury tax in effect every season, reductions in contract lengths and raises
  geom_vline(xintercept = 2011, lty=2)+ # reductions in contract lengths and raises, greater penalties for taxpaying teams
  annotate("text", x=2009, y=74, label="+ Penalties for Taxpaying Teams", angle=90, alpha=0.5)
p
ggsave("league_rc5_annotated.tiff", plot = p, width = 8, height = 5)

cont_rolling <- cont_rolling %>% 
  group_by(Season) %>% 
  mutate(league_mean_cont = mean(continuity, na.rm = T))

cont_rolling <- cont_rolling %>% 
  group_by(Season) %>% 
  mutate(league_mean_rc3 = mean(roll_cont, na.rm = T))


p <- cont_rolling %>%
  filter(Season > 1999) %>% 
  ggplot(aes(x=Season, y=roll_cont))+
  ylab("Roster Continuity (light gray is league average)")+
  geom_line(aes(group=team, color=team))+
  geom_line(aes(x=Season, y=league_mean_rc3), linetype=1, alpha=0.1)+
  theme_tufte()+
  scale_colour_manual(values=color_scheme$NBA_colors)+
  theme(legend.position="none", panel.grid = element_blank(), legend.title=element_blank(), text=element_text(family="Roboto Condensed", colour="black"))+
  facet_wrap(~team) 
p



#### next big thing ####

# can I directly get the rosters and minutes from every team for every year? And then join them all together, group by team and season and calculate total mins, then give each player a % mins for that season. And then track continuity by player contribution in a given year? Like, the idea that continuity is really a guy giving significant minutes over many years. Because you could achieve an average of 70% continuity by alternating between 100% and 40% every year, which is dumb