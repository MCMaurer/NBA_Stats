library(rvest)
library(tidyverse)
library(ggrepel)
library(ggthemes)
library(patchwork)

nba_dat <- nbastatR::teams_players_stats(seasons = 2019)
nba_dat$dataTable

# ballr package is really slick but only gets per-game stats... which is kinda funky tbh
ballr::NBAPerGameStatistics(2019)
ballr::NBAPerGameAdvStatistics(2019)

# nbastatR has broader options but a few issues: a) defaults are really bizarre for player tables and stuff like that, b) really poor descriptions of what data you actually get

# read in data with nbastatR package
nbastatr_player_data <- nbastatR::bref_players_stats(seasons = 2019)

only2019plot <- nbastatr_player_data %>% 
  select(namePlayer, groupPosition, ptsTotals, fgaTotals, fgmTotals, fg2aTotals, fg2mTotals, fg3aTotals, fg3mTotals, ftaTotals, ftmTotals, pctEFG, ratioWSPer48, ratioVORP, ratioPER, pctTrueShooting) %>% 
  # arrange by VORP and then make name a factor, with levels ordered (by VORP, since we arranged by it)
  arrange(desc(ratioVORP)) %>% 
  mutate(namePlayer = factor(namePlayer, levels = unique(namePlayer), ordered = T)) %>% 
  # mutate to rescale all the variables on a 0-1 axis
  mutate_at(.vars = names(.[-c(1:2)]), .funs = funs(scales::rescale(.))) %>% 
  # group by position (C, F, G) and then get the top 10 according to ratioVORP. Note that the rescaling comes before the group_by and top_n, so the filled values are all relative to the WHOLE player dataset
  group_by(groupPosition) %>% 
  top_n(n=10, ratioVORP) %>% 
  # gather all the stats into a long form
  gather(key = stat, value = value, -c(namePlayer, groupPosition)) %>% 
  # plot with descending namePlayer, which is a factor ordered by VORP, so the players are ordered by VORP
  ggplot(aes(x=stat, y=reorder(namePlayer, desc(namePlayer)), fill = value))+
  geom_tile()+
  facet_grid(groupPosition~., space = "free", scales = "free") +
  scale_fill_viridis_c() +
  MCMsBasics::minimal_ggplot_theme()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("stat") +
  ylab("players sorted by VORP") +
  labs(fill="rescaled value,\nacross all 2019 players")
only2019plot
ggsave("2019_stats_heatmap.tiff", width = 8, height = 8)
ggsave("2019_stats_heatmap.jpg", width = 8, height = 8)

names(nbastatr_player_data)



nba_all_seasons <- nbastatR::bref_players_stats(seasons = 1920:2019)

nba_all_seasons

# this is sorting kinda funky
allseasonsplot <- nba_all_seasons %>% 
  select(namePlayer, yearSeason, groupPosition, ptsTotals, fgaTotals, fgmTotals, fg2aTotals, fg2mTotals, fg3aTotals, fg3mTotals, ftaTotals, ftmTotals, pctEFG, ratioWSPer48, ratioVORP, ratioPER, pctTrueShooting) %>%
  # mutate to rescale all the variables on a 0-1 axis
  mutate_at(.vars = names(.[-c(1:3)]), .funs = funs(scales::rescale(.))) %>% 
  filter(yearSeason == 2019) %>% 
  arrange(desc(ratioVORP)) %>% 
  mutate(namePlayer = factor(namePlayer, levels = unique(namePlayer), ordered = T)) %>% 
  # group by position (C, F, G) and then get the top 10 according to ratioVORP. Note that the rescaling comes before the group_by and top_n, so the filled values are all relative to the WHOLE player dataset
  group_by(groupPosition) %>% 
  top_n(n=10, ratioVORP) %>% 
  # gather all the stats into a long form
  gather(key = stat, value = value, -c(namePlayer, groupPosition, yearSeason)) %>% 
  # plot with descending namePlayer, which is a factor ordered by VORP, so the players are ordered by VORP
  ggplot(aes(x=stat, y=reorder(namePlayer, desc(namePlayer)), fill = value))+
  geom_tile()+
  facet_grid(groupPosition~., space = "free", scales = "free") +
  scale_fill_viridis_c(limits = c(0, 1)) +
  MCMsBasics::minimal_ggplot_theme()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("stat") +
  ylab("players sorted by VORP") +
  labs(fill="rescaled value,\nacross all players + seasons")
allseasonsplot
ggsave("2019_stats_heatmap_allseasoncomparison.tiff", width = 8, height = 8)

o192 <- only2019plot + theme(axis.text.x = element_blank(), axis.title.x = element_blank())

o192 + allseasonsplot + plot_layout(ncol = 1, nrow = 2)


nba_all_seasons %>% 
  select(namePlayer, yearSeason, groupPosition, ptsTotals, fgaTotals, fgmTotals, fg2aTotals, fg2mTotals, fg3aTotals, fg3mTotals, ftaTotals, ftmTotals, pctEFG, ratioWSPer48, ratioVORP, ratioPER, pctTrueShooting) %>%
  # mutate to rescale all the variables on a 0-1 axis
  mutate_at(.vars = names(.[-c(1:3)]), .funs = funs(scales::rescale(.))) %>% 
  arrange(desc(ratioVORP)) %>% 
  mutate(playerYear = paste(namePlayer, yearSeason)) %>% 
  mutate(playerYear = factor(playerYear, levels = unique(playerYear), ordered = T)) %>% 
  group_by(groupPosition) %>% 
  top_n(n=10, ratioVORP) %>% 
  # gather all the stats into a long form
  gather(key = stat, value = value, -c(namePlayer, groupPosition, yearSeason, playerYear)) %>% 
  # plot with descending namePlayer, which is a factor ordered by VORP, so the players are ordered by VORP
  ggplot(aes(x=stat, y=reorder(playerYear, desc(playerYear)), fill = value))+
  geom_tile()+
  facet_grid(groupPosition~., space = "free", scales = "free") +
  scale_fill_viridis_c(limits = c(0, 1)) +
  MCMsBasics::minimal_ggplot_theme()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("stat") +
  ylab("players sorted by VORP") +
  labs(fill="rescaled value,\nacross all players + seasons")


nba_all_seasons %>% 
  select(namePlayer, yearSeason, groupPosition, ptsTotals, fgaTotals, fgmTotals, fg2aTotals, fg2mTotals, fg3aTotals, fg3mTotals, ftaTotals, ftmTotals, pctEFG, ratioWSPer48, ratioVORP, ratioPER, pctTrueShooting) %>%
  # mutate to rescale all the variables on a 0-1 axis
  mutate_at(.vars = names(.[-c(1:3)]), .funs = funs(scales::rescale(.))) %>% 
  arrange(desc(ptsTotals)) %>% 
  mutate(playerYear = paste(namePlayer, yearSeason)) %>% 
  mutate(playerYear = factor(playerYear, levels = unique(playerYear), ordered = T)) %>% 
  group_by(groupPosition) %>% 
  top_n(n=10, ptsTotals) %>% 
  # gather all the stats into a long form
  gather(key = stat, value = value, -c(namePlayer, groupPosition, yearSeason, playerYear)) %>% 
  # plot with descending namePlayer, which is a factor ordered by VORP, so the players are ordered by VORP
  ggplot(aes(x=stat, y=reorder(playerYear, desc(playerYear)), fill = value))+
  geom_tile()+
  facet_grid(groupPosition~., space = "free", scales = "free") +
  scale_fill_viridis_c(limits = c(0, 1)) +
  MCMsBasics::minimal_ggplot_theme()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("stat") +
  ylab("players sorted by ptsTotals") +
  labs(fill="rescaled value,\nacross all players + seasons")


transactions <- nbastatR::transactions()

names(nba_all_seasons)
top25_WS <- nba_all_seasons %>% 
  group_by(namePlayer) %>% 
  summarise(totalWS = sum(ratioWS), totalPTS = sum(ptsTotals), meanVORP = mean(ratioVORP), firstYear = min(yearSeason), lastYear = max(yearSeason), meanWS = mean(ratioWS)) %>% 
  arrange(desc(totalWS)) %>% 
  head(25)

top25_WS %>% print(n=Inf)

top25_meanWS <- nba_all_seasons %>% 
  group_by(namePlayer) %>% 
  summarise(totalWS = sum(ratioWS), totalPTS = sum(ptsTotals), meanVORP = mean(ratioVORP), firstYear = min(yearSeason), lastYear = max(yearSeason), meanWS = mean(ratioWS)) %>% 
  arrange(desc(meanWS)) %>% 
  filter(lastYear - firstYear > 3) %>% 
  head(25)

top25_meanWS %>% print(n=Inf)

top25_PER <- nba_all_seasons %>% 
  group_by(namePlayer) %>% 
  summarise(totalPER = sum(ratioPER), totalWS = sum(ratioWS), totalPTS = sum(ptsTotals), meanVORP = mean(ratioVORP), firstYear = min(yearSeason), lastYear = max(yearSeason), totalVORP = sum(ratioVORP), advancedTotal = sum(totalPER, totalWS, totalVORP)) %>% 
  arrange(desc(advancedTotal)) %>% 
  head(25)

top25_PER %>% print(n=Inf)



bestVORP <- nba_all_seasons %>% arrange(desc(ratioVORP)) %>% select(namePlayer, ratioVORP) %>% head(15)
bestVORP

bestWSper48 <- nba_all_seasons %>% filter(minutes >= 1500) %>% arrange(desc(ratioWSPer48)) %>% select(namePlayer, ratioWSPer48) %>% head(15)
bestWSper48

bestWS <- nba_all_seasons %>% arrange(desc(ratioWS)) %>% select(namePlayer, ratioWS) %>% head(15)
bestWS

bestTS <- nba_all_seasons %>% filter(minutes >= 1500) %>% arrange(desc(pctTrueShooting)) %>% select(namePlayer, pctTrueShooting) %>% head(15)
bestTS

nba_all_seasons$minutes


#### kings players ####
nbastatr_player_data %>% 
  select(namePlayer, slugTeamBREF, groupPosition, ptsTotals, fgaTotals, fgmTotals, fg2aTotals, fg2mTotals, fg3aTotals, fg3mTotals, ftaTotals, ftmTotals, pctEFG, ratioWSPer48, ratioVORP, ratioPER, pctTrueShooting) %>% 
  # arrange by VORP and then make name a factor, with levels ordered (by VORP, since we arranged by it)
  arrange(desc(ratioVORP)) %>% 
  mutate(namePlayer = factor(namePlayer, levels = unique(namePlayer), ordered = T)) %>% 
  # mutate to rescale all the variables on a 0-1 axis
  mutate_at(.vars = names(.[-c(1:3)]), .funs = funs(scales::rescale(.))) %>% 
  # group by position (C, F, G) and then get the top 10 according to ratioVORP. Note that the rescaling comes before the group_by and top_n, so the filled values are all relative to the WHOLE player dataset
  filter(slugTeamBREF == "SAC") %>% 
  top_n(n=10, ratioVORP) %>% 
  # gather all the stats into a long form
  gather(key = stat, value = value, -c(namePlayer, groupPosition, slugTeamBREF)) %>% 
  # plot with descending namePlayer, which is a factor ordered by VORP, so the players are ordered by VORP
  ggplot(aes(x=stat, y=reorder(namePlayer, desc(namePlayer)), fill = value))+
  geom_tile()+
  scale_fill_viridis_c() +
  MCMsBasics::minimal_ggplot_theme()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("stat") +
  ylab("players sorted by VORP") +
  labs(fill="rescaled value,\nacross all 2019 players")

ggsave("2019_SAC_stats_heatmap.jpg", width = 8, height = 8)



# t-wolves players --------------------------------------------------------

nbastatr_player_data %>% 
  select(namePlayer, slugTeamBREF, groupPosition, ptsTotals, fgaTotals, fgmTotals, fg2aTotals, fg2mTotals, fg3aTotals, fg3mTotals, ftaTotals, ftmTotals, pctEFG, ratioWSPer48, ratioVORP, ratioPER, pctTrueShooting) %>% 
  # arrange by VORP and then make name a factor, with levels ordered (by VORP, since we arranged by it)
  arrange(desc(ratioVORP)) %>% 
  mutate(namePlayer = factor(namePlayer, levels = unique(namePlayer), ordered = T)) %>% 
  # mutate to rescale all the variables on a 0-1 axis
  mutate_at(.vars = names(.[-c(1:3)]), .funs = funs(scales::rescale(.))) %>% 
  # group by position (C, F, G) and then get the top 10 according to ratioVORP. Note that the rescaling comes before the group_by and top_n, so the filled values are all relative to the WHOLE player dataset
  filter(slugTeamBREF == "MIN") %>% 
  top_n(n=15, ratioVORP) %>% 
  # gather all the stats into a long form
  gather(key = stat, value = value, -c(namePlayer, groupPosition, slugTeamBREF)) %>% 
  # plot with descending namePlayer, which is a factor ordered by VORP, so the players are ordered by VORP
  ggplot(aes(x=stat, y=reorder(namePlayer, desc(namePlayer)), fill = value))+
  geom_tile()+
  scale_fill_viridis_c() +
  MCMsBasics::minimal_ggplot_theme()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("stat") +
  ylab("players sorted by VORP") +
  labs(fill="rescaled value,\nacross all 2019 players")


data <- mtcars
data
