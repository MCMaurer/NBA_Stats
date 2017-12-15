require(rvest)
require(tidyverse)
require(ggrepel)
require(ggthemes)

## web-scraping data on roster continuity
url <- 'https://www.basketball-reference.com/friv/continuity.html'
css_page <- '#continuity'
continuity <- url %>% 
  read_html %>%
  html_nodes(css_page) %>% 
  html_table(header = T) %>% 
  data.frame() %>% 
  as_tibble()

## need to grep out the % symbol and fill in blanks with NAs

