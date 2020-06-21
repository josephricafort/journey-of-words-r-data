# install.packages(c("treemap", "data.tree"))
install.packages("d3r")
install.packages("RJSONIO")

library(tidyverse)
library(treemap)
library(data.tree)
library(jsonlite)
library(RJSONIO)
library(d3r)
library(rvest)
library(magrittr)
library(stringr)

cleanupWord <- function(word){
  # Replace dashes with undescores
  result <- str_replace_all(word, "\\-", "") %>% 
    str_replace_all("\\'", "") %>%
    str_replace_all(" ", "") %>%
    str_replace_all("[^[:alnum:]]", "")
  return (result)
}

language_heirarchy_array <- language_heirarchy_api_clean %>%
  filter(str_detect(group1, "Austronesian")) %>%
  # Rename "Central-Eastern" to "Central-Eastern Malayo Polynesian" for consistency
  mutate(group3 = ifelse(group3 == "Central-Eastern", "Central-Eastern Malayo Polynesian", as.character(group3)))
  # select(-group1)
  # mutate_at(vars(group1:group14), list(cleanupWord)) %>%
  # unite("group", group1:group14, sep="-", na.rm=T)

# Using d3_nest from d3r package
lang_heirarchy_d3 <- language_heirarchy_array %>%
  select(group2:group14) %>% mutate(value = 1) %>%
  d3_nest(value_cols="value", root="Austronesian", json=T)

write_json(language_heirarchy_array, "data/output/json/language_heirarchy_array.json", pretty=T)
write_json(lang_heirarchy_d3, "data/output/json/language_heirarchy_d3.json", pretty=T)

# Scraping the heirarchy from the webpage
url <- "https://abvd.shh.mpg.de/austronesian/classification.php?node=Austronesian"

# # (group) p.gnode: by 10, a style
# # (language) p: by 5, style
# 
# scrape_group <- url %>% read_html() %>% 
#   html_nodes("#content") %>% html_nodes("p.gnode") %>% html_nodes("a")
# level_group <- scrape_group %>% html_attr("style") %>% 
#   str_extract_all("\\d+") %>% unlist
# name_group <- scrape_group %>% html_text
# classification_group <- scrape_group %>% html_attr("href")

language_heirarchy_scrape_data <- tibble(name_group, level_group, classification_group) %>%
  mutate(level_group = str_replace_all(level_group, "0", "") %>% as.factor) %>%
  filter(level_group == 1)

# Getting a better heirarchy in order

branch_order <- c("Formosan", "Philippine", "Western Malayo-Polynesian",
                         "Central Malayo-Polynesian", "South Halmahera/West New Guinea",
                         "Papuan Tip", "North New Guinea", "Admiralties",
                         "South-East Solominic", "Meso-Melanesian", "Temotu",
                         "Southern Oceanic", "Micronesian", "Central Pacific",
                         "Polynesian")

# Restructure heirarchy according to heirarchy graphic found on website
# https://abvd.shh.mpg.de/austronesian/research.php

# Generated another observable d3 forked from a cluster dendogram to serve as reference:
# https://observablehq.com/d/a5e74ac875ca5c6d

# 1. Formosan (no node, created) - all nodes in group 2
gp_formosan <- c("Atayalic", "East-Formosan", "Western Plains", "Tsouic", 
              "Bunun", "Northwest Formosan", "Paiwan", "Puyuma", "Rukai")

# 2. Philippine (node 3) - all nodes in group 4
# Child nodes: group 3
gp_philippine_toadd <- c("Bashiic")
# Possible child nodes: South Sulawesi

# 3. Western Malayo-Polynesian (no node) - all nodes in group 3
gp_western_malayo_polynesian <- c("Celebic", "North Borneo", "Malayo-Chamic", "Greater Barito",
                                  "Land Dayak", "Malayo-Sumbawan North and East", "Bali-Sasak-Sumbawa",
                                  

language_heirarchy_array %>% filter(group2 %in% gp_formosan)

group3 <- language_heirarchy_array %>% 
  filter(group3 == branch_order[2]) %>% 
  select(group4) %>% unique

language_heirarchy_array$group3 %>% unique

 branch_order[group3 %in% branch_order]
