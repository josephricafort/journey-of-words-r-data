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

language_info_api_clean
language_heirarchy_api_clean

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
  mutate_at(vars(group1:group14), list(as.character)) %>%
  # Rename "Central-Eastern" to "Central-Eastern Malayo Polynesian" for consistency
  mutate(group3 = ifelse(group3 == "Central-Eastern", "Central-Eastern Malayo-Polynesian", as.character(group3)))
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

branch_order <- c("Proto-Austronesian", "Formosan", "Philippine", "Malayo-Polynesian",
                  "Western Malayo-Polynesian", "Central Malayo-Polynesian", "South Halmahera-West New Guinea",
                  "Papuan Tip", "North New Guinea", "Admiralty Islands", "Central-Eastern Oceanic", 
                  "Southeast Solomonic", "Meso Melanesian", "Temotu", "New Caledonian", 
                  "Micronesian", "North and Central Vanuatu","Northeast Vanuatu-Banks Islands",
                  "East Vanuatu", "Malekula Coastal", "Polynesian")

# Restructure heirarchy according to heirarchy graphic found on website
# https://abvd.shh.mpg.de/austronesian/research.php

# Generated another observable d3 forked from a cluster dendogram to serve as reference:
# https://observablehq.com/d/a5e74ac875ca5c6d

# 1. Formosan (no node, created) - all children in group 2
gp_formosan <- c("Atayalic", "East-Formosan", "East Formosan", "Western Plains", "Tsouic", 
              "Bunun", "Northwest Formosan", "Paiwan", "Puyuma", "Rukai")

# 2. Philippine (group 3) - all children in group 4
# Child nodes: group 3
gp_philippine_toadd <- c("Bashiic", "Greater Central Philippine", "Bilic")
# Possible child nodes: South Sulawesi

# 3. Western Malayo-Polynesian (no node) - all children in group 3
gp_western_malayo_polynesian <- c("Celebic", "North Borneo", "Malayo-Chamic", "Greater Barito",
                                  "Land Dayak", "Malayo-Sumbawan North and East", "Bali-Sasak-Sumbawa")

# 4. Central-Eastern Malayo Polynesian (group 3)
# 5. South Halmahera-West New Guinea (group 5)
# 6. Papuan Tip (group 7)
# 7. North New Guinea (group 7)
# 8. Admiralty Islands (group 6)
# 9. Southeast Solomonic (group 7)
# 10. Meso Melanesian (group 7)
# 11. Temotu (group 6)
# 12. New Caledonian (group 7) // Southern Oceanic
# 13. Micronesian (group 7)
# 14. Central Pacific (group 8)
# 15. Polynesian (group 10) // under Central Pacific

gp2 <- c("Eastern ", "Malayo-Polynesian")
gp3 <- c("Philippine")
gp4 <- c("Central Malayo-Polynesian")
gp5 <- c("South Halmahera-West New Guinea")
gp6 <- c("Admiralty Islands", "Central-Eastern Oceanic", "Temotu")
gp7 <- c("Papuan Tip", "North New Guinea", "Southeast Solomonic", "Meso Melanesian")
gp8 <- c("North and Central Vanuatu", "Micronesian", "New Caledonian")
# gp8 <- c("North and Central Vanuatu", "New Caledonian")
gp9 <- c("Northeast Vanuatu-Banks Islands")
gp10 <- c("Polynesian", "East Vanuatu", "Malekula Coastal")
new_heirarchy_group <- list(gp3, gp4, gp5, gp6, gp7, gp8, gp10)

# Create a column that would categorize a language's group
new_lang_heir_arr <- language_heirarchy_array %>%
  mutate(branch = as.character("")) %>%
  select(id_lang:silcode, branch, group1:group14) %>%
  mutate(branch = ifelse(is.na(group2), "Proto-Austronesian", branch)) %>%
  mutate(branch = ifelse(group2 %in% gp_formosan, "Formosan", branch)) %>%
  mutate(branch = ifelse(group2 %in% gp2, group2, branch)) %>%
  mutate(branch = ifelse(group3 == "Philippines" & group3 %in% gp_philippine_toadd, "Philippine", branch)) %>%
  mutate(branch = ifelse(group3 %in% gp_western_malayo_polynesian, "Western Malayo-Polynesian", branch)) %>%
  mutate(branch = ifelse(group3 %in% gp3, group3, branch)) %>%
  mutate(branch = ifelse(group4 %in% gp4, group4, branch)) %>%
  mutate(branch = ifelse(group5 %in% gp5, group5, branch)) %>%
  mutate(branch = ifelse(group6 %in% gp6, group6, branch)) %>%
  mutate(branch = ifelse(group7 %in% gp7, group7, branch)) %>%
  mutate(branch = ifelse(group8 %in% gp8, group8, branch)) %>%
  mutate(branch = ifelse(group9 %in% gp9, group9, branch)) %>%
  mutate(branch = ifelse(group10 %in% gp10, group10, branch)) %>%
  # Relevel branch into factor
  mutate(branch = fct_relevel(branch, branch_order)) %>%
  mutate(branch_id = match(branch, branch_order)) %>%
  select(id_lang:silcode, branch, branch_id, group1:group14) %>%
  arrange(branch_id, id_lang) %>%
  left_join(language_info_api_clean %>% select(id_lang, latitude, longitude)) %>%
  # Remove Proto-Austronesian
  filter(branch_id != 1)

write_json(new_lang_heir_arr, "data/output/json/heirarchy/language_heirarchy_array.json", pretty=T)

# Just a quick plot of how the data would look like

branch_list <- new_lang_heir_arr$branch %>% unique

plot_new_lang_heir_arr <- new_lang_heir_arr %>%
  group_by(branch) %>%
  summarize(n = n()) %>% as.data.frame

ggplot(plot_new_lang_heir_arr) +
  geom_bar(aes(branch, weight=n)) +
  theme(axis.text.x = element_text(angle = 90))

# Generate centroid for language groups
cntr_lang_groups <- tibble(
  name = character(),
  lat = numeric(),
  lon = numeric()
)
for (grp in 2:14){
  grp_col <- c(paste0("group", grp))
  result <- new_lang_heir_arr %>% 
    group_by_at(grp_col) %>% 
    summarize(lat = mean(latitude, na.rm=T), 
              lon = mean(longitude, na.rm=T)) %>% 
    ungroup %>% rename_with(function(var){ return ("name")}, starts_with("group"))
  cntr_lang_groups <- bind_rows(cntr_lang_groups, result) %>% 
    arrange(name)
}
cntr_lang_groups <- cntr_lang_groups %>% bind_rows(
  new_lang_heir_arr %>% group_by(branch) %>% 
    summarize(lat = mean(latitude, na.rm=T),lon = mean(longitude, na.rm=T)) %>%
    rename(name = branch)
) %>% arrange(name)

# Generate data for origin map
heirarchy_location <- new_lang_heir_arr %>% select(id_lang, language, latitude, longitude) %>%
  rename(lat = latitude, lon = longitude, name = language) %>%
  bind_rows(cntr_lang_groups) %>%
  filter(!is.na(lat) | !is.na(lon)) %>%
  mutate(node_id = row_number())

# Generate data for flow map
heirarchy_flows <- tibble(
  origin = character(),
  dest = character()
)
# flow: branch -> group 2, 3,.. n, -> language
for(row in 1:nrow(new_lang_heir_arr)){
  branch <- new_lang_heir_arr$branch[row] %>% as.character
  language <- new_lang_heir_arr$language[row] %>% as.character
  id_lang <- new_lang_heir_arr$id_lang[row] %>% as.character
  origin <- dest <- NA
  for(col in 1:14){ # Start from group2 (Ignore 'Austronesian' group)
    if(col == 1){
      # 1. branch ->  group2
      origin <- branch
      dest <- new_lang_heir_arr[row, paste0("group", col+1)] %>% as.character
    } else if (col >= 2 & col < 14) {
      # 2. group 2, 3, 4... n
      origin <- new_lang_heir_arr[row, paste0("group", col)] %>% as.character
      dest <- new_lang_heir_arr[row, paste0("group", col+1)] %>% as.character
    } else if (col == 14) { 
      # 3. group n -> language
      origin <- new_lang_heir_arr[row, paste0("group", col)] %>% as.character
      dest <- language %>% as.character
    }
    if(!is.na(dest)){
      heirarchy_flows <- bind_rows(heirarchy_flows, tibble(origin, dest))
    }
  }
  print(paste0("Row ", row, ": Done!"))
}

heir_orig_ref <- heirarchy_location %>% select(name, node_id)
heirarchy_flows <- heirarchy_flows %>% group_by(origin, dest) %>% summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  mutate(origin_id = heir_orig_ref$node_id[match(origin, heir_orig_ref$name)]) %>%
  mutate(dest_id = heir_orig_ref$node_id[match(dest, heir_orig_ref$name)]) %>%
  select(origin, origin_id, dest, dest_id, count) %>%
  rename(origin_name = origin,
         origin = origin_id,
         dest_name = dest,
         dest = dest_id)

write_json(heirarchy_location, "data/output/json/heirarchy/language_heirarchy_location.json", pretty=T)
write_json(heirarchy_flows, "data/output/json/heirarchy/language_heirarchy_flows.json", pretty=T)
write.csv(heirarchy_location, "data/output/csv/heirarchy/language_heirarchy_location.csv")
write.csv(heirarchy_flows, "data/output/csv/heirarchy/language_heirarchy_flows.csv")
