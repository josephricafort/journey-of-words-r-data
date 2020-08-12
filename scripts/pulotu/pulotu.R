library(tidyverse)
library(rvest)
library(ggplot2)

# url_pulotu <- "https://pulotu.shh.mpg.de"
# 
# # Generate df for all possible cultures/language available
# pulotu_index <- paste0(url_pulotu, "/culture") %>%
#   read_html() %>% html_node("#cults") %>% html_nodes("tr.culture-link")
# 
# data_pulotu_index <- tibble(
#   language = pulotu_index %>% html_text,
#   url = pulotu_index %>% html_nodes("a") %>% html_attr("href")
# ) %>% mutate(url = paste0(url_pulotu, url))
# 
# # Access each pages and scrape all available information about that culture/language
# 
# data_pulotu_cult <- data_pulotu_index$url[2] %>% read_html
# 
# data_pulotu_cult %>% html_nodes(".notes")
# data_pulotu_cult %>% html_nodes(".table-heading") %>% html_text
# data_pulotu_cult %>% html_nodes(".table-heading") %>% html_text
# 
# data_pulotu_cult %>% html_nodes("#Belief-Indigenous--table") %>% html_nodes("table") %>% html_nodes(".quest") %>%
#   html_text %>% str_replace_all("\\\t", "") %>% str_replace_all("\\\n", "")

factor_interest <- c(
  "Culture",
  "Culture_Notes",
  "isocode",
  "id_lang",
  "v1.Traditional_Time_Focus",
  "v2.Number_of_islands_inhabited_by_culture",
  "v4.Distance_to_African_or_Asian_mainland_(km)_",
  "v9.Maximum_elevation_(meters)",
  "v10.Population",
  "v19.Pre-Austronesian_population",
  "v24.Agriculture_/_Horticulture",
  "v37.Nature_Spirits",
  "v38.Nature_god(s)",
  "v39.Ancestral_spirits",
  "v42.God(s)",
  "v51.Social_hierarchy_tapu",
  "v52.Kinship_tapu",
  "v56.Mana_related_to_social_influence_or_technical_skill_",
  "v57.Mana_as_a_spiritual_or_religious_concept_",                                        
  "v58.Mana_as_a_personal_quality",                                                       
  "v59.Mana_and_social_status",                                                           
  "v60.Mana_linked_to_genealogy",
  "v63.Headhunting",
  "v74.Language_shift",
  "v80.Vehicles_and_roads",
  "v105.Importance_of_Patrilateral_descent",
  "v106.Importance_of_Matrilateral_descent"
)

# Dataset download from https://pulotu.shh.mpg.de/dataset
data_pulotu_colnames <- read_tsv("./data/input/pulotu/Pulotu_Database_4_7_2015.txt") %>% colnames
data_pulotu_raw <- read_tsv("./data/input/pulotu/Pulotu_Database_4_7_2015.txt") %>%
  rename(id_lang = ABVD_Code)

dataplot_pulotu <- data_pulotu_raw %>% select(factor_interest) %>% 
  select(Culture, "isocode", "id_lang", 
         "v4.Distance_to_African_or_Asian_mainland_(km)_",
         "v105.Importance_of_Patrilateral_descent", 
         "v106.Importance_of_Matrilateral_descent") %>%
  rename(patriarchal = "v105.Importance_of_Patrilateral_descent", 
         matriarchal = "v106.Importance_of_Matrilateral_descent",
         asia_dist = "v4.Distance_to_African_or_Asian_mainland_(km)_") %>%
  mutate(id_lang = gsub("\\;.*", "", id_lang)) %>%
  mutate_at(vars(patriarchal, matriarchal), as.numeric) %>%
  filter(!is.na(patriarchal) & !is.na(matriarchal)) %>%
  gather(descent, descent_val, c("patriarchal", "matriarchal")) %>%
  mutate(asia_dist_group = as.factor(1*ceiling(asia_dist/1000))) %>%
  group_by(descent, descent_val, asia_dist_group) %>%
  summarize(count = n())

# Quick plot
ggplot(dataplot_pulotu, aes(descent, count,  fill = descent)) +
  # geom_jitter(width = 0, height = 0.2) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~asia_dist_group)
