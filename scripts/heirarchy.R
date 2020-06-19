# install.packages(c("treemap", "data.tree"))
install.packages("d3r")
install.packages("RJSONIO")

library(tidyverse)
library(treemap)
library(data.tree)
library(jsonlite)
library(RJSONIO)
library(d3r)

cleanupWord <- function(word){
  # Replace dashes with undescores
  result <- str_replace_all(word, "\\-", "") %>% 
    str_replace_all("\\'", "") %>%
    str_replace_all(" ", "") %>%
    str_replace_all("[^[:alnum:]]", "")
  return (result)
}

language_heirarchy <- language_heirarchy_api_clean %>%
  filter(str_detect(group1, "Austronesian")) %>%
  select(-group1)
  # mutate_at(vars(group1:group14), list(cleanupWord)) %>%
  # unite("group", group1:group14, sep="-", na.rm=T)

# Using d3_nest from d3r package
lang_heirarchy_d3 <- language_heirarchy %>%
  select(group2:group14) %>% mutate(value = 1) %>%
  d3_nest(value_cols="value", root="Austronesian", json=T)

write_json(lang_heirarchy_d3, "data/output/json/language_heirarchy.json", pretty=T)
