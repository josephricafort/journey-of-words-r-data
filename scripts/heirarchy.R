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
  mutate_at(vars(group1:group14), list(cleanupWord)) %>%
  # Unite groups into a single column
  unite("group", group1:group14, sep="-", na.rm=T)
  # filter(str_detect(group, "-{3,}")) %>%
  # rename(`0` = group) %>% mutate(`1` = 1) %>%
  # select(`0`, `1`) %>%
  
language_heirarchy_tree <- language_heirarchy %>%
  as.Node(pathName="group", pathDelimiter="-")

write_json(language_heirarchy_tree %>% as.list %>% toJSON, "data/output/json/language_heirarchy.json", pretty=T, simplifyVector=F)

write.csv(language_heirarchy, "data/output/csv/language_heirarchy.csv", row.names=F, col.names=F)

# Using d3_nest from d3r package
lang_heirarchy_d3 <- language_heirarchy_api_clean %>%
  select(group1:group14) %>% mutate(value = 1) %>%
  d3_nest(value_cols="value", root="language_heirarchy")

write_json(lang_heirarchy_d3, "data/output/json/language_heirarchy.json", pretty=T, simplifyVector=F)
