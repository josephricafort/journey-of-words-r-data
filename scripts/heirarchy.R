language_heirarchy_api_clean


language_heirarchy_api_clean$group2 %>% unique

# Replace dashes with undescores
replaceDashes <- function(word){
  result <- str_replace_all(word, "\\-", "_")
  return (result)
}

language_heirarchy <- language_heirarchy_api_clean %>% 
  mutate_at(vars(group1:group14), list(replaceDashes)) %>%
  # Unite groups into a single column
  unite("group", group1:group14, sep="-", na.rm=T)
  # rename(`0` = group) %>% mutate(`1` = 1) %>%
  # select(`0`, `1`)

write.csv(language_heirarchy, "data/output/csv/language_heirarchy.csv")