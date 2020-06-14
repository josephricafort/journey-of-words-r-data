
url <- "https://abvd.shh.mpg.de/austronesian/word.php?c="
groups <- c("Adjectives", "Animals", "Body+Parts", "Colors", "Directions", "Kinship+Terms", "Numbers", "Plants", "Verbs")

words_info <- tibble()
for(group_index in 1:length(groups)){
  group <- groups[group_index]
  print(paste0("Downloading group: ", group))
  webpage <- paste0(url, group) %>% read_html() %>% html_node("table")
  result <- webpage %>% html_table() %>% mutate(group = group %>% str_replace_all("\\+", " "))
  words_info %<>% bind_rows(result)
}

write_json(words_info, "data/output_json/words_info.json")