
url <- "https://abvd.shh.mpg.de/austronesian/word.php?c="
url_all <- "https://abvd.shh.mpg.de/austronesian/word.php?group=&sort=word"
groups <- c("Adjectives", "Animals", "Body+Parts", "Colors", "Directions", "Kinship+Terms", "Numbers", "Plants", "Verbs")

library(jsonlite)

words_info <- tibble()
for(group_index in 1:length(groups)){
  group <- groups[group_index]
  print(paste0("Downloading group: ", group))
  webpage <- paste0(url, group) %>% read_html() %>% html_node("table")
  result <- webpage %>% html_table() %>%
    rename(id = ID, word = Word, entries = `Number of Entries`) %>%
    mutate(group = group %>% str_replace_all("\\+", " ")) %>%
    mutate(word = str_replace_all(word, "\\/", ", "),
           word = str_replace_all(word, "\\?", ""),
           word = str_replace_all(word, "worm \\(earthworm\\)", "worm, earthworm"),
           word = str_to_lower(word))
  words_info %<>% bind_rows(result)
}

words_info_others <- url_all %>% read_html() %>% html_node("table") %>% html_table %>%
  rename(id = ID, word = Word, entries = `Number of Entries`) %>%
  mutate(group = "Others" %>% str_replace_all("\\+", " ")) %>%
  mutate(word = str_replace_all(word, "\\/", ", "),
         word = str_replace_all(word, "\\?", ""),
         word = str_replace_all(word, "worm \\(earthworm\\)", "worm, earthworm"),
         word = str_to_lower(word)) %>%
  filter(!(word %in% (words_info$word %>% unique))) %>%
  filter(!(word %in% (words_info_others$word %>% unique)))

group_others <- c("Pronouns", "Question Words", "Environment", "Household")
pronouns <- c("i", "all", "you", "we", "he, she", "that", "this", "they", "thou")
question_words <- c("who", "what", "when", "where", "how")
environment <- c("cloud", "day", "dust", "earth, soil", 
                 "fire", "fog", "lake", "lightning", 
                 "moon", "night", "rain", "road, path", "salt", "sand", "sea", 
                 "sky", "smoke", "star", "stick, wood",
                 "stone", "thunder", "water", "wind", "woods, forest")
household <- c("ash", "fat, grease", "house", "meat, flesh", "needle", 
               "rope", "thatch, roof")
words_group_others_list <- list(pronouns, question_words, environment, household)

words_info_others_regrouped <- tibble()
for(group_index in 1:length(words_group_others_list)){
  group_name <- group_others[group_index]
  word_list <- unlist(words_group_others_list[group_index])
  words_info_others_regrouped <- bind_rows(words_info_others_regrouped, words_info_others %>% 
                                   filter(word %in% word_list) %>% 
                                   mutate(group = group_name))
}

words_others <- words_info_others %>% filter(!(word %in% (words_info_others_regrouped$word %>% unique)))

words_info_all <- words_info %>% 
  bind_rows(words_info_others_regrouped) %>%
  bind_rows(words_others) %>%
  arrange(group) %>%
  mutate(entries = entries %>% str_replace_all("\\,", "") %>% as.numeric)

write_json(words_info_all, "data/output/json/words_info.json")
