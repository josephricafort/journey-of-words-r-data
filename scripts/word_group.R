
url <- "https://abvd.shh.mpg.de/austronesian/word.php?c="
url_all <- "https://abvd.shh.mpg.de/austronesian/word.php?group=&sort=word"
groups <- c("Adjectives", "Animals", "Body+Parts", "Colors", "Directions", "Kinship+Terms", "Numbers", "Plants", "Verbs")

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

group_others <- c("Pronouns", "Question Words", "Environment")
pronouns <- c("I", "you", "we", "he, she", "that", "this", "they", "thou")
question_words <- c("who", "what", "when", "where", "how")
environment <- c("cloud", "day", "dust", "earth, soil", 
                         "fire", "fog", "lake", "lightning", 
                         "moon", "rain", "salt", "sand", "sea", 
                         "sky", "smoke", "star", "stick, wood",
                         "stone", "thunder", "water", "wind", "woods, forest")
words_group_others_list <- list(pronouns, question_words, environment)

words_info_others <- tibble()
for(group_index in 1:length(categories_others)){
  group_index <- 1
  group_name <- group_others[group_index]
  words_info_others <- bind_rows(words_info_others, words_info %>% 
                            filter(word %in% unlist(words_group_others_list[group_index])) %>% 
                            mutate(group = group_name))
}

words_info_not_grouped <- url_all %>% read_html() %>% html_node("table") %>% html_table %>%
  rename(id = ID, word = Word, entries = `Number of Entries`) %>%
  mutate(group = "Others" %>% str_replace_all("\\+", " ")) %>%
  mutate(word = str_replace_all(word, "\\/", ", "),
         word = str_replace_all(word, "\\?", ""),
         word = str_replace_all(word, "worm \\(earthworm\\)", "worm, earthworm"),
         word = str_to_lower(word)) %>%
  filter(!(word %in% (words_info$word %>% unique))) %>%
  filter(!(word %in% (words_info_others$word %>% unique)))

words_info_all <- words_info %>% bind_rows(words_info_others)

write_json(words_info_all, "data/output/json/words_info.json")
