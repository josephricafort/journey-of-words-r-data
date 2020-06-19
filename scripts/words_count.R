library(tidyverse)

language_words_api_clean
words_info_all

language_words_count <- language_words_api_clean %>%
  group_by(item, word) %>% summarize(count = n()) %>% arrange(desc(count)) %>%
  left_join(words_info_all) %>% select(-entries) %>%
  mutate(word = str_replace_all(word, "\\,", ""),
         word = str_replace_all(word, " ", "_"))

# Chop words data into multiple pieces for easy accessing for the API use
word_list <- language_words_count$word %>% unique %>% c()
for(id in 1:length(word_list)){
  word_compare <- word_list[id]
  result <- language_words_count %>% filter(word == word_compare & word != "")
  # filename <- str_replace_all(word_compare, " ", "%")
  
  # Export into separate json files
  maindir <- "data/output/json/words_count/"
  filedir <- paste0(maindir, word_compare, ".json")
  print(paste0("Writing json: ", filedir))
  write_json(result, filedir)
}
