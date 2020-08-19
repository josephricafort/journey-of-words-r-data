library(tidyverse)

language_words_api_clean
words_info_all
language_info_api_clean
new_lang_heir_arr

language_words_count <- language_words_api_clean %>%
  # left_join( language_info_api_clean %>% select(id_lang, latitude, longitude), by=c("id_lang")) %>%
  left_join(new_lang_heir_arr %>% select(id_lang, branch, branch_id), by=c("id_lang")) %>%
  group_by(item, word) %>% summarize(
    count = n(), 
    cognacy1 = mean(cognacy1, na.rm=T),
    # latitude = mean(latitude, na.rm=T),
    # longitude = mean(longitude, na.rm=T)) %>%
    branch_mean = mean(branch_id, na.rm=T)) %>%
  mutate(cognate_group = 10*ceiling(cognacy1/10)) %>%
  arrange(cognacy1, desc(count)) %>%
  left_join(words_info_all) %>% select(-entries) %>%
  mutate(word = str_replace_all(word, "\\,", ""),
         word = str_replace_all(word, " ", "_")) %>%
  ungroup() %>%
  # filter(!is.na(longitude)) %>%
  filter(!is.na(cognacy1)) %>%
  filter(!is.na(branch_mean))

# Top 20% count of every cognate group
language_words_count_top20pc <- language_words_count %>% 
  group_by(cognate_group) %>% top_frac(0.20, count)

# # Filter only top 20 per word
# language_words_count_top20 <- language_words_count %>% group_by(word) %>%
#   slice_max(order_by = count, n = 20) %>%
#   filter(word != "")
# 
# # Filter only top 200 per word
# language_words_count_top200 <- language_words_count %>% group_by(word) %>%
#   slice_max(order_by = count, n = 200) %>%
#   arrange(word, cognacy1, desc(count)) %>%
#   filter(word != "")

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

write_json(language_words_count, "data/output/json/words_count.json")
write_json(language_words_count_top20pc, "data/output/json/words_count_top20pc.json")
