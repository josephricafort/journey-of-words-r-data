library(ggplot2)
library(tidyverse)
library(forcats)

language_words_api_clean
words_info_all

# Plot cognacy vs item_length

cols <- paste0("col", seq(1, 49))
char_remove <- "\\'"

dataplot_phonetics <- language_words_api_clean %>%
  mutate(cognate_group = 10*ceiling(cognacy1/10)) %>%
  filter(!is.na(cognate_group)) %>%
  filter(cognate_group < 200) %>%
  left_join(
    new_lang_heir_arr %>% select(id_lang, branch, branch_id),
    by="id_lang"
  ) %>%
  select(word_id, word, item, item_length, cognacy1) %>%
  arrange(desc(item_length)) %>%
  separate(col=item, into=cols, sep="(?<=.)") %>%
  gather(col, phonetic, col1:col49) %>%
  # Count all unique phonetics per word 
  group_by(word, phonetic, cognacy1) %>%
  summarize(phonetic_count = n()) %>%
  summarize(cognacy_mean = mean(cognacy1, na.rm=T), phonetic_mean = mean(phonetic_count, na.rm=T)) %>%
  filter(nchar(phonetic)!=0) %>%
  filter(phonetic != " ") %>%
  filter(!grepl("[[:punct:]]", phonetic)) %>%
  left_join(
    words_info_all %>% select(word, group),
    by=c("word")
  )

categories <- words_info_all$group %>% unique
cat <- 1
ggplot(dataplot_phonetics %>% filter(group == categories[cat]), aes(x=phonetic_mean, y=cognacy_mean)) +
  geom_point(alpha=0.75) +
  facet_wrap(~phonetic) + labs(title=categories[cat])
  
