library(ggplot2)
library(tidyverse)
library(forcats)

language_words_api_clean
source("./scripts/plot/plot_words.R")

# Plot cognacy vs item_length

dataplot_length <- language_words_api_clean %>%
  mutate(cognate_group = 10*ceiling(cognacy1/10)) %>%
  filter(!is.na(cognate_group)) %>%
  filter(cognate_group < 200) %>%
  left_join(
    new_lang_heir_arr %>% select(id_lang, branch, branch_id),
    by="id_lang"
  ) %>%
  left_join(
    words_info_all %>% select(id, word, group) %>% 
      rename(word_id = id) %>% mutate(word_id = as.character(word_id)),
    by=c("word", "word_id")
  )

top20_cognacy <- dataplot_length %>% group_by(word) %>% summarize(cognacy_mean = mean(cognacy1, na.rm=T))

# word length vs cognacy
# density2d
ggplot(dataplot_length) +
  geom_density2d(aes(x=item_length, y=cognacy1)) +
  # geom_point(aes(x=cognacy1, y=item_length, fill=branch_id), alpha=0.25) +
  facet_wrap(~group)
