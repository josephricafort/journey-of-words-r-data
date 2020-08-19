library(ggplot2)
library(tidyverse)
library(forcats)

language_words_api_clean
words_info_all

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

number_factors <- c("one", "two", "three", "four", "five", 
                    "six", "seven", "eight", "nine", "ten",
                    "twenty", "fifty", "one hundred", "one thousand")

# word length vs cognacy (ALL)
# density
ggplot(dataplot_length %>% #filter(group == "Numbers") %>% 
         mutate(word = fct_relevel(word, number_factors))) +
  # geom_jitter(aes(x=item_length, y=cognacy1, color=group), alpha=1/10, stroke=0, shape=16,
  #             width=0.5, height=0.5) +
  geom_density(aes(x=item_length, color=group)) +
  # geom_density2d(aes(x=item_length, y=cognacy1))
  facet_wrap(~group) +
  xlim(0, 15)

# word length vs cognacy filtered group
ggplot(dataplot_length %>% filter(group == "Numbers") %>% 
         mutate(word = fct_relevel(word, number_factors))) +
  geom_density2d(aes(x=item_length, y=cognacy1)) +
  # geom_jitter(aes(x=item_length, y=cognacy1), alpha=1/5, stroke=0, shape=16) +
  facet_wrap(~word) #xlim(0, 11) 

# word per word
categories <- words_info_all$group %>% unique
for (cat in 1:length(categories)){
  ggplot(dataplot_length %>% filter(group == categories[cat]), aes(x=item_length, y=cognacy1)) +
    geom_jitter(alpha = 0.05) +
    geom_density2d() +
    # stat_density2d(aes(color = ..level..)) +
    # geom_point(aes(x=cognacy1, y=item_length, fill=branch_id), alpha=0.25) +
    facet_wrap(~word) + labs(title=categories[cat])
  filename <- paste0("plot_", categories[cat] %>% str_to_lower() %>% str_replace_all("\\s", "_"), ".jpg")
  path <- paste0("image_plots/density2d")
  ggsave(filename, plot=last_plot(), device="jpg", path=path,
         width=12, height=9, units="in")
}

# branch vs cognate_set
ggplot(dataplot_length, aes(x=branch_id, y=cognacy1)) +
  geom_jitter(alpha = 1/10) +
  geom_density2d() +
  # stat_density2d(aes(color = ..level..)) +
  # geom_point(aes(x=cognacy1, y=item_length, fill=branch_id), alpha=0.25) +
  facet_wrap(~group) #+ labs(title=categories[cat])

categories <- words_info_all$group %>% unique
for (cat in 1:length(categories)){
  cat <- 12
  ggplot(dataplot_length %>% filter(group == categories[cat]), aes(x=branch_id, y=cognacy1)) +
    geom_jitter(alpha = 0.05) +
    geom_density2d() +
    # stat_density2d(aes(color = ..level..)) +
    # geom_point(aes(x=cognacy1, y=item_length, fill=branch_id), alpha=0.25) +
    facet_wrap(~word) + labs(title=categories[cat])
  filename <- paste0("plot_", categories[cat] %>% str_to_lower() %>% str_replace_all("\\s", "_"), ".jpg")
  path <- paste0("image_plots/density2d")
  ggsave(filename, plot=last_plot(), device="jpg", path=path,
         width=12, height=9, units="in")
}

