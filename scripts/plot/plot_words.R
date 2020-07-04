library(ggplot2)
library(tidyverse)
library(forcats)

language_words_api_clean
language_heirarchy_array
new_lang_heir_arr
words_info_all

cognate_group_order <- factor(seq(from=10, to=170, by=10))

dataplot_words <- language_words_api_clean %>%
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
    ) %>% 
  select(-(silcode:id), -loan, -cognacy2, -pmpcognacy) %>%
  group_by(branch, word, cognate_group, group) %>%
  summarize(count = n()) %>%
  left_join(
    new_lang_heir_arr %>% select(branch, branch_id) %>%
      group_by(branch_id, branch) %>% summarize() %>% filter(!is.na(branch_id)),
    by=c("branch")
  ) %>%
  select(branch_id, branch:count) %>%
  # Adding zeros to make area plot smoother later
  spread(cognate_group, count, fill=0) %>%
  gather(cognate_group, count, `10`:`170`) %>%
  # Reorder the cognate_group as factor
  mutate(cognate_group = factor(cognate_group, cognate_group_order))

categories <- words_info_all$group %>% unique

for(cat in 1:length(categories)){
  ggplot(dataplot_words %>%
           filter(group == categories[cat]), aes(x=branch_id, y=count, fill=cognate_group)) +
    # geom_area() +
    geom_area(position=position_fill()) +
    facet_wrap(~word) + labs(title=categories[cat], x="Language group", y="Language count")
  filename <- paste0("plot_", categories[cat] %>% str_to_lower() %>% str_replace_all("\\s", "_"), ".jpg")
  path <- paste0("image_plots")
  ggsave(filename, plot=last_plot(), device="jpg", path=path,
         width=12, height=9, units="in")
}
