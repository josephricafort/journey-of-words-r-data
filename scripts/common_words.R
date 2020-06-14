language_words_api_clean
language_heirarchy_api_clean
words_info_all

data_common_words <- language_words_api_clean %>%
  filter(word != "") %>%
  group_by(word, language) %>%
  summarize(mean_cognacy1 = mean(cognacy1, na.rm=T),
            median_cognacy1 = median(cognacy1, na.rm=T)) %>%
  left_join(words_info_all %>% select(word, group))
  
common_words_group <- data_common_words$group %>% unique %>% sort

# Cognacy density plot
ggplot(data_common_words %>% 
         filter(mean_cognacy1 <= 50) %>%
         filter(group == common_words_group[4]), aes(mean_cognacy1)) +
  geom_density() +
  facet_wrap(~word)

lang_groups_3 <- language_heirarchy_api_clean %>% filter(group1 == "Austronesian") %>% 
  select(group1:group3) %>% unique %>% as.data.frame