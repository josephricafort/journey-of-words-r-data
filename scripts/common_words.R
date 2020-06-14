data_common_words <- language_words_api_clean %>%
  group_by(word, language) %>%
  summarize(mean_cognacy1 = mean(cognacy1, na.rm=T),
            median_cognacy1 = median(cognacy1, na.rm=T))

# Cognacy density plot
ggplot(data_common_words %>% filter(mean_cognacy1 <= 50), aes(mean_cognacy1)) +
  geom_density() +
  facet_wrap(~word, scales="free_y")

lang_groups_3 <- language_heirarchy_api_clean %>% filter(group1 == "Austronesian") %>% 
  select(group1:group3) %>% unique %>% as.data.frame

