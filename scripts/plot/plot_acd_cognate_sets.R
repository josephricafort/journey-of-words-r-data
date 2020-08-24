library(tidyverse)
library(stringi)

acd_data_prep

N_COGNATE_SET <- 12
N_ITEMS_PER_SET <- 7

data_top_words <- acd_data_prep %>% 
  group_by(cognate_gp) %>% summarize(count=sum(count)) %>% arrange(desc(count)) %>%
  top_n(N_COGNATE_SET) %>% select(cognate_gp) %>% pull

data_top_words_per_gp <- acd_data_prep %>%
  filter(cognate_gp %in% data_top_words) %>%
  group_by(cognate_gp, item) %>% summarize(count=sum(count)) %>% arrange(desc(count)) %>% 
  group_by(cognate_gp) %>% top_n(N_ITEMS_PER_SET, count) %>% select(-count)

data_plot <- acd_data_prep %>% 
  semi_join(data_top_words_per_gp) %>%
  group_by(cognate_gp, plang_subgroup) %>% mutate(percentage = count / sum(count)) %>% ungroup %>%
  mutate(cognate_gp = paste0(cognate_gp, " (", cognate_gloss, ") "))
  
ggplot(data_plot, aes(x=plang_subgroup, weight=percentage, fill=item)) +
  geom_bar(aes(color=item)) + facet_wrap(~cognate_gp) +
  geom_text(aes(y=percentage, label=item), size=4, position=position_stack(vjust=0.5)) +
  theme(legend.position = "none")