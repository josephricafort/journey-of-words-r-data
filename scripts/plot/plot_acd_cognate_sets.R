library(tidyverse)
library(stringr)

acd_data_prep

N_COGNATE_SET <- 25
N_ITEMS_PER_SET <- 10

# Visualize Top cognate sets with N number of items per set
data_top_words <- acd_data_prep %>% 
  group_by(cognate_gp, plang_subgroup, item) %>% summarize(count=sum(count)) %>% 
  arrange(cognate_gp, item) %>%
  # Filter with plang_subgroup count >= 3
  group_by(cognate_gp) %>% filter(n_distinct(plang_subgroup) >= 4) %>%
  group_by(item) %>% mutate(perc = count / sum(count)) %>%
  # Weighed significant percentage across 5 major language subgroups
  group_by(cognate_gp, item) %>% summarize(count_median = mean(count),
                                           count_IQR = IQR(count),
                                           count_score = (count_median + count_IQR)/2) %>% 
  arrange(desc(count_score)) %>% ungroup %>%
  top_n(N_COGNATE_SET, count_median) %>%
  select(cognate_gp) %>% pull %>% as.factor

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
  geom_text(aes(y=percentage, label=item), size=3, position=position_stack(vjust=0.5)) +
  theme(legend.position = "none") +
  # To display unicode characters
  theme(text=element_text(family="Arial"))

# ---

# See distribution of the words
data_plot <- acd_data_prep %>% 
  group_by(cognate_gp, plang_subgroup, item) %>% summarize(count=sum(count)) %>% 
  arrange(desc(count)) %>%
  group_by(cognate_gp) %>% mutate(perc = count / sum(count)) %>%
  # Filter with plang_subgroup count >= 3
  group_by(cognate_gp) %>% filter(n_distinct(plang_subgroup) >= 4) %>%
  # Weighed significant percentage across 5 major language subgroups
  group_by(cognate_gp, item) %>% summarize(count_median = median(count), 
                                           count_IQR = IQR(count),
                                           perc_median = median(perc),
                                           perc_IQR = IQR(perc)) %>% 
  # Create a measure that looks both into the 
  # stability (count_median) and variability (count_IQR)
  # mean of the 2 which we called it the 'score'
  mutate(score = (count_median + count_IQR)/2) %>% arrange(desc(score)) %>%
  # Filter those 0 median or IQR
  filter(count_median > 0 & count_IQR > 0)

ggplot(data_plot, aes(x=(count_median + 0.01) %>% log)) +
  geom_density()

ggplot(data_plot, aes(x=count_median, y=count_IQR)) +
  geom_jitter(alpha = 0.3) +
  geom_smooth(model = lm)

ggplot(data_plot, aes(x = item, y = count)) +
  geom_line() + facet_wrap(~cognate_gp, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90))

  