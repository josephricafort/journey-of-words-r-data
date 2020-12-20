library(ggplot2)

source("data/plot/pop_lang.R")
data_poplang_clean

# Trend of language population over the years in Austronesia
# - per country trend
# - ratio of foreign language to Austronesian language

rotate_xaxis <- theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
rotate_yaxis <- theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5))
remove_yaxis <- theme(axis.title.y=element_blank(),
                      axis.text.y=element_blank(),
                      axis.ticks.y=element_blank())
remove_legends <- theme(legend.position = "none")

data_plot <- data_poplang_clean

data_plot %>%
ggplot(aes(x=ctry_area, y=value, group=macro_family, fill=macro_family)) +
  geom_bar(position = "fill", stat="identity") + rotate_xaxis

# Identify top 75% of the langs with no family
data_plot <- data_poplang_clean %>% filter(is.na(is_an)) %>% select(lang, value) %>% 
  arrange(desc(value)) %>% filter(value > 100000) %>% filter(lang != "Other") %>%
  select(lang) %>% pull %>% droplevels()

data_plot %>%
ggplot(aes(x=log(value))) +
  geom_boxplot() + geom_density()

