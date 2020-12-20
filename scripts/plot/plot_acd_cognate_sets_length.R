library(tidyverse)
library(stringr)

acd_data_prep

data_plot <- acd_data_prep %>% select(cognate_gp, item:count, item_length) %>%
  group_by(count, item_length) %>% summarize(n = n())

ggplot(data_plot, aes(x=item_length, y=count, color=n)) +
  geom_point() + scale_colour_manual(values="RdYlBu")
  geom_jitter() + geom_density2d(aes(x=item_length, y=count))
