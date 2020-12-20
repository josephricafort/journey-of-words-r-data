library(ggplot2)

acd_data_grpd

N_ITEMS_PER_SET <- 20

data_plot <- acd_data_grpd %>%
  group_by(cognate_gp, plang_subgroup) %>% mutate(percentage = count / sum(count)) %>% ungroup %>%
  mutate(cognate_gp = paste0(cognate_gp, " (", cognate_gloss, ") ")) %>%
  group_by(cognate_gp) %>% top_n(N_ITEMS_PER_SET, percentage) %>%
  filter(category == "verb")

ggplot(data_plot, aes(x=plang_subgroup, weight=percentage, fill=item)) +
  geom_bar(aes(color=item), position="fill") + facet_wrap(~cognate_gp) +
  geom_text(aes(y=percentage, label=item), size=3, position=position_stack(vjust=0.5)) +
  theme(legend.position = "none") +
  # To display unicode characters
  theme(text=element_text(family="Arial"))
