data_endangered_lang_clean

rotate_xaxis <- theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
rotate_yaxis <- theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5))
remove_yaxis <- theme(axis.title.y=element_blank(),
                      axis.text.y=element_blank(),
                      axis.ticks.y=element_blank())

endanger_lang_palette <- c("#FFCC00", "#FF9900", "#CC9900", "#990000", "#330000")

data_endangered_lang_clean %>%
  ggplot(aes(x=ctry, fill=degree_danger)) +
  geom_bar() + 
  scale_fill_manual(values = endanger_lang_palette) +
  rotate_xaxis
