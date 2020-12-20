install.packages("ggalt")
install.packages("wesanderson")
install.packages("ggrepel")
install.packages("ggbeeswarm")

library(ggplot2)
library(ggalt)
library(RColorBrewer)
library(wesanderson)
library(ggrepel)
library(ggbeeswarm)

data_colonialism

rotate_xaxis <- theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=1))
rotate_yaxis <- theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5))
remove_yaxis <- theme(axis.title.y=element_blank(),
                        axis.text.y=element_blank(),
                        axis.ticks.y=element_blank())
remove_legends <- theme(legend.position = "none")

data_colonialism %>%
  ggplot(aes(x=onset_yr, xend=end_yr, y=reorder(ctry_name, -onset_yr), group=ctry_name)) + 
  geom_dumbbell(colour = "#dddddd",
                size = 3,
                colour_x = "#FAAB18",
                colour_xend = "#1380A1")


# Categorize the indicators
exploit_trade <- c("col_foreign_trade", "col_invest_cnctrate", "col_invest_infra",
                   "col_trade_cnctrate", "plantations", "gold_silver_mining", "mining_dur_col")

domination <- c("violent_col", "violent_ind", "violent_resist", "col_violence", "form_col_domination",
                "power_transform_decol", "foreign_presence", "immigration_foreign_workers", "missionary",
                "col_admin")

ethnic_usage <- c("ethnic_function", "col_borders")

# Plot each variable with faceting

# All variables with maxlevel
data_plot_col <- data_colonialism %>% filter(!lvl)

# Indicators at max level (2, 4, 5)
gen_plot <- function(ind_list, lvlmax){
  data_plot_col %>% filter(max_lvl == lvlmax) %>% filter(ind_short %in% ind_list) %>%
    ggplot(aes(x=ctry_code_wb, y=ind_short, fill=as.numeric(value))) +
    geom_tile() + geom_text(aes(x=ctry_code_wb, y=ind_short, label=value)) +
    scale_fill_gradient2(midpoint = lvlmax/2, low = "gray", mid = "orange", high = "red", space = "Lab" ) +
    remove_legends
}

gen_plot(exploit_trade, 2)
gen_plot(exploit_trade, 4)
gen_plot(domination, 2)
gen_plot(domination, 4)
gen_plot(domination, 5)
gen_plot(ethnic_usage, 2)

pal <- wes_palette("Zissou1", 100, type = "continuous")
data_plot_col %>% filter(max_lvl == 2) %>%
  ggplot(aes(x=ind_short, y=as.numeric(value))) +
  geom_tile() +
  scale_fill_gradient2(midpoint = 2, low = "yellow", mid = "orange", high = "red", space = "Lab" )

# Indicators at max level 100
data_plot_col <- data_colonialism %>% filter(lvl) %>%
  filter(!is.na(value)) %>%
  # Add an average value

MAX_LVL <- 100
data_plot_col %>%
  ggplot(aes(x=ind_short, weight=value, fill=ind_short)) +
  geom_bar() + 
  facet_wrap(ctry_name~.) + rotate_xaxis

data_plot_col_mean <- data_plot_col %>%
  group_by(ind_short) %>% summarize(value = mean(value, na.rm=T),
                                    ctry_name = "All countries")

ggplot(data=data_plot_col, aes(x=ind_short, y=value)) +
  geom_line(data=data_plot_col_mean, aes(x=ind_short, y=value, group=ctry_name), size=2, color="violet") +
  geom_line(aes(x=ind_short, y=value, group=ctry_name, color=ctry_name), size=1, alpha=0.2) +
  geom_beeswarm(aes(color=ctry_name)) +
  geom_text_repel(aes(x=ind_short, label=ctry_code_wb), size=4) +
  ylim(0, 100) + remove_legends

# By FOREIGN POWER
# Indicators at max level (2, 4, 5)
data_plot_col <- data_colonialism %>% filter(!lvl) %>%
  group_by(motherland, ind_short) %>% summarize(value = round(mean(value, na.rm=T), 2),
                                                max_lvl = first(max_lvl)) %>% ungroup

data_plot_col %>% filter(max_lvl == MAX_LVL) %>%
  ggplot(aes(x=ind_short, y=motherland, fill=as.numeric(value))) +
  geom_tile() +
  scale_fill_gradient2(midpoint = MAX_LVL/2, low = "yellow", mid = "orange", high = "red", space = "Lab" ) + 
  theme_minimal() + rotate_xaxis

data_plot_col %>%
  ggplot(aes(x=ind_short, y=motherland, fill=as.numeric(value))) +
  geom_tile() +
  scale_fill_gradient2(midpoint = MAX_LVL/2, low = "yellow", mid = "orange", high = "red", space = "Lab" ) + 
  theme_minimal() + rotate_xaxis

gen_plot_foreign <- function(ind_list, lvlmax){
  data_plot_col %>% filter(max_lvl == lvlmax) %>% filter(ind_short %in% ind_list) %>%
    ggplot(aes(x=motherland, y=ind_short, fill=value)) +
    geom_tile() + geom_text(aes(x=motherland, y=ind_short, label=value)) +
    scale_fill_gradient2(midpoint = lvlmax/2, low = "gray", mid = "orange", high = "red", space = "Lab" ) +
    remove_legends
}

gen_plot_foreign(exploit_trade, 2)
gen_plot_foreign(exploit_trade, 4)
gen_plot_foreign(domination, 2)
gen_plot_foreign(domination, 4)
gen_plot_foreign(domination, 5)
gen_plot_foreign(ethnic_usage, 2)


