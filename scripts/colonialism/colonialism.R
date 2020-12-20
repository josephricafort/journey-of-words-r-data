library(readxl)
library(stringr)
library(tidyverse)

path <- "data/input/colonialism/dataverse_files/Colonial_transformation_data.xls"

excel_sheets(path)

an_countries <- c("FJI", "IDN", "MDG", "MYS", "PHL", "PNG", "SLB", "TWN", "VUT") # BRN and SGP have not enough data
var_interests <- c('Main colonial "motherland", source: Ziltener/KŸnzler')

ctry_code <- "Country Code World Bank"
data_colonialism_raw <- read_excel(path, sheet="Sheet1") %>% filter(`Country Code World Bank` %in% an_countries)

indicators <- colnames(data_colonialism_raw)[7:29]

data_colonialism_clean <- data_colonialism_raw %>% gather(indicator, value, indicators) %>%
  rename(ctry_code_wb = `Country Code World Bank`, ctry_name = `Country Name`,
         motherland = `Main colonial "motherland", source: Ziltener/KŸnzler`,
         onset_yr = `onset of colonialism, source: Ziltener/KŸnzler 2008`,
         end_yr = `end of colonialism, source: Ziltener/KŸnzler 2008`,
         col_yrs = `COLYEARS`) %>%
  # Add Spain as country for Philippines
  mutate(motherland = ifelse(ctry_name == "Philippines", "ES/US", motherland)) %>%
  mutate(ctry_code_wb = fct_inorder(ctry_code_wb, ordered=F),
         ctry_name = fct_inorder(ctry_name, ordered=F),
         motherland = fct_inorder(motherland, ordered=F)) %>%
  mutate(value = as.integer(value)) %>%
  group_by(indicator) %>% mutate(max_lvl = max(value, na.rm=T),
                                 min_lvl = min(value, na.rm=T)) %>% ungroup %>%
  droplevels()

ind_short <- c("col_admin", "col_borders", "col_foreign_trade", "col_invest_cnctrate", "col_invest_infra",
               "col_trade_cnctrate", "col_violence", "ethnic_function", "foreign_presence", "form_col_domination",
               "gold_silver_mining", "immigration_foreign_workers", "lvl_col_transform", "lvl_econ_transform",
               "lvl_pol_transform", "lvl_soc_transform", "mining_dur_col", "missionary",
               "plantations", "power_transform_decol", "violent_col", "violent_ind", "violent_resist")

data_colonialism_legends <- data_colonialism_clean %>% select(indicator, value) %>%
  group_by(indicator) %>% summarize %>% 
  mutate(value_meaning = str_match(indicator, "\\(([^()]*)\\)")[,2]) %>%
  mutate(indicator = fct_inorder(indicator, ordered=NA)) %>%
  bind_cols(ind_short = ind_short) %>% select(ind_short, indicator:value_meaning) %>%
  mutate(lvl = ifelse(str_detect(ind_short, "lvl"), TRUE, FALSE))

data_colonialism <- data_colonialism_clean %>% left_join(data_colonialism_legends)

write_json(data_colonialism, "./data/output/json/colonialism.json")
