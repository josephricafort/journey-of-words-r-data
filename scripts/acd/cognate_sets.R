library(tidyverse)
library(rvest)
library(ggplot2)
library(magrittr)
library(jsonlite)
library(forcats)
library(stringi)

URL_ACD <- "https://www.trussel2.com/ACD/"
URL_ACD_FP <- paste0(URL_ACD, "acd-s_a1.htm")

# Reread the saved data rather than rerunning the script
acd_data_fragment_AtoG <- read.csv("data/output/csv/acd_data_cognatesets_AtoG.csv", stringsAsFactors = F) %>% select(-X) %>% as_tibble
acd_data_fragment_HtoM <- read.csv("data/output/csv/acd_data_cognatesets_HtoM.csv", stringsAsFactors = F) %>% select(-X) %>% as_tibble
acd_data_fragment_NtoR <- read.csv("data/output/csv/acd_data_cognatesets_NtoR.csv", stringsAsFactors = F) %>% select(-X) %>% as_tibble
acd_data_fragment_StoZ <- read.csv("data/output/csv/acd_data_cognatesets_StoZ.csv", stringsAsFactors = F) %>% select(-X) %>% as_tibble

## Uncomment if need to rerun script
# data_acd_raw <- read_html(paste0(URL_ACD_FP))
# 
# acd_letters_AtoZ <- data_acd_raw %>% html_node("map") %>% html_nodes("area") %>% html_attr("href")
# acd_letters_AtoG <- acd_letters_AtoZ[0:7]
# acd_letters_HtoM <- acd_letters_AtoZ[8:13]
# acd_letters_NtoR <- acd_letters_AtoZ[14:22]
# acd_letters_StoZ <- acd_letters_AtoZ[22:29]
# 
# # Input the hyperlinks and return data
# acd_data_fetch <- function(acd_letters_url){
#   
#   # 1. Gather cognate sets url
#   result <- tibble()
#   u_start <- n_start <- e_start <- 1
#   # u_start <- u #13
#   # n_start <- n #85
#   # e_start <- e #1
#   for(u in u_start:length(acd_letters_url)){
#     data_acd_raw <- read_html(paste0(URL_ACD, acd_letters_url[u]))
#     paste("<--- COGNATE SET", u, acd_letters_url[u], "--->", sep=" ") %>% print
#    
#     # 2. Collect cognate sets data per url
#     nsettable <- data_acd_raw %>% html_nodes("table.settable") %>% length
#     for(n in n_start:nsettable){
#       data_acd_table <- data_acd_raw %>% html_nodes("table.settable") %>% extract2(n) %>% html_nodes("td.settable")
#       
#       v_key <- data_acd_table %>% html_nodes("p.setline") %>% html_nodes(".key") %>% html_text
#       v_setline <- data_acd_table %>% html_nodes("p.setline") %>% html_nodes("span.setline") %>% html_text
#       
#       paste(acd_letters_url[u], n, "Fetching...", v_key, v_setline, sep=" ") %>% print
#       
#       # 3 Gather data within the table.entrytable
#       ntable <- data_acd_table %>% html_nodes("table.entrytable") %>% length
#       for(e in e_start:ntable){
#         data_acd_entrytable <- data_acd_table %>% html_nodes("table.entrytable") %>% extract2(e) %>% html_nodes(".entrytable")
#         v_pidno <- data_acd_entrytable %>% html_nodes(".pidno") %>% html_text %>% paste(collapse=", ")
#         v_plang_pcode <- data_acd_entrytable %>% html_nodes(".pLang") %>% html_nodes("span.pcode") %>% html_text %>% paste(collapse=", ")
#         v_plang_lineform <- data_acd_entrytable %>% html_nodes(".pLang") %>% html_nodes("span.lineform") %>% html_text %>% paste(collapse=", ")
#         v_plang_linegloss <- data_acd_entrytable %>% html_nodes(".pLang") %>% html_nodes("span.linegloss") %>% html_text %>% paste(collapse=", ")
#         
#         data_acd_tablebind <- data_acd_entrytable %>% html_nodes(".forms") %>% html_table(fill=TRUE) %>% bind_rows
#         if(dim(data_acd_tablebind)[1] == 0){ next }
#         data_acd_forms <- data_acd_tablebind %>% bind_rows() %>%
#           rename(lg = X1, formuni = X2, gloss = X3) %>%
#           filter(!is.na(lg)) %>% mutate(gloss = as.character(gloss))
#         
#         data_acd_settable <- data_acd_forms %>% mutate(
#           key = v_key, setline = v_setline, pidno = v_pidno, 
#           plang_pcode = v_plang_pcode, plang_lineform = v_plang_lineform, plang_gloss = v_plang_linegloss
#         ) %>% select(key, setline, pidno:plang_gloss, lg:gloss)
#         
#         result <- result %>% bind_rows(data_acd_settable)
#         paste(acd_letters_url[u], n, "-", e, "Done!", v_key, v_setline, sep=" ") %>% print
#       }
#       e_start <- 1
#       note <- data_acd_table %>% html_node("p.setnote") %>% html_text
#     }
#     n_start <- 1
#   }
#   
#   return (result)
# }
# 
# # consolidate all the fragmented data
# fetch_data_fragments <- function(){
#   acd_data_fragment_AtoG <- acd_data_fetch(acd_letters_AtoG)
#   acd_data_fragment_HtoM <- acd_data_fetch(acd_letters_HtoM)
#   acd_data_fragment_NtoR <- acd_data_fetch(acd_letters_NtoR)
#   acd_data_fragment_StoZ <- acd_data_fetch(acd_letters_StoZ)
# }
# 
# fetch_data_fragments()

acd_data_consolidated <- bind_rows(acd_data_fragment_AtoG, acd_data_fragment_HtoM, acd_data_fragment_NtoR, acd_data_fragment_StoZ) %>%
  mutate(subgroup = ifelse(is.na(formuni) & is.na(gloss), lg, NA)) %>% fill(subgroup) %>%
  select(key:plang_gloss, subgroup, lg:gloss)
  
acd_data_clean <- acd_data_consolidated %>% 
  filter(!is.na(formuni) & !is.na(gloss))

# write.csv(acd_data_clean, "data/output/csv/acd_data.csv")

# # Save individual data fragments
# write.csv(acd_data_fragment_AtoG, "data/output/csv/acd_data_cognatesets_AtoG.csv")
# write.csv(acd_data_fragment_HtoM, "data/output/csv/acd_data_cognatesets_HtoM.csv")
# write.csv(acd_data_fragment_NtoR, "data/output/csv/acd_data_cognatesets_NtoR.csv")
# write.csv(acd_data_fragment_StoZ, "data/output/csv/acd_data_cognatesets_StoZ.csv")

# Prep data to be used for web
language_words_count_top20pc  # follow similar structure

subgroup_fac <- c("Formosan", "WMP", "CMP", "SHWNG", "OC")

acd_data_prep <- acd_data_clean %>% rename(
  cognate_gp = key, cognate_gloss = setline,
  plang_id = pidno, plang_subgroup = subgroup,
  item = formuni, lang = lg
) %>% group_by(cognate_gp, plang_subgroup, item) %>%
  summarize(cognate_gloss = first(cognate_gloss),
            plang_id = first(plang_id),
            plang_lineform = first(plang_lineform),
            plang_gloss = first(plang_gloss),
            lang = paste0(lang, collapse=", "),
            count = n()) %>%
  select(cognate_gp, cognate_gloss, plang_subgroup, plang_id:plang_gloss, item, lang, count) %>%
  arrange(cognate_gp) %>% ungroup %>%
  # Remove notes in item
  filter(!str_detect(item, "Note:")) %>%
  # Add indeces
  mutate(plang_subgroup = fct_relevel(plang_subgroup, subgroup_fac),
         plang_subgroup_id = as.integer(plang_subgroup)) %>%
  mutate(plang_lineform = fct_inorder(plang_lineform, ordered = NA),
         plang_lineform_id = as.integer(plang_lineform)) %>%
  # Measure length of the item
  mutate(item_length = str_length(item)) %>%
  # Characters to factors
  mutate(across(c(cognate_gp, plang_subgroup, plang_lineform, item, lang), as.factor)) %>%
  mutate(plang_id = as.integer(plang_id))
  # # Escape unicode characters
  # mutate(across(c(cognate_gp:lang), function (char){
  #   return (cat(stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", char))))
  # }))

# Filter only top20 percent
acd_data_prep_plus25 <- acd_data_prep %>%
  group_by(cognate_gp) %>% filter(n() >= 25 & count >= 2) %>% 
  ungroup %>% arrange(desc(count), item)
  # top_frac(0.15, count)

write_json(acd_data_prep, "data/output/json/acd/cognate_sets.json")
write_json(acd_data_prep_plus25, "data/output/json/acd/cognate_sets_plus25.json")

