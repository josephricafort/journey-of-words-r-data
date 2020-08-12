library(tidyverse)
library(rvest)
library(ggplot2)
library(magrittr)

URL_ACD <- "https://www.trussel2.com/ACD/"
URL_ACD_FP <- paste0(URL_ACD, "acd-s_a1.htm")

data_acd_raw <- read_html(paste0(URL_ACD_FP))

acd_letters_AtoZ <- data_acd_raw %>% html_node("map") %>% html_nodes("area") %>% html_attr("href")
acd_letters_AtoG <- acd_letters_url[0:7]
acd_letters_HtoM <- acd_letters_url[8:13]
acd_letters_NtoR <- acd_letters_url[14:22]
acd_letters_StoZ <- acd_letters_url[22:29]

# Input the hyperlinks and return data
acd_data_fetch <- function(acd_letters_url){
  
  # 1. Gather cognate sets url
  result <- tibble()
  u_start <- n_start <- e_start <- 1
  # u_start <- u #13
  # n_start <- n #85
  # e_start <- e #1
  for(u in u_start:length(acd_letters_url)){
    data_acd_raw <- read_html(paste0(URL_ACD, acd_letters_url[u]))
    paste("<--- COGNATE SET", u, acd_letters_url[u], "--->", sep=" ") %>% print
   
    # 2. Collect cognate sets data per url
    nsettable <- data_acd_raw %>% html_nodes("table.settable") %>% length
    for(n in n_start:nsettable){
      data_acd_table <- data_acd_raw %>% html_nodes("table.settable") %>% extract2(n) %>% html_nodes("td.settable")
      
      v_key <- data_acd_table %>% html_nodes("p.setline") %>% html_nodes(".key") %>% html_text
      v_setline <- data_acd_table %>% html_nodes("p.setline") %>% html_nodes("span.setline") %>% html_text
      
      paste(acd_letters_url[u], n, "Fetching...", v_key, v_setline, sep=" ") %>% print
      
      # 3 Gather data within the table.entrytable
      ntable <- data_acd_table %>% html_nodes("table.entrytable") %>% length
      for(e in e_start:ntable){
        data_acd_entrytable <- data_acd_table %>% html_nodes("table.entrytable") %>% extract2(e) %>% html_nodes(".entrytable")
        v_pidno <- data_acd_entrytable %>% html_nodes(".pidno") %>% html_text %>% paste(collapse=", ")
        v_plang_pcode <- data_acd_entrytable %>% html_nodes(".pLang") %>% html_nodes("span.pcode") %>% html_text %>% paste(collapse=", ")
        v_plang_lineform <- data_acd_entrytable %>% html_nodes(".pLang") %>% html_nodes("span.lineform") %>% html_text %>% paste(collapse=", ")
        v_plang_linegloss <- data_acd_entrytable %>% html_nodes(".pLang") %>% html_nodes("span.linegloss") %>% html_text %>% paste(collapse=", ")
        
        data_acd_tablebind <- data_acd_entrytable %>% html_nodes(".forms") %>% html_table(fill=TRUE) %>% bind_rows
        if(dim(data_acd_tablebind)[1] == 0){ next }
        data_acd_forms <- data_acd_tablebind %>% bind_rows() %>%
          rename(lg = X1, formuni = X2, gloss = X3) %>%
          filter(!is.na(lg)) %>% mutate(gloss = as.character(gloss))
        
        data_acd_settable <- data_acd_forms %>% mutate(
          key = v_key, setline = v_setline, pidno = v_pidno, 
          plang_pcode = v_plang_pcode, plang_lineform = v_plang_lineform, plang_gloss = v_plang_linegloss
        ) %>% select(key, setline, pidno:plang_gloss, lg:gloss)
        
        result <- result %>% bind_rows(data_acd_settable)
        paste(acd_letters_url[u], n, "-", e, "Done!", v_key, v_setline, sep=" ") %>% print
      }
      e_start <- 1
      note <- data_acd_table %>% html_node("p.setnote") %>% html_text
    }
    n_start <- 1
  }
  
  return (result)
}

# consolidate all the fragmented data
acd_data_fragment_AtoG <- acd_data_fetch(acd_letters_AtoG)
acd_data_fragment_HtoM <- acd_data_fetch(acd_letters_HtoM)
acd_data_fragment_NtoR <- acd_data_fetch(acd_letters_NtoR)
acd_data_fragment_StoZ <- acd_data_fetch(acd_letters_StoZ)

acd_data_consolidated <- bind_rows(acd_data_fragment_AtoG, acd_data_fragment_HtoM, acd_data_fragment_NtoR, acd_data_fragment_StoZ) %>%
  mutate(subgroup = ifelse(is.na(formuni) & is.na(gloss), lg, NA)) %>% fill(subgroup) %>%
  select(key:plang_gloss, subgroup, lg:gloss)
  
acd_data_clean <- acd_data_consolidated %>% 
  filter(!is.na(formuni) & !is.na(gloss))

write.csv(acd_data_clean, "data/output/csv/acd_data.csv")
