library(tidyverse)
library(rvest)
library(magrittr)
library(jsonlite)

URL_LOANS <- "https://www.trussel2.com/acd/"

# Collect url links to pages of a-z
data_acd_loans_urls <- read_html(paste0(URL_LOANS, "acd-lo_a.htm")) %>% html_node(".indexline") %>% 
  html_nodes("a") %>% html_attr("href")
data_acd_loans_urls[1] <- "acd-lo_a.htm"

# Fetch function for a single page
fetch_loan_page <- function(url){
  
  # url <- paste0(URL_LOANS, data_acd_loans_urls[1]) # debug only
  data_acd_loans_tables <- read_html(url) %>% html_nodes(".settableLoan")
  table_length <- data_acd_loans_tables %>% html_nodes(".settable") %>% length
  
  result <- tibble()
  for(i in 1:table_length){
    # i <- 1 # debug only
    settable <- data_acd_loans_tables %>% html_nodes(".settable") %>% extract(i)
    keyloan <- settable %>% html_nodes("p.setline") %>% html_text
    setnote <- settable %>% html_nodes("p.setnote") %>% html_text %>% str_trim
    loan_items <- data_acd_loans_tables %>% html_nodes(".settable") %>% 
      html_nodes("table.entrytable") %>% html_nodes("td.entrytable") %>% extract(i) %>% 
      html_node("table.loanforms") %>% html_table(fill=T) %>% as.data.frame %>%
      mutate(keyloan = keyloan, setnote = ifelse(identical(setnote, character(0)), "", setnote))
    
    result <- bind_rows(result, loan_items)
  }
  
  return (result)
}

# Fetch function for all pages
fetch_loan_allpages <- function(){
  
  result <- tibble()
  for(j in 1:length(data_acd_loans_urls)){
    paste0("Fetching: ", data_acd_loans_urls[j]) %>% print
    result <- bind_rows(result, fetch_loan_page(paste0(URL_LOANS, data_acd_loans_urls[j])))
  }
  
  return (result)
}

URL_LANGLIST <- "https://pkgstore.datahub.io/core/language-codes/language-codes-full_json/data/573588525f24edb215c07bec3c309153/language-codes-full_json.json"
origin_full <- fromJSON(URL_LANGLIST) %>% select(English) %>% pull
origin_common <- c("Spanish", "Malay", "Arabic", "Sanskrit", "Chinese", "Tagalog", 
                    "Dumagat", "Kapampangan", "Javanese", "Ilokano", "Prakrit", "Hokkien",
                    "Dutch", "English", "French", "Japanese", "Hindi", "Punjabi",
                    "Portuguese", "German", "Italian")

nonforeign_origin <- c("Malay", "Tagalog", "Javanese", "Indonesian", "Balinese", "Dumagat", "Ilokano", "Iban",
                       "Kapampangan", "Bikol", "Cebuano", "Chamorro", "Philippine languages",
                       "Gilbertese", "Palauan", "Sasak", "Buginese", "Fijian", "Madurese",
                       "Minangkabau", "Makasar", "Pangasinan", "Malagasy", "Samoan",
                       "Austronesian languages", "Sundanese", "Gorontalo", "Mari")

origin_rgx <- c(origin_full, origin_common) %>% 
  unique %>% paste(collapse="|")

subgroup_factor <- c("Formosan", "WMP", "CMP", "SHWNG", "OC")

# Execute scraper
data_acd_loans_raw <- fetch_loan_allpages()
data_acd_loans_clean <- data_acd_loans_raw %>%
  rename(lang = X1, item = X2, gloss = X3) %>%
  mutate(subgroup = ifelse(is.na(item), lang, NA)) %>%
  mutate(lang = ifelse(lang == "", NA, lang)) %>%
  fill(c(subgroup, lang), .direction = "down") %>%
  mutate(setnote = str_trim(setnote)) %>%
  filter(!is.na(item)) %>%
  # Rearrange columns
  select(keyloan, item, subgroup, lang, gloss, setnote) %>%
  # Extract foreign origin
  mutate(origin = str_extract(setnote, origin_rgx)) %>%
  # mutate(origin = ifelse(is.na(origin), "Not enough info", origin)) %>%
  mutate(origin_foreign = ifelse(origin %in% nonforeign_origin, FALSE, TRUE)) %>%
  # mutate(origin_foreign = ifelse(origin_foreign == "Not enough info", NA, origin_foreign)) %>%
  mutate(subgroup = ifelse(subgroup == "xxx", NA, subgroup)) %>%
  mutate(across(c(keyloan:lang, origin), as.factor)) %>%
  mutate(subgroup = fct_relevel(subgroup, subgroup_factor))

write_json(data_acd_loans_clean, "./data/output/json/acd/acd_loans.json")

# Loanwords with origin NA
data_acd_loans_clean %>% filter(is.na(origin)) %>% select(setnote) %>% unique() %>% as.data.frame()
