# This data was accessed from the Austronesian Comparative Dictionary.
# Works of Blust and Trussel https://www.trussel2.com/acd/acd-langhelp.htm

library(tidyverse)
library(rvest)
library(jsonlite)

URL_LANG_HELP <- "https://www.trussel2.com/acd/acd-langhelp.htm"
URL_INDICES <- "https://www.trussel2.com/acd/acd-l_A.htm"

# Access the indices
data_indices_raw <- read_html(URL_INDICES) %>%  html_node(".indexline")

path <- data_indices_raw %>% html_nodes("a") %>% html_attr("href")
letter <- data_indices_raw %>% html_nodes("a") %>% html_text %>% str_trim

a_row <-tibble(letter="A", path="acd-l_a.htm")

key_table <- a_row %>% bind_rows(tibble(letter, path))

# Gather language properties
# Props: 
# langName - the language name
# langPrimarySource - the primary source reference work
# langPrimarySourceLink -  a link to the full entry on the References page
# langSubgroup -
# langISO -	the ISO code
# langISOName -
# langISOLink - a link to the corresponding entry in Ethnologue
# langLocation - the approximate geographical location
# langDialects - a list of dialects, their ISO code links, and counts of words used in the ACD


fetchLanguageInfo <- function (url){
  # #--- debug only, uncomment when not used
  # url <- "acd-l_a.htm"
  # i <- 2
  # #---
  
  URL_LANG <- "https://www.trussel2.com/acd/"
  url_page <- paste0(URL_LANG, url)
  
  data_langinfo_raw <- read_html(url_page) %>% html_nodes("p.langline")
  
  data_langinfo_loop <- tibble(langName = character(),
                               langNameAKA = character(),
                               langSubgroup = character(),
                               langPrimarySource = character(),
                               langPrimarySourceLink = character(),
                               langISOCode = character(),
                               langISOName = character(),
                               langISOLink = character(),
                               langLocation = character())
  for(i in 1:length(data_langinfo_raw)){
    langName <- data_langinfo_raw[i] %>% html_nodes("span.langname") %>% html_text
    langNameAKA <- data_langinfo_raw[i] %>% html_nodes("span.aka") %>% html_text
    langSubgroup <- data_langinfo_raw[i] %>% html_nodes("span.langgroup") %>% html_text
    langPrimarySource <- data_langinfo_raw[i] %>% html_nodes("span.bibref") %>% html_text
    langPrimarySourceLink <- data_langinfo_raw[i] %>% html_nodes("span.bibref") %>% html_nodes("a.bib") %>% html_attr("href")
    langISOCode <- data_langinfo_raw[i] %>% html_nodes("span.ISOline") %>% html_nodes("a.ISO span.ISO") %>% html_text
    langISOName <- data_langinfo_raw[i] %>% html_nodes("span.ISOline") %>% html_nodes("span.ISOname") %>% html_text
    langISOLink <- data_langinfo_raw[i] %>% html_nodes("span.ISOline") %>% html_nodes("a.ISO") %>% html_attr("href")
    langLocation <-  data_langinfo_raw[i] %>% html_nodes("span.ISOline") %>% html_nodes("span.Loc") %>% html_text("text")
    
    # Check for empty variable and replace with NA
    checkEmpty <- function(var){
      result <- ifelse(!identical(var, character(0)), var, NA)
      return (result)
    }
    
    langName <- ifelse(!identical(langName, character(0)), langName, NA)
    langNameAKA <- ifelse(!identical(langNameAKA, character(0)), langNameAKA, NA)
    langSubgroup <- ifelse(!identical(langSubgroup, character(0)), langSubgroup, NA)
    langPrimarySource <- ifelse(!identical(langPrimarySource, character(0)), langPrimarySource, NA)
    langPrimarySourceLink <- ifelse(!identical(langPrimarySourceLink, character(0)), langPrimarySourceLink, NA)
    langISOCode <- ifelse(!identical(langISOCode, character(0)), langISOCode, NA)
    langISOName <- ifelse(!identical(langISOName, character(0)), langISOName, NA)
    langISOLink <- ifelse(!identical(langISOLink, character(0)), langISOLink, NA)
    langLocation <- ifelse(!identical(langLocation, character()), langLocation, NA)
    
    # Gather into a table
    row <- tibble(langName, langNameAKA, langSubgroup, langPrimarySource, langPrimarySourceLink, 
                  langISOCode, langISOName, langISOLink, langLocation)
    data_langinfo_loop <- bind_rows(data_langinfo_loop, row)
  }
  
  return (data_langinfo_loop)
}

fetchLanguageInfo("acd-l_q.htm") # check if working

fetchAllLanguageInfo <- function (){
  letterUrls <- key_table[,2] %>% pull
  
  data_result <- tibble()
  for(i in 1:length(letterUrls)){
    print(paste0("Fetching: ", letterUrls[i]))
    data_result <- data_result %>% bind_rows(fetchLanguageInfo(letterUrls[i]))
    print(paste0(letterUrls[i], " Done!"))
  }
  
  return (data_result)
}

data_langInfo <- fetchAllLanguageInfo()

data_langInfo_clean <- data_langInfo %>%
  mutate(langLocation = recode(langLocation, "anuatu" = "Vanuatu"))

write_json(data_langInfo_clean, "./data/output/json/acd/lang_info.json")
