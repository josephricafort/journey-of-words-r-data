library(tidyverse)
library(rvest)
library(ggplot2)
library(magrittr)

URL_ACD <- "https://www.trussel2.com/ACD/"
URL_ACD_FL <- paste0(URL_ACD, "acd-ak_1.htm")

data_acd_fl_raw <- read_html(URL_ACD_FL)

# Collect urls
acd_fl_urls_raw <- data_acd_fl_raw %>% html_node(".indexline") %>% html_nodes("a") 

## Need to inject 1 at the top
acd_fl_urls <- c("acd-ak_1.htm", acd_fl_urls_raw %>% html_attr("href"))
acd_fl_urls_index <- c("1", acd_fl_urls_raw %>% html_text %>% str_trim) %>% as.factor

acd_fl_urls_df <- tibble(acd_fl_urls_index, acd_fl_urls)

# Scrape through the findertable
fetch_innertable <- function(data_acd_innertable) {
  result_it <- tibble(engword = character(), meaning = character(), langgroup = character(), setword = character())
  engword <- NA
  for(p in 1:length(data_acd_innertable)){
    meaning <- langgroup <- setword <- NA
    currnode <- extract(data_acd_innertable, p)
    classname <- currnode %>% xml_attr("class")
    if(classname == "engword") {
      engword <- extract(data_acd_innertable, p) %>% html_text %>% str_trim
      
    } else if (classname == "engline") {
      meaning <- currnode %>% html_text %>% word(1, sep="\\-") %>% str_trim
      langgroup <- currnode %>% html_node(".langgroup") %>% html_text
      setword <- currnode %>% html_node(".setword") %>% html_text
      
    } else {
      next
    }
    
    result_it <- result_it %>% bind_rows(
      tibble(engword, meaning, langgroup, setword)
    )
  }

  return (result_it)
}

# Loop through every page (1, 2, 3, A, B, C... Z)
fetch_allpages <- function(){
  result_ft <- tibble(engword = character(), meaning = character(), langgroup = character(), setword = character())
  for(u in 1:length(acd_fl_urls)){
    data_acd_fl_raw <- read_html(paste0(URL_ACD, acd_fl_urls[u]))
    paste("<---", acd_fl_urls[u], "Fetching page data --->", sep=" ") %>% print
    
    data_acd_page <- data_acd_fl_raw %>% html_nodes(".findertable") %>% html_nodes("td.innertable") %>%
      html_nodes("p:not(.indexline2)")

    result_ft <- result_ft %>% bind_rows(
      fetch_innertable(data_acd_page)
    )
  }
  return (result_ft)
}

data_acd_findertable <- fetch_allpages()
data_acd_findertable_clean <- data_acd_findertable %>% filter(!is.na(meaning)) %>%
  mutate(setword = paste0("*", setword))

write.csv(data_acd_findertable_clean, "./data/output/csv/data_acd_findertable.csv")
