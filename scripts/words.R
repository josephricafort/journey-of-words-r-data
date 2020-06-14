library(tidyverse)
library(rvest)
library(magrittr)

language_words_fetch <- function(web_content) {
  data_words <- tibble()
  
  # # Debug
  # i <- 454
  # print(paste("Downloading language info ", i, "...", sep = ""))
  # web_content <- paste(url, i %>% as.character, sep = "") %>%
  #   read_html()
  
  language <- web_content %>%
    html_node("#content") %>%
    html_node("h1") %>%
    html_text() %>%
    str_replace("Language: ", "") %>%
    str_trim
  
  lang_words_table <- web_content %>%
    html_nodes(".data") %>%
    extract2(2) %>%
    html_node("table") %>%
    html_table() %>%
    rename(
      "id" = "ID:",
      "word"  = "Word:",
      "item"  = "Item:",
      "annotation"  = "Annotation:",
      "cognacy"  = "Cognacy:",
      "loan"  = "Loan:"
    ) %>%
    mutate(language = language) %>%
    mutate(cognacy = as.character(cognacy), # Fix: Language No. 456 consistent cognacy column as.character
           loan = as.character(loan)) %>% # Fix: Language No. 1195 consistent loan column as.character
    select(language, id:loan) %>%
    as_tibble
  
  # data_words <- bind_rows(data_words, lang_words_table)
  print(paste("Words for ", i, " ", language, " fetched", sep = ""))
  
  return(lang_words_table)
}
