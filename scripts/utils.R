library(tidyverse)
library(rvest)
library(magrittr)

# Fetch the coordinates of a given page for the Austronesian Language

coord_spoken_fetch <- function(link){
  # link <- "http://wals.info/languoid/lect/wals_code_fij"
  
  "Accessing coordinates from: " %>%
    paste(link) %>%
    print()
  
  tables <- link %>%
    read_html() %>%
    html_node("#sidebar") %>%
    html_node(".well") %>%
    html_nodes("table")
  
  coord <- tables %>%
    extract2(1) %>%
    html_nodes("span") %>%
    extract2(2) %>%
    html_text()
  
  spoken <- tables %>%
    extract(2) %>%
    html_nodes("a") %>%
    html_text
  
  return(tibble(coord, spoken))
}