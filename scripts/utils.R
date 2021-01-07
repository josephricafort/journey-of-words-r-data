library(tidyverse)
library(rvest)
library(magrittr)
library(stringi)

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

# Convert to readable unicode characters in R
escape_unicode <- function(char){
  result <- cat(stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", char)))
  return (result)
}

# Calculate distance around the world
calcDistance <- function (lat1, lon1, lat2, lon2) {
  toRad <- function (Value) {
    return (Value * pi) / 180
  }
  
  R <- 6371; # km
  dLat <- toRad(lat2 - lat1)
  dLon <- toRad(lon2 - lon1)
  latitude1 <- toRad(lat1)
  latitude2 <- toRad(lat2)
  
  a <- sin(dLat / 2) * sin(dLat / 2) + sin(dLon / 2) * sin(dLon / 2) * cos(latitude1) * cos(latitude2)
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  d <- R * c
  return (d)
}

distFromHomeland <- function (lat, long){
  HOMELAND_LAT <- 23.817981
  HOMELAND_LONG <- 120.954427
  
  dist <- calcDistance(HOMELAND_LAT, HOMELAND_LONG, lat, long)
  return (dist)
}