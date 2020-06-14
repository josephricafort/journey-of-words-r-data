library(tidyverse)
library(rvest)

language_info_fetch <- function(web_content) {
  data_info <- tibble()
  # data_words <- tibble()
  
  # # Debug
  # i <- 455
  # print(paste("Downloading language info ", i, "...", sep = ""))
  # web_content <- paste(url, i %>% as.character, sep = "") %>%
  #   read_html()
  
  language <- web_content %>%
    html_node("#content") %>%
    html_node("h1") %>%
    html_text() %>%
    str_replace("Language: ", "") %>%
    str_trim
  
  # Access Links to World Atlas and Ethnologue
  resource_list <- web_content %>%
    html_nodes(".resource_list") %>%
    html_nodes("li")
  
  # World Atlas Info and Coordinates
  world_atlas_string <- resource_list %>%
    html_text
  world_atlas_index <- world_atlas_string %>%
    str_subset("not available", negate = TRUE) %>%
    str_which("World Atlas")
    # str_which("^(?!Ethnologue)World Atlas")
  if (world_atlas_index %>% length != 0) {
    world_atlas_link <- resource_list %>%
      extract2(world_atlas_index) %>%
      html_nodes("a") %>%
      html_attr("href")
    coords_spoken <- coord_spoken_fetch(world_atlas_link)
    coords <- coords_spoken$coord
    spoken <- coords_spoken$spoken
  } else {
    world_atlas_link <- NA
    coords_spoken <- NA
    coords <- NA
    spoken <- NA
  }
  
  # Ethnologue Info
  ethno_index <- resource_list %>%
    html_text %>%
    str_which("Ethnologue Information")
  if (ethno_index %>% length != 0) {
    ethno_link <- resource_list %>%
      extract2(ethno_index) %>%
      html_nodes("a") %>%
      html_attr("href")
    language_code <- resource_list %>%
      extract2(ethno_index) %>%
      html_text %>%
      str_replace("Ethnologue Information for", "") %>%
      str_trim
  } else {
    ethno_link <- NA
    language_code <- NA
  }
  
  # Info table
  lang_info_table <- web_content %>%
    html_node(".data") %>%
    html_node("table") %>%
    html_table(fill = TRUE) %>%
    # select(-X3) %>%
    slice(-6)
  
  source_author <- lang_info_table$X2[1]
  notes <- lang_info_table$X2[2]
  data_entry <- lang_info_table$X2[3]
  statistics <- lang_info_table$X2[4]
  classification <- lang_info_table$X2[5]
  
  # Info table
  lang_info_df <- tibble(
    language,
    language_code,
    coords,
    spoken,
    source_author,
    notes,
    data_entry,
    statistics,
    classification,
    world_atlas_link,
    ethno_link
  ) %>%
    separate(coords, c("long", "lat"), sep = "\\,")
  
  # # Words table
  # lang_words_table <- web_content %>%
  #   html_nodes(".data") %>%
  #   extract2(2) %>%
  #   html_node("table") %>%
  #   html_table() %>%
  #   rename(
  #     "id" = "ID:",
  #     "word"  = "Word:",
  #     "item"  = "Item:",
  #     "annotation"  = "Annotation:",
  #     "cognacy"  = "Cognacy:",
  #     "loan"  = "Loan:"
  #   ) %>%
  #   mutate(language = language) %>%
  #   select(language, id:loan)
  
  # data_info <- bind_rows(data_info, lang_info_df)
  # data_words <- bind_rows(data_words, lang_words_table)
  print(paste("Info for ", i, " ", language, " fetched", sep = ""))
  
  return(lang_info_df)
}
