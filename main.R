# install.packages("ggrepel")
# install.packages("RColorBrewer")
# install.packages("jsonlite")

library(tidyverse)
library(rvest)
library(magrittr)
library(maps)
library(ggrepel)
library(RColorBrewer)
library(jsonlite)

source("./info.R")
source("./utils.R")
source("./words.R")

# Data last fetched: November 1, 2019

url <- "https://abvd.shh.mpg.de/austronesian/language.php?id="
url_api <- "https://abvd.shh.mpg.de/utils/save/index.php?type=xml&section=austronesian&language="
# first <- 454. 1195 # Where errors have occured
first <- 1
last <- 1626

language_info <- tibble()
language_words <- tibble()

# There are two ways to fetch the data either by web scraping or using an API provided by the site owners
# Select only 1 of the 2 ways

# # Fetching by Webscraping
# 
# for (i in first:last) {
#   print(paste("Downloading language info ", i, "...", sep = ""))
#   webpage <- paste(url, i %>% as.character, sep = "") %>%
#     read_html()
#   
#   language_info <- bind_rows(
#     language_info,
#     language_info_fetch(webpage)
#   )
#   language_words <- bind_rows(
#     language_words,
#     language_words_fetch(webpage)
#   )
# }
# 
# 
# # # Debug
# # url <- "https://abvd.shh.mpg.de/austronesian/language.php?id="
# # i <- 454
# # webpage <- paste(url, i %>% as.character, sep = "") %>%
# #   read_html()
# # language_info_fetch(webpage)
# # language_words_fetch(webpage)
# 
# language_info_clean <- language_info %>%
#   mutate_at(.vars = vars(c("long", "lat")), as.numeric) %>%
#   mutate_at(.vars = vars(c("language", "language_code", "spoken")), as.factor) %>%
#   separate(statistics, into = c("total_data", "retentions", "empty_col", "loans"), sep="\\n+") %>%
#   mutate_at(vars(c("total_data", "retentions", "empty_col", "loans")), str_trim) %>%
#   select(-empty_col)
# 
# language_words_clean <- language_words %>%
#   mutate_at(.vars = vars(c("language", "word")), as.factor) %>%
#   separate(cognacy, c("cognacy1", "cognacy2"), sep="\\,", fill="warn") %>%
#   mutate(cognacy1 = str_trim(cognacy1) %>% as.numeric,
#          cognacy2 = str_trim(cognacy2) %>% as.numeric,
#          loan = str_trim(loan) %>% as.factor)



# # Fetching by API
# 
# language_info_api <- tibble()
# language_words_api <- tibble()
# 
# for (i in first:last) {
#   language_info <- tibble()
#   language_words <- tibble()
# 
#   print(paste("Downloading language info ", i, "...", sep = ""))
#   xml_string <- paste(url_api, i %>% as.character, sep = "") %>%
#     read_html()
# 
#   language_info <- xml_string %>%
#     html_node("record")
# 
#   id_lang <- language_info %>% html_node("id") %>% html_text
#   language <- language_info %>% html_node("language") %>% html_text
#   author <- language_info %>% html_node("author") %>% html_text
#   silcode <- language_info %>% html_node("silcode") %>% html_text
#   glottocode <- language_info %>% html_node("glottocode") %>% html_text
#   notes <- language_info %>% html_node("notes") %>% html_text
#   problems <- language_info %>% html_node("problems") %>% html_text
#   classification <- language_info %>% html_node("classification") %>% html_text
#   typedby <- language_info %>% html_node("typedby") %>% html_text
#   checkedby <- language_info %>% html_node("checkedby") %>% html_text
# 
#   latitude <- xml_string %>% html_node("latitude") %>% html_text
#   longitude <- xml_string %>% html_node("longitude") %>% html_text
# 
#   language_words <- xml_string %>%
#     html_nodes("record") %>%
#     extract(-1)
# 
#   id <- language_words %>% html_node("id") %>% html_text
#   word_id <- language_words %>% html_node("word_id") %>% html_text
#   word <- language_words %>% html_node("word") %>% html_text
#   item <- language_words %>% html_node("item") %>% html_text
#   annotation <- language_words %>% html_node("annotation") %>% html_text
#   loan <- language_words %>% html_node("loan") %>% html_text
#   cognacy <- language_words %>% html_node("cognacy") %>% html_text
#   pmpcognacy <- language_words %>% html_node("pmpcognacy") %>% html_text
# 
#   language_info <- tibble(id_lang, language, author, silcode, glottocode, notes, problems, classification, typedby, checkedby, latitude, longitude)
#   language_words <- tibble(id_lang, language, id, word_id, word, item, annotation, loan, cognacy, pmpcognacy)
# 
#   language_info_api <- bind_rows(language_info_api, language_info)
#   language_words_api <- bind_rows(language_words_api, language_words)
# 
#   print(paste("Language ", language, " accessed...", sep = ""))
# }

language_info_api_clean <- language_info_api %>%
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude)) %>%
  # filter(str_detect(classification, "Austronesian")) %>%
  as_tibble

language_words_api_clean <- language_words_api %>%
  filter(!is.na(id)) %>%
  left_join(language_info_api %>%
              select(id_lang, language, silcode),
            by = c("id_lang", "language")) %>%
  select(language, silcode, id_lang, id:pmpcognacy) %>%
  separate(col = cognacy, into = c("cognacy1", "cognacy2"), sep="\\,") %>%
  mutate(cognacy1 = as.numeric(cognacy1),
         cognacy2 = as.numeric(cognacy2)) %>%
  arrange(language) %>%
  left_join(language_info_api_clean %>%
              select(language, silcode)) %>%
  # Replace non-alphanumeric characters with url friendly letters
  mutate(word = str_replace_all(word, "\\/", ", "),
         word = str_replace_all(word, "\\?", ""),
         word = str_replace_all(word, "worm \\(earthworm\\)", "worm, earthworm"),
         word = str_replace_all(word, "\\,", ""),
         word = str_replace_all(word, " ", "_"),
         word = str_to_lower(word)) %>%
  as_tibble
  
language_heirarchy_api_clean <- language_info_api_clean %>%
  select(id_lang, language, silcode, classification) %>%
  separate(
    classification,
    "group" %>%
      paste(seq(from = 1, to = 14),
            sep = ""),
    sep = "\\,",
    n = 20
  ) %>%
  mutate_if(is.character, str_trim) %>%
  mutate_if(is.character, as.factor) %>%
  arrange(group1:group14)

# Chop words data into multiple pieces for easy accessing for the API
word_list <- language_words_api_clean$word %>% unique %>% c()
for(id in 1:length(word_list)){
  word_compare <- word_list[id]
  result <- language_words_api_clean %>% filter(word == word_compare & word != "")
  # filename <- str_replace_all(word_compare, " ", "%")
  
  # Export into separate json files
  maindir <- "data/output/json/language_words/"
  filedir <- paste0(maindir, word_compare)
  write_json(result, paste0(filedir, ".json"))
}
  
write_json(language_info_api_clean, "data/output/json/language_info.json", pretty = TRUE)
write_json(language_words_api_clean, "data/output/json/language_words.json", pretty = TRUE)
write_json(language_heirarchy_api_clean, "data/output/json/language_heirarchy.json", pretty = TRUE)

write.csv(language_info_clean, "data/output/csv/language_info.csv")
write.csv(language_words_clean, "data/output/csv/language_words.csv")

# Visualize
# Create scatterplot map of words

world <- map_data("world2")

language_info_api_clean_sel <- language_info_api_clean %>%
  mutate(longitude = ifelse(longitude < 0, longitude + 360, longitude)) %>%
  filter(classification %>% str_detect("Austronesian"))

language_words_api_clean_sel <- language_words_api_clean %>%
  mutate(longitude = ifelse(longitude < 0, longitude + 360, longitude)) %>%
  filter(classification %>% str_detect("Austronesian")) %>%
  # filter(word %>% str_detect("red"))
  filter(word == "woman/female")

ggplot(language_words_api_clean_sel) +
  geom_polygon(data = world, aes(long, lat, group = group), fill="grey", alpha=0.3) +
  geom_density_2d(aes(longitude, latitude)) +
  # stat_contour(aes(x = longitude, y = latitude, z = cognacy1)) +
  geom_point(data = language_info_api_clean_sel, aes(longitude, latitude), size = 0.5) +
  geom_text(aes(longitude, latitude, label = item, color = cognacy1), size = 3, check_overlap = TRUE) +
  theme_minimal() +
  xlim(35, 270) + ylim(-40, 35) +
  scale_color_gradient(low = "red", high = "blue")

language_words_api_clean_sel %>%
  arrange(desc(cognacy1))
