# The goal of this script is to combine and generate the data
# to be used for the Austronesian Data Story

install.packages("rapportools")
library(tidyverse)
library(jsonlite)
library(rapportools)


github_path <- "https://raw.githubusercontent.com/josephricafort/journey-of-words-r-data/master/"
github_api_path <- paste0(github_path, "data/api")
local_output_path <- "./data/output/json"
local_api_path <- "./api/"

chapters <- c("world", "nature", "conversion", "extraction", "fate")

#--- Exploration data ---
# This data will be used for exploration at the end of the interactive
# This data is sourced from Austronesian Basic Vocabulary Database https://abvd.shh.mpg.de/austronesian/

abvd_words_path <- paste0(local_output_path, "/words_info.json")
abvd_lang_path <- paste0(local_output_path, "/language_info.json")

abvdWordsData <- fromJSON(abvd_words_path) %>% as_tibble
abvdLangData <- fromJSON(abvd_lang_path) %>% as_tibble

#--- ACD data ---
# This data will be used for the Words Chart in the scrolly part

# List of data we need and structure
# WordsSelectionData - vars: wordCatEn, wordEn, wordProtoAn, wordAn
# WordsInfoData
# LanguageData
# RegionData

acd_output_path <- paste0(local_output_path, "/acd")
acd_cognate_sets_path <- paste0(acd_output_path, "/cognate_sets.json")
acd_cognate_sets_categories_path <- paste0(acd_output_path, "/acd_cognate_sets_categories.json")
acd_loans_path <- paste0(acd_output_path, "/acd_loans.json")
acd_languages_path <- paste0(acd_output_path, "/lang_info.json")

acdCognateSetsData <- fromJSON(acd_cognate_sets_path) %>% as_tibble # All items from ACD database
acdCognateSetsCategoriesData <- fromJSON(acd_cognate_sets_categories_path) %>% as_tibble # Filtered top 100 words
acdLoansData <- fromJSON(acd_loans_path) %>% as_tibble
acdLanguagesData <- fromJSON(acd_languages_path) %>% as_tibble

unique_rows <- function (data){
  result <- data %>% group_by_all %>% summarize %>% ungroup
  return (result)
}

languagesData <- acdLanguagesData %>%
  select(-langPrimarySource, -langPrimarySourceLink, -langNameAKA) %>%
  unique_rows %>%
  left_join(abvdLangData %>% 
              select(langISOCode = silcode, lat = latitude, long = longitude) %>%
              filter(langISOCode != "") %>%
              unique_rows)

wordsInfoData <- acdCognateSetsCategoriesData %>%
  select(wordCatEn = category,
         wordSubgroupEn = plang_gloss,
         wordEn = cognate_gloss,
         wordProtoAn = cognate_gp,
         wordSubgroupAn = plang_lineform,
         wordAn = item,
         wordAnLength = item_length,
         wordAnCount = count,
         langSubgroup = plang_subgroup,
         langSubgroupId = plang_subgroup_id,
         langs = lang,
         langId = plang_id) %>%
  separate_rows(langs, sep=",") %>%
  rename(langName = langs) %>%
  mutate(langName = str_trim(langName),
         wordEn = gsub("  |   ", " ", wordEn),
         wordEnLong = wordEn,
         wordEn = gsub(",.*|;.*|:.*|\\(.*", "", wordEn)) %>%
  left_join(languagesData)

wordsSelectionData <- wordsInfoData %>%
  select(wordCatEn, wordEn, wordProtoAn) %>%
  group_by_all() %>% summarize() %>% ungroup

# Parse for api use for every word
wordList <- wordsInfoData$wordEn %>% unique %>% tocamel %>% tolower

for(i in 1:length(wordList)){
  pathName <- paste0(local_api_path, "wordsinfodata/", wordList[i], ".json")
  wordsInfoData %>% filter(wordEn == wordList[i]) %>% toJSON %>%
    write_json(pathName)
}

#--- Pulotu data ---
# This data will be used for the Distribution Circles chart in the scrolly part
pulotu_path <- paste0(local_output_path, "/pulotu.json")
pulotuData <- fromJSON(pulotu_path) %>% as_tibble

#--- Colonialism data ---
colonialism_path <- paste0(local_output_path, "/colonialism.json")
colonialismData <- fromJSON(colonialism_path) %>% as_tibble

#--- Language extinction data ---
lang_extinction_path <- paste0(local_output_path, "./lang_extinction.json")
langExtinctionData <- fromJSON(lang_extinction_path) %>% as_tibble
