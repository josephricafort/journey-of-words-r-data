# The goal of this script is to combine and generate the data
# to be used for the Austronesian Data Story

# Major steps involved
# Fetch -> Cleanup -> Parse

# install.packages("rapportools")
library(tidyverse)
library(jsonlite)
library(rapportools)
library(stringi)

source("scripts/utils.R")

# -----------------------------------------------------
# 1) FETCH

github_path <- "https://raw.githubusercontent.com/josephricafort/journey-of-words-r-data/master/"
github_api_path <- paste0(github_path, "data/api")
local_output_path <- "./data/output/json"
local_api_path <- "./api/"

chapters <- c("world", "nature", "conversion", "extraction", "fate")

unescapeUnicode <- function(str) {
  result <- gsub(">","", gsub("<U\\+","\\\\u", str)) %>% stri_unescape_unicode
  return (result)
}

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
  return(result)
}

# -----------------------------------------------------
# 2) CLEANUP

languagesData <- acdLanguagesData %>%
  select(-langPrimarySource, -langPrimarySourceLink, -langNameAKA) %>%
  unique_rows %>%
  left_join(abvdLangData %>%
              select(langISOCode = silcode, lat = latitude,
                long = longitude) %>%
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
  dplyr::rename(langName = langs) %>%
  mutate(langName = str_trim(langName),
         wordEn = gsub("  |   ", " ", wordEn),
         wordEnLong = wordEn,
         wordEn = gsub(",.*|;.*|:.*|\\(.*", "", wordEn),
         langSubgroup = if_else(is.na(langSubgroup), "Ungrouped", langSubgroup)) %>%
  left_join(languagesData) %>%
  mutate_all(unescapeUnicode)

wordsSelectionData <- wordsInfoData  %>%
  select(wordCatEn, wordEn, wordProtoAn) %>%
  group_by_all() %>% summarize() %>% ungroup

locationsList <- wordsInfoData$langLocation %>% unique %>% sort

languagesData %>% filter(langName %in% (acdLoansData$lang %>% unique)) %>% select(langName) %>% unique

langNamesLoanUnique <- acdLoansData$lang %>% unique %>% sort %>% as.character
langNamesDataUnique <- languagesData %>% filter(!is.na(lat)) %>%
  select(langName) %>% unique %>% pull %>% as.character

# Fuzzy match lang names and double check the values
langNamesFuzzy <- tibble(langNameLoan = character(), langNameData = character(), n = integer())
for (i in 1:length(langNamesLoanUnique)){
  result <- agrep(langNamesLoanUnique[i], langNamesDataUnique,
    ignore.case = TRUE, value = TRUE,
    max.distance = 0.05, useBytes = TRUE)
  # print(result)
  for (j in 1:length(result)){
    langNamesFuzzy <- bind_rows(langNamesFuzzy, 
      tibble(langNameLoan = langNamesLoanUnique[i], langNameData = result[j], n = j ))
    # langNamesFuzzy$matches[i] <- result
  }
}

# Loan data
wordsLoanData <- acdLoansData %>%
  # mutate(lang = recode(lang,
  #   "'are'are" = "'Are'are",
  #   "Ida'an begak" = "Ida'an Begak")) %>%
  dplyr::rename(langName = lang, 
    wordEn = keyloan,
    wordAn = item,
    langSubgroup = subgroup,
    wordEnLong = gloss,
    originForeign = origin_foreign,
    # `subgr oup`
    ) %>%
  mutate(langSubgroup = if_else(is.na(langSubgroup), "Ungrouped", langSubgroup)) %>%
  left_join(languagesData)

# -----------------------------------------------------
# 3) PARSE

wordListUnshortened <- wordsInfoData$wordEn %>% unique
wordList <- wordsInfoData$wordEn %>% unique %>% tocamel %>% tolower

wordListLoanUnshortened <- wordsLoanData$wordEn %>% unique
wordListLoan <- wordsLoanData$wordEn %>% unique %>% tocamel %>% tolower

# -wordsInfoData
for (i in 1:length(wordList)){
  pathName <- paste0(local_api_path, "wordsinfodata/", wordList[i], ".json")
  wordsInfoData %>% filter(wordEn == wordListUnshortened[i]) %>% toJSON %>%
    write_json(pathName)
}

# -locationsData
for(i in 1:length(wordList)){
  pathName <- paste0(local_api_path, "locationsdata/", wordList[i], ".json")
  locationsData <- wordsInfoData %>%
    filter(wordEn == wordListUnshortened[i]) %>%
    mutate_at(vars(lat, long), as.double) %>%
    group_by(langLocation) %>%
    summarize(latMean = mean(lat, na.rm=T),
              longMean = mean(long, na.rm=T),
              latMin = min(lat, na.rm=T),
              longMin = min(lat, na.rm=T),
              latMax = max(lat, na.rm=T),
              longMax = max(long, na.rm=T),
              distRangeMin = distFromHomeland(latMin, longMin),
              distRangeMax = distFromHomeland(latMax, longMax))
  locationsData %>% toJSON %>% write_json(pathName)
}

# -dataPerWordTally
for (i in 1:length(wordList)){
  pathName <- paste0(local_api_path, "dataperwordtally/", wordList[i], ".json")
  dataPerWordTally <- wordsInfoData %>%
    filter(wordEn == wordListUnshortened[i]) %>%
    mutate_at(vars(lat, long), as.double) %>%
    group_by(wordAn) %>%
    summarize(langSubgroupsList = paste0(langSubgroup %>% unique, collapse=", "),
              langNamesList = paste0(langName %>% unique, collapse=", "),
              langNamesCount = n_distinct(langName),
              latSubgroupMean = mean(lat, na.rm=T),
              longSubgroupMean = mean(long, na.rm=T))
  dataPerWordTally %>% toJSON %>% write_json(pathName)
}

# -wordsLoanData
for (i in 1:length(wordListLoan)){
  pathName <- paste0(local_api_path, "wordsloandata/", wordListLoan[i], ".json")
  wordsLoanData %>% filter(wordEn == wordListLoanUnshortened[i]) %>% toJSON %>%
    write_json(pathName)
}

# --------------------------------------------------------

 #--- Pulotu data ---
# This data will be used for the Distribution Circles chart in the scrolly part
pulotu_path <- paste0(local_output_path, "/pulotu.json")
pulotu_cultures_path <- paste0(local_output_path, "/pulotu_cultures.json")
pulotu_varslist_path <- paste0(local_output_path, "/pulotu_vars_list.json")
pulotu_nonvarslist_path <- paste0(local_output_path, "/pulotu_nonvars_list.json")
firstToLower <- function(str){
  substr(str, 1, 1) <- tolower(substr(str, 1, 1))
  return (str)
}

pulotuVarsList <- fromJSON(pulotu_varslist_path)
pulotuNonvarsList <- fromJSON(pulotu_nonvarslist_path)
pulotuAllVarsList <- c(pulotuVarsList, pulotuNonvarsList) %>% sort() %>% tocamel
pulotuAllVarsLowerList <- pulotuAllVarsList %>% tolower

pulotuData <- fromJSON(pulotu_path) %>% as_tibble %>%
  left_join(
    fromJSON(pulotu_path) %>% as_tibble %>%
      filter(variable == "lat" | variable == "long") %>%
      select(culture, variable, value) %>%
      spread(key=variable, value=value)
  ) %>%
  rename_at(vars(asia_dist_group, var_def, var_id), tocamel) %>%
  mutate_at(vars(variable, varDef, varId), tocamel) %>%
  mutate(varDef = firstToLower(varDef), isVar = ifelse(varDef %in% pulotuVarsList, TRUE, FALSE))
  
pulotuCulturesData <- fromJSON(pulotu_cultures_path) %>% as_tibble
  
# Parse for api use for every variable
for(i in 1:length(pulotuAllVarsList)){
  pathName <- paste0(local_api_path, "pulotudata/", pulotuAllVarsLowerList[i], ".json")
  pulotuData %>% filter(variable == pulotuAllVarsList[i]) %>% toJSON %>%
    write_json(pathName)
}

pulotuDataDist <- pulotuData %>% filter(variable %in% tocamel(pulotuVarsList)) %>%
  group_by(culture, asiaDistGroup, variable) %>%
  summarize(value = mean(value %>% as.numeric, na.rm=T)) %>%
  group_by(variable, value, asiaDistGroup) %>%
  mutate(asiaDistGroup = asiaDistGroup %>% as.numeric,
    value = value %>% as.factor,
    variable = variable %>% as.factor) %>%
  ungroup %>% arrange(variable, asiaDistGroup, value, culture) %>%
  group_by(variable, asiaDistGroup, value) %>%
  summarize(count = n(culture)) %>% spread(value, count, fill = 0) %>%
  ungroup %>% arrange(variable, asiaDistGroup) %>%
  select(variable:`1`, `2`, `3`, `4`, `NaN`) %>%
  dplyr::rename(lvl0 = `0`, lvl1 = `1`, lvl2 = `2`,
    lvl3 = `3`, lvl4 = `4`, unknown = `NaN`) %>%
  mutate(asiaDistGroup = asiaDistGroup %>% as.factor)

pulotuDistVarsLowerList <- pulotuDataDist$variable %>% unique

# Parse for summarized data per distance from homeland
for(i in 1:length(pulotuDistVarsLowerList)){
  pathName <- paste0(local_api_path, "pulotudatadist/", pulotuDistVarsLowerList[i] %>% tolower, ".json")
  pulotuDataDist %>% filter(variable == pulotuDistVarsLowerList[i]) %>% 
    select(-variable) %>% toJSON %>%
    write_json(pathName)
}

#--- Colonialism data ---
colonialism_path <- paste0(local_output_path, "/colonialism.json")
colonialismData <- fromJSON(colonialism_path) %>% as_tibble

#--- Language extinction data ---
lang_extinction_path <- paste0(local_output_path, "./lang_extinction.json")
langExtinctionData <- fromJSON(lang_extinction_path) %>% as_tibble
