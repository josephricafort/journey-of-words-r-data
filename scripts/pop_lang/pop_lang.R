library(tidyverse)
library(jsonlite)
library(data.table)

PATH_POPLANG <- "data/input/pop_lang/UNdata_Export_20200915_162456126.csv"

data_poplang_raw <- read.csv(PATH_POPLANG, stringsAsFactors = F)

footnotes <- c(seq(1, 81, by=1), "footnoteSeqID")

data_poplang_format <- data_poplang_raw %>% rename(
  ctry_area = `Country.or.Area`,
  year = Year, area = Area, sex = Sex, lang = Language,
  record_type = `Record.Type`, rel = Reliability,
  src_yr = `Source.Year`, value = `Value`,
  val_footnotes = `Value.Footnotes`
) %>% as_tibble %>%
  filter(!(ctry_area %in% footnotes)) %>%
  mutate(across(c(ctry_area, area:rel), as.factor)) %>%
  mutate(across(c(year, src_yr), as.integer))

area_austronesia <- c("Indonesia",
                      "Philippines",
                      "Timor-Leste",
                      "Palau",
                      "Northern Mariana Islands", 
                      "Guam",
                      "American Samoa", 
                      "Cook Islands", 
                      "French Polynesia",
                      "Micronesia (Federated States of)",
                      "Tonga",
                      "Tokelau",
                      "New Zealand"
                      )
# Cambodia, Singapore has some Austronesian people but not included in the data
# Austronesian countries with 25%+ population were considered 

# Categorize whether language is Austronesian and non-Austronesian
PATH_WORLDATLASLANG <- "data/input/pop_lang/language.csv"

data_wal_raw <- read.csv(PATH_WORLDATLASLANG, stringsAsFactors = F) %>% select(wals_code:countrycodes)

data_wal_format <- data_wal_raw %>%
  mutate(across(c(wals_code, iso_code, glottocode, genus, family, macroarea, countrycodes), as.factor)) %>% as_tibble

data_wal_clean <- data_wal_format

# Categorize/Correct some of the languages' families
lang_isna_an <- c("Mambai") # Correction for Mambai for Timor Leste which should be Austronesian (categorized as Niger-Congo)
lang_isna_afroasia <- c("Arabic", "Assyrian", "Hebrew")
lang_isna_indoeu <- c("Punjabi", "Sinhalese", "Indo-Aryan", "Greek", "West Germanic",
                      "Other Balto-Slavic")
lang_isna_creole <- c("Zamboangeno-Chavacano", "Davao-Chavacano", "Caviteno-Chavacano",
                      "Cotabateno-Chavacano", "Ternateno-Chavacano", "Pidgins and Creoles")
lang_isna_sintib <- c("Yue", "Northern Chinese", "Chinese", "Sinitic nfd", "Sinitic", "Min")
lang_isna_papuan <- c("Makasai", "Bunak", "Makalero")
lang_isna_nigercongo <- c("Bantu", "Khassonke", "Runyoro")
lang_isna_unknown <- c("Kontigna", "Gowa", "English and Tongan")
lang_isna_other <- c("Other", "Other Ethiopian", "New Zealand Sign Language", "Other Foreign Languages",
                     "Other Pacific Islands", "Asian", "Other local language/dialect",
                     "Other Sign Language")
lang_isna_na <- c("None", "Not stated")

categorize_family <- function(lang){
    sapply(lang, function(l){
      if(l %in% lang_isna_an){ return ("Austronesian")}
      else if (l %in% lang_isna_afroasia){ return ("Afro-Asiatic")}
      else if (l %in% lang_isna_indoeu){ return ("Indo-European")}
      else if (l %in% lang_isna_creole){ return ("Pidgins and creoles")}
      else if (l %in% lang_isna_sintib){ return ("Sino-Tibetan")}
      else if (l %in% lang_isna_papuan){ return ("Papuan")}
      else if (l %in% lang_isna_nigercongo){ return ("Niger-Congo")}
      else if (l %in% lang_isna_unknown){ return ("Unknown")}
      else if (l %in% lang_isna_other){ return ("Other")}
      else if (l %in% lang_isna_na){ return ("Not stated")}
      else { return ("Austronesian")}
    })
}

lang_macrogp <- c("Austronesian", "Indo-European", "Pidgins and creoles", "Sino-Tibetan", "Papuan")

data_poplang_clean <- data_poplang_format %>% 
  # only look at Austronesian countries
  filter(ctry_area %in% area_austronesia) %>%
  mutate(ctry_area = fct_relevel(ctry_area, area_austronesia)) %>%
  filter(lang != "Total") %>%
  filter(sex == "Both Sexes") %>%
  filter(area == "Total") %>% select(-sex, -area) %>%
  arrange(desc(value)) %>%
  left_join(
    data_wal_clean %>% rename(lang = Name)
  ) %>%
  mutate(lang = as.factor(lang)) %>%
  # variable labeling if language is Austronesian or non-Austronesian
  mutate(is_an = ifelse(family == "Austronesian", TRUE, FALSE)) %>%
  droplevels %>%
  # categorize languages with no family or is na
  mutate(family = ifelse(!is.na(family), as.character(family), categorize_family(as.character(lang)))) %>%
  mutate(family = ifelse(lang %in% lang_isna_an, "Austronesian", as.character(family))) %>%
  mutate(family = ifelse(as.character(family) == "other", "Other", as.character(family))) %>%
  # categorize macro group
  mutate(macro_family = ifelse(family %in% lang_macrogp, as.character(family), "Others"))
data_poplang_clean %>% str

data_poplang_clean %>% group_by(family) %>% summarize(value = sum(value, na.rm=T)) %>% arrange(desc(value))

# # List lang with no lang families
# lang_family_isna <- data_poplang_clean %>% filter(is.na(is_an)) %>% select(lang, value) %>%
#   arrange(desc(value)) %>% 
#   # top 75% of the lang which has 100000 value
#   filter(value > 100000) %>% filter(lang != "Other") %>%
#   # remove non-Austronesians
#   filter(!(lang %in% c("Other Ethiopian", "Zamboangeno-Chavacano"))) %>%
#   select(lang) %>% pull %>% droplevels() %>% as.character

# data_poplang_clean %>% group_by(lang, family, ctry_area) %>% summarize(value = sum(value, na.rm=T)) %>%
#   arrange(desc(value)) %>% as.data.frame

# data_poplang_clean <- data_poplang_clean %>%
#   # Categorize lang with no lang families
#   mutate(family = as.character(family)) %>%
#   mutate(family = fifelse(family %in% lang_family_isna, "Austronesian", family))

  
