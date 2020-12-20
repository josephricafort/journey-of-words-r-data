library(readxl)

path <- "data/input/endangered_languages/unesco_atlas_languages_limited_dataset.xlsx"

excel_sheets(path)
dir("data/input/endangered_languages")

data_endangered_lang_raw <- read_excel(path, sheet="Sheet1") %>% tibble %>%
  rename(id = ID, name_en = `Name in English`, name_fr = `Name in French`,
         name_es = `Name in Spanish`, ctry = Countries, ctry_codesalpha3 = `Country codes alpha 3`,
         iso_code = `ISO639-3 codes`, degree_danger = `Degree of endangerment`)

all_countries <- data_endangered_lang_raw$ctry %>% unique %>% sort

an_countries <- c("Cook Islands", "Indonesia", "Malaysia", "Micronesia (Federated States of)",
                  "Micronesia (Federated States of), Nauru", "Nauru", "New Caledonia (France)",
                  "New Zealand", "Niue", "Norfolk Island (Australia)", "Palau", "Papua New Guinea",
                  "Philippines", "Pitcairn (U.K.)", "Solomon Islands", "Timor-Leste", "Tokelau",
                  "Tuvalu", "Vanuatu") 

danger_fct <- c("Vulnerable", "Definitely endangered", "Severely endangered", "Critically endangered", "Extinct")

# Note: Papua New Guinea was included as some languages were AN in nature

ctry_fct <- data_endangered_lang_raw %>% filter(ctry %in% an_countries) %>%
  group_by(ctry) %>% summarize(n = n()) %>% arrange(desc(n)) %>% top_n(10, n) %>%
  select(ctry) %>% pull

data_endangered_lang_clean <- data_endangered_lang_raw %>% filter(ctry %in% an_countries) %>%
  mutate(degree_danger = fct_relevel(degree_danger, danger_fct),
         ctry = fct_relevel(ctry, ctry_fct)) %>%
  filter(ctry %in% ctry_fct)

write_json(data_endangered_lang_clean, "./data/output/json/lang_extinction.json")
