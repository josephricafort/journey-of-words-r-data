# Group cognate sets into categories

library(jsonlite)

acd_data_prep

N_COGNATE_SET <- 100

# Pull top cognate sets with N number of items per set
data_top_words <- acd_data_prep %>% 
  # Append english meaning of cognate set
  mutate(cognate_gp = paste0(cognate_gp, " (", cognate_gloss, ") ")) %>%
  group_by(cognate_gp, plang_subgroup, item) %>% summarize(count=sum(count)) %>% 
  arrange(cognate_gp, item) %>%
  # Filter with plang_subgroup count >= 3
  group_by(cognate_gp) %>% filter(n_distinct(plang_subgroup) >= 4) %>%
  group_by(item) %>% mutate(perc = count / sum(count)) %>%
  # Weighed significant percentage across 5 major language subgroups
  group_by(cognate_gp, item) %>% summarize(count_median = mean(count),
                                           count_IQR = IQR(count),
                                           count_score = (count_median + count_IQR)/2) %>% 
  arrange(desc(count_score)) %>% ungroup %>%
  top_n(N_COGNATE_SET, count_median) %>%
  select(cognate_gp) %>% pull %>% as.factor

# Generate word categories based from the data_top_words
cat_animals <- c("*manuk", "*manuk", "*la<U+014B>aw", "*quda<U+014B>", "*manuk", "*la<U+014B>aw",
                 "*hasa<U+014B>", "*manuk") %>% unique
cat_kinship <- c("*amax", "*amax", "*ina", "*amax", "*ina", "*ina", "*ina")
cat_bodyparts <- c("*maCa", "*kuCux", "*susu1", "*qaCay", "*sikux", "*susu1", "*maCa", "*maCa", "*Rusuk",
                   "*Rusuk", "*bulu1", "*naNaq", "*maCa", "*<U+014B>usuq", "*batux", "*sikux", "*maCa")
cat_objects <- c("*CaliS", "*Sapuy", "*Sapuy", "*CumeS", "*CuNuh", "*papan", "*Rumaq", "*zalan",
                 "*añam", "*Rumaq", "*zalan", "*kasaw1", "*kawit", "*qubi") %>% unique
cat_numbers <- c("*lima", "*duSa", "*enem", "*pitu", "*duSa", "*telu", "*lima", "*RaCus")
cat_environment <- c("*bata<U+014B>1", "*batux", "*niuR", "*bulaN", "*kaSiw", "*punti1",
                     "*taneq", "*la<U+014B>iC", "*qenay", "*likaC", "*tubah",
                     "*la<U+014B>iC", "*Ruab", "*bulaN") %>% unique
cat_plants <- c("*lumut", "*bu<U+014B>a", "*sa<U+014B>a1","*buaq", "*tebuS", "*pa<U+014B>udaN",
                "*bata<U+014B>1", "*Cubuq", "*pa<U+014B>a1") %>% unique
cat_speech <- c("*maS", "*-ku", "*aku", "*<U+014B>ajan", "*-a2", "*-ku", "*-nu1", "*ki2", "*dalem",
                "*aku", "*-ku", "*ai2", "*aku") %>% unique
cat_verb <- c("*aCay", "*ai3", "*mamaq", "*palu3", "*salaq1", "*NiSawa", "*kaen", "*NiSawa", "*ai3",
              "*ba<U+014B>uN", "*kaen", "*saRman", "*buNi", "*lakaw", "*salaq1", "*takut", "*tiktik1",
              "*tuktuk3") %>% unique
cat_adjective <- c("*qasiN", "*qasiN", "*aCas") %>% unique

category <- tibble(category="animals", cognate_gp=cat_animals) %>%
  bind_rows(tibble(category="kinship", cognate_gp=cat_kinship)) %>%
  bind_rows(tibble(category="bodyparts", cognate_gp=cat_bodyparts)) %>%
  bind_rows(tibble(category="objects", cognate_gp=cat_objects)) %>%
  bind_rows(tibble(category="numbers", cognate_gp=cat_numbers)) %>%
  bind_rows(tibble(category="environment", cognate_gp=cat_environment)) %>%
  bind_rows(tibble(category="plants", cognate_gp=cat_plants)) %>%
  bind_rows(tibble(category="speech", cognate_gp=cat_speech)) %>%
  bind_rows(tibble(category="verb", cognate_gp=cat_verb)) %>%
  bind_rows(tibble(category="adjective", cognate_gp=cat_adjective))
  
acd_data_grpd <- acd_data_prep %>% left_join(category) %>% filter(!is.na(category))

write_json(acd_data_grpd, "./data/output/json/acd/acd_cognate_sets_categories.json")
