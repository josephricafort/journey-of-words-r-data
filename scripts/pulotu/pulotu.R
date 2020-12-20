install.packages("scatterpie")

library(tidyverse)
library(rvest)
library(ggplot2)
library(maps)
library(scatterpie)

# url_pulotu <- "https://pulotu.shh.mpg.de"
# 
# # Generate df for all possible cultures/language available
# pulotu_index <- paste0(url_pulotu, "/culture") %>%
#   read_html() %>% html_node("#cults") %>% html_nodes("tr.culture-link")
# 
# data_pulotu_index <- tibble(
#   language = pulotu_index %>% html_text,
#   url = pulotu_index %>% html_nodes("a") %>% html_attr("href")
# ) %>% mutate(url = paste0(url_pulotu, url))
# 
# # Access each pages and scrape all available information about that culture/language
# 
# data_pulotu_cult <- data_pulotu_index$url[2] %>% read_html
# 
# data_pulotu_cult %>% html_nodes(".notes")
# data_pulotu_cult %>% html_nodes(".table-heading") %>% html_text
# data_pulotu_cult %>% html_nodes(".table-heading") %>% html_text
# 
# data_pulotu_cult %>% html_nodes("#Belief-Indigenous--table") %>% html_nodes("table") %>% html_nodes(".quest") %>%
#   html_text %>% str_replace_all("\\\t", "") %>% str_replace_all("\\\n", "")

factor_interest <- c(
  "Culture",
  "Culture_Notes",
  "isocode",
  "id_lang",
  "v1.Traditional_Time_Focus",
  "v2.Number_of_islands_inhabited_by_culture",
  "v4.Distance_to_African_or_Asian_mainland_(km)_",
  "v9.Maximum_elevation_(meters)",
  "v10.Population",
  "v19.Pre-Austronesian_population",
  "v24.Agriculture_/_Horticulture",
  "v37.Nature_Spirits",
  "v38.Nature_god(s)",
  "v39.Ancestral_spirits",
  "v42.God(s)",
  "v51.Social_hierarchy_tapu",
  "v52.Kinship_tapu",
  "v56.Mana_related_to_social_influence_or_technical_skill_",
  "v57.Mana_as_a_spiritual_or_religious_concept_",                                        
  "v58.Mana_as_a_personal_quality",                                                       
  "v59.Mana_and_social_status",                                                           
  "v60.Mana_linked_to_genealogy",
  "v63.Headhunting",
  "v74.Language_shift",
  "v80.Vehicles_and_roads",
  "v105.Importance_of_Patrilateral_descent",
  "v106.Importance_of_Matrilateral_descent"
)

# Dataset download from https://pulotu.shh.mpg.de/dataset

data_pulotu_raw <- read_tsv("./data/input/pulotu/Pulotu_Database_4_7_2015.txt") %>%
  rename(id_lang = ABVD_Code)
colnames <- colnames(data_pulotu_raw)
data_pulotu_colnames <- colnames[!grepl("Source", colnames)]

data_pulotu <- data_pulotu_raw %>% select(data_pulotu_colnames) %>%
  rename(culture = Culture, culture_notes = Culture_Notes) %>%
  mutate(`v6.Longitude` = ifelse(`v6.Longitude` < 0, `v6.Longitude` + 360, `v6.Longitude`)) %>% # Plot below 0 deg long fix
  mutate(id_lang = gsub("\\;.*", "", id_lang)) %>%
  mutate(asia_dist_group = as.factor(1*ceiling(`v4.Distance_to_African_or_Asian_mainland_(km)_`/1000))) %>%
  gather(variable, value, culture_notes:"v106.Importance_of_Matrilateral_descent") %>%
  mutate(var_def = variable) %>%
  mutate(variable = recode(variable,
                           "v1.Traditional_Time_Focus"                                                            = "time_focus",                                       
                           "v2.Number_of_islands_inhabited_by_culture"                                            = "island_count",                      
                           "v3.Distance_to_closest_landmass_inhabited_by_a_different_culture_(km)"                = "dist_diff_culture",               
                           "v4.Distance_to_African_or_Asian_mainland_(km)_"                                       = "asia_dist",                                   
                           "v5.Latitude"                                                                          = "lat",                                                                         
                           "v6.Longitude"                                                                         = "long",                                                                 
                           "v7.Island_type_(island_with_largest_culture_population_or_largest_island_if_unknown)" = "island_type",
                           "v8.Island_Size_(km²)"                                                                 = "island_size",                                                                
                           "v9.Maximum_elevation_(meters)"                                                        = "max_elev",                                                       
                           "v10.Population"                                                                       = "pop",                                                                    
                           "v11.Population_of_largest_political_community"                                        = "pop_largest",                                   
                           "v14.Conflict_within_communities"                                                      = "conflict_within",                                                   
                           "v15.Conflict_between_communities_of_the_culture"                                      = "conflict_between",                                  
                           "v16.Conflict_with_other_cultures"                                                     = "conflict_other",                                                
                           "v17.Contact_with_other_cultures"                                                      = "contact_other",                                                 
                           "v19.Pre-Austronesian_population"                                                      = "preaustro_pop",                                                  
                           "v20.Hindu_/_Buddhist_influence_on_supernatural_belief"                                = "hindu_buddhist",                        
                           "v21.Islamic_influence_on_supernatural_belief"                                         = "islamic",                                    
                           "v22.Christian_influence_on_supernatural_belief"                                       = "christian",                                   
                           "v24.Agriculture_/_Horticulture"                                                       = "agri",                                                 
                           "v25.Land-based_gathering"                                                             = "land_gather",                                                 
                           "v26.Animal_husbandry_as_a_source_of_food"                                             = "animal_husb",                                        
                           "v27.Land-based_hunting_performed_by_individuals"                                      = "land_hunt_ind",                                  
                           "v28.Land-based_hunting_performed_by_one_or_more_groups"                               = "land_hunt_group",                             
                           "v29.Water-based_gathering"                                                            = "water_gather",                                                     
                           "v30.Polygamy"                                                                         = "polygamy",                                                                       
                           "v31.Fishing_and_water-based_hunting_performed_by_one_or_more_groups"                  = "water_gather_group",               
                           "v32.Trade_/_wage_labour_as_a_source_of_food"                                          = "trade_wage_food",                                  
                           "v35.Forces_of_nature_are_controlled_by_or_imbued_with_the_supernatural"               = "force_nature",           
                           "v37.Nature_Spirits"                                                                   = "nature_spirits",                                                                 
                           "v38.Nature_god(s)"                                                                    = "nature_gods",                                                           
                           "v39.Ancestral_spirits"                                                                = "ancestral_spirits",                                                           
                           "v40.Deified_ancestor(s)"                                                              = "deified_ancestor",                                                          
                           "v41.Culture_hero(es)"                                                                 = "culture_hero",                                                           
                           "v42.God(s)"                                                                           = "gods",                                                                       
                           "v44.Supernatural_punishment_for_impiety"                                              = "punish_impiety",                                             
                           "v46.One's_actions_while_living_can_affect_the_nature_of_one's_afterlife"              = "ones_actions",           
                           "v47.The_actions_of_others_after_one_has_died_can_affect_the_nature_of_one's_afterlife"= "others_actions",
                           "v48.Myth_of_man's_creation"                                                           = "myth_man",                                                     
                           "v49.Primordial_pair"                                                                  = "prim_pair",                                                           
                           "v51.Social_hierarchy_tapu"                                                            = "soc_heir",                                                     
                           "v52.Kinship_tapu"                                                                     = "kinship",                                                      
                           "v53.Resource_management_tapu"                                                         = "resrc_mgmt",                                                    
                           "v56.Mana_related_to_social_influence_or_technical_skill_"                             = "mana_soc_infl",                        
                           "v57.Mana_as_a_spiritual_or_religious_concept_"                                        = "mana_spirit",                                   
                           "v58.Mana_as_a_personal_quality"                                                       = "mana_personal",                                                  
                           "v59.Mana_and_social_status"                                                           = "mana_soc_stat",                                               
                           "v60.Mana_linked_to_genealogy"                                                         = "mana_genealogy",                                                        
                           "v61.Political_and_religious_differentiation"                                          = "pol_rel_diff",                                         
                           "v63.Headhunting"                                                                      = "headhunting",                                                           
                           "v64.Costly_sacrifices_and_offerings"                                                  = "sacrifices",                                                
                           "v65.Size_of_largest_ritual_social_group"                                              = "ritual_gp_largest",                                           
                           "v66.Tattooing"                                                                        = "tattoing",                                                                   
                           "v67.Scarification"                                                                    = "scarifn",                                                                  
                           "v68.Piercing"                                                                         = "piercing",                                                                    
                           "v69.Genital_cutting"                                                                  = "genital_cutting",                                                                
                           "v70.Tooth_pulling"                                                                    = "tooth_pulling",                                                               
                           "v71.Loss_of_political_autonoomy"                                                      = "pol_auton_loss",                                             
                           "v72.Nature_of_loss_of_autonomy"                                                       = "auton_loss_nature",                                                   
                           "v73.Immigration"                                                                      = "immigration",                                                                    
                           "v74.Language_shift"                                                                   = "lang_shift",                                                           
                           "v75.Foreign_education_systems"                                                        = "foreign_educ",                                                
                           "v76.Foreign_government_systems"                                                       = "foreign_gov",                                          
                           "v77.Changes_in_means_of_subsistence"                                                  = "subsistence_change",                                          
                           "v79.Exportation_of_goods_to_other_cultures"                                           = "export_goods_others",                                       
                           "v80.Vehicles_and_roads"                                                               = "vehicles_roads",                                                           
                           "v81.Sea_port"                                                                         = "seaport",                                                               
                           "v82.Air_travel"                                                                       = "air_travel",                                                                
                           "v83.Adoption_of_a_world_religion_"                                                    = "adopt_world_rel",                                      
                           "v84.Resident_missionary_involvement_in_conversion_process_"                           = "missnary_conversn",                   
                           "v85.Use_of_force_in_conversion"                                                       = "force_conversn",                                      
                           "v86.Role_of_social_status_in_conversion_process"                                      = "soc_stat_conversn",                             
                           "v89.Syncretic_religious_movements"                                                    = "rel_mvmt",                                              
                           "v90.Contemporary_Time_Focus"                                                          = "cont_time_focus",                                                       
                           "v91.World_Religions_"                                                                 = "world_rel",                                                          
                           "v92.Dominant_world_religion"                                                          = "dom_world_rel",                                                   
                           "v93.Institutional_religious_syncretism"                                               = "inst_rel_sync",                                              
                           "v94.Unofficial_religious_syncretism"                                                  = "unoff_rel_sync",                                             
                           "v105.Importance_of_Patrilateral_descent"                                              = "patrilateral",                                           
                           "v106.Importance_of_Matrilateral_descent"                                              = "matrilateral"
                           )
         ) %>% 
  mutate(var_id = sub("\\..*", "", var_def), var_def = sub(".*?\\.", "", var_def)) %>%
  arrange(culture)

all_vars <- unique(data_pulotu$variable)
dates_vars <- all_vars[c(4, 77)]
non_vars <- c(all_vars[c(1:14, 19)], dates_vars) %>% unique
vars <- all_vars[!(all_vars %in% non_vars)]

dataplot_pulotu_vars <- data_pulotu %>%
  filter(variable %in% vars) %>% mutate(value = ifelse(is.na(value), "?", value)) %>%
  group_by(variable) %>%
  mutate(val_max = max(as.numeric(value), na.rm=T))

dataplot_pulotu_nonvars <- data_pulotu %>%
  filter(variable %in% non_vars) %>% mutate(value = ifelse(is.na(value), "?", value)) %>%
  group_by(variable) %>%
  mutate(val_max = max(as.numeric(value), na.rm=T))

# Group charts together according to maximum value
for(g in 1:5){
  ggplot(dataplot_pulotu_vars %>% filter(val_max == g)) +
    geom_bar(aes(asia_dist_group, fill=value), stat="count", position="fill") +
    facet_wrap(~variable) +
    scale_fill_brewer(palette = "Blues") +
    theme_minimal() + labs(x="Distance from Asia/Africa (in thousands km)", y="Percent") +
    theme(axis.line = element_line(colour = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
  filename <- paste0("plot_pulotu_vars_maxval", g, ".jpg")
  
  ggsave(
    filename,
    plot = last_plot(),
    device = "jpg",
    path = "image_plots/pulotu/",
    scale = 1,
    # width = NA,
    # height = NA,
    units = c("in", "cm", "mm"),
    dpi = 300,
    limitsize = TRUE,
  )
}

# Groups into categories
basic_vars <- c("island_count", "dist_diff_culture", "island_type", "island_size", "max_elev",
                "pop", "pop_largest", "preaustro_pop")

# Basic vars charts
ggplot(dataplot_pulotu_nonvars %>% select(culture, variable, value) %>%
         spread(variable, value)) +
  geom_bar(aes(asia_dist_group, fill=value), stat="count", position="fill") +
  facet_wrap(~variable) +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() + labs(x="Distance from Asia/Africa (in thousands km)", y="Percent") +
  theme(axis.line = element_line(colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())


## 1. Indigenous time focus
conflict_vars <- c("conflict_within", "conflict_between", "conflict_other", 
                  "contact_other", "rel_mvmt", "world_rel")
resource_vars <- c("land_gather", "animal_husb", "land_hunt_ind", "land_hunt_group", "water_gather",
                  "water_gather_group", "trade_wage_food")
mysticism_vars <- c("force_nature", "nature_spirits", "nature_gods", "ancestral_spirits", "deified_ancestor",
                   "culture_hero", "gods", "punish_impiety", "ones_actions", "others_actions",
                   "myth_man", "prim_pair")
social_vars <- c("soc_heir", "kinship", "resrc_mgmt", "mana_soc_infl", "mana_spirit", "mana_personal",
                "mana_soc_stat", "mana_genealogy", "pol_rel_diff")
ritual_vars <- c("headhunting", "sacrifices", "ritual_gp_largest", "tattoing", "scarifn",
                "piercing", "genital_cutting", "tooth_pulling")
parent_vars <- c("matrilateral", "patrilateral")

## 2. Post-contact history
autonomy_vars <- c("pol_auton_loss", "auton_loss_nature")
foreign_vars <- c("immigration", "foreign_gov", "foreign_educ", "missnary_conversn", "force_conversn", "lang_shift",
                  "export_goods_others", "vehicles_roads", "seaport", "air_travel")
religion_post_vars <- c("islamic", "christian", "polygamy", "missnary_conversn", "adopt_world_rel", "force_conversn", "soc_stat_conversn", "rel_mvmt")

## 3. Current-time focus
religion_curr_vars <- c("dom_world_rel", "inst_rel_sync", "unoff_rel_sync", "inst_rel_sync", "unoff_rel_sync")


# Selected vars charts
ggplot(dataplot_pulotu_vars %>% filter(variable %in% ritual_vars)) +
  geom_bar(aes(asia_dist_group, fill=value), stat="count", position="fill") +
  facet_wrap(~variable) +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() + labs(x="Distance from Asia/Africa (in thousands km)", y="Percent") +
  theme(axis.line = element_line(colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

## Categorize data

# Fetch data for a single culture
fetch_culture <- function(cats_raw){

  result <- tibble() # initialize empty df
  th_length <- cats_raw %>% html_nodes(".table-heading") %>% length
  
  culture_name <- cats_raw %>% html_nodes("div.page-header h1") %>% html_text
  
  for(e in 1:th_length){
    header <- cats_raw %>% html_nodes(".table-heading") %>% extract(e)
    era <- (header %>% html_text %>% str_split("\\(") %>% unlist)[1]
    time <- header %>% html_node("em") %>% html_text %>% str_replace_all("\\(|\\)", "")
    
    dt_length <- cats_raw %>% html_nodes(".dataTable") %>% length
    
    for(sg in 1:dt_length){
      item <- cats_raw %>% html_nodes(".dataTable") %>% extract2(sg) %>% html_table
      
      quest <- response <- source <- NA
      if(length(item) == 1){
        subgroup <- item[1, 1]
      } else if(length(item) > 1){
        quest <- item[1, 1]
        response <- item[1, 2]
        source <- item[2, 1]
      }
      
      result <- result %>% bind_rows(
        tibble(culture_name, era, time, subgroup, quest, response, source)
      )
    }
  }
  
  return (result)
}

# Fetch data for all cultures
fetch_allcultures <- function(){
  
  PULOTU_URL <- "https://pulotu.shh.mpg.de"
  ALL_CULTURE_URL <- "https://pulotu.shh.mpg.de/culture"
  culture_list_url <- read_html(ALL_CULTURE_URL) %>% html_node("#cults") %>% html_nodes(".culture-link a") %>%
    html_attr("href")
  
  result_all <- tibble()
  for(c in 1:length(culture_list_url)){
    cats_raw <- read_html(paste0(PULOTU_URL, culture_list_url[c])) %>% html_node("#content")
    
    paste0("Fetching url: ", culture_list_url[c]) %>% print
    result_all <- result_all %>% bind_rows(
      fetch_culture(cats_raw)
    )
  }
  
  return (result_all)
}

data_pulotu_culturesinfo <- fetch_allcultures()

write_json(data_pulotu_culturesinfo, "./data/output/json/pulotu.json")

# Gather only for 1 category
cats_url <- "https://pulotu.shh.mpg.de/culture/" 
cats_raw <- read_html(cats_url) %>% html_node("#content")
fetch_culture(cats_raw)


".table-title"
".data-table"
".dataTable"
"td.quest"
"td.response"

".table-heading"
".table-title"

  
#---- OLD CODE -----

# data_pulotu <- data_pulotu_raw %>% 
#   select(Culture, isocode, id_lang,
#          "v5.Latitude", "v6.Longitude",
#          "v4.Distance_to_African_or_Asian_mainland_(km)_",
#          "v1.Traditional_Time_Focus",
#          "v2.Number_of_islands_inhabited_by_culture",
#          "v4.Distance_to_African_or_Asian_mainland_(km)_",
#          "v8.Island_Size_(km²)",
#          "v9.Maximum_elevation_(meters)",
#          "v10.Population",
#          "v11.Population_of_largest_political_community",                                        
#          "v14.Conflict_within_communities",                                                      
#          "v15.Conflict_between_communities_of_the_culture",                                      
#          "v16.Conflict_with_other_cultures",                                                     
#          "v17.Contact_with_other_cultures",  
#          "v24.Agriculture_/_Horticulture",
#          "v25.Land-based_gathering",                                                             
#          "v26.Animal_husbandry_as_a_source_of_food",                                             
#          "v27.Land-based_hunting_performed_by_individuals",                                      
#          "v28.Land-based_hunting_performed_by_one_or_more_groups",                               
#          "v29.Water-based_gathering",                                                            
#          "v30.Polygamy",                                                                         
#          "v31.Fishing_and_water-based_hunting_performed_by_one_or_more_groups",
#          "v35.Forces_of_nature_are_controlled_by_or_imbued_with_the_supernatural",
#          "v37.Nature_Spirits",
#          "v38.Nature_god(s)",
#          "v39.Ancestral_spirits",
#          "v42.God(s)",
#          "v51.Social_hierarchy_tapu",
#          "v105.Importance_of_Patrilateral_descent", 
#          "v106.Importance_of_Matrilateral_descent") %>%
#   rename(culture = Culture, 
#          time_focus = "v1.Traditional_Time_Focus",
#          island_count = "v2.Number_of_islands_inhabited_by_culture",
#          island_size = "v8.Island_Size_(km²)",
#          max_elev = "v9.Maximum_elevation_(meters)",
#          population = "v10.Population",
#          pop_largest_polcomm = "v11.Population_of_largest_political_community",                                        
#          conflict_within = "v14.Conflict_within_communities",                                                      
#          conflict_between = "v15.Conflict_between_communities_of_the_culture",                                      
#          conflict_others = "v16.Conflict_with_other_cultures",                                                     
#          contact_others = "v17.Contact_with_other_cultures",
#          agri = "v24.Agriculture_/_Horticulture",
#          land_gather = "v25.Land-based_gathering",                                                             
#          animal_husb = "v26.Animal_husbandry_as_a_source_of_food",                                             
#          land_hunt_ind = "v27.Land-based_hunting_performed_by_individuals",                                      
#          land_hunt_more = "v28.Land-based_hunting_performed_by_one_or_more_groups",                               
#          water_gather = "v29.Water-based_gathering",                                                            
#          polygamy = "v30.Polygamy",                                                                         
#          water_gather_more = "v31.Fishing_and_water-based_hunting_performed_by_one_or_more_groups",
#          force_nature = "v35.Forces_of_nature_are_controlled_by_or_imbued_with_the_supernatural",
#          nature_spirits = "v37.Nature_Spirits",
#          nature_god = "v38.Nature_god(s)",
#          anc_spirits = "v39.Ancestral_spirits",
#          gods = "v42.God(s)",
#          soc_hier = "v51.Social_hierarchy_tapu",
#          patrilateral = "v105.Importance_of_Patrilateral_descent", 
#          matrilateral = "v106.Importance_of_Matrilateral_descent",
#          asia_dist = "v4.Distance_to_African_or_Asian_mainland_(km)_",
#          lat = "v5.Latitude",
#          long = "v6.Longitude") %>%
#   mutate(long = ifelse(long < 0, long + 360, long)) # Plot below 0 deg long to the other side

dataplot_pulotu_parent <- data_pulotu %>%
  select(culture:asia_dist, patrilateral:matrilateral) %>%
  mutate(id_lang = gsub("\\;.*", "", id_lang)) %>%
  mutate_at(vars(patrilateral, matrilateral), as.numeric) %>%
  filter(!is.na(patrilateral) & !is.na(matrilateral)) %>%
  # gather(descent, descent_val, c("patrilateral", "matrilateral")) %>% mutate(descent = as.factor(descent)) %>%
  mutate(asia_dist_group = as.factor(1*ceiling(asia_dist/1000)))

dataplot_pulotu_environment <- data_pulotu %>%
  mutate(asia_dist_group = as.factor(1*ceiling(asia_dist/1000)))

# Quick plot
ggplot(dataplot_pulotu, aes(descent, count,  fill = descent)) +
  # geom_jitter(width = 0, height = 0.2) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~asia_dist_group)

# FAMILY and SOCIETY

## Matrilocal vs patrilocal
world <- map_data("world2") # Quick geographical plot
ggplot() +
  geom_polygon(data = world, aes(long, lat, group=group), fill="grey", alpha=0.3) +
  # geom_point(data = dataplot_pulotu_parent, aes(long, lat, color=descent_val)) +
  geom_scatterpie(data = dataplot_pulotu_parent, aes(long, lat, r=1.5), cols = c("patrilateral", "matrilateral"), color = NA, alpha = 0.7) +
  xlim(25, 270) + ylim(-50, 40) +
  theme_void()

ggplot(dataplot_pulotu_parent %>% 
         gather(parent_type, parent_val, c("matrilateral", "patrilateral")) %>%
         group_by(asia_dist_group, parent_type) %>% summarize(parent_val=mean(parent_val, na.rm=T))
         ) +
    geom_bar(aes(asia_dist_group, parent_val, fill=parent_type %>% as.factor), stat="identity", position="fill")
  # geom_text(aes(asia_dist_group, parent_val, label=round(parent_val, 2)), size=3, position=position_fill(vjust=0.5))
  # facet_wrap(~parent_type, ncol=1)
  # scale_fill_brewer(palette = "YlGn")

ggplot(dataplot_pulotu_environment %>% 
         gather(parent_type, parent_val, c("matrilateral", "patrilateral"))) +
  # geom_histogram(aes(asia_dist_group, fill=water_gather), stat="count", binwidth=10) +
  geom_bar(aes(asia_dist_group, fill=parent_val), stat="count", position="fill") +
  facet_wrap(~parent_type, ncol=1) +
  scale_fill_brewer(palette = "RdPu")

# Polygamy
ggplot(dataplot_pulotu_environment %>% filter(!is.na(polygamy))) +
  geom_bar(aes(asia_dist_group, fill=polygamy), stat="count", position="fill") +
  scale_fill_brewer(palette = "RdPu")

# Social heirarchy tapu
ggplot(dataplot_pulotu_environment %>% filter(!is.na(polygamy))) +
  geom_bar(aes(asia_dist_group, fill=soc_hier), stat="count", position="fill") +
  scale_fill_brewer(palette = "RdPu")

# pop_largest_polcomm = "v11.Population_of_largest_political_community"                                     
# conflict_within = "v14.Conflict_within_communities"                                                    
# conflict_between = "v15.Conflict_between_communities_of_the_culture"                                 
# conflict_others = "v16.Conflict_with_other_cultures"                                                    
# contact_others = "v17.Contact_with_other_cultures"

ggplot(dataplot_pulotu_environment) +
  geom_bar(aes(asia_dist_group, fill=conflict_within), stat="count", position="fill") +
  scale_fill_brewer(palette = "PuRd")

ggplot(dataplot_pulotu_environment) +
  geom_bar(aes(asia_dist_group, fill=conflict_between), stat="count", position="fill") +
  scale_fill_brewer(palette = "PuRd")

ggplot(dataplot_pulotu_environment) +
  geom_bar(aes(asia_dist_group, fill=conflict_others), stat="count", position="fill") +
  scale_fill_brewer(palette = "PuRd")

ggplot(dataplot_pulotu_environment) +
  geom_bar(aes(asia_dist_group, fill=contact_others), stat="count", position="fill") +
  scale_fill_brewer(palette = "PuRd")


# ENVIRONMENT

## Island Size and Count
ggplot(dataplot_pulotu_environment) +
  geom_bar(aes(asia_dist_group, weight=island_size), fill="orange")

ggplot(dataplot_pulotu_environment) +
  geom_bar(aes(asia_dist_group, weight=island_count), fill="orange")

ggplot(dataplot_pulotu_environment) +
  geom_bar(aes(asia_dist_group, weight=population %>% as.numeric), fill="orange")

## Agriculture
ggplot() +
  geom_polygon(data = world, aes(long, lat, group=group), fill="grey", alpha=0.3) +
  geom_point(data = dataplot_pulotu_environment, aes(long, lat, size=population), alpha=0.5) +
  xlim(25, 270) + ylim(-50, 40) +
  theme_void()

ggplot(dataplot_pulotu_environment) +
  geom_bar(aes(asia_dist_group, fill=agri), stat="count", position="fill") +
  scale_fill_brewer(palette = "YlGn")

## Animal husbandry
ggplot(dataplot_pulotu_environment) +
  geom_bar(aes(asia_dist_group, fill=animal_husb), stat="count", position="fill") +
  scale_fill_brewer(palette = "YlGn")

## Land hunt (individual vs group)
ggplot(dataplot_pulotu_environment %>% 
         gather(land_hunt_type, land_hunt_val, c("land_hunt_ind", "land_hunt_more"))) +
  # geom_histogram(aes(asia_dist_group, fill=water_gather), stat="count", binwidth=10) +
  geom_bar(aes(asia_dist_group, fill=land_hunt_val), stat="count", position="fill") +
  facet_wrap(~land_hunt_type, ncol=1) +
  scale_fill_brewer(palette = "YlGn")

## Water gather (individual vs group)
ggplot(dataplot_pulotu_environment %>% 
         gather(water_gather_type, water_gather_val, c("water_gather", "water_gather_more"))) +
  # geom_histogram(aes(asia_dist_group, fill=water_gather), stat="count", binwidth=10) +
  geom_bar(aes(asia_dist_group, fill=water_gather_val), stat="count", position="fill") +
  facet_wrap(~water_gather_type, ncol=1) +
  scale_fill_brewer(palette = "PuBu")
