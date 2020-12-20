library(tidyverse)
library(ggplot2)

data_acd_loans_clean
rotate_xaxis <- theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# BOTH FOREIGN AND NON-FOREIGN
# 1. Proportion between foreign and non-foreign loan words
data_acd_loans_clean %>%
  ggplot(aes(x=origin_foreign, fill=subgroup)) +
  geom_bar()

data_acd_loans_clean %>%
  ggplot(aes(x=subgroup, fill=origin_foreign)) +
  geom_bar()

data_acd_loans_clean %>%
  ggplot(aes(x=subgroup, fill=origin_foreign)) +
  geom_bar(position="fill")



# FOREIGN
# 1. Which loan words have made the most impact?
# 2. Which foreign loan words penetrated the most?
# 3. Which languages or subgroups have most foreign loan words?

# ---
data_acd_loans_clean_foreign <- data_acd_loans_clean %>%
  filter(origin_foreign) %>% droplevels()

# 1. Which loan words have been influenced the most?
top_loanwords_foreign <- data_acd_loans_clean_foreign %>%
  group_by(item, subgroup) %>% summarize(count = n(), keyloan = first(keyloan), 
                               origin = first(origin)) %>% arrange(desc(count)) %>%
  arrange(desc(count)) %>% ungroup %>%
  mutate(item = fct_inorder(item, ordered=NA)) %>% top_n(200, count) %>% droplevels()

top_loanwords_foreign %>%
  ggplot(aes(x=item, weight=count, fill=origin)) +
  geom_bar() + rotate_xaxis +
  facet_wrap(~origin, ncol=3) 

top_loanwords_foreign %>%
  ggplot(aes(x=log(count), fill=origin)) +
  geom_histogram() + rotate_xaxis

 # 2. Which foreign languages where words have been borrowed the most?
ggplot(data_acd_loans_clean_foreign, aes(x=reorder(origin, subgroup), fill=subgroup)) +
  geom_bar() + rotate_xaxis

# 3. Which languages or subgroups have most foreign loan words?
table(data_acd_loans_clean_foreign$item) %>% sort(decreasing = T)

data_acd_loans_clean %>%
  filter(origin_foreign) %>%
  group_by(item) %>% summarize(count = n(), origin = first(origin)) %>% arrange(desc(count))

ggplot(data_acd_loans_clean_foreign, aes(x=subgroup, fill=origin)) +
  geom_bar() + rotate_xaxis

ggplot(data_acd_loans_clean_foreign, aes(x=subgroup, fill=origin)) +
  geom_bar(position = "fill") + rotate_xaxis



# NON-FOREIGN
data_acd_loans_clean_nonforeign <- data_acd_loans_clean %>%
  filter(!origin_foreign) %>% droplevels()

# 1. Which loan words have been influenced the most?
top_loanwords_nonforeign <- data_acd_loans_clean_nonforeign %>%
  group_by(item, subgroup) %>% summarize(count = n(), keyloan = first(keyloan), 
                                         origin = first(origin)) %>% arrange(desc(count)) %>%
  arrange(desc(count)) %>% ungroup %>%
  mutate(item = fct_inorder(item, ordered=NA)) %>% top_n(100, count) %>% droplevels()
yesyestop_loanwords_nonforeign %>%
  ggplot(aes(x=item, weight=count, fill=origin)) +
  geom_bar() + rotate_xaxis +
  facet_wrap(~origin, ncol=3)

# 2. Which foreign languages where words have been borrowed the most?
ggplot(data_acd_loans_clean_nonforeign, aes(x=reorder(origin, subgroup), fill=subgroup)) +
  geom_bar() + rotate_xaxis

# 3. Which languages or subgroups have most foreign loan words?
ggplot(data_acd_loans_clean_nonforeign, aes(x=subgroup, fill=origin)) +
  geom_bar() + rotate_xaxis

ggplot(data_acd_loans_clean_nonforeign, aes(x=subgroup, fill=origin)) +
  geom_bar(position="fill") + rotate_xaxis



# LANGUAGES

# 1. Languages with the most loanwords
top_loanwords_langs <- data_acd_loans_clean %>%
  group_by(lang, origin) %>% summarize(gloss = first(gloss), count = n()) %>% arrange(desc(count)) %>%
  ungroup %>% mutate(item = fct_inorder(lang, ordered=NA)) %>% top_n(20, count) %>% droplevels()
  
top_loanwords_langs %>%
ggplot(aes(x=lang)) +
  geom_bar()
