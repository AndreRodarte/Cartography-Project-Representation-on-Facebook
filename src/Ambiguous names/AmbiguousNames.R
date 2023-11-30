library(tidyverse)
library(tidytext)
library(dplyr)
library(janitor)
library(ggplot2)
library(highcharter)
library(httr)
library(furrr)
library(remotes)
library(lubridate)
library(purrr)
library(data.table)
library(stringdist)
library(stringi)

# NEW
# install.packages("udpipe")
library(udpipe)



# Download the Portuguese model
ud_model <- udpipe_download_model(language = "portuguese")
ud_model <- udpipe_load_model(ud_model$file_model)


###############################
txt <- read_csv("data/(CART) (COLLECTTED 4-1-2023) CrowdTangle Historical-Report- Parliamentary Candidates MG - 2022-01-01-UNTIL-2023-01-02.csv") %>%
  select(page_name, body)

txt$doc_id <- paste("doc", rownames(txt), sep = "")


###############################
###############################

# Perform POS tagging using udpipe
annotated_text <- udpipe_annotate(ud_model, x = txt$body)
write_rds(annotated_text, file = "data/annotated_text.rds")

df_2 <- annotated_text %>%
  as.data.frame() %>%
  select(-sentence)


###############################
###############################
# Extract proper nouns
proper_nouns <- subset(df_2, upos == "PROPN")
proper_nouns_plus <- subset(df_2, upos %in% c("PROPN", "NOUN"))

# Extract specific proper nouns
nouns_bandeira <- subset(proper_nouns, token == "Bandeira") %>%
  filter(feats == "Gender=Masc|Number=Sing")

nouns_machado <- subset(proper_nouns, token == "Machado") %>%
  filter(dep_rel != "flat:name")

nouns_luz <- subset(proper_nouns, token == "Luz") %>%
  filter(feats == "Gender=Unsp|Number=Sing") %>%
  filter(dep_rel == "nmod")

nouns_conquista <- subset(proper_nouns, token == "Conquista") %>%
  filter(feats == "Gender=Masc|Number=Sing")

nouns_divino <- subset(proper_nouns, token == "Divino") %>%
  filter(feats == "Gender=Masc|Number=Sing")

nouns_liberdade <- subset(proper_nouns, token == "Liberdade") %>%
  filter(feats == "Gender=Unsp|Number=Sing")

nouns_extrema <- subset(proper_nouns, token == "Extrema")
nouns_passos <- subset(proper_nouns, token == "Passos")

nouns_campanha <- subset(proper_nouns_plus, token == "Campanha")
# nouns_campanha_analysis <- nouns_campanha %>%
#  select(doc_id, feats, dep_rel)
#    nouns_campanha_analysis2 <- left_join(nouns_campanha_analysis, txt, by="doc_id")

nouns_paiva <- subset(proper_nouns_plus, token == "Paiva")
# nouns_paiva_analysis <- nouns_paiva %>%
#  select(doc_id, feats, dep_rel)
#  nouns_paiva_analysis2 <- left_join(nouns_paiva_analysis, txt, by="doc_id")

nouns_cristina <- subset(proper_nouns_plus, token == "Cristina")
# nouns_cristina_analysis <- nouns_cristina %>%
#  select(doc_id, feats, dep_rel)
#    nouns_cristina_analysis2 <- left_join(nouns_cristina_analysis, txt, by="doc_id")

nouns_goncalves <- subset(proper_nouns_plus, token == "Gonçalves")
# nouns_goncalves_analysis <- nouns_goncalves %>%
#  select(doc_id, feats, dep_rel)
#    nouns_goncalves_analysis2 <- left_join(nouns_goncalves_analysis, txt, by="doc_id")

nouns_pimenta <- subset(proper_nouns_plus, token == "Pimenta")
nouns_pimenta_analysis <- nouns_pimenta %>%
  select(doc_id, feats, dep_rel)
nouns_pimenta_analysis2 <- left_join(nouns_pimenta_analysis, txt, by = "doc_id")


# This analysis revealed that:
# bandeira  = 28
# conquista = 12
# divino    = 65
# extrema   = 22
# liberdade = 9
# luz       = 5
# machado   = 43
# passos    = 103
# _____ manual _______
# paiva     = 1 (doc44515)
# campanha  = 4 (doc69352; doc74614; doc72616; doc5903)
# cristina  = 0
# goncalves = 0
# pimenta   = 15 (8175; 17187; 20332; 20725; 20956; 38328; 40371; 41180; 44423; 57135; 64866; 66183; 68223; 81750)



ambiguous_posts <- rbind(nouns_bandeira, nouns_machado, nouns_luz, nouns_conquista, nouns_divino, nouns_liberdade, nouns_extrema, nouns_passos)

# Values for doc_id to include
amb_posts_include <- c("doc72616", "doc69352", "doc74614", "doc5903", "doc44515", "doc8175", "doc17187", "doc20332", "doc20725", "doc20956", "doc38328", "doc40371", "doc41180", "doc44423", "doc57135", "doc64866", "doc66183", "doc68223", "doc8175", "doc87806")

# Subset rows from df based on doc_ids_to_include
txt_subset <- filter(txt, doc_id %in% amb_posts_include)
ambiguous_posts <- bind_rows(ambiguous_posts, txt_subset)



df_ambiguous <- left_join(ambiguous_posts, txt, by = "doc_id") %>%
  select(doc_id, page_name.y, body.y)
df_ambiguous$body.y <- stri_trans_general(str = df_ambiguous$body.y, id = "Latin-ASCII") # removing accents
df_ambiguous$body.y <- tolower(df_ambiguous$body.y)

ambiguous_names <- c("campanha", "cristina", "bandeira", "conquista", "divino", "extrema", "goncalves", "liberdade", "luz", "machado", "passos", "paiva", "pimenta")

# Count mentions of filtered_muni_names in the body column
muni_ambiguous_analysis <- df_ambiguous %>%
  mutate(mentioned_cities = str_extract_all(body.y, paste(ambiguous_names, collapse = "|"))) %>%
  unnest(mentioned_cities) %>%
  group_by(page_name.y, mentioned_cities) %>%
  summarise(
    total_mentions = n(),
    unique_mentions = 1,
    cities_mentioned = paste(mentioned_cities, collapse = ", ")
  ) %>%
  group_by(page_name.y) %>%
  summarise(
    total_mentions.y = sum(total_mentions), # Total count of city mentions for each account
    unique_mentions.y = n(), # Number of unique cities mentioned for each page_name.
    cities_mentioned.y = list(unique(mentioned_cities)) # A list of city names mentioned by each account
  )



write_rds(muni_ambiguous_analysis, file = "data/muni_ambiguous_analysis.rds")