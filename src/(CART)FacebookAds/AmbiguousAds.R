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

#NEW
#install.packages("udpipe")
library(udpipe)

# Download the Portuguese model
ud_model <- udpipe_download_model(language = "portuguese")
ud_model <- udpipe_load_model(ud_model$file_model)

###############################
###############################
###########Datasets############
adstxt <- df_terms %>% 
  select(page_name, ad_creative_bodies)

adstxt$doc_id <- paste("doc", rownames(adstxt), sep="")

# Create a data frame with the 'text' column
df <- data.frame(adstxt$ad_creative_bodies)

# Perform POS tagging using udpipe
annotated_ads <- udpipe_annotate(ud_model, x = adstxt$ad_creative_bodies)
adstxt2 <- annotated_ads %>% 
  as.data.frame() %>%
  dplyr::select(-sentence)



###############################
###############################
# Extract proper nouns
proper_nouns_ads <- subset(adstxt2, upos == "PROPN")
proper_nouns_plus_ads <- subset(adstxt2, upos %in% c("PROPN", "NOUN"))

#Extract specific proper nouns 
nouns_ads_bandeira <- subset(proper_nouns_plus_ads, token == "bandeira")

nouns_ads_machado <- subset(proper_nouns_plus_ads, token == "machado")

nouns_ads_luz <- subset(proper_nouns_plus_ads, token == "luz") %>%
  filter(feats=="Gender=Unsp|Number=Sing") %>% 
  filter(dep_rel=="nmod")

nouns_ads_conquista <- subset(proper_nouns_plus_ads, token == "conquista") %>% 
  filter(feats=="Gender=Masc|Number=Sing")

nouns_ads_divino <- subset(proper_nouns_plus_ads, token == "divino")%>% 
  filter(feats=="Gender=Masc|Number=Sing")

nouns_ads_liberdade <- subset(proper_nouns_plus_ads, token == "liberdade") %>% 
  filter(feats=="Gender=Unsp|Number=Sing")

nouns_ads_extrema <- subset(proper_nouns_plus_ads, token == "extrema")
nouns_ads_passos <- subset(proper_nouns_plus_ads, token == "passos")

nouns_ads_campanha <- subset(proper_nouns_plus_ads, token == "Campanha")
#nouns_campanha_analysis <- nouns_campanha %>% 
#  select(doc_id, feats, dep_rel)
#    nouns_campanha_analysis2 <- left_join(nouns_campanha_analysis, txt, by="doc_id")

nouns_ads_paiva <- subset(proper_nouns_plus_ads, token == "paiva")
#nouns_paiva_analysis <- nouns_paiva %>% 
#  select(doc_id, feats, dep_rel)
#  nouns_paiva_analysis2 <- left_join(nouns_paiva_analysis, txt, by="doc_id")

nouns_ads_cristina <- subset(proper_nouns_plus_ads, token == "cristina")
#nouns_cristina_analysis <- nouns_cristina %>% 
#  select(doc_id, feats, dep_rel)
#    nouns_cristina_analysis2 <- left_join(nouns_cristina_analysis, txt, by="doc_id")

nouns_ads_goncalves <- subset(proper_nouns_plus_ads, token == "goncalves")
#nouns_goncalves_analysis <- nouns_goncalves %>% 
#  select(doc_id, feats, dep_rel)
#    nouns_goncalves_analysis2 <- left_join(nouns_goncalves_analysis, txt, by="doc_id")

nouns_ads_pimenta <- subset(proper_nouns_plus_ads, token == "pimenta")
#nouns_ads_pimenta_analysis <- nouns_pimenta %>% 
#  select(doc_id, feats, dep_rel)
#nouns_ads_pimenta_analysis2 <- left_join(nouns_pimenta_analysis, txt, by="doc_id")


#This analysis revealed that:
# bandeira  = 2
# conquista = 0
# divino    = 8
# extrema   = 14
# liberdade = 8
# luz       = 6
# machado   = 29
# passos    = 95

# paiva     =  2
# campanha  =  10
# cristina  =  3
# goncalves =  5
# pimenta   =  4

ambiguous_manual_list <- read.csv("AmbiguousAdsList.csv")


# Subset rows from df based on ambiguous_manual_list
ambiguous_ads <- adstxt %>%
  semi_join(ambiguous_manual_list, by = "doc_id")





df_ambiguous_ads <- left_join(ambiguous_ads, txt, by="doc_id")
df_ambiguous$body = stri_trans_general(str = df_ambiguous$body, id = "Latin-ASCII")   #removing accents
df_ambiguous$body <- tolower(df_ambiguous$body)


# Count mentions of filtered_muni_names in the body column
muni_ambiguous_analysis_ads <-  df_ambiguous_ads %>%
  mutate(mentioned_cities = str_extract_all(body, paste(ambiguous_names, collapse = "|"))) %>%
  unnest(mentioned_cities) %>%
  group_by(page_name.y, mentioned_cities) %>%
  summarise(
    total_mentions = n(),
    unique_mentions = 1,
    cities_mentioned = paste(mentioned_cities, collapse = ", ")
  ) %>%
  group_by(page_name.y) %>%
  summarise(
    total_mentions.y = sum(total_mentions),        #Total count of city mentions for each account
    unique_mentions.y = n(),                       #Number of unique cities mentioned for each page_name.
    cities_mentioned.y = list(unique(mentioned_cities))   #A list of city names mentioned by each account
  )



