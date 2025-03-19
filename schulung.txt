## Libraries
library(dplyr)

## Import dataset ----
df <- read.csv("/Users/leonarddost/Downloads/ist-daten-2024-11(2)/2024-11-30_istdaten.csv", 
               stringsAsFactors = FALSE, 
               sep = ";", 
               header = TRUE, 
               check.names = FALSE)

## Check if column names are now correctly parsed ----

colnames(df) 

## Filter for Trains ONLY ----

colnames(df)
df_filter <- df %>% 
  filter(PRODUKT_ID == "Zug")

## Filter NAs ----

##Remove Rows without Ankunftszeit, Ankunftsprognose, Haltestellennamen & DB/ÖBB

df_filter <- df_filter %>% 
  filter(ANKUNFTSZEIT != "" & !is.na(ANKUNFTSZEIT)) %>%
  filter(AN_PROGNOSE != "" & !is.na(AN_PROGNOSE)) %>% 
  filter(HALTESTELLEN_NAME != "" & !is.na(HALTESTELLEN_NAME)) %>% 
  filter(!BETREIBER_ABK %in% c("DB", "DB Regio", "NeTS-ÖBB")) %>% 
  filter(BETREIBER_ABK == "ZB")

