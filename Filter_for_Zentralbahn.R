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

#creating a subset
library(dplyr)

# Define the specific Haltestellen
haltestellen_to_keep <- c("Luzern", "Luzern Allmend/Messe", "Kriens Mattenhof", "Horw", 
                          "Hergiswil Matt", "Hergiswil NW", "Stansstad", "Stans")



## Subset the data for weather region LU
zb_final_subset <- zb_final %>%
  filter(HALTESTELLEN_NAME %in% haltestellen_to_keep)

summary(zb_final_subset$HALTESTELLEN_NAME)

View(zb_final_subset)

