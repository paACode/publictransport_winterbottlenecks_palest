library(dplyr)
library(ggplot2)
library(tidyr)

#### Read in Data ####----------------------------------------------------------
d.zentrahlbahn<- read.csv("zentrahlbahn_final.csv") %>% 
  select(BETRIEBSTAG, LINIEN_TEXT, HALTESTELLEN_NAME, ANKUNFTSZEIT, AN_PROGNOSE,
         ANKUNFTDELAY_sec, ABFAHRTSZEIT, AB_PROGNOSE, ABFAHRTDELAY_sec,
         Delay_Category, TAGESZEIT, RUSH_HOUR, w_precip_mm_Luzern, w_temp_min_c_Luzern, w_temp_avg_c_Luzern)


#### Function to Detect if observation is DEPARTURE; ARRIVAL ; TRANSIT
classify_observation <- function(ankunftszeit, abfahrzeit) {
  if (is.na(ankunftszeit)) {
    return("DEPARTURE")
  } else if (is.na(abfahrzeit) ) {
    return("ARRIVAL")
  } else if (!is.na(ankunftszeit) & !is.na(abfahrzeit)) {
    return("TRANSIT")
  } else {
    stop("Something went wrong")
  }
}


#### Classifiy Trainstations overview #### -----------------------------------

#Count observation typersper TRAINSTATION

overview_counts <- d.zentrahlbahn %>%
  group_by(HALTESTELLEN_NAME) %>%
  summarise(
    STATION_DEPARTURE_COUNT = sum(mapply(classify_observation, ANKUNFTSZEIT,ABFAHRTSZEIT) == "DEPARTURE"),
    STATION_ARRIVAL_COUNT = sum(mapply(classify_observation, ANKUNFTSZEIT, ABFAHRTSZEIT) == "ARRIVAL"),
    STATION_TRANSIT_COUNT = sum(mapply(classify_observation, ANKUNFTSZEIT, ABFAHRTSZEIT) == "TRANSIT"),
    STATION_TOTAL_COUNT = STATION_DEPARTURE_COUNT + STATION_ARRIVAL_COUNT + STATION_TRANSIT_COUNT
  )

overview_counts_long <- overview_counts %>%
  pivot_longer(cols = c(STATION_DEPARTURE_COUNT, STATION_ARRIVAL_COUNT, STATION_TRANSIT_COUNT),
               names_to = "type", values_to = "count") %>%
  mutate(type = factor(type, levels = c("STATION_DEPARTURE_COUNT", "STATION_ARRIVAL_COUNT", "STATION_TRANSIT_COUNT"),
                       labels = c("Departure", "Arrival", "Transit")))

ggplot(overview_counts_long, aes(x = HALTESTELLEN_NAME, y = count, fill = type)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Station Name",
    y = "Count",
    title = "Available observations for each station"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


#### Counter per Station

station_counter <- d.zentrahlbahn %>%
  #Please Note: Dataset is already ordered by Time
  mutate(OBSERVATION_TYPE =mapply(classify_observation, ANKUNFTSZEIT,ABFAHRTSZEIT)) %>% 
  group_by(HALTESTELLEN_NAME) %>%
    mutate(
      TRANSIT_COUNT = cumsum(OBSERVATION_TYPE == "TRANSIT"),
      ARRIVAL_COUNT = cumsum(OBSERVATION_TYPE == "ARRIVAL"),
      DEPARTURE_COUNT = cumsum(OBSERVATION_TYPE == "DEPARTURE"),
      TOTAL_COUNT = TRANSIT_COUNT + ARRIVAL_COUNT +DEPARTURE_COUNT
    ) %>%
  ungroup()


#### Plot the counters ####---------------------------------------------
station_counter_subset <- station_counter %>%
  mutate(idx = row_number()) %>%
  filter(idx %% 10 == 0)  # Keep only every 10th point

ggplot(station_counter_subset, aes(x = idx, y = TOTAL_COUNT, color = HALTESTELLEN_NAME)) +
  geom_line() +  # Customize point size
  labs(
    x = "Row Index",
    y = "Total Count",
    title = "Total Count for Each Station Grouped by HALTESTELLEN_NAME"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Reshape the data into long format 
station_counter_long <- station_counter %>%
  mutate(idx = row_number()) %>%
  pivot_longer(cols = c(TRANSIT_COUNT, ARRIVAL_COUNT, DEPARTURE_COUNT, TOTAL_COUNT),
               names_to = "count_type", values_to = "count") %>%
  mutate(count_type = factor(count_type, levels = c("TRANSIT_COUNT", "ARRIVAL_COUNT", "DEPARTURE_COUNT", "TOTAL_COUNT"),
                             labels = c("Transit", "Arrival", "Departure", "Total Count")))

ggplot(station_counter_long, aes(x = idx, y = count, color = HALTESTELLEN_NAME)) +
  geom_line() +
  facet_wrap(~ count_type, scales = "free_y", ncol = 2) +  # Create separate plots for each count type
  labs(
    x = "Row Index",
    y = "Count",
    title = "Counts for Each Station Grouped by HALTESTELLEN_NAME"
  ) 





