## Data Import Section ----

library(dplyr)
## Import Stat Data
d.statistics <- read.csv("/Users/leonarddost/Downloads/Unbenannte Tabelle - Statistics.csv", header = TRUE, sep = ",")
head(d.statistics)

## Import Player Data 
d.player <- read.csv("/Users/leonarddost/Downloads/Unbenannte Tabelle - Spieler.csv", header = TRUE, sep = ",", na.strings = c("", "NA"))
d.player <- d.player %>% 
  na.omit() %>%  ## remove empty rows
head(d.player)

##Merge Player into Statistics set.

d.fulldata <- d.statistics %>% 
  left_join(d.player, by = c("Player" = "Spieler"))

head(d.fulldata)

## Data Manipulation ----
## convert season into numeric year
# Extract the first year and convert to numeric
d.fulldata$Saison <- as.numeric(substr(d.fulldata$Saison, 1, 4))


# Count NAs in the column "Nationalität"
na_count <- sum(is.na(d.fulldata$Nationalität))

# Print result
print(na_count)
table(na_count)
print(668/4)

##
# Spieler filtern, bei denen "Klub" NA ist
df_filtered <- d.fulldata[is.na(d.fulldata$Klub), "Player", drop = FALSE]

# Gefilterte Daten als CSV speichern
write.csv(df_filtered, "filtered_players.csv", row.names = FALSE, fileEncoding = "UTF-8")

# Bestätigung ausgeben
print("Export erfolgreich: 'filtered_players.csv'")

# Nur den Nachnamen aus der "Player"-Spalte extrahieren
d.fulldata$Player <- sub(".*\\s", "", d.fulldata$Player)

# CSV exportieren mit nur der "Player"-Spalte (Nachnamen)
write.csv(d.fulldata["Player"], "player_list.csv", row.names = FALSE, fileEncoding = "UTF-8")

# Bestätigung ausgeben
print("Export erfolgreich: 'player_list.csv'")


## Plot Section ----
library(ggplot2)

ggplot(d.fulldata, aes(x = Alter)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "white") +
  theme_minimal() +
  labs(title = "Age Distribution of Players", x = "Age", y = "Count")

ggplot(d.fulldata, aes(x = Position, y = Einsatzminuten)) +
  geom_boxplot(fill = "orange") +
  theme_minimal() +
  labs(title = "Minutes Played by Position", x = "Position", y = "Minutes Played")

ggplot(d.fulldata, aes(x = Klub)) +
  geom_bar(fill = "purple") +
  theme_minimal() +
  coord_flip() +  # Rotate for better readability
  labs(title = "Number of Players per Club", x = "Club", y = "Number of Players")

## Investigate Cards, position and clubs

d.avg_cards <- d.fulldata %>%
  group_by(Position, Klub) %>%
  summarise(Avg_Cards = mean(Total_Cards, na.rm = TRUE))

ggplot(d.avg_cards, aes(x = Position, y = Klub, size = Avg_Cards)) +
  geom_point(color = "blue", alpha = 0.7) +
  theme_minimal() +
  scale_size_continuous(range = c(2, 10)) +
  labs(title = "Average Total Cards per Club and Position",
       x = "Position",
       y = "Club",
       size = "Avg. Total Cards")

## Export Dataset

write.csv(d.fulldata, "/Users/leonarddost/Downloads/super_league_dataset.csv")

