
require("DBI")
require("RSQLITe")

#Connect to DB: https://www.kaggle.com/datasets/hugomathien/soccer
con <- dbConnect(RSQLite::SQLite(), "database.sqlite")
player <- dbReadTable(con, "player")
team <- dbReadTable(con, "team")
team_attributes <- dbReadTable(con, "team_attributes")
country <- dbReadTable(con, "country")


player_attributes <- dbReadTable(con, "player_attributes")
head(df)

#Disconntect

dbDisconnect(con)