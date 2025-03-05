
require("DBI")
require("RSQLITe")
require("curl")
url <- "https://www.kaggle.com/api/v1/datasets/download/hugomathien/soccer"

# Destination file path to save the ZIP file
zip_destfile <- "database.zip"

# Download the ZIP file
curl_download(url, zip_destfile)
# Download the SQLite file
curl_download(url, zip_destfile)

unzip(zipfile = zip_destfile, "database.sqlite")

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