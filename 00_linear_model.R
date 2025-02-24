## Required libs ####
require(dplyr)
require(ggplot2)

## Import Data ####

d.superleague <- read.csv("super_league_dataset.csv")

str(d.superleague) # Fits requirements for dataset

## Data Preprocessing ####
##TODO: Cleaninf of all the datatypes
d.clean <- d.superleague %>%
  mutate_all(~ replace(., . == "-", 0)) # Replace - with 0

# Dataset needs quite some fixing. Most numeric values are saved as character!!!


## First Overview ####

## Fixing Datatypes from Character to Numeric  --> not all done yet!!!!
d.forpairs <- d.clean %>%
  mutate(
    Tore = as.numeric(Tore), 
    Schüsse = as.numeric(Schüsse),) %>% 
  select(where(is.numeric))


pairs(d.forpairs)

# --> Mögliche einflüsse auf Tore 
# 1) Schüsse
# 2) Rote Karten
# 3) Gelbe Karten
# 4) Begangene Fouls
# 5) Gefoult worden
# 6) .... natürlich noch viele mehr


## First - Shoot to goal very likely to have an influence

ggplot(d.forpairs, aes(x = Schüsse, y = Tore)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Linear model
  labs(title = "Schüsse vs Tore", x = "Schüsse", y = "Tore") 


## Datensatz vorbereiten mit ersten 5  möglichen einflüssen

d.firstmodel <- d.clean %>%
  mutate(
    Tore = as.numeric(Tore), 
    Schüsse = as.numeric(Schüsse),
    Gelbe.Karten = as.numeric(Gelbe.Karten),
    Rote.Karten = as.numeric(Rote.Karten),
    Begangene.Fouls = as.numeric(Begangene.Fouls),
    Gefoult.worden = as.numeric(Gefoult.worden),) %>% 
  select(where(is.numeric))


lm.easymodel <- lm(data = d.firstmodel, 
                   formula = Tore ~ Schüsse + Gelbe.Karten + 
                     Rote.Karten + Begangene.Fouls + Gefoult.worden)

summary(lm.easymodel)


# First view at model looks like that the covariables gef

