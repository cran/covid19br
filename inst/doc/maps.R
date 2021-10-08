## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE, fig.dim = c(7, 5)----------------------------------------

library(covid19br)
library(tidyverse)

# downloading data at state level:
cities <- downloadCovid19("cities") 

# adding the geometry to the data:
cities_geo <- cities %>%
  filter(date == max(date)) %>%
  add_geo()

# looking at the data:
glimpse(cities_geo)


## ---- fig.dim = c(7, 5)-------------------------------------------------------
ggplot(cities_geo) +
  geom_sf(aes(fill = accumDeaths)) 

## ---- fig.dim = c(7, 5)-------------------------------------------------------

mg <- cities_geo %>%
  filter(state == "MG") %>%
  add_epi_rates()

ggplot(mg) +
  geom_sf(aes(fill = incidence)) 

## ---- message=FALSE, fig.dim = c(7, 5)----------------------------------------
library(leaflet)

# downloading data at state level:
states <- downloadCovid19("states") 

# adding the geometry to the data:
states_geo <- states %>%
  filter(date == max(date)) %>%
  add_geo() %>%
  add_epi_rates()

# looking at the data:
glimpse(states_geo)

reds = colorBin("Reds", domain = states_geo$lethality, bins = 5)
mymap <- states_geo %>%
  leaflet() %>%
  addPolygons(fillOpacity = 1, 
              weight = 1,
              smoothFactor = 0.2,
              color = ~ reds(lethality),
              popup = paste0(states_geo$state, ":  ",  states_geo$lethality, 2)
  ) %>%
  addLegend(position = "bottomright", 
            pal = reds, values = ~states_geo$lethality, 
            title = "lethality")
mymap  


## ---- fig.dim = c(7, 5)-------------------------------------------------------
library(sf)
library(ggrepel)

# getting the data:
capitals <- downloadCovid19("cities") %>%
  filter(date == max(date), capital == TRUE) %>%
  add_geo() %>%
  add_epi_rates()

# adding the coordinates associated with each capital:
capitals <- cbind(capitals, st_coordinates(st_centroid(capitals)))  
# looking at the data:
glimpse(capitals)  

# drawing some maps:
incidence <- ggplot() +
  geom_sf(data = ibgeStates, aes(geometry=geometry)) +
  geom_point(data = capitals, aes(x=X, y=Y, size=incidence, alpha=incidence), color = "orange") +
  geom_text_repel( data=capitals, aes(x=X, y=Y, label=city), size=3)
incidence

mortality <- ggplot() +
  geom_sf(data = ibgeStates, aes(geometry=geometry)) +
  geom_point(data = capitals, aes(x=X, y=Y, size=mortality, alpha=mortality), color = "red") +
  geom_text_repel( data=capitals, aes(x=X, y=Y, label=city), size=3)
mortality

lethality <- ggplot() +
  geom_sf(data = ibgeStates, aes(geometry=geometry)) +
  geom_point(data = capitals, aes(x=X, y=Y, size=lethality, alpha=lethality), color = "darkred") +
  geom_text_repel( data=capitals, aes(x=X, y=Y, label=city), size=3)
lethality


## ---- fig.dim = c(7, 5)-------------------------------------------------------
world <- downloadCovid19("world") %>%
  filter(date == max(date)) %>%
  add_geo() %>%
  add_epi_rates()

ggplot(world) +
  geom_sf(aes(fill = lethality)) + 
  scale_fill_gradient2(low = "red", high = "darkred", na.value = NA) +
  theme(legend.position="bottom")

