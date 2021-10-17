## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE, fig.dim = c(7, 5)----------------------------------------
library(covid19br)
library(tidyverse)

# loading the election data:
data(election2018Cities)

# looking at the data:
glimpse(election2018Cities)

election2018Cities <- election2018Cities %>%
  add_geo() %>%
  mutate(
    prop = 100*Bolsonaro/(Bolsonaro + Haddad),
  ) %>%
  pivot_longer(cols = c("Bolsonaro", "Haddad"), values_to = "votes", names_to = "candidate")


ggplot(election2018Cities) +
  geom_sf(aes(fill = prop)) +
  labs(fill = "% votes")


## ---- message=FALSE, fig.dim = c(7, 5)----------------------------------------

# extracting the data:
mg_election <- election2018Cities %>%
  filter(state == "MG")

# visualizing:
ggplot(mg_election) +
  geom_sf(aes(fill = prop)) +
  labs(fill = "% votes")


## -----------------------------------------------------------------------------

# loading the election data:
data(election2018Regions)

  # putting all together 
regions <- downloadCovid19("regions") %>%
  filter(date == max(date)) %>%
  add_epi_rates() %>%
  left_join(election2018Regions) %>%
  add_geo()

glimpse(regions)

