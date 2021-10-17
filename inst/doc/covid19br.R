## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message = FALSE, fig.dim=c(7, 5)----------------------------------------
library(covid19br)
library(tidyverse)

# downloading the data (at national level):
brazil <- downloadCovid19("brazil")

# looking at the downloaded data:
glimpse(brazil)

# plotting the accumulative number of deaths:
ggplot(brazil, aes(x = date, y = accumDeaths)) +
  geom_point() +
  geom_path()


## ---- message = FALSE---------------------------------------------------------
library(pracma)

# computing the moving average:
brazil <- brazil %>%
  mutate(
    ma_newDeaths = movavg(newDeaths, n = 7, type = "s")
  )

# looking at the transformed data:
glimpse(brazil)

## ---- message = FALSE, fig.dim=c(7, 5)----------------------------------------
deaths <- brazil %>%
  select(date, newDeaths, ma_newDeaths) %>%
  pivot_longer(
    cols = c("newDeaths", "ma_newDeaths"),
    values_to = "deaths", names_to = "type"
  ) %>%
  mutate(
    type = recode(type, 
           ma_newDeaths = "moving average",
           newDeaths = "count",
    )
  )

# looking at the (tidy) data:
glimpse(deaths)

# drawing the desired plot:
ggplot(deaths, aes(x = date, y=deaths, color = type)) +
  geom_point() +
  geom_path() + 
  theme(legend.position="bottom")


## ---- message = FALSE, fig.dim=c(7, 5)----------------------------------------

# downloading the data (region level):
regions <- downloadCovid19("regions") 

# adding the rates to the downloaded data:
regions <- regions %>%
  add_epi_rates()

# looking at the data:
glimpse(regions)


## ---- message = FALSE, fig.dim=c(7, 5)----------------------------------------
library(plotly)

p <- ggplot(regions, aes(x = date, y = mortality, color = region)) +
  geom_point() +
  geom_path()

ggplotly(p)

## ---- message = FALSE---------------------------------------------------------
library(kableExtra)

cities <- downloadCovid19("cities")

capitals <- cities %>%
  filter(capital == TRUE, date == max(date)) %>%
  add_epi_rates() %>%
  select(region, state, city, newCases, newDeaths, accumCases, accumDeaths, incidence, mortality, lethality) %>%
  arrange(desc(lethality), desc(mortality), desc(incidence))

# printing the table:
capitals %>%
 kable(
    full_width = F,
    caption = "Summary of the COVID-19 pandemic in the 27 capitals of Brazilian states."
  )



