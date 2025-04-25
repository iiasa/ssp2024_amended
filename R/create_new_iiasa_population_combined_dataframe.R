new_filename_population <- "iiasa wic 2023 pop V15 rebase.csv"
new_filename_education <- "iiasa wic 2023 mys V15 rebase.csv"
new_filename_UN_historical <- "UN WPP 2022 pop V15 rebase.csv"
new.version <- "v20250417_pop"
variables.to.investigate <- c("Population")


# Load libraries ---------------------------------------------------------------
library("here")
library("tidyverse")
library("vroom")
library("readxl")
library("countrycode")
library("ggthemes")
library("testthat")

here::i_am("ssp2024_amended.Rproj")

source(here("R","utils.R"))

out.path.figures <- here("output", new.version, "figures")
dir.create(out.path.figures, recursive = T)
out.path.data <- here("output", new.version, "data")
dir.create(out.path.data, recursive = T)

# Load ----

new <-
  bind_rows(
    vroom(here("data", "final_new_population", new_filename_UN_historical), locale = locale(encoding = "ISO-8859-1")),
    bind_rows(
      vroom(here("data", "final_new_population", new_filename_population), locale = locale(encoding = "ISO-8859-1")),
      vroom(here("data", "final_new_population", new_filename_education), locale = locale(encoding = "ISO-8859-1"))
    )
  ) %>%
  iamc_wide_to_long(upper.to.lower = T) %>%
  mutate_cond(region=="Micronesia", region="Micronesia (Federated States of)") %>% # countrycode package doesn't recognise "Micronesia" by itself
  mutate(iso=countrycode(region, origin = "country.name", destination = "iso3c")) %>%
  mutate_cond(region=="Kosovo", iso=KOSOVO.ISO3.CODE) %>%
  drop_na(iso) %>%  # drop regions
  filter(
    model != "UN WPP2022 POP" # exclude historical reference data
  ) %>%
  # some formatting to get the same format as from the SSP database
  mutate(
    model = "IIASA-WiC POP 2023",
    scenario = substr(scenario,start=1,stop=4)
  ) %>%
  # fix value to align with unit
  mutate_cond(
    grepl(variable, pattern="Population", fixed=T),
    value = value/1000
  )

write_delim(
  x = new,
  file = here("output", "v20250417_pop", "data", "DEMOGRAPHICS_longformat.csv"),
  delim = ","
)

## SAVE NEW LONG FORMAT DATA FOR USE IN CALCULATING GDP per capita -------------

write_delim(
  x = new %>% filter(variable=="Population"),
  file = here("output", "v20250417_pop", "data", "POPULATION_longformat.csv"),
  delim = ","
)
