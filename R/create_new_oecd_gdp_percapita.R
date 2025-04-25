#' Steps before this script:
#' 1. run "R/create_new_iiasa_population_combined_dataframe.R"
#' 2. run "R/create_new_oecd_gdp_dataframe_with_kosovo.R"
#' Then:
#' 1. run this script
#'


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

# Load v3.1 for GDP format ----
year.columns <- c("1950", "1955", "1960", "1965", "1970", "1975", "1980", "1985",
                  "1990", "1995", "2000", "2005", "2010", "2015", "2020", "2025",
                  "2030", "2035", "2040", "2045", "2050", "2055", "2060", "2065",
                  "2070", "2075", "2080", "2085", "2090", "2095", "2100")
v3_1_path <- here("data", "1721734326790-ssp_basic_drivers_release_3.1_full.xlsx")
gdp_format_v3_1 <- read_excel(
  v3_1_path,
  sheet = "data",
  col_types = c(rep("text", 5), rep("numeric", length(year.columns)))
) %>% iamc_wide_to_long(upper.to.lower = T) %>%
  filter(
    !grepl(region, pattern="(R5)", fixed=T),
    !grepl(region, pattern="(R9)", fixed=T),
    !grepl(region, pattern="(R10)", fixed=T),
    region!="World"
  ) %>%
  mutate_cond(region=="Micronesia", region="Micronesia (Federated States of)") %>% # countrycode package doesn't recognise "Micronesia" by itself
  mutate(iso=countrycode(region, origin = "country.name", destination = "iso3c")) %>%
  drop_na(iso) %>% # drop regions
  filter(
    scenario != "Historical Reference" # exclude historical reference data
  ) %>%
  filter(variable %in% c("GDP|PPP", "GDP|PPP [per capita]"),
         model == oecd2023name)
gdp_format_v3_1 %>% distinct(
  model,variable,unit
)

# Create per capita GDP ----

## Updated OECD GDP ----
# from: "R/create_new_oecd_gdp_dataframe_with_kosovo.R"
gdp <- read_csv(
  here("output", "v20250417_gdp", "data", "GDP_PPP_longformat.csv")
)


## Updated Population ----
# from: "R/create_new_iiasa_population_combined_dataframe.R"
pop <- read_csv(
  here("output", "v20250417_pop", "data", "POPULATION_longformat.csv")
)

## Compare country coverage ----
### region name
print("In POP, not in GDP")
setdiff(unique(pop %>% pull(region)),
        unique(gdp %>% pull(region)))
print("In GDP, not in POP")
setdiff(unique(gdp %>% pull(region)),
        unique(pop %>% pull(region)))
### iso
print("In POP, not in GDP")
setdiff(unique(pop %>% pull(iso)),
        unique(gdp %>% pull(iso)))
print("In GDP, not in POP")
setdiff(unique(gdp %>% pull(iso)),
        unique(pop %>% pull(iso)))

## Calculate GDP|PPP [per capita] ----
gdp.per.capita.wdetail <- gdp %>% left_join(
  pop %>%
    mutate_cond(region=="Virgin Islands, U.S.", region="United States Virgin Islands") %>%
    rename(pop = value,
           model.pop = model,
           variable.pop = variable,
           unit.pop = unit),
  by = c("scenario", "region", "iso", "year")
) %>%
  mutate_cond(
    ((pop!=0) & !is.na(pop)),
    value = value * 1e9 / (pop * 1e6) ,
    variable = "GDP|PPP [per capita]",
    unit = "USD_2017/yr"
  ) %>%
  mutate_cond(
    ((pop==0) | is.na(pop)),
    value = NA,
    variable = "GDP|PPP [per capita]",
    unit = "USD_2017/yr"
  )

### Format ----
gdp.per.capita <- gdp.per.capita.wdetail %>%
  select(model,scenario,region,iso,variable,unit,year,value) %>%
  arrange(model,scenario,region,iso,variable,unit,year)


### Some checks ----
new_countries <- 5
expect_equal(
  length(gdp.per.capita %>% pull(region) %>% unique()),
  length(gdp_format_v3_1 %>% pull(region) %>% unique()) + new_countries
)

expect_equal(
  length(gdp.per.capita %>% pull(variable) %>% unique()),
  1
)
expect_equal(
  gdp.per.capita %>% pull(variable) %>% unique(),
  "GDP|PPP [per capita]"
)

expect_equal(
  length(gdp %>% pull(variable) %>% unique()),
  1
)
expect_equal(
  gdp %>% pull(variable) %>% unique(),
  "GDP|PPP"
)


## SAVE FINALISED GDP DATA ----
write_delim(
  x = gdp.per.capita,
  file = here("output", "v20250417_gdp", "data", "GDP_PPP_percapita_longformat.csv"),
  delim = ","
)
