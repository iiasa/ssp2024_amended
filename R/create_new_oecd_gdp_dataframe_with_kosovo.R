#' Steps:
#' 1. load original data
#' 2. add in supplemental countries
#' 3. create GDP (total) for Kosovo
#' -> create IMF timeseries
#' -> calculate growth rates of surrounding countries
#' -> apply to Kosovo
#' 4. calculate GDP per capita
#' --> in here("R", "create_new_gdp_percapita.R")


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

new.version <- "v20250417_gdp"
out.path.figures <- here("output", new.version, "figures")
dir.create(out.path.figures, recursive = T)
out.path.data <- here("output", new.version, "data")
dir.create(out.path.data, recursive = T)

# Create the updated version ----

source(here("R", "KOSOVO_ISO3CODE.R"))
kosovo.region.name <- "Kosovo" # follow SSP population data

oecd2023name <- "OECD ENV-Growth 2023"
oecd2023name.supplemental <- "OECD ENV-Growth (supplemental)"

unit.gdp <- "billion USD_2017/yr"

GDP.YEARS.TO.KEEP <- seq(1950,2100,5)

## Load ----
### v3.1 -----------------------------------------------------------------------
# N.B. need to force the read_excel function to read all year_columns as numeric
year.columns <- c("1950", "1955", "1960", "1965", "1970", "1975", "1980", "1985",
                  "1990", "1995", "2000", "2005", "2010", "2015", "2020", "2025",
                  "2030", "2035", "2040", "2045", "2050", "2055", "2060", "2065",
                  "2070", "2075", "2080", "2085", "2090", "2095", "2100")
v3_1_path <- here("data", "1721734326790-ssp_basic_drivers_release_3.1_full.xlsx")
v3_1 <- read_excel(
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
  )
# we only retain total GDP, only OECD
v3_1 <- v3_1 %>%
  filter(variable %in% c("GDP|PPP"),
         model == oecd2023name)

### new countries: 'Submission additional countries approximation v2.xlsx' ----
#' Shared by Rob Dellink per email
#' Sent (date): Saturday, November 23, 2024 15:21
#' Subject: RE: SSP OECD GDP: dealing with missing countries and population deviation mismatch
#' To: KIKSTRA Jarmo <kikstra@iiasa.ac.at>
#' CC: CRESPO.CUARESMA Jesus (FWD) <Jesus.Crespo.Cuaresma@wu.ac.at>; VAN RUIJVEN Bas <vruijven@iiasa.ac.at>; KREY Volker <krey@iiasa.ac.at>; RIAHI Keywan <riahi@iiasa.ac.at>; Jean.CHATEAU <Jean.CHATEAU@oecd.org>
#'
new.countries.oecd <- read_excel(path = here("data", "final_new_gdp", "Submission additional countries approximation v2.xlsx")) %>%
  rename(
    model = `...1`,
    scenario = `...2`,
    iso = `...3`,
    variable = `...4`,
    unit = `...5`
  ) %>%
  iamc_wide_to_long() %>%
  mutate_cond(variable=="GDP_per_capita|PPP", variable = "GDP|PPP [per capita]") %>%
  mutate(scenario = substr(scenario, 1,4))
# we only retain total GDP, only OECD
new.countries.oecd <- new.countries.oecd %>%
  filter(variable %in% c("GDP|PPP"),
         model == oecd2023name)

### Starting point for Kosovo ----
#' Shared by Rob Dellink per email
#' Sent (date): Friday, January 24, 2025 15:23
#' Subject: SSP OECD GDP: dealing with missing countries and population deviation mismatch
#' To: KIKSTRA Jarmo <kikstra@iiasa.ac.at>; CRESPO.CUARESMA Jesus (FWD) <Jesus.Crespo.Cuaresma@wu.ac.at>
#' CC: VAN RUIJVEN Bas <vruijven@iiasa.ac.at>; KREY Volker <krey@iiasa.ac.at>; RIAHI Keywan <riahi@iiasa.ac.at>; Jean.CHATEAU <Jean.CHATEAU@oecd.org>; HUPPMANN Daniel <huppmann@iiasa.ac.at>
#'
#' Usage instructions (from Rob Dellink's email):
#' - Kosovo – I’m pretty sure it is missing, not merged with Serbia. I had a quick look and if we do another round we can probably add it. Below is a brief table with the IMF GDP data and projections for 2010-2028 if you want to already do something manually.
#' References (WEO 2024/2025):
#' - https://www.imf.org/external/datamapper/PPPGDP@WEO/UVK (see 2017 current prices GDP PPP: 18.69 bln)

kosovo.starting.point <- read_excel(path = here("data", "final_new_gdp", "kosovo_startingpoint.xlsx"), skip = 1, sheet = "data") %>%
  rename(year = `...1`) %>%
  mutate(model = oecd2023name.supplemental, # IMF data, here used just for its 2025 and other historic numbers
         iso = KOSOVO.ISO3.CODE,
         variable = "GDP|PPP",
         unit = unit.gdp,
         value = KSV / 1e9
  ) %>%
  select(-KSV)
kosovo.starting.point.2025 <- kosovo.starting.point %>% filter(year==2025) %>% pull(value) %>% as.numeric()
ssps <- c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")


## Create Kosovo SSP timeseries ----

### Cross history with SSPs ----
kosovo.history <- crossing(
  kosovo.starting.point,
  scenario = ssps
) %>%
  select(model,scenario,iso,variable,unit,year,value) %>%
  arrange(model,scenario,iso,variable,unit,year)

### Find growth rates (from 2025 onwards) of a few surrounding countries ----
kosovo.growth.rate.countries <- c("SRB", "ALB", "MNE", "MKD") # Serbia, Albania, Montenegro, and North Macedonia
kosovo.growth.rate.countries.GDPPPP <- v3_1 %>%
  filter(iso %in% kosovo.growth.rate.countries)
kosovo.growth.rate.countries.GDPPPP.normalised <- kosovo.growth.rate.countries.GDPPPP %>%
  normalise_iamc_long(starting.year = 2025) %>%
  mutate(unit = "Index (2025=1)")
kosovo.growth.rate.countries.GDPPPP.normalised.average <- kosovo.growth.rate.countries.GDPPPP.normalised %>%
  reframe(
    value = mean(value),
    note = paste0("Average of 2025-year change for: ", paste(kosovo.growth.rate.countries, collapse = ", ")),
    iso = KOSOVO.ISO3.CODE,
    .by = c("model", "scenario", "variable", "unit", "year")
  ) %>%
  mutate_cond(model==oecd2023name, model = oecd2023name.supplemental) %>%
  select(model,scenario,iso,variable,unit,year,value,note) %>%
  arrange(model,scenario,iso,variable,unit,year)

### Apply growth rates (from 2025 onwards) to Kosovo history ----
kosovo.future.wdetail <-
  # join
  kosovo.growth.rate.countries.GDPPPP.normalised.average %>% rename(unit.index=unit, index=value, note.index=note) %>%
  full_join(kosovo.history,
            by = c("model", "scenario", "iso", "variable", "year")) %>%
  arrange(model,scenario,iso,variable,unit,year) %>%
  # add 2025 value as the starting point
  mutate(value.2025 = kosovo.starting.point.2025) %>%
  # apply calculation
  mutate_cond(
    (is.na(value) & year>2025),
    value = value.2025 * index,
    unit = unit.gdp
  )

### Format ----
kosovo.future <- kosovo.future.wdetail %>%
  select(model,scenario,iso,variable,unit,year,value) %>%
  arrange(model,scenario,iso,variable,unit,year)


## Combine data ----
new.oecd.gdp.ppp <- v3_1 %>%
  # new, supplied by Rob Dellink
  bind_rows(new.countries.oecd %>%
              mutate(region=NA_character_) %>%
              #' follow SSP population names
              #'   region      iso
              #   <chr>       <chr>
              #   1 Afghanistan AFG
              #   2 Palestine   PSE
              #   3 Syria       SYR
              #   4 Venezuela   VEN
              mutate_cond(iso=="AFG", region = "Afghanistan") %>%
              mutate_cond(iso=="PSE", region = "Palestine") %>%
              mutate_cond(iso=="SYR", region = "Syria") %>%
              mutate_cond(iso=="VEN", region = "Venezuela")
  ) %>%
  # new, calculated here
  bind_rows(kosovo.future %>%
              mutate(region=NA_character_) %>%
              #' follow SSP population names
              mutate_cond(iso==KOSOVO.ISO3.CODE, region = kosovo.region.name)) %>%
  # keep only certain years
  filter(
    year %in% GDP.YEARS.TO.KEEP
  )

## SAVE FINALISED GDP DATA ----
write_delim(
  x = new.oecd.gdp.ppp,
  file = here("output", new.version, "data", "GDP_PPP_longformat.csv"),
  delim = ","
)



## Visualise ----

#### growth rates ----
p.growth <- ggplot(kosovo.growth.rate.countries.GDPPPP.normalised,
                   mapping=aes(x=year,y=value,colour=iso,
                               group=interaction(model,scenario,iso,variable))) +
  facet_grid(~scenario) +
  geom_line(linewidth=1, linetype="dashed") +
  geom_line(data=kosovo.growth.rate.countries.GDPPPP.normalised.average,
            linewidth=1.2) +
  theme_jsk() + mark_history(sy=2025) +
  labs(
    y = kosovo.growth.rate.countries.GDPPPP.normalised %>% pull(unit) %>% unique(),
    title = kosovo.growth.rate.countries.GDPPPP.normalised %>% pull(variable) %>% unique()
  )
p.growth

save_ggplot(
  p = p.growth,
  f = here("output", new.version, "figures", "indexed_growth_comparison_kosovo"),
  w = 150,
  h = 150,
  format = "pdf"
)

