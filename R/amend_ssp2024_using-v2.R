# ssp2024_amended: Use SSP v3 data and amend it where data for some countries is missing.
#' Produced by Jarmo Kikstra
#'
#' Latest update: 14.08.2024
#' - initial version
#'
#'


# TODO list --------------------------------------------------------------------
#' - [x] read in data v3
#' - [x] identify & report missing data
#' - [x] switch from v3.0.1 to v3.1 data
#' - [x] save out how much of the global population is missing (in 2025 and each timestep, in % and totals?)
#' - [ ] fill in missing data
#'     - [x] method: using v2 trajectories after converting units using GDPuc, and IIASA v3 trajectories otherwise (if price data is not available)
#'     - [x] method: using v3 IIASA trajectories
#'     - [ ] method: using v2 relationships (to another country or other countries)
#'            - [ ] explore performance using SSP v2?
#'     - [ ] method: tbd.
#'

# Load libraries ---------------------------------------------------------------
library("here")
library("tidyverse")
library("vroom")
library("readxl")
library("countrycode")
# install.packages("GDPuc")
library(GDPuc)

here::i_am("ssp2024_amended.Rproj")

source(here("R","utils.R"))


# Versioning -------------------------------------------------------------------
# Previous versions:
# version.demands <- "20240813_v1" # only shared with Bas on Teams, on 14.08.2024 only missing data analysis

# Current version
version.demands <- "20240814_v1" # newest version; updated to SSPv3.1

output.folder <- here("output",version.demands)
dir.create(output.folder)




# Loading in data --------------------------------------------------------------

### SSPv3 data -----------------------------------------------------------------

##### Public, original ---------------------------------------------------------
# N.B. need to force the read_excel function to read all year_columns as numeric
year.columns <- c("1950", "1955", "1960", "1965", "1970", "1975", "1980", "1985",
                  "1990", "1995", "2000", "2005", "2010", "2015", "2020", "2025",
                  "2030", "2035", "2040", "2045", "2050", "2055", "2060", "2065",
                  "2070", "2075", "2080", "2085", "2090", "2095", "2100")

sspv3 <- read_excel(
  here("data", "1721734326790-ssp_basic_drivers_release_3.1_full.xlsx"),
  sheet = "data",
  col_types = c(rep("text", 5), rep("numeric", length(year_columns)))
) %>% iamc_wide_to_long(upper.to.lower = T) %>%
  filter(
    !grepl(region, pattern="(R5)", fixed=T),
    !grepl(region, pattern="(R9)", fixed=T),
    !grepl(region, pattern="(R10)", fixed=T),
    region!="World"
  ) %>%
  mutate_cond(region=="Micronesia", region="Micronesia (Federated States of)") %>% # countrycode package doesn't recognise "Micronesia" by itself
  mutate(iso=countrycode(region, origin = "country.name", destination = "iso3c")) %>%
  drop_na(iso)
sspv3.population.marker <- sspv3 %>% filter(variable=="Population")

##### Oliver, MESSAGE-internal -------------------------------------------------
# from: https://iiasahub.sharepoint.com/:x:/r/sites/eceprog/Shared%20Documents/SharedSocioEconomicPathways2023/WP%20-%20Drivers/SSP_drivers_v3/SSP_drivers_v3_2024-04-12.csv?d=w04381c54fe6e48059c0687e1522d596c&csf=1&web=1&e=D4DGTh
# - has multiple different USD units
sspv3_oliver <- vroom(
  here("data", "SSP_drivers_v3_2024-04-12(in).csv")
) %>% rename(scenario=SSP, region=ISO, variable=Variable, unit=Unit) %>% mutate(model="Marker model") %>% select(all_of(c("model","scenario","region","variable","unit")),
                                                                                                                 everything()) %>%
  iamc_wide_to_long()


### SSPv2 (original) data ------------------------------------------------------
sspv2 <- vroom(
  here("data", "SspDb_country_data_2013-06-12.csv")
) %>% allcaps_to_lower() %>% iamc_wide_to_long()
sspv2.population.marker <- sspv2 %>% filter(variable=="Population")
sspv2.GDP.marker <- sspv2 %>% filter(variable=="GDP|PPP", model=="OECD Env-Growth")
sspv2.GDPpc.marker <- sspv2.GDP.marker %>% left_join(sspv2.population.marker %>% select(-c(model,variable,unit)) %>% rename(population.million = value)) %>%
  mutate(
    variable="GDP|PPP [per capita]",
    unit = "USD_2005/yr",
    value = value*1e9/(population.million*1e6)
  ) %>% select(-population.million)
sspv2.GDP.GDPpc.marker <- sspv2.GDP.marker %>% bind_rows(sspv2.GDPpc.marker)


# Check missing countries in SSPv3 (specific versions/variables) ---------------

### List all countries (with _some_ data) --------------------------------------
all.countries.iso.v3 <- sspv3 %>%
  pull(iso) %>% unique() %>% sort()
all.countries.iso.v2 <- sspv2 %>%
  region_unique()

yesv2.nov3 <- all.countries.iso.v2[all.countries.iso.v2 %nin% all.countries.iso.v3] # all countries that were in v2 are also present in v3
yesv3.nov2 <- all.countries.iso.v3[all.countries.iso.v3 %nin% all.countries.iso.v2] # new countries in v3: ATG (Antigua and Barbuda), CUW (Curaçao), ESH (Western Sahara), KIR (Kiribati), SSD (South Sudan), SYC (Seychelles)

### Create overview of data missing in important variables ---------------------
sspv3 %>% variable_unique()


##### availability (ALL variables) ---------------------------------------------
sspv3.availability.allvariables <- sspv3 %>% #filter(variable %in% basic.variables) %>%
  mutate(model.variable = paste0(model, "|", variable)) %>%
  distinct(model.variable,scenario,iso,region) %>%
  mutate(present.in.data="yes") %>%
  pivot_wider(names_from = model.variable, values_from = present.in.data, values_fill = NA_character_)

write_delim(
  x = sspv3.availability.allvariables,
  file = file.path(output.folder, "country_availability_ALLVARIABLES_sspv31.csv"),
  delim = ","
)


##### availability (basic variables) -------------------------------------------
basic.variables <- c(
  "GDP|PPP",
  "Population"
)
sspv3.availability <- sspv3 %>% filter(variable %in% basic.variables) %>%
  mutate(model.variable = paste0(model, "|", variable)) %>%
  distinct(model.variable,scenario,iso,region) %>%
  mutate(present.in.data="yes") %>%
  pivot_wider(names_from = model.variable, values_from = present.in.data, values_fill = NA_character_)

write_delim(
  x = sspv3.availability,
  file = file.path(output.folder, "country_availability_sspv31.csv"),
  delim = ","
)

##### missing only (basic variables) -------------------------------------------
list.of.model.variable.combinations <- sspv3 %>% filter(variable %in% basic.variables) %>%
  mutate(model.variable = paste0(model, "|", variable)) %>% pull(model.variable) %>% unique()
sspv3.missing <- sspv3.availability %>% filter(
  if_any(all_of(list.of.model.variable.combinations), is.na)
)
write_delim(
  x = sspv3.missing,
  file = file.path(output.folder, "country_missing_sspv31.csv"),
  delim = ","
)

##### missing GDP of countries, in terms of population -------------------------
# tbd



# Create amended SSPv3 (only GDP) ----------------------------------------------

### Replace with (a) SSPv2 GDP PPP per capita (b) of own model, (c) multiplied by new population ----
#' Notes & Issues:
#' * Issues:
#' - [ ] does not correct for the (important!) difference in units yet. Could perhaps be fixed using GDPuc? https://cran.r-project.org/web/packages/GDPuc/index.html
#' - [ ] has no solution for countries not in v3 and also not in v2;
#'          - for OECD, this is the case for four small island states, namely: CUW (Curacao), GLP (Guadeloupe), MTQ (Martinique), REU (Reunion)
#'
#' Notes:
#' TODO:
#' - [ ] For my current workflow, I'll then make two versions:
#'          * OECDv3 -> OECDv2 (+unit conversion [WB data not available for SYR and VEN]) -> IIASAv3
#'          * OECDv3 -> IIASAv3


##### OECD ---------------------------------------------------------------------
sspv3.oecd.missingcountries <- sspv3.missing %>% filter(is.na(`OECD ENV-Growth 2023|GDP|PPP`)) %>% pull(iso) %>% unique() %>% sort()
# missing countries are: "AFG" (Afghanistan) "CUW" (Curacao) "GLP" (Guadeloupe) "MTQ" (Martinique) "PSE" (Palestine) "REU" (Reunion) "SYR" (Syria) "VEN" (Venezuela)
sspv3.oecd <- sspv3 %>% filter(model=="OECD ENV-Growth 2023",
                               # scenario!="Historical Reference"
                               )

# missing countries as share of population
missing.population.oecd.sspv3 <- sspv3 %>% filter(variable=="Population") %>%
  mutate(in.oecd.sspv3 = "Yes") %>%
  mutate_cond(iso%in%sspv3.oecd.missingcountries, in.oecd.sspv3 = "No") %>%
  reframe(
    population = sum(value),
    .by = c("model", "scenario", "year", "in.oecd.sspv3")
  ) %>% pivot_wider(names_from = in.oecd.sspv3, values_from = population) %>%
  mutate(percentage.missing = `No`/(`Yes`+`No`) * 100)
p.missing.percentage <- ggplot(missing.population.oecd.sspv3, aes(x=year, y=percentage.missing, group=interaction(model,scenario), colour=scenario)) +
  geom_line()
save_ggplot(
  p = p.missing.percentage,
  f = file.path(output.folder, "OECD_SSPv3_GDPmissing_populationshare")
)

# sspv2 OECD -- obtain missing countries and bring into the same unit and format
sspv2.oecd.forv3 <- sspv2.GDP.GDPpc.marker %>% filter(region%in%sspv3.oecd.missingcountries)
## calculate values in the same unit
sspv2.oecd.forv3.withv3unit <- sspv2.oecd.forv3 %>% rename(iso3c=region) %>%
  convertGDP(unit_in = "constant 2005 Int$PPP", unit_out = "constant 2017 Int$PPP") %>%
  mutate_cond(unit == "billion US$2005/yr", unit = "billion USD_2017/yr") %>%
  mutate_cond(unit == "USD_2005/yr", unit = "USD_2017/yr") %>%
  drop_na()
## bring into same format
sspv2.oecd.forv3.withv3unit.sameformat <- sspv2.oecd.forv3.withv3unit %>%
  rename(
    iso = iso3c
  ) %>%
  mutate_cond(grepl(scenario, pattern = "SSP1", fixed=T), scenario="SSP1") %>%
  mutate_cond(grepl(scenario, pattern = "SSP2", fixed=T), scenario="SSP2") %>%
  mutate_cond(grepl(scenario, pattern = "SSP3", fixed=T), scenario="SSP3") %>%
  mutate_cond(grepl(scenario, pattern = "SSP4", fixed=T), scenario="SSP4") %>%
  mutate_cond(grepl(scenario, pattern = "SSP5", fixed=T), scenario="SSP5") %>%
  mutate(region.name=countrycode(sourcevar=iso, origin="iso3c", destination="country.name"))
cat(paste0(
  (sspv2.oecd.forv3.withv3unit.sameformat %>% pull(region.name) %>% unique() %>% length()),
  " out of ",
  length(sspv3.oecd.missingcountries),
  " countries can be filled with SSPv2 data.\n    Countries: ",
  paste(sspv2.oecd.forv3.withv3unit.sameformat %>% pull(region.name) %>% unique(), collapse = ", ")
))

# sspv3 IIASA -- obtain missing countries and bring into the same unit and format
sspv3.iiasa.forv3 <- sspv3 %>% filter(iso%in%sspv3.oecd.missingcountries,
                                     model=="IIASA GDP 2023") %>% drop_na()
cat(paste0(
  (sspv3.iiasa.forv3 %>% region_unique() %>% length()),
  " out of ",
  length(sspv3.oecd.missingcountries),
  " countries can be filled with SSPv2 data.\n    Countries: ",
  paste(sspv3.iiasa.forv3 %>% region_unique(), collapse = ", ")
))


## calculate values in the same unit
# already in same unit
## bring into same format
# already in same format
## calculate per capita values in the same unit
sspv3.iiasa.forv3.percapita <- sspv3.iiasa.forv3 %>% left_join(
  sspv3.population.marker %>% select(scenario,iso,year,value) %>% rename(population.million = value)
) %>%
  mutate(
    variable="GDP|PPP [per capita]",
    unit = "USD_2017/yr",
    value = value*1e9/(population.million*1e6)
  ) %>% select(-population.million)

sspv3.iiasa.forv3.GDP.GDPpc <- sspv3.iiasa.forv3 %>% bind_rows(sspv3.iiasa.forv3.percapita)


## all relevant data: bind_rows, pivot
sspv3.future.marker.data <- sspv3.oecd %>%
  bind_rows(sspv2.oecd.forv3.withv3unit.sameformat) %>%
  bind_rows(sspv3.iiasa.forv3.GDP.GDPpc) %>%
  select(model,scenario,iso,variable,unit,year,value) %>%
  pivot_wider(names_from = model, values_from = value, values_fill = NA_real_)


## OECD versions first
sspv3.amended.oecdfirst <- sspv3.future.marker.data %>%
  mutate(value=`OECD ENV-Growth 2023`) %>% # first choice marker
  mutate_cond((
    (
    is.na(value)|value==0
    )&(
    !((is.na(`OECD Env-Growth`))|(`OECD Env-Growth`==0))
    )
  ),
              value = `OECD Env-Growth`) %>% # add OECD SSPv2
  mutate_cond((
    (
      is.na(value)|value==0
    )&(
      !((is.na(`IIASA GDP 2023`))|(`IIASA GDP 2023`==0))
    )
  ),
              value = `IIASA GDP 2023`) %>%  # add IIASA SSPv3
  # reformat
  mutate(model = "GDP marker (amended)") %>%
  select(model,scenario,iso,variable,unit,year,value) %>%
  rename(Model = model, Scenario = scenario, Region = iso, Variable = variable, Unit = unit) %>%
  pivot_wider(values_from = value, names_from = year)

write_delim(
  x = sspv3.amended.oecdfirst,
  file = file.path(output.folder, "gdp_sspv31_markeramended_oecdfirst.csv"),
  delim = ","
)


## v3 version first
sspv3.amended.v3only <- sspv3.future.marker.data %>%
  mutate(value=`OECD ENV-Growth 2023`) %>% # first choice marker
  mutate_cond((
    (
      is.na(value)|value==0
    )&(
      !((is.na(`IIASA GDP 2023`))|(`IIASA GDP 2023`==0))
    )
  ),
  value = `IIASA GDP 2023`) %>%  # add IIASA SSPv3
  # reformat
  mutate(model = "GDP marker (amended)") %>%
  select(model,scenario,iso,variable,unit,year,value) %>%
  rename(Model = model, Scenario = scenario, Region = iso, Variable = variable, Unit = unit) %>%
  pivot_wider(values_from = value, names_from = year)


write_delim(
  x = sspv3.amended.v3only,
  file = file.path(output.folder, "gdp_sspv31_markeramended_v3only.csv"),
  delim = ","
)


##### IIASA
# not yet implemented.













