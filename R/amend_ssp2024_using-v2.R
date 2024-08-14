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
#' - [ ] save out how much of the global population is missing (in 2025 and each timestep, in % and totals?)
#' - [ ] fill in missing data
#'     - [ ] method: using v2 paths directly
#'     - [ ] method: using v2 relationships (to another country or other countries)
#'            - [ ] explore performance using SSP v2?
#'     - [ ] method: tbd.
#'
#'

# Load libraries ---------------------------------------------------------------
library("here")
library("tidyverse")
library("vroom")
library("readxl")
library("countrycode")

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
sspv3 <- read_excel(
  here("data", "1721734326790-ssp_basic_drivers_release_3.1_full.xlsx"),
  sheet = "data"
) %>% iamc_wide_to_long(upper.to.lower = T) %>%
  mutate_cond(region=="Micronesia", region="Micronesia (Federated States of)") %>% # countrycode package doesn't recognise "Micronesia" by itself
  mutate(iso=countrycode(region, origin = "country.name", destination = "iso3c")) %>%
  drop_na(iso)

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
                               scenario!="Historical Reference")
sspv2.oecd.forv3 <- sspv2 %>% filter(region%in%sspv3.oecd.missingcountries,
                                     model=="OECD Env-Growth")
print(paste0((sspv2.oecd.forv3 %>% region_unique() %>% length()), " out of ", length(sspv3.oecd.missingcountries), " countries can be filled with SSPv2 data"))

install.packages("GDPuc")
library(GDPuc)
sspv2.oecd.forv3.withv3unit <- sspv2.oecd.forv3 %>% rename(iso3c=region) %>%
  convertGDP(unit_in = "constant 2005 Int$PPP", unit_out = "constant 2017 Int$PPP")


##### IIASA
# not yet implemented.













