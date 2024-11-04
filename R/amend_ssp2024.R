# ssp2024_amended: Use SSP v3 data and amend it where data for some countries is missing.
#' Produced by Jarmo Kikstra
#'
#' Latest update: 04.11.2024
#' - add regional growth rates
#' Latest update: 08.10.2024
#' - add versions based on a country's R10 share
#' Latest update: 14.08.2024
#' - initial version
#'
#'

# Load libraries ---------------------------------------------------------------
library("here")
library("tidyverse")
library("vroom")
library("readxl")
library("countrycode")
library("ggthemes")
# install.packages("GDPuc")
library(GDPuc)

here::i_am("ssp2024_amended.Rproj")

source(here("R","utils.R"))


# Versioning -------------------------------------------------------------------
# Previous versions:
# version.demands <- "20240813_v1" # only shared with Bas on Teams, on 14.08.2024 only missing data analysis
# version.demands <- "20240814_v1" # newest version; updated to SSPv3.1
# version.demands <- "20241008_v2" # add R10 shares methods

# Current version
version.demands <- "20241104_v3" # add R10 growth rates method

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
  drop_na(iso)
sspv3.population.marker <- sspv3 %>% filter(variable=="Population")


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


### R10 grouping -----------------------------------------------------------------
r10 <- load_official_country_grouping(grouping.to.load = "region_ar6_10_ipcc_fgd") %>%  # note: could check consistency with https://github.com/IAMconsortium/common-definitions/blob/main/definitions/region/common.yaml
  rename(region_r10=region_ar6_10_ipcc_fgd)


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


# Create amended SSPv3 (fill missing countries in OECD GDP) --------------------

### What percentage of the world population is missing? ------------------------

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

### Filling in based on SSPv2-OECD only (using unit conversions) ---------------

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

### Filling in based on SSPv3-IIASA only ---------------------------------------

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


### Filling in: R10 shares IIASA -----------------------------------------------
##### calculate shares in R10 of IIASAv3 ---------------------------------------
gdp.iiasav3 <- sspv3 %>% filter(model=="IIASA GDP 2023") %>% drop_na()
r10.gdp.iiasav3 <- gdp.iiasav3 %>%
  left_join(r10) %>%
  reframe(
    value_r10 = sum(value),
    .by = c("model", "scenario", "region_r10", "variable", "unit", "year")
  )
share.of.r10.each.missing.country.iiasav3 <- gdp.iiasav3 %>%
  left_join(r10) %>%
  left_join(r10.gdp.iiasav3) %>%
  mutate(country.missing = ifelse(iso%in%sspv3.oecd.missingcountries,
                                  "yes",
                                  "no")) %>%
  mutate(share_r10 = value/value_r10) %>%
  filter(country.missing == "yes") %>%
  select(-value,
         -value_r10,
         -model) # don't use iiasa value directly

##### calculate R10 OECDv3 (which misses few countries) ------------------------
gdp.oecdv3 <- sspv3 %>% filter(model=="OECD ENV-Growth 2023") %>% drop_na()
r10.gdp.oecdv3 <- gdp.oecdv3 %>%
  left_join(r10) %>%
  reframe(
    value_r10 = sum(value),
    .by = c("model", "scenario", "region_r10", "variable", "unit", "year")
  )

##### calculate OECDv3 values based on IIASA R10 shares ------------------------
sspv3.iiasar10shares.forv3 <- share.of.r10.each.missing.country.iiasav3 %>%
  left_join(r10.gdp.oecdv3) %>%
  mutate(
    value = (value_r10/(1-share_r10))-value_r10
  ) %>%
  select(model,scenario,region,variable,unit,year,iso,value)

##### add/calculate GDP per capita ---------------------------------------------
sspv3.iiasar10shares.forv3.percapita <- sspv3.iiasar10shares.forv3 %>% left_join(
  sspv3.population.marker %>% select(scenario,iso,year,value) %>% rename(population.million = value)
) %>%
  mutate(
    variable="GDP|PPP [per capita]",
    unit = "USD_2017/yr",
    value = value*1e9/(population.million*1e6)
  ) %>% select(-population.million)

sspv3.iiasar10shares.forv3.percapita.GDP.GDPpc <- sspv3.iiasar10shares.forv3 %>% bind_rows(sspv3.iiasar10shares.forv3.percapita)


### Filling in: R10 shares OECD SSPv2 ------------------------------------------
##### calculate shares in R10 of OECDv2 ---------------------------------------
gdp.oecdv2 <- sspv2 %>% filter(model=="OECD Env-Growth") %>% drop_na() %>%
  rename(iso=region) %>%
  mutate_cond(grepl(scenario, pattern = "SSP1", fixed=T), scenario="SSP1") %>%
  mutate_cond(grepl(scenario, pattern = "SSP2", fixed=T), scenario="SSP2") %>%
  mutate_cond(grepl(scenario, pattern = "SSP3", fixed=T), scenario="SSP3") %>%
  mutate_cond(grepl(scenario, pattern = "SSP4", fixed=T), scenario="SSP4") %>%
  mutate_cond(grepl(scenario, pattern = "SSP5", fixed=T), scenario="SSP5")
r10.gdp.oecdv2 <- gdp.oecdv2 %>%
  left_join(r10) %>%
  reframe(
    value_r10 = sum(value),
    .by = c("model", "scenario", "region_r10", "variable", "unit", "year")
  )
share.of.r10.each.missing.country.oecdv2 <- gdp.oecdv2 %>%
  left_join(r10) %>%
  left_join(r10.gdp.oecdv2) %>%
  mutate(country.missing = ifelse(iso%in%sspv3.oecd.missingcountries,
                                  "yes",
                                  "no")) %>%
  mutate(share_r10 = value/value_r10) %>%
  filter(country.missing == "yes") %>%
  select(-value,
         -value_r10,
         -unit,
         -model) # don't use oecd SSPv2 value directly

##### calculate R10 OECDv3 (which misses few countries) ------------------------
gdp.oecdv3 <- sspv3 %>% filter(model=="OECD ENV-Growth 2023") %>% drop_na()
r10.gdp.oecdv3 <- gdp.oecdv3 %>%
  left_join(r10) %>%
  reframe(
    value_r10 = sum(value),
    .by = c("model", "scenario", "region_r10", "variable", "unit", "year")
  )

##### calculate OECDv3 values based on IIASA R10 shares ------------------------
sspv2.oecdr10shares.forv3 <- share.of.r10.each.missing.country.oecdv2 %>%
  left_join(r10.gdp.oecdv3) %>%
  mutate(
    value = (value_r10/(1-share_r10))-value_r10
  ) %>%
  mutate(region=countrycode(iso, origin = "iso3c", destination = "country.name")) %>%
  select(model,scenario,region,variable,unit,year,iso,value) %>%
  drop_na() # e.g. value 2015 and earlier will be NA

##### add/calculate GDP per capita ---------------------------------------------
sspv2.oecdr10shares.forv3.percapita <- sspv2.oecdr10shares.forv3 %>% left_join(
  sspv3.population.marker %>% select(scenario,iso,year,value) %>% rename(population.million = value)
) %>%
  mutate(
    variable="GDP|PPP [per capita]",
    unit = "USD_2017/yr",
    value = value*1e9/(population.million*1e6)
  ) %>% select(-population.million)

sspv2.oecdr10shares.forv3.percapita.GDP.GDPpc <- sspv2.oecdr10shares.forv3 %>% bind_rows(sspv2.oecdr10shares.forv3.percapita)




### R10 growth rates-based -----------------------------------------------------
# sspv3.iiasa.starting.year <- sspv3.iiasa.forv3 %>% filter(year==2025)
# sspv3.r10.oecd.growthrates <- r10.gdp.oecdv3 %>%
#   arrange(model,scenario,region_r10,variable,unit,
#           year) %>%
#   group_by(model,scenario,region_r10,variable,unit) %>%
#   mutate(growth.rate = (value_r10 / lag(value_r10))^(1/(year-lag(year))) - 1 )
sspv3.r10.oecd.normalised <- r10.gdp.oecdv3 %>%
  rename(region=region_r10, value=value_r10) %>%
  normalise_iamc_long(starting.year = 2025) %>%
  rename(region_r10=region, normalised.value_r10=value)

sspv3.iiasa.forv3.2025 <- sspv3.iiasa.forv3 %>% filter(year==2025) %>%
  rename(value2025=value)
sspv3.iiasa.forv3amended.R10growthrates <- sspv3.iiasa.forv3 %>%
  mutate_cond(year!=2025, value=NA) %>%
  mutate(model="OECD ENV-Growth 2023") %>%
  left_join(
    r10
  ) %>%
  # calculate new growth-rate based values (growth.rate)
  # left_join(
  #   sspv3.r10.oecd.growthrates
  # ) %>%
  # group_by(model,scenario,region,variable,unit) %>%
  # mutate(
  #   # year!=2025,
  #   value.recon = lag(value) * (1+growth.rate)^(year - lag(year))
  # ) %>%
  # calculate new value based on normalised value
  left_join(
    sspv3.r10.oecd.normalised
  ) %>%
  left_join(
    sspv3.iiasa.forv3.2025 %>% select(-model,-year)
  ) %>%
  mutate_cond(
    year!=2025,
    value = value2025 * normalised.value_r10
  )

## calculate values in the same unit
# already in same unit
## bring into same format
# already in same format
## calculate per capita values in the same unit
sspv3.iiasa.forv3amended.R10growthrates.percapita <- sspv3.iiasa.forv3amended.R10growthrates %>% left_join(
  sspv3.population.marker %>% select(scenario,iso,year,value) %>% rename(population.million = value)
) %>%
  mutate(
    variable="GDP|PPP [per capita]",
    unit = "USD_2017/yr",
    value = value*1e9/(population.million*1e6)
  ) %>% select(-population.million)

sspv3.iiasa.forv3amended.R10growthrates.GDP.GDPpc <- sspv3.iiasa.forv3amended.R10growthrates %>% bind_rows(sspv3.iiasa.forv3amended.R10growthrates.percapita)



### COMBINE versions -----------------------------------------------------------

## all relevant data: bind_rows, pivot
sspv3.future.marker.data.long <- sspv3.oecd %>% mutate(data.version="OECD") %>%
  bind_rows(sspv2.oecd.forv3.withv3unit.sameformat %>% mutate(data.version="OECD old (unit conversion)")) %>%
  bind_rows(sspv3.iiasa.forv3.GDP.GDPpc %>% mutate(data.version="IIASA")) %>%
  bind_rows(sspv2.oecdr10shares.forv3.percapita.GDP.GDPpc %>% mutate(data.version="Using old OECD R10 shares")) %>%
  bind_rows(sspv3.iiasar10shares.forv3.percapita.GDP.GDPpc %>% mutate(data.version="Using IIASA R10 shares")) %>%
  bind_rows(sspv3.iiasa.forv3amended.R10growthrates.GDP.GDPpc %>% mutate(data.version="Using OECD R10 growth rates + IIASA 2025")) %>%
  select(-model,-region.name,
         scenario,iso,variable,unit,year,value,data.version)
sspv3.future.marker.data <- sspv3.future.marker.data.long %>%
  pivot_wider(names_from = data.version, values_from = value, values_fill = NA_real_)



##### visualise ----------------------------------------------------------------

p.gdp <- ggplot(
  sspv3.future.marker.data.long %>% filter(iso%in%sspv3.oecd.missingcountries) %>%
    filter(variable=="GDP|PPP [per capita]") %>%
    filter(data.version!="OECD old (unit conversion)"),
  aes(x=year,y=value,
      colour=scenario,
      linetype=data.version,
      group=interaction(scenario,iso,data.version))
) +
  facet_grid(scenario~iso, scales="free_y") +
  geom_line(linewidth=0.4) +
  geom_point(aes(shape=data.version))+
  ylab("USD_2017/yr") + xlab(NULL) +
  labs(title = "GDP|PPP [per capita]",
       subtitle = "Different infilling options") +
  theme_jsk() +
  scale_color_ptol() +
  guides(linetype = guide_legend(ncol = 2),
         shape = guide_legend(ncol = 2))
  mark_history(sy=2025)

p.gdp

save_ggplot(
  p = p.gdp,
  f = file.path(output.folder, "gdp_sspv31_infillingoptions"),
  w = 350,
  h = 200
)

##### save long data -----------------------------------------------------------
write_delim(
  x = sspv3.future.marker.data.long,
  file = file.path(output.folder, "gdp_sspv31_withallextradata.csv"),
  delim = ","
)




# ### SELECT and SAVE priorities -------------------------------------------------
# ## OECD versions first
# sspv3.amended.oecdfirst <- sspv3.future.marker.data %>%
#   mutate(value=`OECD ENV-Growth 2023`) %>% # first choice marker
#   mutate_cond((
#     (
#     is.na(value)|value==0
#     )&(
#     !((is.na(`OECD Env-Growth`))|(`OECD Env-Growth`==0))
#     )
#   ),
#               value = `OECD Env-Growth`) %>% # add OECD SSPv2
#   mutate_cond((
#     (
#       is.na(value)|value==0
#     )&(
#       !((is.na(`IIASA GDP 2023`))|(`IIASA GDP 2023`==0))
#     )
#   ),
#               value = `IIASA GDP 2023`) %>%  # add IIASA SSPv3
#   # reformat
#   mutate(model = "GDP marker (amended)") %>%
#   select(model,scenario,iso,variable,unit,year,value) %>%
#   rename(Model = model, Scenario = scenario, Region = iso, Variable = variable, Unit = unit) %>%
#   pivot_wider(values_from = value, names_from = year)
#
# write_delim(
#   x = sspv3.amended.oecdfirst,
#   file = file.path(output.folder, "gdp_sspv31_markeramended_oecdfirst.csv"),
#   delim = ","
# )
#
#
# ## v3 version first
# sspv3.amended.v3only <- sspv3.future.marker.data %>%
#   mutate(value=`OECD ENV-Growth 2023`) %>% # first choice marker
#   mutate_cond((
#     (
#       is.na(value)|value==0
#     )&(
#       !((is.na(`IIASA GDP 2023`))|(`IIASA GDP 2023`==0))
#     )
#   ),
#   value = `IIASA GDP 2023`) %>%  # add IIASA SSPv3
#   # reformat
#   mutate(model = "GDP marker (amended)") %>%
#   select(model,scenario,iso,variable,unit,year,value) %>%
#   rename(Model = model, Scenario = scenario, Region = iso, Variable = variable, Unit = unit) %>%
#   pivot_wider(values_from = value, names_from = year)
#
#
# write_delim(
#   x = sspv3.amended.v3only,
#   file = file.path(output.folder, "gdp_sspv31_markeramended_v3only.csv"),
#   delim = ","
# )




