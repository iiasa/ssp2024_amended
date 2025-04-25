variables.to.investigate <- c("GDP|PPP [per capita]", "GDP|PPP", "Population")

# Load ----
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
    scenario != "Historical Reference", # exclude historical reference data
    model != "IIASA GDP 2023" # don't look at non-marker data
  ) %>%
  filter(variable %in% variables.to.investigate)

# Check for missing data ----
regions.with.missing.data <- v3_1 %>%
  select(-unit,-model) %>%
  group_by(scenario,region,year) %>%
  count() %>%
  filter(n<length(variables.to.investigate)) %>%
  ungroup() %>%
  pull(region) %>%
  unique()

# Check for duplicate data ----
regions.with.duplicate.data <- v3_1 %>%
  select(-unit,-model) %>%
  group_by(scenario,region,year) %>%
  count() %>%
  filter(n>length(variables.to.investigate)) %>%
  ungroup() %>%
  pull(region) %>%
  unique()



# Check that GDP|PPP [per capita] == GDP|PPP / Population ----
acceptable_population_difference <- 1e3 # total people
acceptable_population_difference_percentage <- 0.1 # percentage difference

v3_1_wide <- v3_1 %>%
  filter(region%nin%regions.with.missing.data,
         region%nin%regions.with.duplicate.data) %>%
  select(-unit,-model) %>%
  pivot_wider(names_from = variable,
              values_from = value) %>%
  mutate(
    diff.gdp_per_capita = `GDP|PPP [per capita]` - (`GDP|PPP` / `Population` * 1e3),
    implied.population = `GDP|PPP` / `GDP|PPP [per capita]` * 1e3
  ) %>%
  mutate(diff.population = implied.population - `Population`,
         diff.population.perc = (implied.population - `Population`)/`Population` * 100 )
big.differences <- v3_1_wide %>%
  filter(abs(diff.population)>acceptable_population_difference/1e6,
         abs(diff.population.perc)>acceptable_population_difference_percentage)

write_delim(
  x = big.differences,
  file = here("output", "V3_1", "data", "internal_inconsistencies_longformat.csv"),
  delim = ","
)
