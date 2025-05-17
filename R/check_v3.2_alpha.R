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

new.version <- "draft_release_v3_2_alpha"

out.path.figures <- here("output", new.version, "figures")
dir.create(out.path.figures, recursive = T)
out.path.data <- here("output", new.version, "data")
dir.create(out.path.data, recursive = T)

# File ----

##  draft-release_3..alpha (full) ----

ssp3.2.file <- "ssp_basic_drivers_release_3.2.alpha_full.xlsx"

# Load ----

# Draft release (3.2.alpha) ----
pop <- load_excel_iamc(here("data", "final_before_ScenExp_upload", ssp3.2.file)) %>%
  filter(!grepl(x=Variable,pattern="GDP",fixed=T)) %>%
  iamc_wide_to_long(upper.to.lower = T)

gdp <- load_excel_iamc(here("data", "final_before_ScenExp_upload", ssp3.2.file)) %>%
  filter(grepl(x=Variable,pattern="GDP",fixed=T)) %>%
  iamc_wide_to_long(upper.to.lower = T)


# Issues Dominik Paprotny ----

## Check: 2025 same population ----

pop2025 <- pop %>% filter(year==2025,variable=="Population") %>% pivot_wider(names_from = scenario) %>%
  rowwise() |>
  mutate(
    all_equal = max(c_across(`Historical Reference`:`SSP5`)) - min(c_across(`Historical Reference`:`SSP5`)) <= 1e-6,
    all_ssps_equal = max(c_across(`SSP1`:`SSP5`)) - min(c_across(`SSP1`:`SSP5`)) <= 1e-6,
    hist_ssp2_equal = max(`Historical Reference`,`SSP2`) - min(`Historical Reference`,`SSP2`) <= 1e-6,
    max_diff = max(c_across(`Historical Reference`:`SSP5`)) - min(c_across(`Historical Reference`:`SSP5`))
  ) %>%
  ungroup()
pop2025 %>% group_by(all_equal) %>% count()
View(pop2025)
#' I can replicate Dominik's found issue.
#' example country: Russian Federation


### Follow-up: check same GDP per capita ----
gdp.pc.2025 <- gdp %>% filter(year==2025,variable=="GDP|PPP [per capita]") %>% pivot_wider(names_from = scenario) %>%
  rowwise() |>
  mutate(
    all_equal = max(c_across(`Historical Reference`:`SSP5`)) - min(c_across(`Historical Reference`:`SSP5`)) <= 1e-6,
    all_ssps_equal = max(c_across(`SSP1`:`SSP5`)) - min(c_across(`SSP1`:`SSP5`)) <= 1e-6,
    hist_ssp2_equal = max(`Historical Reference`,`SSP2`) - min(`Historical Reference`,`SSP2`) <= 1e-6,
    max_diff = max(c_across(`Historical Reference`:`SSP5`)) - min(c_across(`Historical Reference`:`SSP5`))
  ) %>%
  ungroup()
View(gdp.pc.2025)
#' Somehow, all GDP/cap for SSPs are the same, but they are not the same as historical reference (where does this come from?)
#' example country: Guam

### Follow-up: check same GDP ----
gdp2025 <- gdp %>% filter(year==2025,variable=="GDP|PPP") %>%
  arrange(scenario) %>%
  pivot_wider(names_from = scenario) %>%
  rowwise() |>
  mutate(
    all_equal = max(c_across(`Historical Reference`:`SSP5`)) - min(c_across(`Historical Reference`:`SSP5`)) <= 1e-6,
    all_ssps_equal = max(c_across(`SSP1`:`SSP5`)) - min(c_across(`SSP1`:`SSP5`)) <= 1e-6,
    hist_ssp2_equal = max(`Historical Reference`,`SSP2`) - min(`Historical Reference`,`SSP2`) <= 1e-6,
    max_diff = max(c_across(`Historical Reference`:`SSP5`)) - min(c_across(`Historical Reference`:`SSP5`))
  ) %>%
  ungroup()
View(gdp2025)
#' Issues between SSPs:
#' example country: Cuba

## Save files showing issues ----
write_delim(
  file = file.path(out.path.data, "check2025_pop.csv"),
  x=pop2025,
  delim=","
)
write_delim(
  file = file.path(out.path.data, "check2025_gdppercap.csv"),
  x=gdp.pc.2025,
  delim=","
)
write_delim(
  file = file.path(out.path.data, "check2025_gdp.csv"),
  x=gdp2025,
  delim=","
)
