#' Useful functions for the `decent` software package


# Package management -----------------------------------------------------------
#' # Create updated lock files for renv
#' options(renv.config.auto.snapshot = FALSE) # to prevent the issue as described in https://github.com/iiasa/decent/pull/133#issuecomment-1632287450
#' #' Install and load required packages
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' install_and_load_reqs <- function(){
#'   if(!require(here)) {
#'     install.packages("here");
#'     library(here)
#'   }
#'   # load the script which has the packages that our scripts depend on
#'   source(here("R", "dev", "renv_dependencies.R"))  # `selection 3` (3: Cancel, and resolve the situation on your own.)
#' }
#' install_and_load_reqs()
#' ENVIRONMENT <<- "dev" # default environment is dev (for development)
#'
#' # default set is the basis of the other sets
#' if (ENVIRONMENT == "default") {
#'   renv::restore(lockfile = here("renv.lock"), prompt = FALSE) # prompt=FALSE: to not have to wait to write "Y" in the console
#' } else if (ENVIRONMENT == "test"){
#'   renv::restore(lockfile = here("renv-test.lock"), prompt = FALSE) # prompt=FALSE: to not have to wait to write "Y" in the console
#'   pkgs <- c(test.pkgs, pkgs)
#' } else if (ENVIRONMENT == "plot") {
#'   renv::restore(lockfile = here("renv-plot.lock"), prompt = FALSE) # prompt=FALSE: to not have to wait to write "Y" in the console
#'   pkgs <- c(plot.pkgs, pkgs)
#' } else if (ENVIRONMENT == "dev"){
#'   renv::restore(lockfile = here("renv-dev.lock"), prompt = FALSE) # prompt=FALSE: to not have to wait to write "Y" in the console
#'   pkgs <- c(pkgs, plot.pkgs, test.pkgs, dev.pkgs)
#' }
#'
#' # Load needed libraries
#' invisible(lapply(pkgs, library, character.only = TRUE))

select <- dplyr::select # explicitly say that we mean dplyr's select function whenever we use select (not the one from the MASS library...)
filter <- dplyr::filter # explicitly say that we mean dplyr's filter function whenever we use filter (not the one from the stats library...)
mutate <- dplyr::mutate # explicitly say that we mean dplyr's mutate function whenever we use mutate
map <- purrr::map # explicitly say that we mean purrr's map function whenever we use map (conflicting with maps::map)



# Decent Living Standards and DLE utils ----------------------------------------

### Combined Decent Living Standards and DLE -----------------------------------

##### Load and initialize DLS dimensions ---------------------------------------

#' Load and initialize DLS dimensions
#' - sources DLE_*.R code
#'     * the integration structure code (DLE_integration_data_structure.R)
#'     * all dimensions (e.g., DLE_transport.R and DLE_appliances.R)
#' - used especially in dle_core.R, but also in collect-*.R scripts
#'
#' @return
#' @export
#'
#' @examples
#' load_dimensions()
load_dimensions <- function() {
  source(here("R", "DLE_integration_data_structure.R")) # integration structure for dimensions

  source(here("R", "DLE_clothing.R"))
  source(here("R", "DLE_nutrition.R"))
  source(here("R", "DLE_health.R"))
  source(here("R", "DLE_water.R"))
  source(here("R", "DLE_sanitation.R"))
  source(here("R", "DLE_roads.R"))
  source(here("R", "DLE_housing.R"))
  source(here("R", "DLE_cooling_con.R"))
  source(here("R", "DLE_cooling_op.R"))
  source(here("R", "DLE_heating_con.R"))
  source(here("R", "DLE_heating_op.R"))
  source(here("R", "DLE_hotwater_op.R"))
  source(here("R", "DLE_appliances.R"))
  source(here("R", "DLE_education.R"))
  source(here("R", "DLE_transport.R"))
}

#' Generate DLE dimensions using DLE.dimension.* functions
#' - requires the DLE.dimension.* functions to be loaded with R/DLE_integration_data_structure.R
#' - used especially in collect-*.R scripts
#'
#' @return list of DLE dimensions
#' @export
#'
#' @examples
#' dim.list <<- generate_all_dimensions()
#' dims <<- generate_all_dimensions()
generate_all_dimensions <- function() {
  # NB. currently should follow exactly the function 'AggregateDimensions()'!!
  return(
    list(
      transport = DLE.dimension.transport(
        name_dim = "Transport",
        indicator = "pkm/cap/year",
        unit_rollout = "cap",
        grp = c("car", "bus", "rail", "twothree")
      ),
      appliances = DLE.dimension.appliances(
        name_dim = "Appliance",
        indicator = "unit/hh", # percentage household that have a certain appliance
        unit_rollout = "hh", # all of these are considered on a household level
        grp = c("clean_cooking_fuel", "television", "mobile_telephone", "refrigerator")
      ),
      water = DLE.dimension.water(
        name_dim = "Water",
        indicator = "m3/cap/year",
        unit_rollout = "cap",
        grp = "water"
      ),
      sanit = DLE.dimension.sanit(
        name_dim = "Sanitation",
        indicator = "Share of population requiring safely managed sanitation services.",
        unit_rollout = "cap",
        grp = "sanitation"
      ),
      nutrition = DLE.dimension.nutrition(
        name_dim = "Nutrition",
        unit_rollout = "cap",
        indicator = "kcal/cap/day",
        grp = "nutrition"
      ),
      clothing = DLE.dimension.clothing(
        name_dim = "Clothing",
        indicator = "kg/cap/year",
        unit_rollout = "cap",
        grp = c("clothing", "footwear")
      ),
      health = DLE.dimension.health(
        name_dim = "Health",
        indicator = "$/cap/year",
        unit_rollout = "cap",
        grp = "health"
      ),
      education = DLE.dimension.education(
        name_dim = "Education",
        indicator = "$/cap/year",
        unit_rollout = "cap",
        grp = c("primary", "lower_secondary")
      ),
      housing = DLE.dimension.housing(
        name_dim = "Housing",
        indicator = "m2/cap",
        unit_rollout = "cap",
        grp = c("rural", "urban")
      ),
      heating_con = DLE.dimension.heating_con(
        name_dim = "Heating CON",
        indicator = "Share of population requiring heating.",
        unit_rollout = "cap",
        grp = c("rural", "urban")
      ),
      cooling_con = DLE.dimension.cooling_con(
        name_dim = "Cooling CON",
        indicator = "Share of population requiring cooling.",
        unit_rollout = "cap",
        grp = c("rural", "urban")
      ),
      heating_op = DLE.dimension.heating_op(
        name_dim = "Heating OP",
        indicator = "m2/cap",
        unit_rollout = "cap",
        grp = c("rural", "urban")
      ),
      cooling_op = DLE.dimension.cooling_op(
        name_dim = "Cooling OP",
        unit_rollout = "cap",
        indicator = "m2/cap",
        grp = c("rural", "urban")
      ),
      roads = DLE.dimension.roads(
        name_dim = "Roads",
        indicator = "km/cap", ## Note: one of the threshold methods (road density km/km2) does not have same unit as indicator (km/gap)
        unit_rollout = "abs",
        grp = "roads"
      ),
      hotwater_op = DLE.dimension.hotwater_op(
        name_dim = "Hot Water OP",
        indicator = "Share of population requiring hot water services.", # for the gap analysis calculation, this dimension may need some improvement
        unit_rollout = "cap",
        grp = c("rural", "urban")
        # df.input = data.frame(expenditure=3) # Some custom inputs to the dimension (placeholder)
      )
    )
  )
}


##### Input data ---------------------------------------------------------------
#' Load some general input data for the DLE_* module and beyond
#' Loads the following variables into global environment:
#' - MESSAGE region mappings:
#'     * message.R11.all (from mapping file)
#'     * message.R11 (crossed with population file for overlap)
#' - Population data (and aggregates)
#'     * pop
#'     * R11.pop.baseyr
#'     * R11.pop
#'     * R11.pop.urbrur
#'     * G.pop
#' - Urbanization data:
#'     * urbanization
#' - Household size data:
#'     * hh_size
#' - USD to EURO conversion rate at baseyear:
#'     * usd2eur.baseyr
#' - GDP data:
#'     * gdp
#'
#'
#' @param ssp
#' @param pov.thres
#' @param year.base
#'
#' @return
#' @export
#'
#' @examples
run_initiatialization_input_data <- function(ssp = "SSP2", pov.thres = 20, year.base = 2015) {
  # MESSAGE R11 mapping to country iso
  message.R11.all <<- vroom(here("data-raw", "iam_regions", "region_definitions_message.csv"), show_col_types=FALSE) %>%
    select(iso, country, region.message) %>%
    rename(R11.region = `region.message`,
           country_name = country)

  # placeholder population - TODO: import SSP population projections and replace this
  # pop <- population %>%
  #   filter(year == 2013) %>% select(-year) %>%  # why is this for 2013? The latest year in this R-provided 'population' df
  #   mutate(iso = countrycode(country, 'country.name', 'iso3c'))

  # Population and Urbanization rate
  # For now, focus on SSP2
  pop <<- read_excel(paste0(data.path, "/iamc_db_SSP_population.xlsx")) %>%
    filter(Model == "IIASA-WiC POP", Scenario == ssp) %>%
    select(-Variable, -Model, -Scenario) %>%
    pivot_longer(cols = `2010.0`:`2100.0`, names_to = "year", values_to = "population") %>%
    mutate(year = as.numeric(year), population = population * mega) %>%
    select(-Notes, -Unit) %>%
    rename(iso = Region)

  # Filter countries based on available pop data
  message.R11 <<- message.R11.all %>% filter(iso %in% unique(pop$iso))

  # Urbanization
  urbanization <<- read_excel(paste0(data.path, "/iamc_db_SSP_urbanshare.xlsx")) %>%
    filter(Scenario == ssp) %>%
    select(-Variable, -Model, -Scenario) %>%
    pivot_longer(cols = `2010.0`:`2100.0`, names_to = "year", values_to = "urb.rate") %>%
    mutate(year = as.numeric(year), urb.rate = urb.rate / 100) %>%
    select(-Notes, -Unit) %>%
    rename(iso = Region)

  # household size data - constant over time
  hh_size <<- read.csv(paste0(data.path, "/hh_size.csv"), stringsAsFactors = FALSE)
  hh_size <<- message.R11 %>%
    left_join(hh_size) %>% # fill NAs
    group_by(R11.region) %>%
    mutate(hh_size_avg = mean(na.omit(hh_size))) %>%
    ungroup() %>%
    mutate(hh_size = ifelse(is.na(hh_size), hh_size_avg, hh_size)) %>%
    select(-c(country_name, R11.region, hh_size_avg))

  # Merge three demographic DFs above
  pop <<- message.R11 %>%
    left_join(pop) %>%
    left_join(urbanization) %>%
    left_join(hh_size) %>%
    mutate(n_hh = round(population / hh_size)) %>%
    mutate(population.urb = population * urb.rate, population.rur = population * (1 - urb.rate)) %>%
    select(-c(country_name, R11.region))

  # Exchange rate (for EXIO accounting)
  usd2eur.baseyr <<- WDI(country = "XC", indicator = "PA.NUS.FCRF", start = year.base, end = year.base)$PA.NUS.FCRF

  # poverty headcount closing rate
  pov.pop.var <<- paste0("pop_", pov.thres) # Name of the thres variable in the csv input file
  pov.gap <<- read.csv(paste0(data.path, "/gdpginipop_povcountdata.csv")) %>%
    rename(iso = country) %>%
    filter(scenario == ssp, year >= year.base) %>%
    select(scenario, iso, year, {{ pov.pop.var }}) %>%
    group_by(iso) %>%
    left_join(pop %>% select(iso, year, population)) %>%
    mutate(pov.pcap = get(pov.pop.var) / population) %>%
    mutate(r.closing = pov.pcap / first(pov.pcap)) %>% # gap closing rate index per capita (year.base = 1)
    mutate(r.diff = r.closing - lead(r.closing, default = tail(r.closing, 1))) %>% # % difference at each time period
    mutate(r.diff = pmax(0, r.diff))

  # gdp pathway for GDP-driven (income) scenario pathways
  gdp <<- read_csv(here::here("data-raw", "DLE_scaleup", "Paper_data_final", "input", "gdp_gini_pop_ssp.csv")) %>%
    mutate(gdp.pcap = gdp_ppp_2005_bil / pop_mil * 1000) %>%
    rename(iso = country) %>%
    filter(scenario == ssp) %>%
    select(iso, year, gdp.pcap) %>%
    drop_na()

  # some potentially useful population aggregates
  # regional population in base year
  R11.pop.baseyr <<- message.R11 %>%
    left_join(pop %>% filter(year == year.base)) %>%
    select(R11.region, population) %>%
    group_by(R11.region) %>%
    summarise(population = sum(population))
  # regional population
  R11.pop <<- message.R11 %>%
    left_join(pop) %>%
    select(iso, year, R11.region, population) %>%
    group_by(R11.region, year) %>%
    summarise(population = sum(population))
  R11.pop.urbrur <<- message.R11 %>%
    left_join(pop) %>%
    select(iso, year, R11.region, population, population.urb, population.rur) %>%
    group_by(R11.region, year) %>%
    summarise(population = sum(population), population.urb = sum(population.urb), population.rur = sum(population.rur))
  # # visualise regional population
  # ggplot(R11.pop, aes(x=year,y=population)) + geom_line(size=2) + facet_wrap(~R11.region)
  # ggplot(R11.pop.urbrur%>% pivot_longer(c(population.urb,population.rur), names_to="population.urbrur"), aes(x=year,y=value, colour=population.urbrur)) + geom_line(size=2) + facet_wrap(~R11.region)
  # global population
  G.pop <<- R11.pop %>%
    group_by(year) %>%
    summarise(population = sum(population))
}

### Decent Living Standards utils ----------------------------------------------

##### Dimension interdependencies ----------------------------------------------
#' Use gap of another dimension to derive the gap of the current dimension
#' - cooling_con and heating_con gaps are dependent on housing gap
#' - hotwater_op gap is dependent on water and heating_con gaps
#'
#' @param dims
#'
#' @return
#' @export
#'
#' @examples
#' setup_threshold_and_gap_helper(dims=dims)
setup_threshold_and_gap_helper <- function(dims) {
  # The scenario-specific rollout can be taken care of in each dimension.
  lapply(dims, function(x) {
    x$DeriveThreshold()
  })

  lapply(
    names(dims),
    function(x) {
      if (x %in% c("cooling_con", "heating_con")) {
        # Based on the assumption that 'housing' is generated earlier.
        dims[[x]]$IdentifyGap(DF.DLS.housing = dims$housing$DF.DLS)
      } else if (x == "hotwater_op") {
        dims[[x]]$IdentifyGap(gap_water.df = dims$water$DF.DLS, gap_heating.df = dims$heating_con$DF.DLS)
      } else {
        dims[[x]]$IdentifyGap()
      }
    }
  )
}


##### Price data (esp. EXIOBASE) -----------------------------------------------
#' Calculates price index change
#' - requires connection to World Development Indicators (WDI) API
#'
#' SHOULD BE DEPRECATED, BETTER TO USE GDP unit-converter package, such as:
#' https://pik-piam.github.io/GDPuc/index.html by Johannes Koch.
#' See issue #63
#'
#' options:
#' - d.fromto
#'   * ppp2mer
#'   * mer2ppp
#' - year.from
#' - year.to
#'
#' @return df.price.conv: data frame with price conversion factors
#' @export
#'
#' @examples
calculate_price_index_change <- function() {

  # yet to be implemented;
  # for at minimum DLE_clothing, DLE_education, DLE_health.
  DLE.DB.yr <- 2010 # Standardized year for DLE DB
  df.ppp.r <- WDI(country = "all", indicator = "PA.NUS.PRVT.PP", start = DLE.DB.yr, end = DLE.DB.yr, extra = FALSE) %>%
    rename(ppp.r = PA.NUS.PRVT.PP)
  df.ex.r <- WDI(country = "all", indicator = "PA.NUS.FCRF", start = DLE.DB.yr, end = DLE.DB.yr, extra = FALSE) %>%
    rename(ex.r = PA.NUS.FCRF)
  df.cpi <- WDI(country = "all", indicator = "FP.CPI.TOTL", start = year.base, end = year.base, extra = FALSE) %>%
    rename(cpi = FP.CPI.TOTL) # (2010=100)

  # Need to convert PPP price in 2010 to our base year USD MER to be consistent with the intensity
  df.price.conv <- df.ppp.r %>%
    left_join(df.ex.r) %>%
    mutate(ex.r = ifelse(is.na(ex.r), df.ex.r$ex.r[df.ex.r$iso2c == "XC"], ex.r)) %>%
    left_join(df.cpi, by = c("iso2c", "iso3c")) %>%
    # filter(iso3c%in%tei.EXIO$iso) %>% # alternative (leads to 9.4% higher df.tei.global.total): do conversion only for countries with EXIO data, to avoid artefacts such as for VNM.
    # left_join(message.R11 %>% select(iso,R11.region) %>% rename(iso3c=iso)) %>% group_by(R11.region) %>% # alternative: do conversion only for countries with EXIO data, to avoid artefacts such as for VNM.
    # mutate(ppp.r = ifelse(is.na(ppp.r), mean(ppp.r), ppp.r),
    #        cpi = ifelse(is.na(cpi), mean(cpi), cpi),
    #        ex.r = ifelse(is.na(ex.r), mean(ex.r), ex.r)) %>% # alternative: do conversion only for countries with EXIO data, to avoid artefacts such as for VNM.
    filter(!is.na(ppp.r)) %>%
    mutate(ppp2mer.baseyr = ppp.r / ex.r) %>% # PPP USD to MER USD for the base year
    # filter(!(iso2c %in% c("ZM", "RW", "PR", "TM", "TC", "PS"))) %>% # Remove non-EU countries without ex.r info
    mutate(conv = ppp2mer.baseyr * cpi / 100) %>% # this 'conv' converts 2010 USD PPP to 2015 USD MER (year.base)
    filter(!is.na(ex.r)) %>%
    select(iso3c, conv) %>%
    rename(iso = iso3c)

  return(df.price.conv)
}

##### Housing related utils ----------------------------------------------------
#' Extract housing rollout df (including stock data) from a DLS scenario object
#'
#' @param dims
#'
#' @return dims$housing$DF.rollout
#' @export
#'
#' @examples
#' dims <<- generate_all_dimensions()
#' stock <- extract_housing_data(dims = dims)
extract_housing_data <- function(dims = dims){
  # check if housing rollout exists (scenario object has been created). if not, create data for housing stock for base year only.
  rollout.base <- dims$housing$DF.rollout.base
  df.dls <- dims$housing$DF.DLS
  if (all(is.na(dims$housing$DF.rollout$rollout))) {
    r.rep <- dims$housing$r.rep
    df <- left_join(rollout.base, df.dls, by = c("iso", "grp"))
    rollout_housing.df <- df %>%
      mutate(
        rollout.pcap = as.numeric(thres - gap),
        rollout.pcap = ifelse(type == "CON.new" & year == as.character(year.base), 0, rollout.pcap),
        rollout.pcap = ifelse(type == "CON.rep" & year == as.character(year.base), r.rep * (thres - gap), rollout.pcap)
      ) %>%
      mutate(type_copy = type) %>%
      spread(type, "rollout.pcap") %>%
      group_by(iso, grp) %>%
      mutate(
        stock_new_pcap = r.rep * thres,
        stock_old_pcap = pmax(0, thres - gap - stock_new_pcap)
      ) %>%
      rename(type = type_copy) %>%
      return(rollout_housing.df)
  } else {
    return(dims$housing$DF.rollout)
  }
}


##### Transport related utils --------------------------------------------------
#' Load gini coefficient data required for calculating transport pkm gaps
#' - loads Gini data and imputes where necessary
#' - uses population weighted gini of MESSAGE R11 region to impute missing gini values
#'
#' @param fname_gini
#'
#' @return economic_data:
#' @export
#'
#' @examples
#' pkm.data.gap.base <<- transport_pkm_dod_helper(
#'     thresholds_df = DF.DLS %>% select(iso,grp,thres), # load and process gini data
#'     pkm_data = transport_pkm_helper(fname_pkm = fname_pkm,
#'                                 data.version=data.version,
#'                                 totals.or.shares = "totals"), # load and process pkm by mode data
#'     gini_data = economic_data_helper(fname_gini = fname_gini),
#'     transport_cutoff = transport_cutoff
#'     )
economic_data_helper <- function(fname_gini) {

  ## Load data:
  if (fname_gini=="API_SI.POV.GINI_DS2_en_csv_v2_1217501_withIndiaAustraliaJapanUSA.csv"){

    # imputation strategy:
    # - if 2015 available, go for it
    # - if not, impute for 2015 based on data of 2010-2018
    # (below: if still not, apply mean of R11)

    gini <- read_csv(file.path(data.path, fname_gini), skip = 4, col_names = TRUE) %>%
      rename(iso = `Country Code`) %>%
      gather(year, index, `2010`:`2018`) %>%
      select(c(iso, year, index)) %>% # pivot to long format while only keeping recent year values
      group_by(iso) %>%
      arrange(iso, year) %>%
      mutate(gini = na.approx(index, maxgap = Inf, na.rm = FALSE) / 100) %>%
      filter(year == as.character(year.base)) %>%
      select(-year)
    # 104 countries with data
  }
  if (fname_gini=="wiid_gini.csv"){
    # TODO / NOTE: currently not an ideal way to have the location inside this if-statement [if it is updated in the future]

    # imputation strategy:
    # - if 2015 available, go for it
    # - if not, take mean of 2010-2020 (whatever data is available)
    # (below: if still not, apply mean of R11)

    gini <- vroom(here("data-raw","dls_data","gini","wiid","downloaded_20230803","wiid_gini.csv"), show_col_types=FALSE) %>%
      filter(year>=2010, year<=2020) %>% # data anyway goes only until 2020 in this WIID version
      group_by(iso) %>%
      summarise(mean.gini=mean(gini)) %>% # for 178 countries

      # add counties with actual 2015 numbers (73)
      left_join(
        vroom(here("data-raw","dls_data","gini","wiid","downloaded_20230803","wiid_gini.csv"), show_col_types=FALSE) %>%
          filter(year==year.base) %>% select(-year,-population),
        by = "iso"
      ) %>%

      mutate(gini=ifelse(is.na(gini),mean.gini,gini)) %>%

      select(iso,gini)

    # 178 countries with data
  }
  # # compare gini datasets:
  # data <-
  #   read_csv(file.path(data.path, fname_gini), skip = 4, col_names = TRUE) %>%
  #   rename(iso = `Country Code`) %>%
  #   gather(year, index, `2010`:`2018`) %>%
  #   select(c(iso, year, index)) %>% # pivot to long format while only keeping recent year values
  #   group_by(iso) %>%
  #   arrange(iso, year) %>%
  #   mutate(gini = na.approx(index, maxgap = Inf, na.rm = FALSE) / 100) %>%
  #   filter(year == as.character(year.base)) %>%
  #   select(-year) %>%
  #   rename(gini.wb2019=gini) %>%
  #   # wiid
  #   left_join(
  #     vroom(here("data-raw","dls_data","gini","wiid","downloaded_20230803","wiid_gini.csv")) %>%
  #       filter(year>=2010, year<=2020) %>% # data anyway goes only until 2020 in this WIID version
  #       group_by(iso) %>%
  #       summarise(mean.gini=mean(gini)) %>% # for 178 countries
  #
  #       # add counties with actual 2015 numbers (73)
  #       left_join(
  #         vroom(here("data-raw","dls_data","gini","wiid","downloaded_20230803","wiid_gini.csv")) %>%
  #           filter(year==year.base) %>% select(-year)
  #       ) %>%
  #
  #       mutate(gini=ifelse(is.na(gini),mean.gini,gini)) %>% select(-mean.gini) %>%
  #       rename(gini.wiid2023=gini)
  #   )
  # # ... number of people covered extra:
  # data %>% left_join(select(subset(pop, year == year.base), iso, population)) %>%
  #   filter(is.na(gini.wb2019),!is.na(gini.wiid2023)) %>% pull(population) %>% sum(na.rm = T) / 1e6 -
  # data %>% left_join(select(subset(pop, year == year.base), iso, population)) %>%
  #   filter(!is.na(gini.wb2019),is.na(gini.wiid2023)) %>% pull(population) %>% sum(na.rm = T) / 1e6
  #
  # # ... correlation:
  # ggplot(
  #   data = data %>% left_join(select(subset(pop, year == year.base), iso, population)),
  #   aes(x=gini.wb2019,y=gini.wiid2023/100, size=population)
  # ) +
  #   geom_abline(intercept = 0, slope = 1) +
  #   geom_point()
  # # ... diff in outcome: probably best measured in the DoD/deprivation.headcount outcomes


  ## current by-pass: calculate average gini per region (to fill gaps) for DoD gap analysis -> as pkm is only available at a regional level, applied per country.
  # read in countries and message regions (message.R11)
  # keep only those in message.R11
  economic_data <- message.R11 %>%
    select(iso, R11.region) %>%
    left_join(select(gini, iso, gini), by = c("iso"))
  # ... remove all that have na in gini or population
  # ... do groupby
  # ... do average
  # ... left_join back to df
  economic_data_weighted_gini <- economic_data %>%
    left_join(select(subset(pop, year == year.base), iso, population), by = "iso") %>%
    drop_na(c("gini", "population")) %>%
    group_by(R11.region) %>%
    mutate(R11.gini = weighted.mean(na.omit(gini), population)) %>%
    select(R11.gini, R11.region) %>%
    distinct()
  economic_data <- economic_data %>%
    left_join(economic_data_weighted_gini, by = c("R11.region")) %>%
    # apply average per region where countries did not have a gini already available
    mutate(gini=ifelse(is.na(gini),R11.gini,gini))
  economic_data <- economic_data %>%
    select(iso, gini)

  return(economic_data)

}

#' Passenger-kilometre (pkm) data: load and format chosen source for transport mode shares and total pkm
#' Options (data.version):
#' - IMAGE2023
#' - IMAGE2023-all (includes aviation, and cycling and walking -- not used in the analysis)
#' - ERL2021
#'
#' @param fname_pkm
#' @param data.version
#' @param totals.or.shares
#'
#' @return pkm_data: long data frame with pkm per capita data by country and mode
#' @export
#'
#' @examples
#' data_pkm_shares <- transport_pkm_helper(fname_pkm = fname_pkm, data.version="IMAGE2023", totals.or.shares = "shares") %>%
#'  pivot_wider(names_from = grp, values_from = pkm) %>%
#'  rename(ldv = car)
transport_pkm_helper <- function(fname_pkm,
                                 data.version,
                                 totals.or.shares="shares") {

  ## Load data:
  print("Load data: transport")
  if(data.version=="IMAGE2023"){
    if (fname_pkm=="ssp2_npi_20230825-2daysago.xlsx"){

      # p-km totals
      # ... Regions IMAGE
      data_pkm_bymode_image <- read_excel(here("data-raw", "dls_data", "transport_pkm", "image", "shape",
                                               fname_pkm),
                                          sheet = ifelse(totals.or.shares=="totals",
                                                         "pkm_dls",
                                                         ifelse(totals.or.shares=="shares",
                                                                "pkm_modeshares",
                                                                NA
                                                         )),
                                          col_names = TRUE) %>%
        select(region.image, ldv, twothree, bus, rail) %>%
        rename(car = ldv)

      # ... onto ISO3
      transport_pkm <- regions.image %>%
        left_join(data_pkm_bymode_image, by = c("region.image")) %>% # 223 countries
        select(iso, car, twothree, bus, rail) %>%
        filter(iso%in%c(message.R11 %>% pull(iso) %>% unique())) # 191 countries (1 country (PSE) in MESSAGE that is not in IMAGE)

      transport_pkm <- transport_pkm %>% drop_na() # drop countries that have any NA (NB.: this needs to be revised if some regions/countries have partial estimates)

      # add missing countries by averaging
      transport_pkm_missing_countries <- message.R11 %>% select(iso, R11.region) %>% filter(!(iso%in%c(transport_pkm %>% pull(iso) %>% unique())))
      transport_pkm_missing_countries.regional.average.data <- transport_pkm %>%
        left_join(message.R11 %>% select(iso,R11.region)) %>%
        filter(R11.region%in%c(transport_pkm_missing_countries %>% pull(R11.region) %>% unique())) %>%
        reframe(car=mean(car),
                twothree=mean(twothree),
                bus=mean(bus),
                rail=mean(rail),
                .by = c(R11.region))
      transport_pkm_missing_countries.data <- transport_pkm_missing_countries %>%
        left_join(transport_pkm_missing_countries.regional.average.data, by=c("R11.region")) %>%
        select(-R11.region)

      pkm_data <- transport_pkm %>%
        bind_rows(transport_pkm_missing_countries.data)

      # bring to long format
      pkm_data <- pkm_data %>%
        gather(grp, pkm, car, twothree, bus, rail)
    } else if (fname_pkm=="image_shape_ssp_20230825_addedtwothree_replacedCHN.xlsx"){

      # p-km totals
      # ... Regions IMAGE
      data_pkm_bymode_image <- read_excel(here("data-raw", "dls_data", "transport_pkm", "image", "shape",
                                               fname_pkm),
                                          sheet = ifelse(totals.or.shares=="totals",
                                                         "pkm_dls_feb2024",
                                                         ifelse(totals.or.shares=="shares",
                                                                "pkm_modeshares_feb2024",
                                                                NA
                                                         )),
                                          col_names = TRUE) %>%
        select(region.image, ldv, twothree, bus, rail) %>%
        rename(car = ldv)

      # ... onto ISO3
      transport_pkm <- regions.image %>%
        left_join(data_pkm_bymode_image, by = c("region.image")) %>% # 223 countries
        select(iso, car, twothree, bus, rail) %>%
        filter(iso%in%c(message.R11 %>% pull(iso) %>% unique())) # 191 countries (1 country (PSE) in MESSAGE that is not in IMAGE)

      transport_pkm <- transport_pkm %>% drop_na() # drop countries that have any NA (NB.: this needs to be revised if some regions/countries have partial estimates)

      # add missing countries by averaging
      transport_pkm_missing_countries <- message.R11 %>% select(iso, R11.region) %>% filter(!(iso%in%c(transport_pkm %>% pull(iso) %>% unique())))
      transport_pkm_missing_countries.regional.average.data <- transport_pkm %>%
        left_join(message.R11 %>% select(iso,R11.region)) %>%
        filter(R11.region%in%c(transport_pkm_missing_countries %>% pull(R11.region) %>% unique())) %>%
        reframe(car=mean(car),
                twothree=mean(twothree),
                bus=mean(bus),
                rail=mean(rail),
                .by = c(R11.region))
      transport_pkm_missing_countries.data <- transport_pkm_missing_countries %>%
        left_join(transport_pkm_missing_countries.regional.average.data, by=c("R11.region")) %>%
        select(-R11.region)

      pkm_data <- transport_pkm %>%
        bind_rows(transport_pkm_missing_countries.data)

      # bring to long format
      pkm_data <- pkm_data %>%
        gather(grp, pkm, car, twothree, bus, rail)
    }
  } else if(data.version=="IMAGE2023-all"){
    if (fname_pkm=="ssp2_npi_20230825-2daysago.xlsx"){

      # p-km totals
      # ... Regions IMAGE
      data_pkm_bymode_image <- read_excel(here("data-raw", "dls_data", "transport_pkm", "image", "shape",
                                               fname_pkm),
                                          sheet = ifelse(totals.or.shares=="totals",
                                                         "pkm_allmodes", # NOTE: cycling and 2/3 wheeler are duplicates of each other!!
                                                         ifelse(totals.or.shares=="shares",
                                                                "pkm_allmodes_modeshares",
                                                                NA
                                                         )),
                                          col_names = TRUE) %>%
        select(region.image, ldv, twothree, bus, rail, walking, cycling, aviation) %>%
        rename(car = ldv)

      # ... onto ISO3
      transport_pkm <- regions.image %>%
        left_join(data_pkm_bymode_image, by = c("region.image")) %>% # 223 countries
        select(iso, car, twothree, bus, rail, walking, cycling, aviation) %>%
        filter(iso%in%c(message.R11 %>% pull(iso) %>% unique())) # 191 countries (1 country (PSE) in MESSAGE that is not in IMAGE)

      transport_pkm <- transport_pkm %>% drop_na() # drop countries that have any NA (NB.: this needs to be revised if some regions/countries have partial estimates)

      # add missing countries by averaging
      transport_pkm_missing_countries <- message.R11 %>% select(iso, R11.region) %>% filter(!(iso%in%c(transport_pkm %>% pull(iso) %>% unique())))
      transport_pkm_missing_countries.regional.average.data <- transport_pkm %>%
        left_join(message.R11 %>% select(iso,R11.region)) %>%
        filter(R11.region%in%c(transport_pkm_missing_countries %>% pull(R11.region) %>% unique())) %>%
        reframe(car=mean(car),
                twothree=mean(twothree),
                bus=mean(bus),
                rail=mean(rail),
                .by = c(R11.region))
      transport_pkm_missing_countries.data <- transport_pkm_missing_countries %>%
        left_join(transport_pkm_missing_countries.regional.average.data, by=c("R11.region")) %>%
        select(-R11.region)

      pkm_data <- transport_pkm %>%
        bind_rows(transport_pkm_missing_countries.data)

      # bring to long format
      pkm_data <- pkm_data %>%
        gather(grp, pkm, car, twothree, bus, rail, walking, cycling, aviation)
    } else if (fname_pkm=="image_shape_ssp_20230825_addedtwothree_replacedCHN.xlsx"){

      # p-km totals
      # ... Regions IMAGE
      data_pkm_bymode_image <- read_excel(here("data-raw", "dls_data", "transport_pkm", "image", "shape",
                                               fname_pkm),
                                          sheet = ifelse(totals.or.shares=="totals",
                                                         "pkm_allmodes_feb2024", # NOTE: cycling and 2/3 wheeler are duplicates of each other!!
                                                         ifelse(totals.or.shares=="shares",
                                                                "pkm_allmodes_shares_feb2024",
                                                                NA
                                                         )),
                                          col_names = TRUE) %>%
        select(region.image, ldv, twothree, bus, rail, walking, cycling, aviation) %>%
        rename(car = ldv)

      # ... onto ISO3
      transport_pkm <- regions.image %>%
        left_join(data_pkm_bymode_image, by = c("region.image")) %>% # 223 countries
        select(iso, car, twothree, bus, rail, walking, cycling, aviation) %>%
        filter(iso%in%c(message.R11 %>% pull(iso) %>% unique())) # 191 countries (1 country (PSE) in MESSAGE that is not in IMAGE)

      transport_pkm <- transport_pkm %>% drop_na() # drop countries that have any NA (NB.: this needs to be revised if some regions/countries have partial estimates)

      # add missing countries by averaging
      transport_pkm_missing_countries <- message.R11 %>% select(iso, R11.region) %>% filter(!(iso%in%c(transport_pkm %>% pull(iso) %>% unique())))
      transport_pkm_missing_countries.regional.average.data <- transport_pkm %>%
        left_join(message.R11 %>% select(iso,R11.region)) %>%
        filter(R11.region%in%c(transport_pkm_missing_countries %>% pull(R11.region) %>% unique())) %>%
        reframe(car=mean(car),
                twothree=mean(twothree),
                bus=mean(bus),
                rail=mean(rail),
                .by = c(R11.region))
      transport_pkm_missing_countries.data <- transport_pkm_missing_countries %>%
        left_join(transport_pkm_missing_countries.regional.average.data, by=c("R11.region")) %>%
        select(-R11.region)

      pkm_data <- transport_pkm %>%
        bind_rows(transport_pkm_missing_countries.data)

      # bring to long format
      pkm_data <- pkm_data %>%
        gather(grp, pkm, car, twothree, bus, rail, walking, cycling, aviation)
    }
  } else if (data.version=="ERL2021"){
    if (fname_pkm=="R11_pkm.xlsx"){

      # p-km totals
      # ... R11
      transport_pkm_R11 <- read_excel(file.path(data.path, "Transport", fname_pkm), sheet = "Data", col_names = TRUE)
      # ... onto ISO3
      transport_pkm <- message.R11 %>% select(iso, R11.region)
      transport_pkm <- left_join(transport_pkm, transport_pkm_R11, by = c("R11.region")) %>%
        select(-c(ldv, twothree, bus, rail, R11.region))

      if (totals.or.shares=="totals"){
        pkm_data <- message.R11 %>%
          select(iso, R11.region) %>%
          left_join(transport_pkm_R11 %>%
                      mutate(
                        ldv = ldv * pkm_pc,
                        twothree = twothree * pkm_pc,
                        bus = bus * pkm_pc,
                        rail = rail * pkm_pc
                      ) %>% select(-pkm_pc), by = c("R11.region")) %>%
          select(c(iso, ldv, twothree, bus, rail)) %>%
          rename(car = ldv) %>%
          gather(grp, pkm, car, twothree, bus, rail)
      } else if (totals.or.shares=="shares"){
        # p-km mode shares
        # ... R11
        data_pkm_shares_R11 <- read_excel(file.path(data.path, "Transport", fname_pkm), sheet = "Data", col_names = TRUE)
        # ... onto ISO3
        data_pkm_shares <- message.R11 %>%
          select(iso, R11.region) %>%
          left_join(data_pkm_shares_R11, by = c("R11.region")) %>%
          select(c(iso, ldv, twothree, bus, rail))

        # bring to long format
        pkm_data <- data_pkm_shares %>%
          select(c(iso, ldv, twothree, bus, rail)) %>%
          rename(car = ldv) %>%
          gather(grp, pkm, car, twothree, bus, rail)
      }

    }
  }

  return(pkm_data)
}

#' Passenger-kilometre (pkm) data: calculate depth of deficit of pkm.
#'
#'
#' @param thresholds_df
#' @param pkm_data
#' @param gini_data
#' @param transport_cutoff
#'
#' @return pkm_data_out: pkm gap with depth of deficit (DoD) and deprivation headcount (share) added.
#' @export
#'
#' @examples
#' pkm.data.gap.base <<- transport_pkm_dod_helper(
#'     thresholds_df = DF.DLS %>% select(iso,grp,thres), # load and process gini data
#'     pkm_data = transport_pkm_helper(fname_pkm = fname_pkm,
#'                                 data.version=data.version,
#'                                 totals.or.shares = "totals"), # load and process pkm by mode data
#'     gini_data = economic_data_helper(fname_gini = fname_gini),
#'     transport_cutoff = transport_cutoff
#'     )
transport_pkm_dod_helper <- function(thresholds_df, pkm_data, gini_data, transport_cutoff) {
  pkm_data_DoD <- pkm_data %>%
    left_join(gini_data, by = "iso") %>%

    filter(!is.na(gini)) %>%

    mutate(sigma = sapply(GetInverseCDF_lognormal(gini), "[")) %>%
    mutate(nu = log(pkm) - (sigma^2) / 2) %>%

    left_join(thresholds_df) %>%

    mutate(
      dod = sapply(GetDepthofDeficit_lognormal(nu, sigma, thres, "DoD"), "[", 1),
      deprivation.headcount = sapply(GetDepthofDeficit_lognormal(nu, sigma, thres, "share"), "[", 1)
    ) %>%
    mutate(corr_pkm_gap = dod * deprivation.headcount)

  # get final p-km gap (accounting for DoD, with or without affordability cutoff)
  if (transport_cutoff == FALSE) {
    pkm_data_out <- pkm_data_DoD
  } else if (transport_cutoff == TRUE) {
    pkm_data_out <- pkm_data_DoD %>%
      mutate(corr_pkm_gap = dod * deprivation.headcount) %>%
      # set pkm-gap to zero in the message regions where the pkm gap is just an afffordability issue.
      left_join(message.R11) %>%
      mutate(
        corr_pkm_gap = ifelse(
          R11.region %in% c("NAM", "PAO", "WEU", "EEU"), 0,
          corr_pkm_gap
        ),
        deprivation.headcount = ifelse(
          R11.region %in% c("NAM", "PAO", "WEU", "EEU"), 0,
          deprivation.headcount
        ),
        dod = ifelse(
          R11.region %in% c("NAM", "PAO", "WEU", "EEU"), 0,
          dod
        )
      )
  }

  pkm_data_out <- pkm_data_out %>% rename(gap=corr_pkm_gap)

  return(pkm_data_out)
}

##### Miscellanenous -----------------------------------------------------------

#' Collect the gap of all dimensions
#' - loads into environment Gap.alldims: DLS gap of all dimensions
#' @param dims
#'
#' @return
#' @export
#'
#' @examples
#' dims <<- generate_all_dimensions() # create DLE object
#' setup_threshold_and_gap_helper(dims=dims)
#' collect_all_gap_helper(dims=dims)
collect_all_gap_helper <- function(dims=dims) {
  # DLE <<- lapply(dims, function(x) {x$UpdateDLE()})
  Gap <- lapply(
    names(dims),
    function(x) {
      print(x)

      df.dls <- dims[[x]]$DF.DLS # class instance of this dimension

      if (names(df.dls)[1] != "iso") { # When the regional resolution is for R11
        df.gap <- message.R11 %>%
          left_join(df.dls, by = "R11.region") %>%
          select(iso, gap, grp)
      } else {
        df.gap <- df.dls %>% select(iso, gap, grp)
      }
      df.gap <- df.gap %>% mutate(dim = dims[[x]]$name_dim, indicator = dims[[x]]$indicator)
      return(df.gap)
    }
  )
  Gap.alldims <<- do.call("rbind", Gap)
}


### Decent Living Energy utils -------------------------------------------------

##### Dimension interdependencies ----------------------------------------------
#' Use energy intensity of another dimension to derive the energy intensity of the current dimension
#' - requires function extract_housing_data() to already be defined
#'
#' @param dims
#'
#' @return
#' @export
#'
#' @examples
#' call_derive_energy_intensity_helper(dims=dims)
call_derive_energy_intensity_helper <- function(dims) {
  # Determine the value of stock based on the condition
  stock <- extract_housing_data(dims = dims)

  lapply(
    names(dims),
    function(x) {
      if (x %in% c("heating_op", "cooling_op")) {
        dims[[x]]$DeriveEnergyIntensity(rollout1.df = stock)
      } else if (x == "hotwater_op") {
        dims[[x]]$DeriveEnergyIntensity(rollout_housing.df = stock)
      } else {
        dims[[x]]$DeriveEnergyIntensity()
      }
    }
  )
}


### DLE scenario runs: basic and wrapper functions -----------------------------

##### Basic DLE scenario run functions -----------------------------------------
#' Run the housing dimension of a DLE scenario separately (for all types of scenario runs)
#' - with the option to write out the housing stock data (using "SAVE.DLE.HOUSING.STOCK" global variable)
#'
#' @param scen
#'
#' @return
#' @export
#'
#' @examples
#' run_housing(scen = "ACCEL")
#' run_housing(scen = "Income")
#' run_housing(scen = "Income.regression")
run_housing <- function(scen = scen) {
  # do housing separately, ad-hoc solution

  # Initiate an inherited, specific dimension object
  DLE.housing <<- DLE.dimension.housing(
    name_dim = "Housing",
    indicator = "m2/cap",
    unit_rollout = "cap",
    grp = c("rural", "urban") # ,
    # df.input = data.frame(expenditure=3) # Some custom inputs to the dimension (placeholder)
  )

  DLE.housing$DeriveThreshold()
  DLE.housing$IdentifyGap()
  DLE.housing$DeriveEnergyIntensity()
  DLE.housing$ConstructRolloutScenario(scen)
  DLE.housing$UpdateDLE()

  if (exists("SAVE.DLE.HOUSING.STOCK")) {
    if (SAVE.DLE.HOUSING.STOCK == TRUE) {
      housing.stock.timeseries <- DLE.housing$DLE.tot %>%
        ungroup() %>%
        select(iso, grp, year, stock_new_pcap, stock_old_pcap) %>%
        distinct()

      # check that there's no duplicates or extra information not captured in the selected columns above
      if (
        !(nrow(housing.stock.timeseries) == nrow(DLE.housing$DLE.tot %>% ungroup() %>% select(iso, grp, year) %>% distinct()))
      ) {
        stop("Housing stock file that you are saving contains non-unique data. There is more information than is captured in the file.")
      } else {
        write_delim(
          x = housing.stock.timeseries,
          file = file.path(out.path, paste0(
            ssp, "_",
            as.character(year.target),
            # "_lct", as.character(lct)), # N.B.: this will lead to a slight naming inconsistency, because others will have lctTRUE/lctFALSE, but comes with the benefit of not having two files with the same information (housing stock is not affected by transport mode convergence).
            "_housingstock.csv"
          )),
          delim = ","
        )
      }
    }
  }
}

##### Types of DLE scenario runs -----------------------------------------------
#' "ACCEL" scenario run
#' This is a scenario that closes the DLS gap by a certain year (year.target).
#' There is the option to specify lct = TRUE, with lct standing for low carbon technologies, which will in effect change the energy intensity of the transport dimension through higher modal shares of public tranpsort in regions currently dominated by private transport use.
#'
#' This function is used especially in dle_core.R, whether directly to allow for investigation, or within a wrapper (run_accel_scenario_wrapper) to enable easy multiple runs.
#'
#' @param ssp
#' @param year.target
#' @param lct
#'
#' @return
#' @export
#'
#' @examples
#' run_accel_scenario(ssp, year.target, T)
run_accel_scenario <- function(ssp = "SSP2", year.target = year.target, lct = FALSE) {
  # Construct a normative target scenario (ACCEL)
  load_dimensions()

  # Build a list of dimensions
  dim.list <<- generate_all_dimensions()

  # Initiate a scenario object - thres/gap/E.int are taken care of here.
  DLE.ACCEL <<- DLE.scenario(scenario.name = "ACCEL", dims = dim.list, year.tgt.scen = year.target, lct = lct)

  # Create the gap df from all the dims
  DLE.ACCEL$CollectAllGap()

  # do housing
  run_housing(scen = "ACCEL")

  # roll out scenario
  DLE.ACCEL$SetupRollout(lct = lct)

  # get energy intensities
  DLE.ACCEL$CallDeriveEnergyIntensity(dims=dim.list)

  # do aggregates.
  DLE.ACCEL$SumDLE()
  DLE.ACCEL$AggregateRegions()
  DLE.ACCEL$AggregateDimensions()
  DLE.ACCEL$PlotDLEpcap_ByRegByDim()
  DLE.ACCEL$PlotDLEpcap_ByRegByNeed()
  DLE.ACCEL$PlotDLEpcap_GlobalByDim()

  # we use global DLE.ACCEL here instead of return because we use it as a global object throughout all steps.
}

#' "Income" scenario run
#' This is a scenario that closes the DLS gap based on a poverty closing scenario.
#' For instance, for a 5.5 USD/day/cap poverty threshold, based on relative gap closing, we close this gap.
#'
#' This function is currently not in use and its functioning is not guaranteed.
#'
#' @param ssp
#' @param pov.thres
#'
#' @return
#' @export
#'
#' @examples
run_gap_closing_proxy_scenario <- function(ssp, pov.thres) {
  # Construct an poverty income gap closing scenario (Income)

  load_dimensions()

  # Build a list of dimensions
  dim.list <<- generate_all_dimensions()

  # Initiate a scenario object - thres/gap/E.int are taken care of here.
  DLE.income <<- DLE.scenario(scenario.name = "Income", dims = dim.list, year.tgt.scen = Inf)

  # Create the gap df from all the dims
  DLE.income$CollectAllGap()

  # do housing
  run_housing(scen = "Income")

  # roll out scenario
  DLE.income$SetupRollout() # lct would need a target year - for transport, where it's forced to be set to Inf now

  # get energy intensities
  DLE.income$CallDeriveEnergyIntensity(dims=dim.list)


  # do aggregates.
  DLE.income$SumDLE()
  DLE.income$AggregateRegions()
  DLE.income$AggregateDimensions()
  DLE.income$PlotDLEpcap_ByRegByDim()
  DLE.income$PlotDLEpcap_ByRegByNeed()
  DLE.income$PlotDLEpcap_GlobalByDim()

  # we use global DLE.ACCEL here instead of return because we use it as a global object throughout all steps.
}

#' "Income.regression" scenario run
#' This is a scenario that closes the DLS gap based driven by GDP, based on a dimension-specific regression with GDP per capita.
#'
#' This function is currently not in use and its functioning is not guaranteed.
#'
#' @param ssp
#' @param income.indicator
#'
#' @return
#' @export
#'
#' @examples
run_income_crosssection_regression_scenario <- function(ssp, income.indicator) {
  # Construct a GDP regression scenario (Income.regression)

  load_dimensions()

  # Build a list of dimensions
  dim.list <<- generate_all_dimensions()

  # Initiate a scenario object - thres/gap/E.int are taken care of here.
  DLE.income.regression <<- DLE.scenario(scenario.name = "Income.regression", dims = dim.list, year.tgt.scen = Inf) # scenario name, used later in ConstructRolloutScenario

  # Create the gap df from all the dims
  DLE.income.regression$CollectAllGap()

  # do housing
  run_housing(scen = "Income.regression") # Called first to prevent thermal comfort (and later hot water) depending on this while not called yet.

  # roll out scenario
  DLE.income.regression$SetupRollout(lct = lct)

  # get energy intensities
  DLE.income.regression$CallDeriveEnergyIntensity(dims=dim.list)


  # do aggregates.
  DLE.income.regression$SumDLE()
  DLE.income.regression$AggregateRegions()
  DLE.income.regression$AggregateDimensions()
  DLE.income.regression$PlotDLEpcap_ByRegByDim()
  DLE.income.regression$PlotDLEpcap_ByRegByNeed()
  DLE.income.regression$PlotDLEpcap_GlobalByDim()

  # we use global DLE.ACCEL here instead of return because we use it as a global object throughout all steps.
}


##### Wrapper functions for DLE scenario runs ----------------------------------

#' Wrapper for running "ACCEL" scenario (using function run_accel_scenario)
#' Options for:
#' - SSP: Shared Socioeconomic Pathway
#' - year.target: year to close the DLS gaps
#' - lct: lct standing for low carbon technologies, which will in effect change the energy intensity of the transport dimension through higher modal shares of public tranpsort in regions currently dominated by private transport use.
#' - save.option: save options
#'
#' @param ssp
#' @param year.target
#' @param lct
#' @param save.option
#'
#' @return
#' @export
#'
#' @examples
#' # see dle_core.R:
#' run_accel_scenario_wrapper(ssp = ssp, year.target = year.target, lct = TRANSPORT.MODAL.CONVERGENCE, save.option = DLE.SCENARIO.OUTPUT.FORMAT)
run_accel_scenario_wrapper <- function(ssp = "SSP2", year.target = 2040, lct = FALSE, save.option = c("R11.csvneeds")) {
  # run scenario
  if (SUPPRESS.DLE.PRINTLOG) {
    quiet(
      run_accel_scenario(ssp, year.target, lct)
    )
  } else {
    run_accel_scenario(ssp, year.target, lct)
  }

  # scenario results by dimensions - national
  out.df.national <- DLE.ACCEL$DLE.alldims
  # scenario results by dimensions - R11
  out.df.r11 <- DLE.ACCEL$DLE.alldims.agg
  # scenario results aggregated by needs group - R11
  out.df.r11.need.aggregation <- DLE.ACCEL$DLE.group.agg

  # save.options (style; regional_level.save_format): c("national.csvdims", "R11.csvdims", "R11.csvneeds", "R11.htmldims", "R11.htmlneeds", "global.htmlneeds")
  save_options_wrapper(
    df.national = out.df.national,
    df.r11 = out.df.r11,
    df.r11.need.aggregation = out.df.r11.need.aggregation,
    save.option = save.option,
    save.string = paste0(ssp, "_", as.character(year.target), "_lct", as.character(lct))
  )
}

#' Wrapper for running "Income" scenario (using function run_gap_closing_proxy_scenario)
#' Options for:
#' - ssp: Shared Socioeconomic Pathway
#' - pov.thres: poverty threshold indicator to use for pacing the closing of the gap
#' - save.option: save options
#'
#' This function is currently not in use and its functioning is not guaranteed.
#'
#' @param ssp
#' @param pov.thres
#' @param save.option
#'
#' @return
#' @export
#'
#' @examples
run_income_scenario_wrapper <- function(ssp = "SSP2", pov.thres = 10, save.option = c("R11.csvneeds")) {
  # run scenario
  run_gap_closing_proxy_scenario(ssp, pov.thres)


  # scenario results by dimensions - national
  out.df.national <- DLE.income$DLE.alldims
  # scenario results by dimensions - R11
  out.df.r11 <- DLE.income$DLE.alldims.agg
  # scenario results aggregated by needs group - R11
  out.df.r11.need.aggregation <- DLE.income$DLE.group.agg


  # save.options (style; regional_level.save_format): c("national.csvdims", "R11.csvdims", "R11.csvneeds", "R11.htmldims", "R11.htmlneeds", "global.htmlneeds")
  save_options_wrapper(
    df.national = out.df.national,
    df.r11 = out.df.r11,
    df.r11.need.aggregation = out.df.r11.need.aggregation,
    save.option = save.option,
    save.string = paste0(ssp, "_", as.character(pov.thres))
  )
}


#' Wrapper for running "Income.regression" scenario (using function run_income_crosssection_regression_scenario)
#' Options for:
#' - ssp: Shared Socioeconomic Pathway
#' - income.indicator: income indicator to regress against
#' - save.option: save options
#'
#' This function is currently not in use and its functioning is not guaranteed.
#'
#' @param ssp
#' @param income.indicator
#' @param save.option
#'
#' @return
#' @export
#'
#' @examples
run_income_regression_scenario_wrapper <- function(ssp = "SSP2", income.indicator = "log.gdp", save.option = c("R11.csvneeds")) {
  run_income_crosssection_regression_scenario(ssp, income.indicator)

  # scenario results by dimensions - national
  out.df.national <- DLE.income.regression$DLE.alldims
  # scenario results by dimensions - R11
  out.df.r11 <- DLE.income.regression$DLE.alldims.agg
  # scenario results aggregated by needs group - R11
  out.df.r11.need.aggregation <- DLE.income.regression$DLE.group.agg

  # save.options (style; regional_level.save_format): c("national.csvdims", "R11.csvdims", "R11.csvneeds", "R11.htmldims", "R11.htmlneeds", "global.htmlneeds")
  save_options_wrapper(
    df.national = out.df.national,
    df.r11 = out.df.r11,
    df.r11.need.aggregation = out.df.r11.need.aggregation,
    save.option = save.option,
    save.string = paste0(ssp, "_regression_", as.character(income.indicator))
  )
}

##### Saving DLE scenario runs -------------------------------------------------
#' Save interactive HTML area plotly plot, global or R11
#' - currently available as an option to be called in save_options_wrapper
#'
#' @param df
#' @param filename
#' @param needs.aggregation
#' @param regional.level
#'
#' @return
#' @export
#'
#' @examples
save_scenario_to_html_timeseries <- function(df, filename = "SSP2_2030", needs.aggregation = F, regional.level = "R11") {
  type.remove <- "all"

  if (regional.level == "global") {
    if (needs.aggregation) {
      # global picture:
      df.world.pc <- df %>%
        filter(elec != "total") %>%
        ungroup() %>%
        select(type, elec, year, group, region, DLE) %>%
        filter(type != type.remove) %>%
        group_by(year, group) %>%
        summarise(val = sum(DLE, na.rm = TRUE))

      p.need.world <- ggplot(
        data = df.world.pc,
        aes(
          x = year,
          y = val,
          fill = group
        )
      ) +
        geom_area() +
        labs(
          title = "Total decent living energy per capita, by need group",
          y = "EJ/year"
        )

      phtml.need.world <- plotly::ggplotly(p.need.world)
      htmlwidgets::saveWidget(phtml.need.world, paste0(out.path, "/", filename, "_Globaldle_need.html"))
    } else {
      message("Not implemented yet ...")
    }
  } else if (regional.level == "R11") {
    if (needs.aggregation) {
      df.region.pc <- df %>%
        filter(elec != "total") %>%
        ungroup() %>%
        select(type, elec, year, group, region, DLE) %>%
        filter(type != type.remove) %>%
        group_by(year, group, region) %>%
        summarise(val = sum(DLE, na.rm = TRUE)) %>%
        left_join(R11.pop %>% rename(region = R11.region)) %>%
        mutate(val = val / population * exa / giga) %>%
        select(-population)

      p.need.region <- ggplot(
        data = df.region.pc,
        aes(
          x = year,
          y = val,
          fill = group
        )
      ) +
        geom_area() +
        facet_wrap(~region) +
        labs(
          title = "Total decent living energy per capita, by need group",
          y = "GJ/cap/year"
        )

      phtml.need.region <- plotly::ggplotly(p.need.region)
      htmlwidgets::saveWidget(phtml.need.region, paste0(out.path, "/", filename, "_R11dle_need.html"))
    } else {
      df.grp.region.pc <- df %>%
        filter(elec != "total") %>%
        ungroup() %>%
        select(type, elec, year, dimension, region, DLE) %>%
        filter(type != type.remove) %>%
        group_by(year, dimension, region) %>%
        summarise(val = sum(DLE, na.rm = TRUE)) %>%
        left_join(R11.pop %>% rename(region = R11.region)) %>%
        mutate(val = val / population * exa / giga) %>%
        select(-population)

      p.grp.region <- ggplot(
        data = df.grp.region.pc,
        aes(
          x = year,
          y = val,
          fill = dimension
        )
      ) +
        geom_area() +
        facet_wrap(~region) +
        labs(
          title = "Total decent living energy per capita",
          y = "GJ/cap/year"
        )

      phtml.grp.region <- plotly::ggplotly(p.grp.region)
      htmlwidgets::saveWidget(phtml.grp.region, paste0(out.path, "/", filename, "_R11dle.html"))
    }
  }
}

#' Choose which formats to write out for DLE scenario data results
#' - used in scenario wrappers such as run_accel_scenario_wrapper()
#'
#' @param df.national
#' @param df.r11
#' @param df.r11.need.aggregation
#' @param save.option
#' @param save.string
#'
#' @return
#' @export
#'
#' @examples
#' # save.options (style; regional_level.save_format): c("national.csvdims", "R11.csvdims", "R11.csvneeds", "R11.htmldims", "R11.htmlneeds", "global.htmlneeds")
#' save_options_wrapper(
#'   df.national = out.df.national,
#'   df.r11 = out.df.r11,
#'   df.r11.need.aggregation = out.df.r11.need.aggregation,
#'   save.option = save.option,
#'   save.string = paste0(ssp, "_", as.character(year.target), "_lct", as.character(lct))
#' )
save_options_wrapper <- function(df.national, df.r11, df.r11.need.aggregation, save.option, save.string) {
  if ("national.csvdims" %in% save.option) {
    write_delim(
      x = df.national,
      file = file.path(out.path, paste0(save.string, "_country.csv")),
      delim = ","
    )
  }
  if ("R11.csvdims" %in% save.option) {
    write_delim(
      x = df.r11,
      file = file.path(out.path, paste0(save.string, "_R11.csv")),
      delim = ","
    )
  }
  if ("R11.csvneeds" %in% save.option) {
    write_delim(
      x = df.r11.need.aggregation,
      file = file.path(out.path, paste0(save.string, "_R11need.csv")),
      delim = ","
    )
  }
  if ("R11.htmldims" %in% save.option) {
    save_scenario_to_html_timeseries(df.r11,
                                     filename = save.string,
                                     needs.aggregation = F
    )
  }
  if ("R11.htmlneeds" %in% save.option) {
    save_scenario_to_html_timeseries(df.r11.need.aggregation,
                                     filename = save.string,
                                     needs.aggregation = T
    )
  }
  if ("global.htmlneeds" %in% save.option) {
    save_scenario_to_html_timeseries(df.r11.need.aggregation,
                                     filename = save.string,
                                     needs.aggregation = T,
                                     regional.level = "global"
    )
  }
}


### DLS output products: streamling reporting ----------------------------------

#' Streamline DLS dimension variable names in reporting
#'
#' @param df
#' @param type
#'
#' @return df: df with streamlined variable names
#' @export
#'
#' @examples
#' df %>% streamline_variable_names_dls(type = "service.gaps")
#' df %>% streamline_variable_names_dls(type = "thresholds")
#' df %>% streamline_variable_names_dls(type = "headcounts")
#' df %>% streamline_variable_names_dls(type = "ei")
streamline_variable_names_dls <- function(df, type){
  # N = 26 (of all variables and units, for threshold or service gap)

  # type.options: c("service.gaps", "thresholds", "dod", "headcounts", "ei")
  if (type %in% c("service.gaps", "thresholds", "dod")){
    df <- df %>%
      mutate_cond(variable=="Health|health", variable = "Health care" ) %>%
      mutate_cond(variable=="Nutrition|nutrition", variable = "Nutrition" ) %>%
      mutate_cond(variable=="Roads|roads", variable = "Roads" ) %>%
      mutate_cond(variable=="Sanitation|sanitation", variable = "Sanitation" ) %>%
      mutate_cond(variable=="Transport|total", variable = "Transport" ) %>%
      mutate_cond(variable=="Water|water", variable = "Water" )
  } else if (type %in% c("headcounts")){
    df <- df %>%
      mutate_cond(variable=="Education|lower_secondary", variable = "Education") %>% # headcount-specific

      # mutate_cond(variable=="Health|health", variable = "Health care" ) %>% # not defined (and not reported)
      mutate_cond(variable=="Nutrition|nutrition", variable = "Nutrition" ) %>%
      # mutate_cond(variable=="Roads|roads", variable = "Roads" ) %>% # not defined
      mutate_cond(variable=="Sanitation|sanitation", variable = "Sanitation" ) %>%
      mutate_cond(variable=="Transport|total", variable = "Transport" ) %>%
      mutate_cond(variable=="Water|water", variable = "Water" )
  } else if (type %in% c("ei")){
    df <- df %>%
      mutate_cond(variable=="Health|health", variable = "Health care" ) %>%
      mutate_cond(variable=="Nutrition|nutrition", variable = "Nutrition" ) %>%
      mutate_cond(variable=="Roads|roads", variable = "Roads" ) %>%
      mutate_cond(variable=="Sanitation|sanitation", variable = "Sanitation" ) %>%
      mutate_cond(variable=="Transport|total", variable = "Transport" ) %>%
      mutate_cond(variable=="Water|water", variable = "Water" )
  } else {
    log_error("The `type` provided is not compatible with this function.")
  }
  return(df)
}

#' Change unit from household to capita on a df with DLS dimensions
#'
#' @param df
#' @param type
#'
#' @return df: df with unit changed from household to capita
#' @export
#'
#' @examples
#' df %>% from_hh_to_cap_dls(type = "service.gaps")
#' df %>% from_hh_to_cap_dls(type = "thresholds")
#' df %>% from_hh_to_cap_dls(type = "ei")
from_hh_to_cap_dls <- function(df, type){
  # type.options: c("service.gaps", "thresholds", "ei")
  # N.B. assumes that `hh_size` is already loaded in the environment

  if (type=="thresholds"){
    df <- df %>%
      left_join(hh_size, by = "iso") %>%
      mutate_cond(unit == "unit/hh", thres = thres/`hh_size`) %>%
      mutate_cond(unit == "unit/hh", unit = "unit/cap") %>%
      select(-`hh_size`)
  } else if (type=="service.gaps"){
    df <- df %>%
      left_join(hh_size, by = "iso") %>%
      mutate_cond(unit == "unit/hh", gap = gap/`hh_size`) %>%
      mutate_cond(unit == "unit/hh", unit = "unit/cap") %>%
      select(-`hh_size`)
  } else if (type=="ei"){
    df <- df %>%
      left_join(hh_size, by = "iso") %>%
      mutate_cond(grepl(variable,pattern="ppliance"), e.int = e.int/`hh_size`) %>%
      select(-`hh_size`)
  } else {
    log_error("The `type` provided is not compatible with this function.")
  }

  return(df)
}

#' Add units to energy intensities in reporting where still incorrect
#' - used primarily in the collect-ei.R script
#'
#' @param df
#' @param type
#'
#' @return
#' @export
#'
#' @examples
#' df %>% add_ei_units(type = "op")
#' df %>% add_ei_units(type = "con")
add_ei_units <- function(df, type){
  # N = 36

  # type.options: c("op", "con")
  # returns energy intensity units (for streamlined variable names)
  # CON.new: per unit.
  # CON.rep: per unit divided by lifetime.
  # OP: per service unit.

  if (type == "op"){
    df <- df %>%
      mutate(unit = "not assigned") %>%
      mutate_cond(variable %in% c("Appliance|clean_cooking_fuel",
                                  "Appliance|mobile_telephone",
                                  "Appliance|refrigerator",
                                  "Appliance|television"),
                  unit = "MJ/unit/year") %>%
      mutate_cond(variable %in% c("Transport",
                                  "Transport|bus",
                                  "Transport|car",
                                  "Transport|rail",
                                  "Transport|twothree"),
                  unit = "MJ/pkm") %>%
      mutate_cond(variable %in% c("Clothing|clothing",
                                  "Clothing|footwear"),
                  unit = "MJ/kg") %>%
      mutate_cond(variable %in% c("Cooling OP|rural",
                                  "Cooling OP|total",
                                  "Cooling OP|urban",
                                  "Heating OP|rural",
                                  "Heating OP|total",
                                  "Heating OP|urban"),
                  unit = "MJ/m2/year") %>%
      mutate_cond(variable %in% c("Sanitation",
                                  "Hot Water OP|rural",
                                  "Hot Water OP|total",
                                  "Hot Water OP|urban"),
                  unit = "MJ/cap/year") %>%
      mutate_cond(variable %in% c("Water"),
                  unit = "MJ/m3") %>% # is documented as KJ/L, which is the same as MJ/m3
      mutate_cond(variable %in% c("Nutrition"),
                  unit = "MJ/kcal") %>%
      mutate_cond(variable %in% c("Education|lower_secondary",
                                  "Education|primary",
                                  "Health care"),
                  unit = "MJ/$") %>%

      mutate_cond(variable %in% c("Cooling CON|rural",
                                  "Cooling CON|total",
                                  "Cooling CON|urban",
                                  "Heating CON|rural",
                                  "Heating CON|total",
                                  "Heating CON|urban",
                                  "Housing|rural",
                                  "Housing|total",
                                  "Housing|urban",
                                  "Roads"),
                  unit = NA)
  } else if (type == "con"){
    df <- df %>%
      mutate(unit = "not assigned") %>%
      mutate_cond(variable %in% c("Appliance|clean_cooking_fuel",
                                  "Appliance|mobile_telephone",
                                  "Appliance|refrigerator",
                                  "Appliance|television",
                                  "Cooling CON|rural",
                                  "Cooling CON|total",
                                  "Cooling CON|urban",
                                  "Heating CON|rural",
                                  "Heating CON|total",
                                  "Heating CON|urban"),
                  unit = "MJ/unit") %>% #MJ/unit?? --> heating and cooling con vs appliances: where's the household size correction?
      mutate_cond(variable %in% c("Transport",
                                  "Transport|bus",
                                  "Transport|car",
                                  "Transport|rail",
                                  "Transport|twothree"),
                  unit = "MJ/pkm") %>%
      mutate_cond(variable %in% c("Housing|rural",
                                  "Housing|total",
                                  "Housing|urban"),
                  unit = "MJ/m2") %>%
      mutate_cond(variable %in% c("Roads"),
                  unit = "MJ/km") %>%
      mutate_cond(variable %in% c("Sanitation"),
                  unit = "MJ/cap") %>%
      mutate_cond(variable %in% c("Water"),
                  unit = "MJ/m3") %>%

      mutate_cond(variable %in% c("Cooling OP|rural",
                                  "Cooling OP|total",
                                  "Cooling OP|urban",
                                  "Clothing|clothing",
                                  "Clothing|footwear",
                                  "Education|lower_secondary",
                                  "Education|primary",
                                  "Health care",
                                  "Heating OP|rural",
                                  "Heating OP|total",
                                  "Heating OP|urban",
                                  "Hot Water OP|rural",
                                  "Hot Water OP|total",
                                  "Hot Water OP|urban",
                                  "Nutrition"),
                  unit = NA)
  } else {
    log_error("The `type` provided is not compatible with this function.")
  }

  return(df)
}



#' Create a data frame with energy intensities for all dimensions
#' - loads into global environment the variable EI.alldims
#'
#' @param dims
#'
#' @return
#' @export
#'
#' @examples
#' run_initiatialization_input_data()
#' load_dimensions()
#' dims <<- generate_all_dimensions()
#' call_derive_energy_intensity_helper(dims=dims)
#' collect_all_ei_helper(dims=dims)
collect_all_ei_helper <-function(dims=dims){
  EI <- lapply(
    names(dims),
    function(x) {
      print(x)

      df.dls <- dims[[x]]$DF.tei # class instance of this dimension

      if ((names(df.dls)[1] != "iso") & !("year"%in%colnames(df.dls))) { # When the regional resolution is for R11
        df.ei <- df.dls %>%
          ungroup() %>%
          rename(country.reg = R11.region) %>%
          select(country.reg, grp, type, elec, e.int) %>%
          mutate(year="Entire period")
      } else if (!("year"%in%colnames(df.dls))) {
        df.ei <- df.dls %>%
          ungroup() %>%
          rename(country.reg = iso) %>%
          select(country.reg, grp, type, elec, e.int) %>%
          mutate(year="Entire period")
      } else if ("year"%in%colnames(df.dls)){
        df.ei <- df.dls %>%
          ungroup() %>%
          rename(country.reg = iso) %>%
          select(country.reg, grp, type, elec, e.int, year)
      }
      df.ei <- df.ei %>%
        mutate(dim = dims[[x]]$name_dim) %>%
        select(dim, everything()) %>%
        mutate(year = ifelse(year == "2015" & dim %in% c("Heating OP", "Cooling OP"), "Entire period", year))

      return(df.ei)
    }
  )
  EI.alldims <<- do.call("rbind", EI)
}




# DESIRE (calculator) utils ----------------------------------------------------


### Distribution functions -----------------------------------------------------
#' Get share or depth below a threshold, based on the (nu, sigma) parameters of a lognormal distribution.
#'
#' Note that nu and sigma take some work to derive from the average (energy per capita) and inequality parameter (gini) before putting into this function.
#' Specifically,
#' sig = GetInverseCDF_lognormal(energy.gini); using another function in this package
#' nu = log(energy.per.capita) - (sig^2) / 2; noting that the mean of the lognormal distribution can be expressed as exp(nu + sig^2 / 2)
#'
#' @param nu
#' @param sigma
#' @param thres: threshold
#' @param ret: "share" below the threshold (default) or "DoD" depth of deficit
#'
#' @return
#' @export
#'
#' @examples
#' data %>%
#' mutate(
#'   sig = GetInverseCDF_lognormal(energy.gini)
#'   ) %>%
#'   mutate(nu = log(energy.per.capita) - (sig^2) / 2) %>%
#'   mutate(share.below.projected.curtech = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.curtech, "share")) %>%
#'   mutate(depth.below.projected.curtech = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.curtech, "DoD")) %>%
#'   mutate(share.below.projected.adjusted = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.adjusted, "share")) %>%
#'   mutate(depth.below.projected.adjusted = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.adjusted, "DoD"))
GetDepthofDeficit_lognormal <- function(nu, sigma, thres, ret = "share") {
  if (is.na(nu) | is.na(sigma) | is.na(thres)) {
    return(NA)
  }

  # Typical lognormal distribution to be used for integration
  f <- function(x, nu, sigma) {
    dlnorm(x, meanlog = nu, sdlog = sigma, log = FALSE)
  }
  xf <- function(x, nu, sigma) {
    x * f(x, nu, sigma)
  }

  mean.subthres <- integrate(xf, 0, thres, nu, sigma)
  sh.subthres <- integrate(f, 0, thres, nu, sigma)

  DoD <- thres - mean.subthres$value / sh.subthres$value

  if (ret == "DoD") {
    return(DoD)
  }
  if (ret == "share") {
    return(sh.subthres$value)
  }
}
GetDepthofDeficit_lognormal <- Vectorize(GetDepthofDeficit_lognormal)

#' Using invcdf(), calculate the sigma of a lognormal from a gini coefficient
#' Function for lognormal cumulative distribution function, for depth-of-deficit calculation (methodology: see https://www.econstor.eu/bitstream/10419/173337/1/wp-gut-fme-a-41-Kot.pdf, page 9)
#'
#' @param g: gini coefficient (from 0 to 1)
#'
#' @return
#' @export
#'
#' @examples
#' data %>%
#' mutate(
#'   sig = GetInverseCDF_lognormal(energy.gini)
#'   ) %>%
#'   mutate(nu = log(energy.per.capita) - (sig^2) / 2) %>%
#'   mutate(share.below.projected.curtech = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.curtech, "share")) %>%
#'   mutate(depth.below.projected.curtech = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.curtech, "DoD")) %>%
#'   mutate(share.below.projected.adjusted = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.adjusted, "share")) %>%
#'   mutate(depth.below.projected.adjusted = GetDepthofDeficit_lognormal(nu, sig, dle.threshold.adjusted, "DoD"))
GetInverseCDF_lognormal <- function(g) {
  x <- sqrt(2) * invcdf(normal(), (g + 1) / 2)[[1]]
  if (is.list(length(x))) {
    stop("error")
  }

  return(
    x %>% unlist()
  )
}
GetInverseCDF_lognormal <- Vectorize(GetInverseCDF_lognormal)


# example of the functions below (still to be documented)
# df.core %>%
#   filter(
#     scenario=="SDP_RC-1p5C", year%in%c(2020,2040),
#     iso%in%c("IND","USA"),
#     model=="IMAGE 3.3"
#   ) %>%
#   mutate(sig = get_sigma_lognormal(gini=energy.gini)) %>%
#   mutate(nu = get_nu_lognormal(mean = energy.per.capita, gini = energy.gini)) %>%
#   mutate(bottom_10p_mean = get_mean_of_share_lognormal(
#     nu=nu, sigma=sig, perc.low=0, perc.high=0.1, ret = "mean"
#   )) %>%
#   mutate(bottom_40p_mean = get_mean_of_share_lognormal(
#     nu=nu, sigma=sig, perc.low=0, perc.high=0.4, ret = "mean"
#   )) %>%
#   mutate(top_10p_mean = get_mean_of_share_lognormal(
#     nu=nu, sigma=sig, perc.low=0.9, perc.high=1, ret = "mean"
#   )) %>%
#   mutate(palma.ratio = get_palma_ratio_lognormal(nu=nu,sigma=sig))

get_sigma_lognormal <- function(gini){
  return(
    GetInverseCDF_lognormal(gini)
  )
}
get_sigma_lognormal <- Vectorize(get_sigma_lognormal)
get_nu_lognormal <- function(mean, gini){
  return(
    log(mean) - (get_sigma_lognormal(gini)^2) / 2
  )
}
get_nu_lognormal <- Vectorize(get_nu_lognormal)

get_mean_of_range_lognormal <- function(nu, sigma, thres.low, thres.high,
                                        ret = "mean") {
  if (is.na(nu) | is.na(sigma) | is.na(thres.low) | is.na(thres.high)) {
    return(NA)
  }

  # Typical lognormal distribution to be used for integration
  f <- function(x, nu, sigma) {
    dlnorm(x, meanlog = nu, sdlog = sigma, log = FALSE)
  }
  xf <- function(x, nu, sigma) {
    x * f(x, nu, sigma)
  }

  mean.range <- integrate(xf, thres.low, thres.high, nu, sigma)
  sh.range <- integrate(f, thres.low, thres.high, nu, sigma)

  if (ret == "mean") {
    return(mean.range$value / sh.range$value)
  }
  if (ret == "share") {
    return(sh.range$value)
  }
}
get_mean_of_range_lognormal <- Vectorize(get_mean_of_range_lognormal)
get_mean_of_share_lognormal <- function(nu, sigma, perc.low, perc.high,
                               ret = "mean") {
  if (is.na(nu) | is.na(sigma) | is.na(perc.low) | is.na(perc.high)) {
    return(NA)
  }

  estimated.min.value = qlnorm(p=perc.low, meanlog = nu, sdlog = sigma)
  estimated.max.value = qlnorm(p=perc.high, meanlog = nu, sdlog = sigma)

  if (ret == "mean") {
    return(
      get_mean_of_range_lognormal(
        nu=nu, sigma=sigma,
        thres.low = estimated.min.value,
        thres.high = estimated.max.value,
        ret = "mean"
      )
    )
  }
  if (ret == "share") {
    return(
      get_mean_of_range_lognormal(
        nu=nu, sigma=sigma,
        thres.low = estimated.min.value,
        thres.high = estimated.max.value,
        ret = "share"
      )
    )
  }
}
get_mean_of_share_lognormal <- Vectorize(get_mean_of_share_lognormal)

get_palma_ratio_lognormal <- function(nu, sigma){

  bottom_40p_mean <- get_mean_of_share_lognormal(
    nu=nu, sigma=sigma, perc.low=0, perc.high=0.4, ret = "mean"
  )
  top_10p_mean <- get_mean_of_share_lognormal(
    nu=nu, sigma=sigma, perc.low=0.9, perc.high=1, ret = "mean"
  )

  palma_ratio <- (top_10p_mean * 0.1) / (bottom_40p_mean * 0.4)

  return(palma_ratio)
}
get_palma_ratio_lognormal <- Vectorize(get_palma_ratio_lognormal)




#' Calculate a gini coefficient from a dataframe with per capita data and population data for each observation
#'
#' Function assumes that the dataframe has a column called "population" and a column with the per capita data to derive the gini from (e.g. in the fe [var="fe"] column)
#' Dataframe also is assumed to have an iso column.
#' Data does not have to be preordered, as it is arranged by arrange(iso,var) in the function.
#'
#' @param df
#' @param var: column name with the per capita data to derive the gini from
#'
#' @return
#' @export
#'
#' @examples
#' fe.total.avg <- fe.total.by.income %>%
#'    left_join(population.fe) %>%
#'    group_by(iso) %>%
#'    summarise(fe = sum(fe * population / total.pop))
#' fe.total.gini <- get_gini(fe.total.by.income)
get_gini <- function(df, var = "fe") {
  # get shares
  df.pop.total <- df %>%
    group_by(iso) %>%
    summarise(total.pop = sum(population))
  df <- df %>% mutate(
    var.mul.pop = (!!as.name(var)) * population,
    na.rm = T
  )
  df.var.total <- df %>%
    group_by(iso) %>%
    summarise(total.var = sum(var.mul.pop, na.rm = T))

  df.shares <- df %>%
    left_join(df.pop.total, by = "iso") %>%
    left_join(df.var.total, by = "iso") %>%
    arrange(iso, (!!as.name(var))) %>%
    group_by(iso) %>%
    mutate(
      pop.share = population / total.pop,
      var.share = ifelse(population == 0, 0, var.mul.pop / total.var)
    )

  # get cumulative shares
  df.cum.shares <- df.shares %>%
    arrange(iso, (!!as.name(var))) %>%
    group_by(iso) %>%
    mutate(
      cum.pop.share = cumsum(pop.share),
      cum.var.share = cumsum(var.share)
    ) %>%
    select(iso, cum.pop.share, cum.var.share, pop.share, var.share)

  # check cumulative shares adding up to 1
  # View(df.shares %>% group_by(iso) %>% summarise(pop=sum(pop.share), var=sum(var.share)))


  # get gini
  gini <- df.cum.shares %>%
    group_by(iso) %>%
    mutate(
      area.points.rec = lag(cum.var.share, default = 0) * pop.share,
      area.points.tri = (cum.var.share - lag(cum.var.share, default = 0)) * pop.share * 0.5
    ) %>%
    mutate(area.points = area.points.rec + area.points.tri) %>%
    summarise(
      area = sum(area.points)
    ) %>%
    mutate(
      gini = (0.5 - area) / 0.5
    )

  return(gini %>% select(iso, gini))
}

### Locating paths and data ----------------------------------------------------
#' Set DATA.LOCATION as global variable.
#' - loads into global environment the variable DATA.LOCATION
#' - using get_data_location(), which ...
#'     * if test = TRUE, then DATA.LOCATION is set to the test data location
#'     * if test = FALSE, then DATA.LOCATION is set to the main data location
#'
#' @param test
#'
#' @return DATA.LOCATION
#' @export
#'
#' @examples
set_data_location <- function(test = FALSE){

  DATA.LOCATION <<- get_data_location(test=test)

  return(DATA.LOCATION)
}

#' Gets data location, but does not set it as global variable.
#' - if test = TRUE, then DATA.LOCATION is set to the test data location ("tests", "testthat", "testdata", "output")
#' - if test = FALSE, then DATA.LOCATION is set to the main data location ("data")
#'
#' @param test
#'
#' @return DATA.LOCATION
#' @export
#'
#' @examples
get_data_location <- function(test = FALSE){
  if (test == TRUE) {
    DATA.LOCATION <- file.path("tests", "testthat", "testdata", "output")
  } else {
    DATA.LOCATION <- "data"
  }
  return(DATA.LOCATION)
}

#' Set DATA.LOCATION.RAW as global variable.
#' - loads into global environment the variable DATA.LOCATION.RAW
#' - using get_data_location_raw(), which ...
#'     * if test = TRUE, then DATA.LOCATION.RAW is set to the test data-raw location
#'     * if test = FALSE, then DATA.LOCATION.RAW is set to the main data-raw location
#'
#' Function is currently not used, nor tested. It is here for future use, and may or may not work.
#'
#' @param test
#'
#' @return DATA.LOCATION.RAW
#' @export
#'
#' @examples
set_data_location_raw <- function(test = FALSE){

  DATA.LOCATION.RAW <<- get_data_location_raw(test=test)

  return(DATA.LOCATION.RAW)
}

#' Gets raw data location, but does not set it as global variable.
#' - if test = TRUE, then DATA.LOCATION.RAW is set to the test data-raw location ("tests", "testthat", "testdata", "input", "data-raw")
#' - if test = FALSE, then DATA.LOCATION.RAW is set to the main data-raw location ("data-raw")
#'
#' @param test
#'
#' @return DATA.LOCATION.RAW
#' @export
#'
#' @examples
#' original.regionallevel.iam.data.path <- here(get_data_location_raw(test=FALSE),"scenario_data","original_regional")
#' dle.pc <- vroom(here(get_data_location_raw(test = TEST),"dle_threshold","DLE_threshold_input.csv"), show_col_types=FALSE)
get_data_location_raw <- function(test = FALSE){
  if (test==TRUE) {
    DATA.LOCATION.RAW <- file.path("tests", "testthat", "testdata", "input", "data-raw")
  } else {
    DATA.LOCATION.RAW <- "data-raw"
  }
  return(DATA.LOCATION.RAW)
}

### SSP related utils ----------------------------------------------------------

#' When an SSP variant is in the scenario name, add a column ("scenario.mapping") that returns the ssp variant
#' This is for when many scenarios are assessed, but not all are properly mapped to e.g. a population pathway. Then we can use this function to map the scenarios to the appropriate SSP variants.
#'
#' NB/TOFIX: consider replacing this by forcing the use of a config / YAML mapping file
#'
#' @param df
#' @param default
#'
#' @return df with scenario.mapping column indicating SSP1/SSP2/SSP3/SSP4/SSP5/NA
#' @export
#'
#' @examples
add_ssp_mapping <- function(df, default = "NA") {

  if (default == "NA") {
    df <- df %>%
      mutate(scenario.mapping = ifelse(grepl("SSP1", x = scenario, fixed = T), "SSP1",
                                       ifelse(grepl("SSP2", x = scenario, fixed = T), "SSP2",
                                              ifelse(grepl("SSP3", x = scenario, fixed = T), "SSP3",
                                                     ifelse(grepl("SSP4", x = scenario, fixed = T), "SSP4",
                                                            ifelse(grepl("SSP5", x = scenario, fixed = T), "SSP5",
                                                                   NA
                                                            )
                                                     )
                                              )
                                       )
      ))
  } else {
    df <- df %>%
      mutate(scenario.mapping = ifelse(grepl("SSP1", x = scenario, fixed = T), "SSP1",
                                       ifelse(grepl("SSP2", x = scenario, fixed = T), "SSP2",
                                              ifelse(grepl("SSP3", x = scenario, fixed = T), "SSP3",
                                                     ifelse(grepl("SSP4", x = scenario, fixed = T), "SSP4",
                                                            ifelse(grepl("SSP5", x = scenario, fixed = T), "SSP5",
                                                                   default
                                                            )
                                                     )
                                              )
                                       )
      ))
  }


  return(df)
}




### Project-specific utils -----------------------------------------------------


##### SHAPE (current default) --------------------------------------------------

#' Add a "scenario.mapping" column to match SHAPE scenario data with their respective Gini pathways
#'
#' @param df
#' @param default
#' @param version: SHAPE scenario set version, "v1-v2" or "v3" (v3 is latest, and current default)
#'
#' @return
#' @export
#'
#' @examples
#' scenario.template %>%
#'   add_shape_mapping_gini(default = DEFAULT.SSP.MAPPING.GINI, version = "v3") %>%
#'   group_by(model, scenario, iso, variable)
add_shape_mapping_gini <- function(df, default = "NA",
                                   version = "v3") {
  # TODO: consider replacing this by a config / YAML mapping file

  if (version == "v1-v2"){
    if (default == "NA") {
      df <- df %>%
        mutate(scenario.mapping = ifelse(grepl("EI", x = scenario, fixed = T), "EI",
                                         ifelse(grepl("MC", x = scenario, fixed = T), "MC",
                                                ifelse(grepl("RC", x = scenario, fixed = T), "RC",
                                                       NA
                                                )
                                         )
        ))
    } else {
      stop(
        "not yet implemented, could add some is.na() based mapping "
      )
    }
  } else if (version == "v3"){
    if (default == "NA") {
      df <- df %>%
        mutate(scenario.mapping = ifelse(grepl("EI", x = scenario, fixed = T), "EI",
                                         ifelse(grepl("MC", x = scenario, fixed = T), "MC",
                                                ifelse(grepl("RC", x = scenario, fixed = T), "RC",
                                                       ifelse(grepl("SSP1", x = scenario, fixed = T), "SSP1",
                                                              ifelse(grepl("SSP2", x = scenario, fixed = T), "SSP2",
                                                                     NA
                                                              )
                                                       )
                                                )
                                         )
        ))
    } else {
      stop(
        "not yet implemented, could add some is.na() based mapping "
      )
    }
  }


  return(df)
}


#' Format downscaled data from Fabio Sferra's downscaler tool to a format the calculator expects
#'
#' Main function in `calculator_activity-fe-process-downscaled.R`
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
#' scenario.data.fe <- vroom(here(
#'     "data-raw",
#'     "scenario_data",
#'     paste0(IAM.SCENARIO.DATA.FILE, ".csv")
#'     ),
#'     show_col_types=FALSE) %>%
#'   format_final_energy_downscaled_shape()
format_final_energy_downscaled_shape <- function(df) {

  # IMAGE top-level variables:
  # Final Energy
  # Final Energy|Agriculture
  # Final Energy|Commercial
  # Final Energy|Industry
  # Final Energy|Non-Energy Use
  # Final Energy|Other Sector
  # Final Energy|Residential
  # Final Energy|Transportation
  # REMIND top-level variables:
  # Final Energy
  # Final Energy (w/o bunkers) // don't use
  # Final Energy|Bunkers
  # Final Energy|Industry
  # Final Energy|Non-Energy Use
  # Final Energy|Residential and Commercial
  # Final Energy|Transportation



  df.out.temp <- df %>%
    rename(model = MODEL, scenario = SCENARIO, iso = ISO, variable = VARIABLE, unit = UNIT) %>%
    filter(grepl(x = variable, pattern = "Final Energy", fixed = T)) %>%
    pivot_longer(
      !!as.symbol(IAM.SCENARIO.DATA.STARTYEAR):!!as.symbol(IAM.SCENARIO.DATA.ENDYEAR),
      names_to = "year",
      values_to = "value"
    ) %>%
    select(model, scenario, iso, variable, unit, year, value) %>% # in case there's years outside the range of interest

    # ad-hoc edit 01.02.21: seems like NA is sometimes reported where we can estimate this to be zero.
    mutate(value = ifelse(is.na(value), 0, value)) %>%
    mutate(year = as.numeric(year)) %>%
    mutate(agg.var = ifelse(
      variable %in% c("Final Energy|Industry", "Final Energy|Agriculture", "Final Energy|Other Sector"), "Final Energy|Industry", ifelse(
        variable %in% c("Final Energy|Transportation", "Final Energy|Bunkers"), "Final Energy|Transportation", ifelse(
          variable %in% c("Final Energy|Residential and Commercial"), "Final Energy|Residential and Commercial", ifelse(
            variable %in% c("Final Energy|Residential"), "Final Energy|Residential", ifelse(
              variable %in% c("Final Energy|Commercial"), "Final Energy|Commercial",
              ifelse(
                variable == "Final Energy",
                "Final Energy",
                NA
              )
            )
          )
        )))) %>%
    filter(!is.na(agg.var)) %>% # drop final energy variables that are not needed for the sectoral aggregates

    group_by(model, scenario, iso, unit, year, agg.var) %>%
    summarise(
      value = sum(value)
    )



  df.out <- df.out.temp %>%
    select(
      model, scenario, iso, agg.var, unit, year, value
    ) %>% rename(variable = agg.var) %>%
    ungroup()

  return(df.out)
}


#' Rename non-final SHAPE REMIND scenarios to match their expected naming convention
#'
#' This function is not relevant anymore and can be deleted when older versions of SHAPE code are deleted.
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
rename_remind_scenarios <- function(df) {
  df <- df %>%
    mutate(scenario = ifelse(startsWith(scenario, "SusDev_"), substr(scenario, 8, nchar(scenario)), scenario)) %>%
    mutate(scenario = ifelse(endsWith(scenario, "-PkBudg650"), paste0(substr(scenario, 1, nchar(scenario) - 10), "-1p5C"), scenario))

  return(df)
}


##### MESSAGE SSP (future placeholder) -----------------------------------------
# ...


### Deprecated utils (non-project specific) ------------------------------------

##### Gini-to-Gini utils -------------------------------------------------------
#' Income gini to energy gini function based on global elasticities from gini_to_gini_data.RData (new version)
#'
#' Function ready to be deprecated.
#' - not used anywhere anymore
#'
#' @param energy.gini
#' @param income.gini.old
#' @param income.gini.new
#' @param sector
#'
#' @return
#' @export
#'
#' @examples
GetGiniToGiniElasticity_new <- function(energy.gini, income.gini.old, income.gini.new, sector = variable) {
  gini_to_gini_data <- readRDS(here(DATA.LOCATION, "gini_to_gini_data.RData"))

  energy.gini <- energy.gini / 100
  income.gini.old <- income.gini.old / 100
  gini.new <- income.gini.new

  energy.variable <- ifelse(sector == "Final Energy|Transportation",
                            "energy.transport",
                            ifelse(sector == "Final Energy|Residential and Commercial",
                                   "energy.rescom",
                                   ifelse(sector == "Final Energy|Industry",
                                          "energy.industry", # NB. this is currently assumed to be the same as energy.total!
                                          ifelse(sector == "Final Energy",
                                                 "energy.total", # TODO: think of consistent way of treating total final energy for elasticities; (a) to check consistency, or (b) just discard and only work on sectoral? or (c) something else?
                                                 NA
                                          )
                                   )
                            )
  )

  low.income <-
    elasticity.gini <-
    # = delta energy.gini / delta income gini
    (
      # end point (find the closest numerical interpolated value to the actual future income.gini and return the energy.gini at the same level)
      (sectoral.elasticity.gini.to.gini %>% select(
        c(
          "income.total",
          energy.variable
        )
      ) %>% filter(abs(income.total - (gini + gini.diff)) == min(abs(income.total - (gini + gini.diff)))) %>% pull(energy.variable)
      ) -
        # starting point (find the closest numerical interpolated value to the actual current income.gini and return the energy.gini at the same level)
        (sectoral.elasticity.gini.to.gini %>% select(
          c(
            "income.total",
            energy.variable
          )
        ) %>% filter(abs(income.total - gini) == min(abs(income.total - gini))) %>% pull(energy.variable)
        )
    ) /
    gini.diff


  return(
    elasticity.gini
  )
}
GetGiniToGiniElasticity_new <- Vectorize(GetGiniToGiniElasticity_new) # to allow use on tibbles (vectors)

#' Income gini to energy gini function based on global elasticities (old, slower version)
#'
#' Function ready to be deprecated.
#' - not used anywhere anymore
#'
#' @param gini
#' @param gini.diff
#' @param sector
#'
#' @return
#' @export
#'
#' @examples
GetGiniToGiniElasticity <- function(gini, gini.diff, sector = variable) {
  # function too slow? (i suspect due to having to find the minimum difference every time)
  # --> maybe make model, do linear fit, and return "predicted" 't' and 't+1' values
  # this function takes in two values, and calculates the change.
  # NB. due to some funny R magic i don't understand, the passing of a tibble doesn't seem to work so we will just make a global variable now and continue
  # gini.data.file = sectoral.elasticity.gini.to.gini
  if (is.na(gini) | is.na(gini.diff) | is.na(sector)) {
    return(NA)
  }
  if (gini.diff == 0 | (abs(gini.diff) < 0.05)) {
    return(1)
  }

  gini <- gini / 100
  gini.diff <- gini.diff / 100
  energy.variable <- ifelse(sector == "Final Energy|Transportation",
                            "energy.transport",
                            ifelse(sector == "Final Energy|Residential and Commercial",
                                   "energy.rescom",
                                   ifelse(sector == "Final Energy|Industry",
                                          "energy.industry", # NB. this is currently assumed to be the same as energy.total!
                                          ifelse(sector == "Final Energy",
                                                 "energy.total", # TODO: think of consistent way of treating total final energy for elasticities; (a) to check consistency, or (b) just discard and only work on sectoral? or (c) something else?
                                                 NA
                                          )
                                   )
                            )
  )
  elasticity.gini <-
    # = delta energy.gini / delta income gini
    (
      # end point (find the closest numerical interpolated value to the actual future income.gini and return the energy.gini at the same level)
      (sectoral.elasticity.gini.to.gini %>% select(
        c(
          "income.total",
          energy.variable
        )
      ) %>% filter(abs(income.total - (gini + gini.diff)) == min(abs(income.total - (gini + gini.diff)))) %>% pull(energy.variable)
      ) -
        # starting point (find the closest numerical interpolated value to the actual current income.gini and return the energy.gini at the same level)
        (sectoral.elasticity.gini.to.gini %>% select(
          c(
            "income.total",
            energy.variable
          )
        ) %>% filter(abs(income.total - gini) == min(abs(income.total - gini))) %>% pull(energy.variable)
        )
    ) /
    gini.diff

  if (length(elasticity.gini) == 2) {
    # sometimes there can be two minima that are exactly the same, then we take the average
    elasticity.gini <- mean(elasticity.gini)
  } else if (length(elasticity.gini) > 2) {
    stop("something wrong with finding a numerical solution here")
  }

  return(
    elasticity.gini
  )
}
GetGiniToGiniElasticity <- Vectorize(GetGiniToGiniElasticity) # to allow use on tibbles (vectors)



# IAM utils --------------------------------------------------------------------

### IAM model/region -------------------------------------------------------------
#' Load into global environment the variable:
#' - regions.message (if MESSAGE.REGION.MAPPING exists in global environment)
#' - regions.image (if IMAGE.REGION.MAPPING exists in global environment)
#' - regions.remind (if REMIND.REGION.MAPPING exists in global environment)
#' - regions (if all of the following variables exist in global environment: MESSAGE.REGION.MAPPING, IMAGE.REGION.MAPPING, REMIND.REGION.MAPPING)
#'
#' @return
#' @export
#'
#' @examples
#' load_iam_region_mappings()
#' # subfolder `R/raw-data/iam_regions`
#' MESSAGE.REGION.MAPPING <<- "region_definitions_message.csv"
#' IMAGE.REGION.MAPPING <<- "region_definitions_image.csv"
#' REMIND.REGION.MAPPING <<- "region_definitions_remind.csv"
#' load_iam_region_mappings()
#' regions
load_iam_region_mappings <- function() {
  # TODO: rewrite to function with return-only, instead of global variables

  if (exists("MESSAGE.REGION.MAPPING")) {
    f.regions.message <- here("data-raw", "iam_regions", MESSAGE.REGION.MAPPING)
    regions.message <<- vroom(f.regions.message, show_col_types=FALSE) %>%
      select(iso, region.message)
  }
  if (exists("IMAGE.REGION.MAPPING")) {
    f.regions.image <- here("data-raw", "iam_regions", IMAGE.REGION.MAPPING)
    regions.image <<- vroom(f.regions.image, show_col_types=FALSE) %>% select(iso, region.image)
  }
  if (exists("REMIND.REGION.MAPPING")) {
    f.regions.remind <- here("data-raw", "iam_regions", REMIND.REGION.MAPPING)
    regions.remind <<- vroom(f.regions.remind, show_col_types=FALSE) %>% select(iso, region.remind)
  }
  if (exists("MESSAGE.REGION.MAPPING") & exists("IMAGE.REGION.MAPPING") & exists("REMIND.REGION.MAPPING")) {
    regions <<- regions.message %>%
      left_join(regions.image, by = "iso") %>%
      left_join(regions.remind, by = "iso")
  }
}

### IAMC data ------------------------------------------------------------------


##### Filter data --------------------------------------------------------------

#' An imperfect filter function for IAMC data, allowing for wildcards (*).
#' Currently does not work for more than 2 wildcards.
#'
#' NB (to be fixed): this function does not work like pyam in that it does not respect only a wildcard in the front, for instance that "*Emissions|CO2" should end with "|CO2", rather it implicitly treats it as "*Emissions|CO2*"
#'
#' @param df
#' @param variable.string
#' @param lowercase.var.variable: variable or Variable
#'
#' @return df: filtered df
#' @export
#'
#' @examples
filter_wildcard_var <- function(df, variable.string,
                                lowercase.var.variable = F) {

  split.string <- str_split(string = variable.string, pattern = "\\*", n = Inf, simplify = FALSE)[[1]]
  n.split.string <- length(split.string)

  if (lowercase.var.variable) {
    # TODO: find a way to map this to go beyond 2 * characters
    if (n.split.string > 3) {
      message("Maximum wildcards that can be used in one string search is currently set to 2.")
    } else if (n.split.string == 2) {
      df <- df %>%
        filter(grepl(x = variable, pattern = split.string[1], fixed = T) & grepl(x = variable, pattern = split.string[2], fixed = T))
    } else if (n.split.string == 3) {
      df <- df %>%
        filter(grepl(x = variable, pattern = split.string[1], fixed = T) & grepl(x = variable, pattern = split.string[2], fixed = T) & grepl(x = variable, pattern = split.string[3], fixed = T))
    }
  } else {
    # TODO: find a way to map this to go beyond 2 * characters
    if (n.split.string > 3) {
      message("Maximum wildcards that can be used in one string search is currently set to 2.")
    } else if (n.split.string == 2) {
      df <- df %>%
        filter(grepl(x = Variable, pattern = split.string[1], fixed = T) & grepl(x = Variable, pattern = split.string[2], fixed = T))
    } else if (n.split.string == 3) {
      df <- df %>%
        filter(grepl(x = Variable, pattern = split.string[1], fixed = T) & grepl(x = Variable, pattern = split.string[2], fixed = T) & grepl(x = Variable, pattern = split.string[3], fixed = T))
    }
  }



  return(df)
}


##### Adjust IAMC column names: lower/upper case -------------------------------

#' Capatilised first letter of IAMC columns to all lowercase
#'
#' @param df: with all upper case first letter
#'
#' @return df: with all lower case
#' @export
#'
#' @examples
upper_to_lower <- function(df){
  return(
    df %>% rename(
      model = Model,
      scenario = Scenario,
      region = Region,
      variable = Variable,
      unit = Unit
    )
  )
}

#' All lower case of IAMC columns to capitalised first letter
#'
#' @param df: with all lower case
#'
#' @return df: with all upper case first letter
#' @export
#'
#' @examples
lower_to_upper <- function(df){
  return(
    df %>% rename(
      Model = model,
      Scenario = scenario,
      Region = region,
      Variable = variable,
      Unit = unit
    )
  )
}

#' All only capitalised first letter IAMC columns to all fully capitalised
#'
#' @param df: with all upper case first letter
#'
#' @return df: with all capitalised
#' @export
#'
#' @examples
upper_to_allcaps <- function(df){
  return(
    df %>% rename(
      MODEL = Model,
      SCENARIO = Scenario,
      REGION = Region,
      VARIABLE = Variable,
      UNIT = Unit
    )
  )
}

#' All lower case IAMC columns to all fully capitalised
#'
#' @param df: with all lower case
#'
#' @return df: with all capitalised
#' @export
#'
#' @examples
lower_to_allcaps <- function(df){
  return(
    df %>% rename(
      MODEL = model,
      SCENARIO = scenario,
      REGION = region,
      VARIABLE = variable,
      UNIT = unit
    )
  )
}

#' All fully capitalised IAMC columns to all only capitalised first letter
#'
#' @param df: with all capitalised
#'
#' @return df: with all upper case first letter
#' @export
#'
#' @examples
allcaps_to_upper <- function(df){
  return(
    df %>% rename(
      Model = MODEL,
      Scenario = SCENARIO,
      Region = REGION,
      Variable = VARIABLE,
      Unit = UNIT
    )
  )
}

#' All fully capitalised IAMC columns to all lower case
#'
#' @param df: with all capitalised
#'
#' @return df: with all lower case
#' @export
#'
#' @examples
allcaps_to_lower <- function(df){
  return(
    df %>% rename(
      model = MODEL,
      scenario = SCENARIO,
      region = REGION,
      variable = VARIABLE,
      unit = UNIT
    )
  )
}

##### Transform data format ----------------------------------------------------


#' Transform IAMC data from wide to long format
#'
#' Function assumes all five basic IAMC columns are there, and then the year columns (and nothing more)
#'
#' @param df
#' @param upper.to.lower: default = F (assume lower case), if T: converts only from first letter capitalized IAMC column names
#'
#' @return df: IAMC data in long format, always with lower case column names
#' @export
#'
#' @examples
#' population.data <- vroom(here(get_data_location_raw(test=FALSE),"scenario_data","population",POPULATION.PROJECTION.FILE), show_col_types=FALSE) %>%
#'     iamc_wide_to_long(upper.to.lower = F)
#'
iamc_wide_to_long <- function(df, upper.to.lower = F) {

  if (upper.to.lower) {
    df <- df %>%
      upper_to_lower()
  }

  first.year <- colnames(df)[6] # assumes all five basic IAMC columns are there, and nothing more
  last.year <- colnames(df)[length(colnames(df))]

  df <- df %>%
    pivot_longer(
      cols = all_of(first.year):all_of(last.year),
      names_to = "year",
      values_to = "value"
    ) %>%
    drop_na(value) %>%
    mutate(year = as.numeric(year))

  return(df)
}

#' Transform IAMC data from long to wide format
#'
#' Function assumes:
#' - columns: year, value
#' - (any other columns possible, and uniquely define values; e.g. five basic IAMC columns)
#'
#' @param df
#'
#' @return df: {IAMC} data in wide format
#' @export
#'
#' @examples
#' read_excel(here("data-raw", "scenario_data", "original_regional", "IMAGE-original-files", "Consolidated_IMAGE_SHAPE_DLS_results.xlsx"),sheet = "data") %>%
#' iamc_wide_to_long() %>%
#' iamc_long_to_wide()
#'
iamc_long_to_wide <- function(df){
  df <- df %>%
    pivot_wider(
      values_from = value,
      names_from = year
    )

  return(df)
}

##### Return unique occurernces of IAMC columns --------------------------------

#' Return unique variables
#'
#' @param df
#'
#' @return sorted list of unique variables
#' @export
#'
#' @examples
variable_unique <- function(df){
  return(
    df %>% pull(variable) %>% unique() %>% sort()
  )
}

#' Return unique units
#'
#' @param df
#'
#' @return sorted list of unique units
#' @export
#'
#' @examples
unit_unique <- function(df){
  return(
    df %>% pull(unit) %>% unique() %>% sort()
  )
}

#' Return unique years
#'
#' @param df
#'
#' @return sorted list of unique years
#' @export
#'
#' @examples
year_unique <- function(df){
  return(
    df %>% pull(year) %>% unique() %>% sort()
  )
}

#' Return unique regions
#'
#' @param df
#'
#' @return sorted list of unique regions
#' @export
#'
#' @examples
region_unique <- function(df){
  return(
    df %>% pull(region) %>% unique() %>% sort()
  )
}

#' Return unique model-scenario combinations
#'
#' @param df
#'
#' @return sorted list of unique model-scenario combinations
#' @export
#'
#' @examples
ms_unique <- function(df){
  return(
    df %>% mutate(`model-scenario`=paste0(model,"-",scenario)) %>% pull(`model-scenario`) %>% unique() %>% sort()
  )
}

#' Return unique scenario names
#'
#' @param df
#'
#' @return sorted list of unique scenario names
#' @export
#'
#' @examples
scenario_unique <- function(df){
  return(
    df %>% pull(`scenario`) %>% unique() %>% sort()
  )
}

#' Return unique Variables
#'
#' @param df
#'
#' @return sorted list of unique Variables
#' @export
#'
#' @examples
Variable_unique <- function(df){
  return(
    df %>% pull(Variable) %>% unique() %>% sort()
  )
}

#' Return unique Units
#'
#' @param df
#'
#' @return sorted list of unique Units
#' @export
#'
#' @examples
Unit_unique <- function(df){
  return(
    df %>% pull(Unit) %>% unique() %>% sort()
  )
}

#' Return unique Years
#'
#' @param df
#'
#' @return sorted list of unique Years
#' @export
#'
#' @examples
Year_unique <- function(df){
  return(
    df %>% pull(Year) %>% unique() %>% sort()
  )
}

#' Return unique Regions
#'
#' @param df
#'
#' @return sorted list of unique Regions
#' @export
#'
#' @examples
Region_unique <- function(df){
  return(
    df %>% pull(Region) %>% unique() %>% sort()
  )
}

#' Return unique Model-Scenario combinations
#'
#' @param df
#'
#' @return sorted list of unique Model-Scenario combinations
#' @export
#'
#' @examples
MS_unique <- function(df){
  return(
    df %>% mutate(`Model-Scenario`=paste0(Model,"-",Scenario)) %>% pull(`Model-Scenario`) %>% unique() %>% sort()
  )
}

#' Return unique Scenario names
#'
#' @param df
#'
#' @return sorted list of unique Scenario names
#' @export
#'
#' @examples
Scenario_unique <- function(df){
  return(
    df %>% pull(`Scenario`) %>% unique() %>% sort()
  )
}



##### Add model-scenario column ------------------------------------------------
#' Add "model-scenario" column to an IAMC data frame
#'
#' Assumes that the data frame has columns "model" and "scenario"
#'
#' @param df
#'
#' @return df with extra column "model-scenario"
#' @export
#'
#' @examples
ms_add <- function(df){
  return(
    df %>% mutate(`model-scenario`=paste0(model,"-",scenario))
  )
}

#' Add "Model-Scenario" column to an IAMC data frame
#'
#' Assumes that the IAMC data frame has columns "Model" and "Scenario"
#'
#' @param df
#'
#' @return df with extra column "Model-Scenario"
#' @export
#'
#' @examples
MS_add <- function(df){
  return(
    df %>% mutate(`Model-Scenario`=paste0(Model,"-",Scenario))
  )
}

##### Adjusting "value" --------------------------------------------------------

#' Normalise values in a long IAMC data frame to a starting year where the starting year value thus becomes 1
#'
#' @param df
#' @param starting.year
#'
#' @return
#' @export
#'
#' @examples
normalise_iamc_long <- function(df, starting.year) {

  # normalise the values in column "value" to the year "starting.year = 1"
  # - by (model, scenario, region, variable, unit)

  df.temp <- df %>%
    left_join(
      df %>% filter(year==starting.year) %>% rename(value.start=value) %>%
        select(model, scenario, region, variable, unit, value.start),
      by = c("model", "scenario", "region", "variable", "unit")
    ) %>%
    mutate(value=value/value.start)

  return(
    df.temp %>% select(-value.start)
  )

}

#' Divide all values by the value of the "Population" variable
#'
#' Assumes
#' - the IAMC data frame is in long format (with columns "model", "scenario", "region", "variable", "unit", "year", "value")
#' - the IAMC data frame has a variable called "Population" for all model-scenario combinations
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
to_per_capita <- function(df){
  pop <- df %>% filter(variable=="Population") %>% rename(pop=value) %>% select(model,scenario,region,year,pop)
  df <- df %>% filter(variable!="Population") %>%
    left_join(pop) %>%
    mutate(value=value/pop) %>%
    select(-pop)
  return(df)
}

#' Divide all values by the value of the GDP variable (e.g. "GDP (PPP)")
#'
#' Assumes
#' - the IAMC data frame is in long format (with columns "model", "scenario", "region", "variable", "unit", "year", "value")
#' - the IAMC data frame has the GDP variable variable for all model-scenario combinations
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
to_per_gdp <- function(df, vgdp = "GDP (PPP)"){
  gdp <- df %>% filter(variable==vgdp) %>% rename(gdp=value) %>% select(model,scenario,region,year,gdp)
  df <- df %>% filter(variable!=vgdp) %>%
    left_join(gdp) %>%
    mutate(value=value/gdp) %>%
    select(-gdp)
  return(df)
}

#' Add columns for the percentiles of the values across groups of the (IAMC) data frame
#'
#' Options:
#' - percentiles: vector of percentiles between 0 and 1
#' - group.cols: vector of column names to group by (default is c("model", "scenario", "variable", "year"))
#'
#' @param df
#' @param percentiles
#' @param group.cols
#'
#' @return same df, but with extra columns, called e.g. p25 and p75 for percentiles = c(0.25, 0.75)
#' @export
#'
#' @examples
add_percentile_columns <- function(df, percentiles = c(0.25, 0.5, 0.75),
                                   group.cols = c("model", "scenario", "variable", "year")) {
  # standard = across countries (iso is left out of group.cols)

  p_names <- map_chr(percentiles, ~ paste0("p", .x * 100))

  p_funs <- map(percentiles, ~ partial(quantile, probs = .x, na.rm = TRUE)) %>%
    set_names(nm = p_names)

  group.cols <- enquo(group.cols) # need to quote

  df.percentiles <- df %>%
    group_by_at(vars(!!group.cols)) %>%
    summarize_at(vars(value), .funs = p_funs)

  return(df %>% left_join(df.percentiles))
}

#' Extrapolate with constant values.
#' In the case that later years do not have data values, but NAs, and you want to extrapolate with constant values, try this function.
#'
#' Currently not used, nor tested. It is here for future use, and may or may not work.
#' - Its use was considered and tried out in `calculator_needs-dle-efficiency.R`, for industry in the SHAPE SDP scenarios.
#'
#' @param df
#' @param variable.to.fill
#'
#' @return
#' @export
#'
#' @examples
constant_year_fill <- function(df,
                               variable.to.fill="value"){
  df <- df %>%
    group_by(model,scenario,region,variable) %>%
    arrange(model,scenario,region,variable,year) %>%
    fill((!!as.name(variable.to.fill)),
         .direction = "down" #c("down", "up", "downup", "updown")
    ) %>%
    ungroup()
  return(df)
}

# Function wishes:
# - "annualise" function, using something like tidyr::complete(year=2000:2100), and na.approx


##### Aggregation functions -----------------------------------------------------
# here, or across ISO

# Function wishes:
# - a proper function for region aggregation (e.g. see SHAPE manuscript figures for an example)
# region_aggregate_sum <- function(df,
#   region.col.name,
#   group.cols = c("model", "scenario", "variable", "year")
# ){
#   # only works for one variable at the moment (& column must be named `value`)
#
#   group.cols <- c(group.cols, region.col.name)
#   group.cols <- enquo(group.cols) # need to quote
#
#   grp.df <- df %>%
#     group_by(vars(!!group.cols)) %>%
#     summarise(
#       value = sum(value)
#     )
#
#   return(grp.df)
# }



##### Adjusting "variable" string ----------------------------------------------

#' In the IAMC variable column, use enters instead of pipes to separate levels
#'
#' @param df
#'
#' @return df with altered variable column strings
#' @export
#'
#' @examples
iamc_variable_enterise_levels <- function(df){

  df <- df %>%
    mutate(
      variable = str_replace(variable, "\\|", "\n")
    )

  return(df)
}

#' Keep only one level (between pipes) of the IAMC variable column
#'
#' Note: does not check against creating duplicate variable names.
#'
#' @param df
#' @param level
#'
#' @return df with altered variable column strings
#' @export
#'
#' @examples
iamc_variable_keep_one_level <- function(df, level){

  df <- df %>%
    mutate(str.split = strsplit(variable, "|", fixed = TRUE)) %>%
    mutate(variable = sapply(str.split,
                             function(x) if (length(x) >= abs(level)) ifelse(level>0,x[[level]],x[[length(x)+level+1]]) else NA)) %>%
    select(-str.split)

  return(df)
}

#' Keep two levels (between pipes) of the IAMC variable column
#'
#' Note: does not check against creating duplicate variable names.
#'
#' @param df
#' @param levels
#'
#' @return df with altered variable column strings
#' @export
#'
#' @examples
iamc_variable_keep_two_levels <- function(df, levels){

  l1 <- levels[[1]]
  l2 <- levels[[2]]

  df <- df %>%
    mutate(str.split = strsplit(variable, "|", fixed = TRUE)) %>%
    mutate(variable = sapply(str.split, function(x) if (length(x) >= abs(l1)) ifelse(l1>0,x[[l1]],x[[length(x)+l1+1]]) else x[[length(x)]])) %>%
    mutate(variable = sapply(str.split, function(x) if (length(x) >= abs(l2)) paste0(ifelse(l1>0,x[[l1]],x[[length(x)+l1+1]]),
                                                                                     "|",
                                                                                     ifelse(l2>0,x[[l2]],x[[length(x)+l2+1]])) else x[[length(x)]])) %>%
    select(-str.split)

  return(df)
}

##### Climate and AR6 specific functions ---------------------------------------
#' Load a variable from a climate assessment file
#'
#' Uses utils function filter_wildcard_var() if a wildcard (*) is used in the variable string
#'
#' Function is currently not used, nor tested. It is here for future use, and may or may not work.
#'
#' @param variable
#' @param file
#' @param key.var
#'
#' @return
#' @export
#'
#' @examples
load_climate_var <- function(variable, file = CLIMATE.EMULATOR.FILE, key.var = F) {
  # key.var could be used to filter faster from the smaller snapshot with key climate file

  full.climate.data <- vroom(
    here("data-raw", "scenario_data", "climate", file), show_col_types=FALSE
  )

  if (grepl(pattern = "\\*", x = variable)) {
    df <- filter_wildcard_var(
      full.climate.data,
      variable.string = variable
    )
  } else {
    df <- full.climate.data %>%
      filter(
        Variable == variable
      )
  }

  return(df %>%
           iamc_wide_to_long(upper.to.lower = T))
}

### SSP data -------------------------------------------------------------------

##### Population data ----------------------------------------------------------
#' Load population data from IIASA-WIC original SSP projections (with Taiwan added from OECD_Env-growth)
#' Population by country returned in millions.
#'
#' @param ssp: string of one SSP (out of "SSP1", "SSP2", "SSP3", "SSP4", "SSP5") or a combination of them, e.g. c("SSP1", "SSP2")
#'
#' @return
#' @export
#'
#' @examples
load_population_projection <- function(ssp) {
  if (is.vector(ssp) & length(ssp) > 1) {
    p <- read_excel(here("data-raw", "iamc_db_SSP_population.xlsx")) %>%
      filter(Model == "IIASA-WiC POP", Scenario %in% ssp) %>%
      select(-Variable, -Model) %>%
      pivot_longer(cols = `2010.0`:`2100.0`, names_to = "year", values_to = "population") %>%
      mutate(year = as.numeric(year), pop_mil = population) %>%
      select(-Notes, -Unit) %>%
      rename(
        iso = Region,
        scenario.mapping = Scenario
      )

    # Given that IIASA-WIC does not report data for Taiwan, we use data from "OECD_Env-Growth" (source: https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome)
    p.twn <- vroom(here("data-raw", "twn_oecd_ssp_pop.csv"), show_col_types=FALSE) %>%
      filter(Scenario %in% ssp) %>%
      select(-Variable, -Model) %>%
      pivot_longer(cols = `2010`:`2100`, names_to = "year", values_to = "population") %>%
      mutate(year = as.numeric(year), pop_mil = population) %>%
      select(-Unit) %>%
      rename(
        iso = Region,
        scenario.mapping = Scenario
      )

    p <- p %>% bind_rows(p.twn)
  } else if (ssp %in% c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")) {
    p <- read_excel(here("data-raw", "iamc_db_SSP_population.xlsx")) %>%
      filter(Model == "IIASA-WiC POP", Scenario == ssp) %>%
      select(-Variable, -Model, -Scenario) %>%
      pivot_longer(cols = `2010.0`:`2100.0`, names_to = "year", values_to = "population") %>%
      mutate(year = as.numeric(year), pop_mil = population) %>%
      select(-Notes, -Unit) %>%
      rename(iso = Region)

    # Given that IIASA-WIC does not report data for Taiwan, we use data from "OECD_Env-Growth" (source: https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome)
    p.twn <- vroom(here("data-raw", "twn_oecd_ssp_pop.csv"), show_col_types=FALSE) %>%
      filter(Scenario == ssp) %>%
      select(-Variable, -Model, -Scenario) %>%
      pivot_longer(cols = `2010`:`2100`, names_to = "year", values_to = "population") %>%
      mutate(year = as.numeric(year), pop_mil = population) %>%
      select(-Unit) %>%
      rename(iso = Region)

    p <- p %>% bind_rows(p.twn)
  } else if (ssp == "all") {
    p <- read_excel(here("data-raw", "iamc_db_SSP_population.xlsx")) %>%
      filter(Model == "IIASA-WiC POP") %>%
      select(-Variable, -Model) %>%
      pivot_longer(cols = `2010.0`:`2100.0`, names_to = "year", values_to = "population") %>%
      mutate(year = as.numeric(year), pop_mil = population) %>%
      select(-Notes, -Unit) %>%
      rename(
        iso = Region,
        scenario.mapping = Scenario
      )

    # Given that IIASA-WIC does not report data for Taiwan, we use data from "OECD_Env-Growth" (source: https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome)
    p.twn <- vroom(here("data-raw", "twn_oecd_ssp_pop.csv"), show_col_types=FALSE) %>%
      select(-Variable, -Model) %>%
      pivot_longer(cols = `2010`:`2100`, names_to = "year", values_to = "population") %>%
      mutate(year = as.numeric(year), pop_mil = population) %>%
      select(-Unit) %>%
      rename(
        iso = Region,
        scenario.mapping = Scenario
      )

    p <- p %>% bind_rows(p.twn)
  }

  return(p)
}

##### Urbanisation data ---------------------------------------------------------
#' Load urbanisation data from original SSP projections (with Taiwan added from OECD_Env-growth)
#' Urbanisation data is returned as a rate between 0 and 1 (percentage of the total population).
#'
#' @param ssp: string of one SSP (out of "SSP1", "SSP2", "SSP3", "SSP4", "SSP5") or a combination of them, e.g. c("SSP1", "SSP2")
#'
#' @return
#' @export
#'
#' @examples
#' load_urbanisation_projection(ssp = c("SSP1", "SSP2"))
load_urbanisation_projection <- function(ssp) {
  if (is.vector(ssp) & length(ssp) > 1) {
    u <- read_excel(here("data-raw", "iamc_db_SSP_urbanshare.xlsx")) %>%
      select(-Variable, -Model) %>%
      filter(Scenario %in% ssp) %>%
      pivot_longer(cols = `2010.0`:`2100.0`, names_to = "year", values_to = "urb.rate") %>%
      mutate(year = as.numeric(year), urb.rate = urb.rate / 100) %>%
      select(-Notes, -Unit) %>%
      rename(
        iso = Region,
        scenario.mapping = Scenario
      )
  } else if (ssp %in% c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")) {
    u <- read_excel(here("data-raw", "iamc_db_SSP_urbanshare.xlsx")) %>%
      filter(Scenario == ssp) %>%
      select(-Variable, -Model, -Scenario) %>%
      pivot_longer(cols = `2010.0`:`2100.0`, names_to = "year", values_to = "urb.rate") %>%
      mutate(year = as.numeric(year), urb.rate = urb.rate / 100) %>%
      select(-Notes, -Unit) %>%
      rename(iso = Region)
  } else if (ssp == "all") {
    u <- read_excel(here("data-raw", "iamc_db_SSP_urbanshare.xlsx")) %>%
      select(-Variable, -Model) %>%
      pivot_longer(cols = `2010.0`:`2100.0`, names_to = "year", values_to = "urb.rate") %>%
      mutate(year = as.numeric(year), urb.rate = urb.rate / 100) %>%
      select(-Notes, -Unit) %>%
      rename(
        iso = Region,
        scenario.mapping = Scenario
      )
  }

  return(u)
}


# Code development utils -------------------------------------------------------

### Testing utils ----------------------------------------------------------------

#' Remove all files produced by running the tests from the test data output directory
#'
#' @return
#' @export
#'
#' @examples
remove_test_data_output <- function() {
  output.files.from.running.tests <- list.files(here(get_data_location(test=TRUE)))
  output.files.from.running.tests <- output.files.from.running.tests[! output.files.from.running.tests == "README.txt"]
  output.files.from.running.tests <- file.path(here(get_data_location(test=TRUE)),
                                               output.files.from.running.tests)

  file.remove(output.files.from.running.tests)

  print(paste0("Removed ", length(output.files.from.running.tests), " files from ", here(get_data_location(test=TRUE))))
}

### Coding style utils ---------------------------------------------------------

#' Clean up the code in this directory, following tidyverse style
#'
#' NOTE: can change the code quite a lot, but most often only enters and spaces
#'
#' @return
#' @export
#'
#' @examples
clean_code_style <- function() {
  library(styler)
  styler::style_dir()
}



# Generic utils ----------------------------------------------------------------

### Country classifications ----------------------------------------------------

#' Load categories that countries fall into.
#' This country classification can be useful for region aggregation of country-level results.
#'
#' Groupings available (grouping.to.load) are:
#' "region_ar6_6_ipcc_fgd": IPCC R6 (AR6 version)
#' "region_ar6_10_ipcc_fgd": IPCC R10 (AR6 version)
#' "region_ar6_22_ipcc_fgd": IPCC R22 (AR6 version)
#' "M49_Hi_M49_Regions": UN Statistics Division M49 High level of aggregation
#' "M49_Med__M49_Regions": UN Statistics Division M49 Medium level of aggregation
#' "M49_lo_M49_Regions": UN Statistics Division M49 Low level of aggregation
#' "Developing_2021_M49_other": UN Statistics Division M49 Developing/Developed countries classification
#' "SIDS_M49_other": UN Statistics Division M49 Small Island Developing States classification
#' "LLDC_M49_other": UN Statistics Division M49 Landlocked Developing Countries classification
#' "LDC_M49_other": UN Statistics Division M49 Least Developed Countries classification
#' "Annex_I_unfccc": UNFCCC Annex I countries classification
#' "Annex _II_unfccc": UNFCCC Annex II countries classification
#' "WMO": World Meteorological Organization classification
#' "EU": European Union classification (post-brexit; EU-27)
#' "OECD": Organization for Economic Co-operation and Development classification
#' "Income _status_WB": World Bank Income Status classification
#' "Former Soviet Union": Former Soviet Union countries (15 countries)
#' "iamc_r5": Integrated Assessment Modeling Consortium (IAMC) R5 regions
#' "iamc_r10": Integrated Assessment Modeling Consortium (IAMC) R10 regions
#' "iamc_r11": Integrated Assessment Modeling Consortium (IAMC) R11 regions
#' "iamc_r12": Integrated Assessment Modeling Consortium (IAMC) R12 regions
#'
#' @param grouping.to.load
#' @param keep.long.names: F (default), T: include longer names besides iso3c in the output
#'
#' @return
#' @export
#'
#' @examples
load_official_country_grouping <- function(grouping.to.load,
                                           keep.long.names = F) {
  #' Load grouping of ISO (with longer names, which are removed by default)

  if (is.na(grouping.to.load) & !keep.long.names) {
    stop("Please specify a grouping to load.")
  } else if (is.na(grouping.to.load) & keep.long.names) {
    grp <- vroom(
        here("data", "countrygroupings_2024_01.csv"), show_col_types=FALSE
      ) %>%
      rename(iso = ISO) %>%
      select(
        iso,
        name
      )
  } else {
    grp <- vroom(
      here("data", "countrygroupings_2024_01.csv"), show_col_types=FALSE
    ) %>%
      rename(iso = ISO) %>%
      left_join(
        vroom(
          here("data", "iso3c_region_mapping.csv"), show_col_types=FALSE
        ) %>%
          rename(iso = iso3c) %>%
          select(
            iso,
            iamc_r5, iamc_r10, iamc_r11, iamc_r12
          ),
        by = "iso"
      ) %>%
      select(
        iso,
        name,
        !!as.symbol(grouping.to.load)
      )
  }


  if (!keep.long.names) {
    grp <- grp %>% select(-name)
  }


  return(grp)
}


#' Add long country names alongside an iso3c column
#'
#' Takes a dataframe with an `iso` column and adds a `name` column to it with full names for the country.
#'
#' @return a dataframe with iso3c and long names
#' @export
#'
#' @examples df %>% left_join(load_long_names_of_iso3c())
load_long_names_of_iso3c <- function(){
  return(
    load_official_country_grouping(grouping.to.load = NA, keep.long.names = T)
  )
}


### Visualisation utils --------------------------------------------------------

##### Plotting style -----------------------------------------------------------
#' Apply Jarmo's custom plotting style
#'
#' @param
#'
#' @return
#' @export
#'
#' @examples
#' # example ggplot
#' ggplot(s %>% filter(variable=="Primary Energy|Gas")),
#'     aes(x=year,y=value,colour=variable,linetype=scenario,group=interaction(model,scenario,region,variable))) +
#'     facet_wrap(~region, scales = "free_y") +
#'     geom_line() +
#'     theme_jsk() +
#'     ylab("Primary Energy Gas")
theme_jsk <- function() {

  theme_classic() + theme_hc() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_blank()
    )

}

#' Apply background colour to historical values (in a timeseries plot)
#'
#' @param
#'
#' @return
#' @export
#'
#' @examples
#' # example ggplot
#' ggplot(s %>% filter(variable=="Primary Energy|Gas")),
#'     aes(x=year,y=value,colour=variable,linetype=scenario,group=interaction(model,scenario,region,variable))) +
#'     facet_wrap(~region, scales = "free_y") +
#'     mark_history() +
#'     geom_line()
mark_history <- function(sy=STARTYEAR){
  annotate("rect", xmin=-Inf, xmax=sy, ymin=-Inf, ymax=Inf, alpha=0.2, fill="lightgrey", colour=NA)
}

##### Adjusting plots ----------------------------------------------------------
#' Desaturate colors by specified proportion
#'
#' @param cols
#' @param sat
#'
#' @return
#' @export
#'
#' @examples
#' # example using colours from the brewer.pal function of RColorBrewer
#' c.pal <- desat(brewer.pal(n = 12, name = "Set3"), 1.5)
desat <- function(cols, sat = 0.5) {
  X <- diag(c(1, sat, 1)) %*% rgb2hsv(col2rgb(cols))
  hsv(X[1, ], X[2, ], X[3, ])
}


##### Saving plots -------------------------------------------------------------
#' Save ggplot objects as both PNG and PDF (or one of the two)
#'
#' @param p: ggplot object
#' @param f: filename
#' @param h: height
#' @param w: width
#' @param format: "png-pdf" (default) or "png" or "pdf"
#' @param unit: "mm" (default) or "cm" or "in" or "px"
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' p <- ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width)) + geom_point()
#' save_ggplot(p=p, f="testplotname", h=150, w=150, format="png-pdf", unit="mm")
save_ggplot = function(p,f,h=150,w=150,format="png-pdf",unit="mm",
                       ...){
  if(format=="png-pdf"){
    ggsave(
      plot = p,
      file = paste0(f,".png"),
      height = h,
      width = w,
      unit = unit,
      ...
    )
    ggsave(
      plot = p,
      file = paste0(f,".pdf"), device = cairo_pdf,
      height = h,
      width = w,
      unit = unit,
      ...
    )
  } else if (format=="png") {
    ggsave(
      plot = p,
      file = paste0(f,".png"),
      height = h,
      width = w,
      unit = unit,
      ...
    )
  } else if (format=="pdf") {
    ggsave(
      plot = p,
      file = paste0(f,".pdf"), device = cairo_pdf,
      height = h,
      width = w,
      unit = unit,
      ...
    )
  }
}

### Data manipulation ----------------------------------------------------------
# The opposite of %in%, to be used in a dplyr::filter() call
`%nin%` <- Negate(`%in%`)

#' Conditional manipulation, of subsets of data with dplyr
#'
#' @param .data
#' @param condition
#' @param ...
#' @param envir
#'
#' @return same dataframe format but with the mutations as specified
#' @export
#'
#' @examples
#' DF.tei %>% mutate_cond(elec == "non.elec", e.int = 0)
#' df %>% mutate_cond(variable == "Final Energy|Industry", variable = "Industry")
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

#' Interpolate NA values by year
#'
#' Currently not used, nor tested. It is here for future use, and may or may not work.
#'
#' NB: could be made specific for IAMC data. At the moment it is not, although it assumes the column called "value" to be the data column to be interpolated.
#'
#' @param df
#' @param mg
#' @param r
#'
#' @return
#' @export
#'
#' @examples
interpolate_NA_annual <- function(df, mg = Inf, r = 2) {
  return(
    df %>%
      mutate(
        value = na.approx(value, maxgap = mg, rule = r)
      )
  )
}



### R/RStudio behaviour --------------------------------------------------------
#' Silences R output of a function x
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' quiet(load_dimensions())
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}


### Formatting other data sources ----------------------------------------------

##### World Bank data ----------------------------------------------------------

#' Parse World Bank wide data (already loaded), to long format, aligning closer to long format IAMC style
#'
#' @param df
#'
#' @return long dataframe, with the collumns iso, indicator, year, value
#' @export
#'
#' @examples
wb_parse <- function(df){

  return(
    df %>%
      select(`Country Code`, `Indicator Name`, where(is.numeric)) %>%
      rename(iso = `Country Code`, indicator = `Indicator Name`) %>%
      pivot_longer(names_to = "year", values_to = "value", cols = -c("iso", "indicator")) %>%
      mutate(year = as.numeric(year)) %>%
      arrange(iso,indicator,year) %>%
      drop_na()
  )

}
