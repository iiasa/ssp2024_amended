# ssp2024_amended: compare new population update to SSPv3.1 data
#' Produced by Jarmo Kikstra
#'
#' Latest update: 23.04.2025
#' - initial version, based on data shared by Samir KC on
#' Latest update: 25.04.2025
#' - split out combining of dataframe of new data for GDP calculation

#'
#' Notes (23.04.2025, version 'V15 rebase')
#' - setdiff(v3_1.regions, new.regions); "Micronesia (Federated States of)" "United States Virgin Islands" --> both still there but have been renamed
#' - unit [variable: 'Population']: you say this is millions but it looks more like thousands
#' - max changes population: +15mln in India SSP1, -17mln in India SSP3
#' - percentage changes are in the order of


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

# Compare against v3.1 ---------------------------------------------------------

source(here("R", "KOSOVO_ISO3CODE.R"))

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
# we won't need gdp in this script
v3_1 <- v3_1 %>%
  filter(variable %nin% c("GDP|PPP", "GDP|PPP [per capita]"))

### new ------------------------------------------------------------------------
source(here("R","create_new_iiasa_population_combined_dataframe.R"))

## Compare: check formats - same variables, more countries ---------------------

# same variables
expect_equal(v3_1 %>% variable_unique(),
             new %>% variable_unique())

# covering _at least_ all the same regions as before [region name]
v3_1.regions <- v3_1 %>% region_unique()
new.regions <- new %>% region_unique()
setdiff(v3_1.regions, new.regions) # in old, not in new
setdiff(new.regions, v3_1.regions) # in new, not in old

# covering _at least_ all the same regions as before [iso]
v3_1.iso <- v3_1 %>% pull(iso) %>% unique()
new.iso <- new %>% pull(iso) %>% unique()
setdiff(v3_1.iso, new.iso) # in old, not in new
setdiff(new.iso, v3_1.iso) # in new, not in old
expect_equal(length(setdiff(new.iso, v3_1.iso)),
             1)
expect_equal(setdiff(new.iso, v3_1.iso),
             KOSOVO.ISO3.CODE)

# more countries
expect_gt(length(new.iso),
          length(v3_1.iso))

# more rows
expect_gt(nrow(new),
          nrow(v3_1))

# same information per country
expect_equal(nrow(new)/length(new.iso),
             nrow(v3_1)/length(v3_1.iso))

## Compare: plot total population ----------------------------------------------

### Global total (aggregate countries), until 2100 ----
for (var in variables.to.investigate){
  for (y.version in c("2015_2030", "until2100")){

    df <- bind_rows(
      new %>% mutate(version = new.version),
      v3_1 %>% mutate(version = "v3.1")
    ) %>% filter(variable==var) %>%
      reframe(
        value = sum(value),
        region = "World (aggregated countries)",
        .by = c(model,scenario,variable,unit,year,version)
      )

    p.ts.world <- ggplot(
      df %>% mutate(model=paste0(model,"-",version)) %>% normalise_iamc_long(starting.year = 2025),
      aes(x=year,y=value,colour=version,linetype=version)
    ) +
      facet_wrap(scenario~., ncol = 5) +
      geom_line() +
      theme_jsk() +
      labs(title = "World (aggregated countries)",
           y = "Index (1=2025)") +
      theme(strip.placement = "outside",
            axis.text = element_text(size = 8))

    p.ts.world

    save_ggplot(
      p = p.ts.world,
      f = file.path(out.path.figures, paste0(clean_string(var),"_",y.version, "_World")),
      w = 150,
      h = 65,
      format = "pdf",
      limitsize = FALSE
    )
  }
}
df <- bind_rows(
  new %>% mutate(version = new.version),
  v3_1 %>% mutate(version = "v3.1")
) %>% filter(variable==var,
             iso == i)

### Per country ----
for (var in variables.to.investigate){
  for (i in new.iso){
    for (y.version in c("2015_2030", "until2100")){

      df <- bind_rows(
        new %>% mutate(version = new.version),
        v3_1 %>% mutate(version = "v3.1")
        ) %>% filter(variable==var,
                   iso == i)
      if (y.version=="2015_2030"){
        df <- df %>% filter(
          year>=2015,
          year<=2030
        )
      }

      p.ts <- ggplot(
        df,
        aes(x=year,y=value,colour=version,linetype=version)
      ) +
        facet_wrap(scenario~region, ncol = 5,
                   scales = "free", strip.position = "top") +
        geom_line() +
        theme_jsk() +
        theme(strip.placement = "outside",
              axis.text = element_text(size = 8))

      save_ggplot(
        p = p.ts,
        f = file.path(out.path.figures, "country_pdfs", paste0(clean_string(var),"_",y.version,"_", i)),
        w = 150,
        h = 65,
        format = "pdf",
        limitsize = FALSE
      )
    }

  }
}
for (var in variables.to.investigate){
  for (y.version in c("2015_2030", "until2100")){
    path.model.out <- file.path(out.path.figures, "combined_pdfs")
    path.model.in <- file.path(out.path.figures, "country_pdfs")

    FILES.pdf <- file.path(path.model.in, dir(path.model.in, pattern = ("*.pdf")))  # get file names

    OUT_FILE_NAME <- file.path(path.model.out, paste0(clean_string(var),"_",y.version,"_combined.pdf"))

    qpdf::pdf_combine(
      input = FILES.pdf[((grepl(FILES.pdf, pattern=y.version,fixed=T)))],
      output = OUT_FILE_NAME
    )

  }
}





### differences (quantitative) ----

for (var in variables.to.investigate){
  diff <- bind_rows(
    new %>% mutate(version = new.version),
    v3_1 %>% mutate(version = "v3_1")
  ) %>%
    filter(variable==var) %>%
    pivot_wider(names_from = version, values_from = value)
  diff$difference <- diff[[new.version]] - diff$v3_1
  diff$difference_percentage <- (diff[[new.version]] - diff$v3_1)/diff$v3_1 * 100

  write_delim(x = diff,
              file = file.path(out.path.data, paste0("diff_",clean_string(var),".csv")),
              delim = ",")
  # only diff bigger than 0.1%
  write_delim(x = diff %>% filter(difference_percentage > 0.1),
              file = file.path(out.path.data, paste0("diff_",clean_string(var),"_largerthan0_1p.csv")),
              delim = ",")

  # add plots
  for (y in c("2025", "2100", "all-years")){
    if (y=="all-years"){
      df <- diff %>% filter(variable==var)
    } else {
      df <- diff %>% filter(variable==var, year==as.numeric(y))
    }

    p.diff <- ggplot(
      df,
      aes(x=difference_percentage,y=difference,colour=scenario)
    ) +
      geom_point(alpha=0.1,aes(size=v3_1)) +
      labs(
        y = "Difference in millions",
        x = "Difference in percentage",
        title = var,
        subtitle = y,
        caption = paste0(new.version, " compared to v3.1")
      )

    save_ggplot(
      p = p.diff,
      f = file.path(out.path.figures, paste0("diff_",clean_string(var),"_", y)),
      w = 150,
      h = 150,
      format = "pdf"
    )

  }


}

