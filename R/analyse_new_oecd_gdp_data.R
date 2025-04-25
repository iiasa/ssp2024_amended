# first run `R/create_new_oecd_gdp_percapita.R`

new.version <- "v20250417_gdp"
out.path.figures <- here("output", new.version, "figures")
dir.create(out.path.figures, recursive = T)
out.path.data <- here("output", new.version, "data")
dir.create(out.path.data, recursive = T)
variables.to.investigate <- c("GDP|PPP [per capita]", "GDP|PPP")

# Load ----
year.columns <- c("1950", "1955", "1960", "1965", "1970", "1975", "1980", "1985",
                  "1990", "1995", "2000", "2005", "2010", "2015", "2020", "2025",
                  "2030", "2035", "2040", "2045", "2050", "2055", "2060", "2065",
                  "2070", "2075", "2080", "2085", "2090", "2095", "2100")
v3_1_path <- here("data", "1721734326790-ssp_basic_drivers_release_3.1_full.xlsx")
v3_1.percapita <- read_excel(
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
  filter(variable %in% variables.to.investigate,
         model == oecd2023name)

new.gdp <- read_csv(
  here("output", new.version, "data", "GDP_PPP_longformat.csv")
)
new.gdp.per.capita <- read_csv(
  here("output", new.version, "data", "GDP_PPP_percapita_longformat.csv")
)

# Differences ----

### differences (quantitative) ----

for (var in variables.to.investigate){
  diff <- bind_rows(
    bind_rows(
      new.gdp,
      new.gdp.per.capita
    ) %>% mutate(version = new.version),
    v3_1.percapita %>% mutate(version = "v3_1")
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
        y = paste0("Difference in ", diff %>% pull(unit) %>% unique() %>% as.character()),
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

## Compare: plot total GDP ----------------------------------------------

### Global total (aggregate countries), until 2100 ----
for (var in c("GDP|PPP")){
  for (y.version in c("until2100")){

    df <- bind_rows(
      bind_rows(
        new.gdp,
        new.gdp.per.capita
      ) %>% mutate(version = new.version),
      v3_1.percapita %>% mutate(version = "v3_1")
    ) %>% filter(variable==var) %>%
      reframe(
        value = sum(value),
        region = "World (aggregated countries)",
        .by = c(scenario,variable,unit,year,version)
      )

    p.ts.world <- ggplot(
      df %>% mutate(model=paste0(version)) %>% normalise_iamc_long(starting.year = 2025) %>%
        filter(year>=2025),
      aes(x=year,y=value,colour=version,linetype=version,
          group)
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
### Per country ----
new.iso <- new.gdp.per.capita %>% pull(iso) %>% unique() %>% sort()
for (var in variables.to.investigate){
  for (i in new.iso){
    for (y.version in c("until2100")){

      df <- bind_rows(
        bind_rows(
          new.gdp,
          new.gdp.per.capita
        ) %>% mutate(version = new.version),
        v3_1.percapita %>% mutate(version = "v3_1")
      ) %>% filter(variable==var,
                   iso == i)

      p.ts <- ggplot(
        df %>% filter(year>=2025),
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
  for (y.version in c("until2100")){
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
