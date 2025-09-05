###
# Simulation: Trump tariff simulator
# 250904
###

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table)
p_load(readr)
p_load(countrycode)
p_load(stringr)
p_load(arrow)
if (!require("KITE")) devtools::install_local("code/KITE_24.01.tar.gz")
library(KITE) # main branch, v24.01
suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
  library(rvest)
  library(xml2)
  library(countrycode)
})
pacman::p_load(doParallel)

# functions
source("input/countrygroups.R")

`%nin%` <- Negate(`%in%`)

match_replace <- function (code, from, to, dictionary) {
  dictionary[match(code, dictionary[[from]]),][[to]]
}

# 0 - custom functions and settings ----

# functions
`%nin%` <- Negate(`%in%`)

# 1 - load data ----

## 1.1 - load initial conditions ----
initial_conditions = read_rds(str_c("input/initial_conditions_GTAP11_241129.rds"))
GTAP_sectors = fread("input/sectorlist_gtap_de.csv")

## 1.2 get NYT tariff tracker ----
if(!file.exists("input/nyt_tariffs_2025-07-28.csv")) {
  nyt_url <- "https://www.nytimes.com/interactive/2025/07/28/business/economy/trump-tariff-tracker.html"
  doc <- read_html(nyt_url)
  data_nyt <- html_table(doc, fill = TRUE)
  data_nyt = data_nyt[[1]] %>% setDT
  colnames(data_nyt)[1] = "country"

  # get status ----
  status_levels <- c("Baselinetariff", "New rateimposed", "Upcomingdeadline", "Deal rate")
  pat <- paste0("(", paste(status_levels, collapse = "|"), ")$")

  # Extract status (the tag at the end of V1)
  data_nyt[, status := sub(paste0(".*\\s", pat), "\\1", country, perl = TRUE)]
  # Extract country (V1 without the trailing status tag)
  data_nyt[, country := trimws(sub(paste0("\\s+", pat), "", country, perl = TRUE))]

  # 3. Clean Total: remove everything except numbers and dot, convert to numeric
  data_nyt[, total := as.numeric(gsub("[^0-9.]+", "", Total))]

  data_nyt = data_nyt[,.(country, status, value = total)]

  # transform country names into ISO3c
  data_nyt[, country := countrycode(country, "country.name", "iso3c", warn = TRUE)]

  # transform rate
  data_nyt[, value := 1 + (value/100)]

  # save tariffs
  fwrite(data_nyt, "input/nyt_tariffs_2025-07-28.csv")
}
data_nyt = fread("input/nyt_tariffs_2025-07-28.csv")

# 2 - scenarios ----
scenario = list()

## 2.1 - Scenario 1: trump tariffs ----
scenario[["trump_tariffs"]]$tariff_new = copy(initial_conditions$tariff)

### countries ----
scenario[["trump_tariffs"]]$tariff_new = merge(scenario[["trump_tariffs"]]$tariff_new,
                                               data_nyt,
                                               by.x = "origin",
                                               by.y = "country",
                                               all.x = TRUE) %>%
  setnames(c("value.x", "value.y"), c("value", "value_nyt"))

scenario[["trump_tariffs"]]$tariff_new[destination == "USA" & origin != "USA" &
                                         sector %nin% GTAP_sectors[Category %in% c("Services"), Code],
                                       value := ifelse(is.na(value_nyt), value, value_nyt)]

# account for USMCA
scenario[["trump_tariffs"]]$tariff_new[origin %in% c("MEX", "CAN") & destination == "USA" &
                                         sector %nin% GTAP_sectors[Category %in% c("Services"), Code],
                                       value := 0.5*value + 0.5*1] # 0% tariffs on USMCA imports, around 50% of US imports from MEX and CAN

# China retaliation
scenario[["trump_tariffs"]]$tariff_new[destination == "CHN" & origin == "USA" &
                                         sector %nin% GTAP_sectors[Category %in% c("Services"), Code],
                                       value := 1.1]

### sectoral ----
sector_tariffs_trump_tariffs = c("i_s", "nfm", "fmp", "mvh") # Steel and aluminum, automotives
sectors_exempted = c("lum", "bph", "oil", "gas", "coa", "p_c", "ely", "gdt", "ele") # lumber, pharmacueticals, energy products, also computers and smartphones
# what about semiconducters, cooper, and critical minerals?

# Steel & Aluminum
scenario[["trump_tariffs"]]$tariff_new[origin != "USA" & destination == "USA" &
                                         status %nin% c("Deal rate") &
                                         origin %nin% c("MEX", "CAN") &
                                         sector %in% c("i_s", "nfm", "fmp"),
                                       value := 1.5] # 50% tariffs on all steel/aluminum

# Automotives
scenario[["trump_tariffs"]]$tariff_new[origin != "USA" & destination == "USA" &
                                         sector %in% c("mvh") &
                                         status %nin% c("Deal rate") &
                                         origin %nin% c("MEX", "CAN"),
                                       value := value + 0.25] # 25% tariffs on automotives

## EU-US deal with sectoral exemptions (and zero in return!?) ----
scenario[["trump_tariffs"]]$tariff_new[origin == "USA"  & destination %in% EU27 &
                                         sector %nin% GTAP_sectors[Category %in% c("Services"), Code],
                                       value := 1] # 0% tariff on US exports

sector_steel = c("i_s", "nfm", "fmp") # Steel and aluminum
sectors_exempted_EU = c("otn", "ele") # other transport (ie aerospace), chips exempted

# semiconductors
scenario[["trump_tariffs"]]$tariff_new[origin %in% EU27 & destination == "USA" &
                                         sector %in% "ele",
                                       value := 0.5*value + 0.5*1] # 0% tariff on semiconductors (which are about 50% of ele for US imports from EU)
scenario[["trump_tariffs"]]$tariff_new[destination %in% EU27 & origin == "USA" &
                                         sector %in% "ele",
                                       value := 0.4*value + 0.6*1] # 0% tariff on semiconductors (which are about 50% of ele for US imports from EU)

# aeronautics
scenario[["trump_tariffs"]]$tariff_new[origin %in% EU27 & destination == "USA" &
                                         sector %in% "otn",
                                       value := 0.1*value + 0.9*1] # 0% tariff on aeronautics (which are about 90% of US and EU bilateral exports in otn)
scenario[["trump_tariffs"]]$tariff_new[destination %in% EU27 & origin == "USA" &
                                         sector %in% "otn",
                                       value := 0.2*value + 0.8*1] # 0% tariff on aeronautics (which are about 90% of US and EU bilateral exports in otn)

# clean table
scenario[["trump_tariffs"]]$tariff_new = scenario[["trump_tariffs"]]$tariff_new[,.(origin, destination, sector, value)]


# 3 - counterfactuals ----
focus_countries = c("CAN", "CHN", "MEX", "JPN", "EU27")
tariff_sequence = seq(1, 1.5, .1)

# get benchmarks Trump tariffs
benchmarks <- data_nyt[country %in% c(focus_countries),.(country, value)] %>% rbind(data_nyt[country %in% EU27, .(country = "EU27", value = mean(value, na.rm=TRUE))])

# Build the full combinations grid
rate_lists <- setNames(
  lapply(benchmarks$country, function(cty) {
    sort(unique(c(tariff_sequence, benchmarks[country == cty, value])))
  }),
  benchmarks$country
)

# (Sanity) print per-country levels
print(rate_lists)

combo_grid <- do.call(CJ, rate_lists)  # columns: CAN, CHN, MEX, JPN, EU27

# Helper for naming files like "CAN135_CHN110_MEX100_JPN115_EU27_120"
name_combo <- function(row_named) {
  paste(sprintf("%s-%02d", names(row_named), round((row_named - 1) * 100)),
        collapse = "_")
}

# parallel loop
UseCores = detectCores() - 1 # how many cores to use
cl = parallel::makeCluster(UseCores, setup_strategy = "sequential") # register CoreCluster
registerDoParallel(cl)

gc()
foreach(i = seq_len(nrow(combo_grid)),
        .packages = c("KITE", "readr", "data.table", "stringr")) %dopar% {

  combo_vec <- as.numeric(combo_grid[i])
  names(combo_vec) <- names(combo_grid)
  fn <- sprintf("temp/results/counterfactuals/%s.csv", name_combo(combo_vec))
  if(file.exists(fn)) next

  tariff_temp = copy(scenario[["trump_tariffs"]]$tariff_new)

  # Apply ALL five country overrides to USA, excluding services/exempt sectors
  for (c in names(combo_vec)) {
    if (c == "EU27"){origins = EU27} else {origins = c}
    fac     <- combo_vec[[c]]
    tariff_temp[
      origin %in% origins & destination == "USA" &
        sector %nin% c(GTAP_sectors[Category %in% c("Services"), Code],
                       sectors_exempted,
                       sector_tariffs_trump_tariffs),
      value := fac
    ]
  }

  # model run
  output = update_equilibrium(initial_conditions = initial_conditions,
                              model = caliendo_parro_2015_cpp,
                              model_scenario = list(tariff_new = tariff_temp),
                              settings = list(verbose = 2L,
                                              tolerance = 1e-4))

  # process output
  output = process_results(output)
  welfar_change = output$output$welfare_change
  exports_change = output$output$exports_change
  production_change = output$output$production_total_real_change
  data_export = merge(welfar_change, exports_change, by = "country", suffixes = c("_welfare", "_exports")) %>%
    merge(production_change, by = "country") %>%
    setnames("value", "value_production")

  # save
  fwrite(data_export, fn, compress = "gzip")
  rm(output)
}
stopCluster(cl) # end cluster


# 4 - output -----

focus_countries <- c("CAN","CHN","MEX","JPN","EU27")

# 1) Read all files and build a scenario key ----------------------------------
files <- list.files("temp/results/counterfactuals", pattern = "\\.csv.gz$", full.names = TRUE)
parse_combo <- function(fn, countries = focus_countries) {
  base <- basename(sub("\\.csv$", "", fn))
  toks <- strsplit(base, "_", fixed = TRUE)[[1]]

  # split each token into country and step allowing '.' OR '-'
  parts   <- tstrsplit(toks, "[-.]", fixed = FALSE)
  country <- parts[[1]]
  step    <- suppressWarnings(as.integer(parts[[2]]))  # e.g., "10" -> 10

  # initialize named list of NAs, then fill the matching countries
  out <- as.list(rep(NA_integer_, length(countries)))
  names(out) <- countries

  keep <- country %in% countries
  if (any(keep)) out[country[keep]] <- step[keep]

  as.data.table(out)
}
key_rows <- lapply(files, parse_combo, countries = focus_countries)
key_list <- rbindlist(key_rows, use.names = TRUE, fill = TRUE)
key_list[, scenario_id := .I][]

# 2) Build a stable country key ----------------------------------------------
# read any one csv to get countries (or build from your model regions)
one <- fread(files[1])
country_key <- unique(one[, .(iso3c = country)])[ , country_id := .I ][]
setkey(country_key, iso3c)

# 3) Stream everything into one long table (and scale to integers) ------------
to_int <- function(x) as.integer(round((x - 1) * 100000L))

# helper to read one scenario and emit long rows with ids
read_one <- function(fn, sid) {
  dt <- fread(fn)  # columns: country, value_welfare, value_exports, value_production
  dt <- country_key[dt, on = .(iso3c = country)]
  # scale to permille (or bp) and drop doubles
  out <- dt[, .(scenario_id = sid,
                country_id,
                welfare    = to_int(value_welfare),
                exports    = to_int(value_exports),
                production = to_int(value_production))]
  out[]
}

# iterate (chunk if needed)
facts <- rbindlist(
  Map(read_one, files, key_list$scenario_id),
  use.names = TRUE
)

# 4) Write compact Parquet files ---------------------------------------------
# enforce integer schema
tab <- as_arrow_table(
  facts,
  schema = schema(
    scenario_id   = int32(),
    country_id    = int16(),
    welfare    = int32(),
    exports    = int32(),
    production = int32()
  )
)
# dir.create("app_data", showWarnings = FALSE)
write_parquet(tab, "app_data/facts.parquet",
              compression = "gzip", use_dictionary = TRUE)

write_parquet(
  as_arrow_table(key_list[, .(scenario_id,
                              CAN = as.integer(CAN),
                              CHN = as.integer(CHN),
                              MEX = as.integer(MEX),
                              JPN = as.integer(JPN),
                              EU27 = as.integer(EU27))]),
  "app_data/scenario_key.parquet",
  compression = "gzip", use_dictionary = TRUE
)

fwrite(country_key[, .(country_id, iso3c)], "app_data/country_key.csv")



