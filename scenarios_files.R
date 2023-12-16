library(KITE)
library(tidyverse)
library(data.table)

initial_conditions = read_rds("../KITE/data/GTAP_10/initial_conditions.rds")


EU = c("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GRC", "HRV", "HUN", "IRL", "ITA", "LTU", "LUX", "LVA", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "SWE")
countries = c("USA","CHN", "IND", "BRA", "RUS")

results = list()
for (c in countries) {

    ntbs = copy(initial_conditions$tariff)
    ntbs[, value := 1]

    results_temp = data.table()
    for (ntb_increase in c(1.1,1.25,1.5,2)) {

      # IMPORTS ----
      ntb_imports = copy(ntbs)
      ntb_imports[origin %in% c & destination %in% EU, value := ntb_increase]
      temp = update_equilibrium_cpp(
        initial_conditions = initial_conditions,
        ntb_change = ntb_imports,
        verbose = F
      ) %>% process_results()
      temp = temp$equilibrium_new$welfare_change
      temp[, ntb := ntb_increase][, scenario := "imports"]
      results_temp = rbind(
        results_temp,
        temp
        )

      # EXPORT ----
      ntbs_exports = copy(ntbs)
      ntbs_exports[destination %in% c & origin %in% EU, value := ntb_increase]
      temp = update_equilibrium_cpp(
        initial_conditions = initial_conditions,
        ntb_change = ntbs_exports,
        verbose = F
      ) %>% process_results()
      temp = temp$equilibrium_new$welfare_change
      temp[, ntb := ntb_increase][, scenario := "exports"]
      results_temp = rbind(
        results_temp,
        temp
      )

      # IMPORTS & EXPORTS ----
      ntbs_both = copy(ntbs)
      ntbs_both[destination %in% c & origin %in% EU, value := ntb_increase]
      ntbs_both[origin %in% c & destination %in% EU, value := ntb_increase]
      temp = update_equilibrium_cpp(
        initial_conditions = initial_conditions,
        ntb_change = ntbs_both,
        verbose = F
      ) %>% process_results()
      temp = temp$equilibrium_new$welfare_change
      temp[, ntb := ntb_increase][, scenario := "imports_and_exports"]
      results_temp = rbind(
        results_temp,
        temp
      )
    }
  results[[c]] = results_temp
  print(str_c("Decoupling from ", c, " done"))
}

rbindlist(results, use.names = T, idcol = "enemy") %>% fwrite("decopuling_results.csv")
