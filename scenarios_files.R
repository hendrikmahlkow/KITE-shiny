library(KITE)
library(tidyverse)
library(data.table)

initial_conditions = read_rds("../KITE/data/GTAP_10/initial_conditions.rds")



EU = c("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GRC", "HRV", "HUN", "IRL", "ITA", "LTU", "LUX", "LVA", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "SWE")
countries = c("USA","CHN", "IND")

results = list()
for (c in countries) {
  for (enemy in countries[!(countries %in% c)]) {

    ntbs = copy(initial_conditions$tariff)
    ntbs[, value := 1]

    for (ntb_increase in c(1.1,1.25,1.5,2)) {

      ntbs[origin %in% c & destination %in% enemy, value := ntb_increase]
      ntbs[destination %in% c & origin %in% enemy, value := ntb_increase]

      # run model
      temp = update_equilibrium_cpp(
        initial_conditions = initial_conditions,
        ntb_change = ntbs,
      ) %>% process_results()
      temp = temp$equilibrium_new$welfare_change

      # save
      if(any(c == EU)){
        results[["EU"]][[enemy]][[paste0(ntb_increase)]] = temp
      }
      if(any(enemy == EU)){
        results[[c]][["EU"]][[paste0(ntb_increase)]] = temp
      }else{
        results[[c]][[enemy]][[paste0(ntb_increase)]] = temp
        }
    }
    print(str_c("Decoupling ", c, " from ", enemy, " done"))
  }
}
data = data.table()
for (c1 in c("USA","CHN", "IND")) {
  for (c2 in c("USA","CHN", "IND")) {
    if(c1 == c2){
      next
    }else{
      temp = rbindlist(results[[c1]][[c2]], idcol = "ntb_increase")
      temp[, country_1 := c1][, country_2 := c2]
      data = rbind(data, temp)
    }
  }
}
fwrite(data, "decopuling_results.csv")
