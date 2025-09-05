# prerequirments
# install.packages("pak")
# pak::pak("posit-dev/r-shinylive")
# pak::pak("rstudio/httpuv")

# Render the Shiny App
# shinylive::export(appdir = ".", destdir = "docs")
# httpuv::runStaticServer("docs/", port=8008)


library(rsconnect)
rsconnect::setAccountInfo(name='hendrikmahlkow',
                          token='0AE9AA6DB4A0CD3A2BEF56EB9F3F121B',
                          secret='+pufzbYKsAkz0T40FD6ufgHBCilRMdNf+A2Q7IzE')
# renv::remove(package_names[!(package_names %in% c("shiny", "leaflet", "sf", "data.table", "RColorBrewer", "tidyverse"))])
# Remove any other unnecessary packages in a similar manner
renv::remove("KITE")
renv::snapshot()

rsconnect::deployApp(
  # appDir = "/files/Projects/KITE-shiny",
  appFiles = c(
    "app.R",
    "facts.parquet",
    "scenario_key.parquet",
    "country_key.csv",
    "map_world_gtap.rds",   # your map
    "renv.lock"                      # optional, if using renv
  )
)

# if error occurs
rsconnect::showLogs(appName = "kite-shiny", streaming = TRUE)
