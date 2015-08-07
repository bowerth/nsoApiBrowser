## shiny::runApp(file.path(dbpath, "GitHub", "nsoApi", "inst", "nsoApiBrowser"), launch.browser = FALSE, port = 3838)
## source(file.path(dbpath, "GitHub", "apiBrowser", "master.R"))

## remove.packages("nsoApi")
## detach("package:nsoApi", unload = TRUE)
## devtools::install(file.path(dbpath, "GitHub", "nsoApi"))

path <- file.path(dbpath, "GitHub", "nsoApi", "inst", "nsoApiBrowser")
setwd(path)

## install.packages("shinydashboard")
source("app.R")
## shinyApp(ui, server, options = list(port = 4848, launch.browser = FALSE))
shinyApp(ui, server, options = list(port = 3838, launch.browser = FALSE)))

## ?shiny::shinyApp

## path.dlpackage <- file.path(dlpath, "Rpkgs")
## ## dir.create(path = path.dlpackage)
## download.packages(pkgs = c("stringi", "XML", "tidyr"),
##                   destdir = path.dlpackage,
##                   type = "source")

## outfname <- file.path(dlpath, "icudt55l (1).zip")
## tools::md5sum(outfname)

## curl <- RCurl::getCurlHandle()
## RCurl::curlSetOpt(.opts = list(proxy = "wsg-proxy.oecd.org:80"), curl = curl)
## RCurl::getURL("http://opendata.cbs.nl/ODataApi/OData/82572ENG/SectorBranchesSIC2008", curl = curl)

