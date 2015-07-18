library(reshape2)
library(ISOweek)
library(shinyAce)

library(ggplot2)

library(xts)
library(dygraphs)

## apiBEA
library(stanApi)

## devtools::document(file.path(dbpath, "GitHub", "stanApi"))
## devtools::install(file.path(dbpath, "GitHub", "stanApi"))

library(RCurl)
library(jsonlite)

if (Sys.info()[["user"]]=="werth_b") {
  ui.proxy <- "wsg-proxy.oecd.org:80"
} else ui.proxy <- ""
