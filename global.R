library(reshape2)
library(ISOweek)
library(shinyAce)

library(ggplot2)

library(xts)
library(dygraphs)
## devtools::install_github("rstudio/dygraphs")

## apiBEA
## library(stanApi)
library(nsoApi)
## remove.packages("stanApi")

library(RCurl)
library(jsonlite)

## if (length(Sys.getenv("http_proxy")) > 0) {
##   ui.httpproxy <- Sys.getenv("http_proxy")
  
## } else if (Sys.info()[["user"]]%in%c("werth_admin", "shiny")) {
if (Sys.info()[["user"]]%in%c("werth_admin", "shiny")) {
  ui.httpproxy <- "wsg2.oecd.org:80"
  
} else if (Sys.info()[["user"]]%in%c("werth_b")) { # "werth_admin", "shiny"
  ui.httpproxy <- "wsg-proxy.oecd.org:80"
  
} else ui.httpproxy <- ""



## load key file
file.apiKey.enc <- system.file("apiKey.R.gpg", package = "nsoApi")
file.apiKey.dec <- sub(".gpg", "", file.apiKey.enc)
if (file.exists(file.apiKey.dec)) {
  source(file.apiKey.dec)
} else {
    if (Sys.info()[["sysname"]]=="Windows") {
                nsoApi::nsoApiGPG(
                    file = file.apiKey.enc,
                    gpg = file.path("D:", "GPG", "gpg2.exe"),
                    shell = file.path("C:", "Windows", "System32", "cmd.exe"),
                    keep = TRUE)
    } else {
        nsoApi::nsoApiGPG(file = file.apiKey.enc, keep = TRUE)
    }
}
## file.remove(file.apiKey)

## get from keyfile
if (exists("apiKey")) {

  ui.apiGENESIS.kennung <- apiKey$GENESIS$kennung
  ui.apiGENESIS.passwort <- apiKey$GENESIS$passwort
  ui.apiBEA.userid <- apiKey$BEA$userid
  ui.apiONS.apikey <- apiKey$ONS$apikey

}

ui.datatable.options <- list(
     pageLength = 20,
     lengthMenu = c(10, 20, 50, 100)
  )

ui.queryyear.min <- 1970
ui.queryyear.max <- as.numeric(format(Sys.time(), "%Y"))

nsoApiBrowser_dygraph <- function(data, show.boolean) {

  ## show <- ifelse(input$sidebar_dygraphlegendshow, "always", "never")
  show <- ifelse(show.boolean, "always", "never")

  dygraphs::dygraph(data = data) %>%
    dyLegend(
      width = 400,
      hideOnMouseOut = TRUE,
      show = show
      )

}
