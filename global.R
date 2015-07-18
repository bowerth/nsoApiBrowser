library(reshape2)
library(ISOweek)
library(shinyAce)

library(ggplot2)

library(xts)
library(dygraphs)

## apiBEA
## library(stanApi)
library(nsoApi)
## remove.packages("stanApi")
  
library(RCurl)
library(jsonlite)

if (Sys.info()[["user"]]=="werth_b") {
  ui.proxy <- "wsg-proxy.oecd.org:80"
} else ui.proxy <- ""



## load key file
file.apiKey.enc <- system.file("apiKey.R.gpg", package = "nsoApi")
file.apiKey.dec <- sub(".gpg", "", file.apiKey.enc)
if (file.exists(file.apiKey.dec)) {
  source(file.apiKey.dec)
} else {
  nsoApi::nsoApiGPG(file = file.apiKey.enc, keep = TRUE)
}
## file.remove(file.apiKey)

## get from keyfile
if (exists("apiKey")) {
  
  ui.apiGENESIS.kennung <- apiKey$GENESIS$kennung
  ui.apiGENESIS.passwort <- apiKey$GENESIS$passwort
  ui.apiBEA.userid <- apiKey$BEA$userid
  ui.apiONS.apikey <- apiKey$ONS$apikey

}
