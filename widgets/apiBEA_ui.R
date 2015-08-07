## ui.apiBEA.userid defined in global.R

ui.apiBEA.datasetname = rbind.data.frame(
  c("GDPbyIndustry", TRUE, c("IndustrYDescription")),
  c("NIPA", FALSE, c("SeriesCode")),
  c("FixedAssets", FALSE, c("SeriesCode"))
  ## c("RegionalData", 7),
  ## c("NIUnderlyingDetail", 1),
  ## c("MNE", 1),
  ## c("ITA", 1),
  ## c("IIP", 1)
  )
names(ui.apiBEA.datasetname) <- c("name", "multiple", "filterlist")

ui.apiBEA.parameterlist <- c("Frequency", "Industry", "TableID", "Year")

## ui.apiBEA.curl <- RCurl::getCurlHandle()
## ## ui.proxy defined in global.R
## ## RCurl::curlSetOpt(.opts = list(proxy = ui.httpproxy), curl = ui.apiBEA.curl)
## RCurl::curlSetOpt(.opts = list(proxy = input$sidebar_httpproxy), curl = ui.apiBEA.curl)

## ui.apiBEA.method <- c("GETDATA")
ui.apiBEA.resultformat <- c("JSON")
## ui.apiBEA.year <- as.numeric(c(1970:2014))
ui.apiBEA.prefix <- "apibea_"

apiBEA.col1 <- column(width = 3,
                      box(
                        width = 12,
                        ## title = "API Query",

                          textInput("apibea_userid", "User ID", value = ifelse(exists("ui.apiBEA.userid"), ui.apiBEA.userid, ""))
                         ,

                          selectInput("apibea_datasetname", "Datasetname",
                                    choices = as.character(ui.apiBEA.datasetname[["name"]]),
                                    ## selected = "NIPA",
                                    selected = "GDPbyIndustry",
                                    multiple = FALSE)
                        ,
                        ## sliderInput("apibea_year", "Time Period",
                        ##             min = min(ui.apiBEA.year),
                        ##             max = max(ui.apiBEA.year),
                        ##             value = c(1970, 2014),
                        ##             sep = "")
                        ## ,
                        uiOutput("uiBEA_parameterlist")
                        ,
                        uiOutput("uiBEA_parametervalues")

                        )

                      ,
                      box(
                        width = 12,
                        title = "Filter Results",
                        uiOutput("uiBEA_filtervalues")

                        )

                      )

apiBEA.col2 <- column(width = 9,

                      box(width = 3,
                          downloadButton("download_data_apiBEA", "Download Data") # these are outputs and inputIds at the same time
                         ,
                         downloadButton("download_plot_apiBEA", "Download Plot") # these are outputs and inputIds at the same time
                          )
                      ,

                      box(width = 9,
                              uiOutput("uiBEA_queryuri")
                          )
                     ,

                     ##  box(width = 3,
                     ##      radioButtons("apibea_download_data_format", "Download Format",
                     ##                   choices = list(
                     ##                     "Data Table (all)" = "df_all",
                     ##                     "Data Table (filter)" = "df_filter",
                     ##                     "Time Series" = "xts")
                     ##                   )
                     ##      )
                     ## ,

                      ## box(width = 6,
                      ##     wellPanel(
                              ## textInput("apibea_userid", "User ID", value = ifelse(exists("ui.apiBEA.userid"), ui.apiBEA.userid, ""))
                      ##       )
                      ##     )
                      ## ,

                      box(width = 12, collapsible = TRUE,
                          title = "Time Series Plot",
                          ## http://www.rdocumentation.org/packages/dygraphs/functions/dygraph-shiny
                          dygraphOutput("dygraphs_apiBEA",
                                        height = "400px")
                          )

                      ,
                      box(width = 12, collapsible = TRUE,
                          title = "Data Table", dataTableOutput("datatable_apiBEA")
                          )


                      )

## apiBEA.row2 <- column(width = 12,

##                       ## box(width = NULL, collapsible = TRUE, #
##                       ##     title = "Summary", verbatimTextOutput("summary_apiBEA")
##                       ##     )

##                      ## ,
##                       box(title = "Data Table", dataTableOutput("datatable_apiBEA"), width = NULL, collapsible = TRUE)

##                       )
