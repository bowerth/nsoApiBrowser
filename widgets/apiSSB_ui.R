## see ~/Dropbox/GitHub/desk/inst/industry/tools/indic/apiBEA.R

ui.apiSSB.baseurl <- "http://data.ssb.no/api/v0/dataset"

ui.apiSSB.datasetname <- list(
  "44631",
  ## "1104"
  "1106"
)

ui.apiSSB.prefix <- "apissb_"

ui.apiSSB.curl <- RCurl::getCurlHandle()

apiSSB.col1 <- column(width = 4,

                      box(width = 12,
                          ## selectInput("apissb_baseurl", "Provider",
                          ##             ## choices = as.character(ui.apiSSB.provider[["name"]]),
                          ##             choices = ui.apiSSB.baseurl,
                          ##             selected = ui.apiSSB.baseurl[[1]], # "http://api.scb.se/OV0104/v1/doris/en/ssd"
                          ##             multiple = FALSE)
                         ## ,
                          selectInput("apissb_datasetname", "Dataset",
                                      choices = ui.apiSSB.datasetname,
                                      selected = "44631",
                                      multiple = FALSE)
                          ## ,
                          ## uiOutput("uiSSB_datasetname")
                         ,
                          uiOutput("uiSSB_filtervalues")
                      )

                      )


apiSSB.col2 <- column(width = 8,

                        box(width = 3,
                            downloadButton("download_data_apiSSB", "Download Data"),
                            downloadButton("download_plot_apiSSB", "Download Plot"))
                       ,
                        box(width = 9,
                            uiOutput("uiSSB_queryuri"))
                       ,
                        box(width = 12, collapsible = TRUE,
                            title = "Time Series Plot",
                            dygraphOutput("dygraphs_apiSSB",
                                          height = "400px"))
                       ,
                        box(width = 12,
                            collapsible = TRUE,
                            dataTableOutput("datatable_apiSSB"))

                        )
