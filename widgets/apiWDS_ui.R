## see ~/Dropbox/GitHub/desk/inst/industry/tools/indic/apiBEA.R

## ui.apiWDS.baseurl <- "http://data.wds.no/api/v0/dataset"

ui.apiWDS.datasetname <- list(
  "2810027" # ,
  ## "1104"
  ## "1106"
)

ui.apiWDS.prefix <- "apiwds_"

ui.apiWDS.curl <- RCurl::getCurlHandle()

apiWDS.col1 <- column(width = 4,

                      box(width = 12,
                          ## selectInput("apiwds_baseurl", "Provider",
                          ##             ## choices = as.character(ui.apiWDS.provider[["name"]]),
                          ##             choices = ui.apiWDS.baseurl,
                          ##             selected = ui.apiWDS.baseurl[[1]], # "http://api.scb.se/OV0104/v1/doris/en/ssd"
                          ##             multiple = FALSE)
                         ## ,
                          selectInput("apiwds_datasetname", "Dataset",
                                      choices = ui.apiWDS.datasetname,
                                      selected = "2810027",
                                      multiple = FALSE)
                          ## ,
                          ## uiOutput("uiWDS_datasetname")
                         ,
                          uiOutput("uiWDS_filtervalues")
                      )

                      )


apiWDS.col2 <- column(width = 8,

                        box(width = 3,
                            downloadButton("download_data_apiWDS", "Download Data"),
                            downloadButton("download_plot_apiWDS", "Download Plot"))
                       ,
                        box(width = 9,
                            uiOutput("uiWDS_queryuri"))
                       ,
                        box(width = 12, collapsible = TRUE,
                            title = "Time Series Plot",
                            dygraphOutput("dygraphs_apiWDS",
                                          height = "400px"))
                       ,
                        box(width = 12,
                            collapsible = TRUE,
                            dataTableOutput("datatable_apiWDS"))

                        )
