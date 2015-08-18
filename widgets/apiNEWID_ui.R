## see ~/Dropbox/GitHub/desk/inst/industry/tools/indic/apiBEA.R

ui.apiNEWID.datasetname <- c("")

ui.apiNEWID.prefix <- "apinewid_"

ui.apiNEWID.curl <- RCurl::getCurlHandle()

apiNEWID.col1 <- column(width = 4,

                      box(width = 12,
                          selectInput("apinewid_datasetname", "Dataset",
                                      choices = ui.apiNEWID.datasetname,
                                      selected = "",
                                      multiple = FALSE)
                         ,
                          uiOutput("uiNEWID_filtervalues")
                          )

                      )


apiNEWID.col2 <- column(width = 8,

                        box(width = 3,
                            downloadButton("download_data_apiNEWID", "Download Data"),
                            downloadButton("download_plot_apiNEWID", "Download Plot"))
                       ,
                        box(width = 9,
                            uiOutput("uiNEWID_queryuri"))
                       ,
                        box(width = 12, collapsible = TRUE,
                            title = "Time Series Plot",
                            dygraphOutput("dygraphs_apiNEWID",
                                          height = "400px"))
                       ,
                        box(width = 12,
                            collapsible = TRUE,
                            dataTableOutput("datatable_apiNEWID"))

                        )
