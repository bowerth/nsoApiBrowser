## ui.apiGENESIS.kennung and ui.apiGENESIS.passwort set in global.R

ui.apiGENESIS.datasetname = c("81000BJ002", "81000BJ105", "81000BJ102")

## ui.apiGENESIS.curl <- RCurl::getCurlHandle()
## ## ui.proxy defined in global.R
## ## RCurl::curlSetOpt(.opts = list(proxy = ui.httpproxy), curl = ui.apiGENESIS.curl)
## RCurl::curlSetOpt(.opts = list(proxy = input$sidebar_httpproxy), curl = ui.apiGENESIS.curl)

ui.apiGENESIS.prefix <- "apigenesis_"

apiGENESIS.col1 <- column(width = 4,
                          box(
                            width = NULL,
                            ## title = "Query Builder",

                            selectInput("apigenesis_datasetname", "Dataset",
                                        choices = ui.apiGENESIS.datasetname,
                                        selected = "81000BJ002",
                                        multiple = FALSE)

                            ,
                            uiOutput("uiGENESIS_filtervalues")

                            )


                          ## ,
                          ## downloadButton("download_parameter_apiGENESIS", "Download Parameters (rdata)")

                          )

apiGENESIS.col2 <- column(width = 8,

                          box(width = 12,
                              uiOutput("uiGENESIS_queryuri")
                              )
                         ,

                          box(width = 6,
                              ## collapsible = TRUE,
                              ## title = "Credentials",

                              wellPanel(
                                  textInput("apigenesis_kennung", "Kennung", ifelse(exists("ui.apiGENESIS.kennung"), ui.apiGENESIS.kennung, "")),
                                  textInput("apigenesis_passwort", "Passwort", ifelse(exists("ui.apiGENESIS.passwort"), ui.apiGENESIS.passwort, ""))

                              )

                          )

                         ,
                          box(
                              width = 3,
                              ## collapsible = TRUE,
                              ## title = "Export Data",
                              radioButtons("apigenesis_download_data_format", "Download Data Format",
                                           choices = list(
                                               "Data Table (all)" = "df_all",
                                               "Data Table (filter)" = "df_filter",
                                               "Time Series" = "xts")
                                           )
                              )
                         ,
                          box(width = 3,
                              downloadButton("download_data_apiGENESIS", "Download Data")
                             ,
                              downloadButton("download_plot_apiGENESIS", "Download Plot")

                          )

                         ,
                          box(width = 12, collapsible = TRUE,
                              title = "Time Series Plot", dygraphOutput("dygraphs_apiGENESIS"))

                          )

apiGENESIS.row2 <- column(width = 12,

                          ## box(title = "Summary", verbatimTextOutput("summary_apiGENESIS"), width = NULL, collapsible = TRUE)
                          ## ,
                          box(width = NULL, collapsible = TRUE,
                              title = "Data Table", dataTableOutput("datatable_apiGENESIS"))

                          )
