## ui.apiGENESIS.kennung and ui.apiGENESIS.passwort set in global.R

ui.apiGENESIS.datasetname = c("81000BJ002", "81000BJ105", "81000BJ102")

ui.apiGENESIS.curl <- RCurl::getCurlHandle()
## ui.proxy defined in global.R
RCurl::curlSetOpt(.opts = list(proxy = ui.proxy), curl = ui.apiGENESIS.curl)

apiGENESIS.col1 <- column(width = 4,
                           box(
                               width = NULL,
                               ## title = "Controls",
                               selectInput("apigenesis_datasetname", "Dataset:",
                                           choices = ui.apiGENESIS.datasetname,
                                           selected = "81000BJ002",
                                           multiple = FALSE)

                           )


                               ## ,
                               ## downloadButton("download_apiGENESIS_parameter", "Download Parameters (rdata)")

                           )

apiGENESIS.col2 <- column(width = 8,

                           box(
                               width = 6,
                               radioButtons("download_apiGENESIS_data_format", "Download Data Format",
                                            choices = list(
                                                "Data Table" = "df",
                                                "Time Series" = "xts")
                                            )
                               )

                          ,
                           box(
                               width = 6,
                               downloadButton("download_apiGENESIS_data", "Download Data")
                           )

                          ,
                          box(width = 12, collapsible = TRUE,
                              title = "Time Series Plot", dygraphOutput("dygraphs_apiGENESIS"))

                          )

apiGENESIS.row2 <- column(width = 12,

                          ## box(title = "Summary", verbatimTextOutput("summary_apiGENESIS"), width = NULL, collapsible = TRUE)
                          ## ,
                          box(title = "Data Table", dataTableOutput("datatable_apiGENESIS"), width = NULL, collapsible = TRUE)

                          )
