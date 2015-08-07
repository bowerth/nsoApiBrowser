## ui.apiGENESIS.kennung and ui.apiGENESIS.passwort set in global.R

ui.apiGENESIS.datasetname = c(
    "81000BJ100", # Volkswirtschaftliche Gesamtrechnungen des Bundes, Produktionswert, Vorleistungen, Bruttowertschpfung, Deutschland insgesamt, WZ2008: Wirtschaftsbereiche der VGR, Preisbasis (jeweilige Preise / preisbereinigt), Jahr
    "81000BJ002",
    "81000BJ105",
    "81000BJ102")

## ui.apiGENESIS.curl <- RCurl::getCurlHandle()
## ## ui.proxy defined in global.R
## ## RCurl::curlSetOpt(.opts = list(proxy = ui.httpproxy), curl = ui.apiGENESIS.curl)
## RCurl::curlSetOpt(.opts = list(proxy = input$sidebar_httpproxy), curl = ui.apiGENESIS.curl)

ui.apiGENESIS.prefix <- "apigenesis_"

apiGENESIS.col1 <- column(width = 3,
                          box(
                            width = 12,
                            ## title = "Query Builder",

                              textInput("apigenesis_kennung", "Kennung", ifelse(exists("ui.apiGENESIS.kennung"), ui.apiGENESIS.kennung, ""))
                              ,
                              textInput("apigenesis_passwort", "Passwort", ifelse(exists("ui.apiGENESIS.passwort"), ui.apiGENESIS.passwort, ""))
                              ,

                            selectInput("apigenesis_datasetname", "Dataset",
                                        choices = ui.apiGENESIS.datasetname,
                                        selected = "81000BJ100",
                                        multiple = FALSE)

                            ,
                            uiOutput("uiGENESIS_filtervalues")
                          )

                          )


                          ## ,
                          ## downloadButton("download_parameter_apiGENESIS", "Download Parameters (rdata)")


apiGENESIS.col2 <- column(width = 9,

                          box(width = 3,

                              ## title = "Download",
                              ## helpText("Download"),
                              downloadButton("download_data_apiGENESIS", "Download Data")
                              ## downloadLink("download_data_apiGENESIS", "Download Data")
                             ,
                              downloadButton("download_plot_apiGENESIS", "Download Plot")
                              ## downloadLink("download_plot_apiGENESIS", "Download Plot")
                              )

                          ,

                          box(width = 9,
                              uiOutput("uiGENESIS_queryuri")
                              )

                         ## ,
                         ##  box(width = 2,
                         ##      ## collapsible = TRUE,
                         ##      ## title = "Credentials",
                         ##      ## wellPanel(
                         ##          textInput("apigenesis_kennung", "Kennung", ifelse(exists("ui.apiGENESIS.kennung"), ui.apiGENESIS.kennung, ""))
                         ##      )
                         ##     ,
                         ##  box(width = 2,
                         ##      textInput("apigenesis_passwort", "Passwort", ifelse(exists("ui.apiGENESIS.passwort"), ui.apiGENESIS.passwort, ""))
                         ##      )
                         ##      ## )

                         ## ,
                         ##  box(
                         ##      width = 3,
                         ##      ## collapsible = TRUE,
                         ##      ## title = "Export Data",
                         ##      radioButtons("apigenesis_download_data_format", "Download Format",
                         ##                   choices = list(
                         ##                       "Data Table (all)" = "df_all",
                         ##                       "Data Table (filter)" = "df_filter",
                         ##                       "Time Series" = "xts")
                         ##                   )
                         ##      )

                         ,
                          box(width = 12, collapsible = TRUE,
                              title = "Time Series Plot", dygraphOutput("dygraphs_apiGENESIS")
                              )

                          ,
                          box(width = 12,
                              collapsible = TRUE,
                              title = "Data Table", dataTableOutput("datatable_apiGENESIS")
                              )

                          )

## apiGENESIS.row2 <- column(width = 12,

##                           ## box(title = "Summary", verbatimTextOutput("summary_apiGENESIS"), width = NULL, collapsible = TRUE)
##                           ## ,

##                           box(width = NULL, collapsible = TRUE,
##                               title = "Data Table", dataTableOutput("datatable_apiGENESIS")
##                               )

##                           )
