## see ~/Dropbox/GitHub/desk/inst/industry/tools/indic/apiBEA.R


ui.apiCBS.baseurl <- "http://opendata.cbs.nl/ODataApi/OData"
ui.apiCBS.datasetname <- c("82572ENG", "83068ENG")
ui.apiCBS.prefix <- "apicbs_"

ui.apiCBS.curl <- RCurl::getCurlHandle()
## ui.proxy defined in global.R
## RCurl::curlSetOpt(.opts = list(proxy = ui.proxy), curl = ui.apiCBS.curl)

## apiCBS.input <- column(width = 4,
apiCBS.col1 <- column(width = 4,
                      box(
                          width = 12,
                          ## title = "Controls",

                          ## selectInput("apibea_method", "Method:", choices = ui.apiCBS.method, selected = "GETDATA", multiple = FALSE),
                          selectInput("apicbs_datasetname", "Dataset",
                                      choices = ui.apiCBS.datasetname,
                                      selected = "82572ENG",
                                      multiple = FALSE)
                         ,
                          uiOutput("uiCBS_filtervalues")
                      )

                      )


apiCBS.col2 <- column(width = 8,

                      ## box(
                      ##     width = 3,
                      ##     radioButtons("download_data_format_apiCBS", "Download Format",
                      ##                  choices = list(
                      ##                      "Data Table (all)" = "df_all",
                      ##                      "Data Table (filter)" = "df_filter",
                      ##                      "Time Series" = "xts")
                      ##                  )
                      ##     )
                      ##     ,

                      box(
                          width = 3,
                          downloadButton("download_data_apiCBS", "Download Data")
                          ,
                          downloadButton("download_plot_apiCBS", "Download Plots")
                      )
                      ,

                      box(
                          width = 9
                         ,
                          uiOutput("uiCBS_queryuri")
                        )

                      ## ,
                      ## downloadButton("download_parameter_apiCBS", "Download Parameters (rdata)")

                      ## output
                      ,
                        box(width = 12, collapsible = TRUE,
                            title = "Time Series Plot",
                            ## http://www.rdocumentation.org/packages/dygraphs/functions/dygraph-shiny
                            dygraphOutput("dygraphs_apiCBS",
                                          ## width = "100%",
                                          ## height = "auto")
                                          height = "400px")
                            )

                     ,
                      box(width = 12,
                          collapsible = TRUE,
                          dataTableOutput("datatable_apiCBS")
                          )

                      )

## apiCBS.output <- column(width = 8,
## apiCBS.row2 <- column(width = 12,
## apiCBS.output <- row(height = 8,
                        ##       box(title = "Time series plot", plotOutput("plot1", height = 350), width = NULL, collapsible = TRUE)
                       ## ,
                        ## box(title = "Time series plot", dygraphOutput("dygraphs_apiCBS"), width = NULL, collapsible = TRUE)
                        ## ,
                        ## box(title = "Summary", verbatimTextOutput("summary_apiCBS"), width = NULL, collapsible = TRUE)
                        ## ,


                        ##       ## ,
                        ##       box(title = "Data table", dataTableOutput("table1"), width = NULL, collapsible = TRUE)

                        ## )
