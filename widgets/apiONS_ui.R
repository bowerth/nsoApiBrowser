## see ~/Dropbox/GitHub/desk/inst/industry/tools/indic/apiBEA.R


ui.apiONS.baseurl <- "http://data.ons.gov.uk/ons/api/data"
## ui.apiONS.datasetname <- c("QS208EW", "QS104EW")
## ui.apiONS.context <- c("Census", "Economic", "Social")

ui.apiONS.datasetname <- rbind.data.frame(
    c("-- please select a dataset --", ""),
    c("QS208EW", "Census"),
    c("QS104EW", "Census")
   ## ,
   ##  c("TG", "Economic")
)
names(ui.apiONS.datasetname) <- c("ID", "Context")

ui.apiONS.prefix <- "apions_"

ui.apiONS.curl <- RCurl::getCurlHandle()
## ui.proxy defined in global.R
## RCurl::curlSetOpt(.opts = list(proxy = ui.proxy), curl = ui.apiONS.curl)

## apiONS.input <- column(width = 4,
apiONS.col1 <- column(width = 4,
                      box(
                          width = 12,
                          ## title = "Controls",

                          ## selectInput("apibea_method", "Method:", choices = ui.apiONS.method, selected = "GETDATA", multiple = FALSE),
                         ##  selectInput("apions_context", "Context",
                         ##              choices = ui.apiONS.context,
                         ##              selected = "Census",
                         ##              multiple = FALSE)
                         ## ,
                          selectInput("apions_datasetname", "Dataset",
                                      choices = as.character(ui.apiONS.datasetname$ID),
                                      selected = "QS208EW",
                                      ## selected = "-- please select a dataset --",
                                      multiple = FALSE)
                         ,
                          uiOutput("uiONS_filtervalues")
                      )

                      )


apiONS.col2 <- column(width = 8,

                      ## box(
                      ##     width = 3,
                      ##     radioButtons("download_data_format_apiONS", "Download Format",
                      ##                  choices = list(
                      ##                      "Data Table (all)" = "df_all",
                      ##                      "Data Table (filter)" = "df_filter",
                      ##                      "Time Series" = "xts")
                      ##                  )
                      ##     )
                      ##     ,

                      box(
                          width = 3,
                          downloadButton("download_data_apiONS", "Download Data")
                          ,
                          downloadButton("download_plot_apiONS", "Download Plot")
                      )

                     ,
                      box(
                          width = 9
                         ,
                          uiOutput("uiONS_queryuri")
                        )

                      ## ,
                      ## downloadButton("download_parameter_apiONS", "Download Parameters (rdata)")

                      ## output

                     ## ,
                     ##    box(width = 12, collapsible = TRUE,
                     ##        title = "Time Series Plot",
                     ##        ## http://www.rdocumentation.org/packages/dygraphs/functions/dygraph-shiny
                     ##        dygraphOutput("dygraphs_apiONS",
                     ##                      ## width = "100%",
                     ##                      ## height = "auto")
                     ##                      height = "400px")
                     ##        )

                     ,
                        box(width = 12, collapsible = TRUE,
                            title = "Cross-Section Plot",
                            ## http://www.rdocumentation.org/packages/dygraphs/functions/dygraph-shiny
                            dimpleOutput("dimple_apiONS",
                                          ## width = "100%",
                                          ## height = "auto")
                                          height = "400px")
                            )

                     ,
                      box(width = 12,
                          collapsible = TRUE,
                          dataTableOutput("datatable_apiONS")
                          )

                      )

## apiONS.output <- column(width = 8,
## apiONS.row2 <- column(width = 12,
## apiONS.output <- row(height = 8,
                        ##       box(title = "Time series plot", plotOutput("plot1", height = 350), width = NULL, collapsible = TRUE)
                       ## ,
                        ## box(title = "Time series plot", dygraphOutput("dygraphs_apiONS"), width = NULL, collapsible = TRUE)
                        ## ,
                        ## box(title = "Summary", verbatimTextOutput("summary_apiONS"), width = NULL, collapsible = TRUE)
                        ## ,


                        ##       ## ,
                        ##       box(title = "Data table", dataTableOutput("table1"), width = NULL, collapsible = TRUE)

                        ## )
