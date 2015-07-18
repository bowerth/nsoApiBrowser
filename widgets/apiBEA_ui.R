## ui.apiBEA.userid defined in global.R

ui.apiBEA.datasetname = rbind.data.frame(
    c("GDPbyIndustry", TRUE),
    c("NIPA", FALSE),
    c("FixedAssets", FALSE)
    ## c("RegionalData", 7),
    ## c("NIUnderlyingDetail", 1),
    ## c("MNE", 1),
    ## c("ITA", 1),
    ## c("IIP", 1)
)
names(ui.apiBEA.datasetname) <- c("name", "multiple")

ui.apiBEA.parameterlist <- c("Frequency", "Industry", "TableID", "Year")

ui.apiBEA.curl <- RCurl::getCurlHandle()
## ui.proxy defined in global.R
RCurl::curlSetOpt(.opts = list(proxy = ui.proxy), curl = ui.apiBEA.curl)
## ui.apiBEA.method <- c("GETDATA")
ui.apiBEA.resultformat <- c("JSON")
ui.apiBEA.year <- as.numeric(c(1970:2014))

apiBEA.col1 <- column(width = 4,
                       box(
                           width = NULL,
                           ## title = "Controls",

                           ## selectInput("apibea_method", "Method:", choices = ui.apiBEA.method, selected = "GETDATA", multiple = FALSE),
                           selectInput("apibea_datasetname", "Datasetname:",
                                       choices = as.character(ui.apiBEA.datasetname[["name"]]),
                                       ## selected = "FixedAssets",
                                       ## selected = "GDPbyIndustry",
                                       selected = "NIPA",
                                       multiple = FALSE),
                           uiOutput("uiBEA_parameterlist"),
                           uiOutput("uiBEA_parametervalues")
                          ,
                           ## uiOutput("uiBEA_timeperiod")

                           sliderInput("apibea_year", "Time Period:",
                                       ## min = dim.value[1], max = dim.value[length(dim.value)],
                                       min = min(ui.apiBEA.year),
                                       max = max(ui.apiBEA.year),
                                       ## value = c(2010, 2014),
                                       value = c(1970, 2014),
                                       sep = "")
                          ## ,

                           ## selectInput("apibea_resultformat", "Resultformat:", "JSON"),
                           ## wellPanel(
                               ## h5("Summary"),
                               ## checkboxInput("apibea_showparam", "Show API parameters", FALSE),
                               ## checkboxInput("apibea_showquery", "Show Query", FALSE),
                               ## checkboxInput("apibea_showdata", "Show Data", FALSE),
                               ## checkboxInput("apibea_resultraw", "Return JSON string", FALSE)
                           ## )
                           ## ,
                       )
                       )

apiBEA.col2 <- column(width = 8,
                        ##       box(title = "Time series plot", plotOutput("plot1", height = 350), width = NULL, collapsible = TRUE)
                        ## ,

                      ## download options
                        box(
                            width = 6,
                            radioButtons("download_apiBEA_data_format", "Download Data Format",
                                         choices = list(
                                             "Data Table" = "df",
                                             "Time Series" = "xts")
                                         )
                        )

                       ,
                        box(width = 6,
                            downloadButton("download_apiBEA_data", "Download Data")
                           ## ,
                           ##  downloadButton("download_apiBEA_parameter", "Download Parameters (rdata)")
                            )
                       ,

                      ## output
                        box(width = 12, collapsible = TRUE,
                            title = "Time Series Plot",
                            ## dygraphOutput("dygraphs_apiBEA")
                            ## http://www.rdocumentation.org/packages/dygraphs/functions/dygraph-shiny
                            dygraphOutput("dygraphs_apiBEA",
                                          ## width = "100%",
                                          ## height = "auto")
                                          height = "400px")
                            )


                        )

apiBEA.row2 <- column(width = 12,

                      ## box(width = NULL, collapsible = TRUE, #
                      ##     title = "Summary", verbatimTextOutput("summary_apiBEA")
                      ##     )

                     ## ,
                      box(title = "Data Table", dataTableOutput("datatable_apiBEA"), width = NULL, collapsible = TRUE)

                      )
