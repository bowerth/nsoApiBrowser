## see ~/Dropbox/GitHub/desk/inst/industry/tools/indic/apiBEA.R

## ui.apiPXWEB.baseurl <- "http://opendata.pxweb.nl/ODataApi/OData"
ui.apiPXWEB.baseurl <- list(
    "SCB Sweden" = "http://api.scb.se/OV0104/v1/doris/en/ssd")

ui.apiPXWEB.datasetname <- list(
    NR0103ENS2010T08A = "NR/NR0103/NR0103E/NR0103ENS2010T08A", # Value added, detail components
    NR0103ENS2010T09A = "NR/NR0103/NR0103E/NR0103ENS2010T09A" # Output, intermediate consumption
)

ui.apiPXWEB.prefix <- "apipxweb_"

ui.apiPXWEB.curl <- RCurl::getCurlHandle()

apiPXWEB.col1 <- column(width = 4,

                      box(width = 12,
                          selectInput("apipxweb_baseurl", "Provider",
                                      ## choices = as.character(ui.apiPXWEB.provider[["name"]]),
                                      choices = ui.apiPXWEB.baseurl,
                                      selected = "http://api.scb.se/OV0104/v1/doris/en/ssd",
                                      multiple = FALSE)
                         ,
                          selectInput("apipxweb_datasetname", "Dataset",
                                      choices = ui.apiPXWEB.datasetname,
                                      ## selected = "NR0103ENS2010T08A",
                                      selected = "NR/NR0103/NR0103E/NR0103ENS2010T08A",
                                      multiple = FALSE)
                         ,
                          uiOutput("uiPXWEB_filtervalues")
                      )

                      )


apiPXWEB.col2 <- column(width = 8,

                        box(width = 3,
                            downloadButton("download_data_apiPXWEB", "Download Data"),
                            downloadButton("download_plot_apiPXWEB", "Download Plots"))
                       ,
                        box(width = 9,
                            uiOutput("uiPXWEB_queryuri"))
                       ,
                        box(width = 12, collapsible = TRUE,
                            title = "Time Series Plot",
                            dygraphOutput("dygraphs_apiPXWEB",
                                          height = "400px"))
                       ,
                        box(width = 12,
                            collapsible = TRUE,
                            dataTableOutput("datatable_apiPXWEB"))

                        )
