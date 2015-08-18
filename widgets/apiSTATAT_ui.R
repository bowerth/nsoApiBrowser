## see ~/Dropbox/GitHub/desk/inst/industry/tools/indic/apiBEA.R

## dataset_id <-
##     baseURL %>%
##         RCurl::getURL() %>%
##             stringr::str_match_all("meta.jsp[?]dataset=.+[\"\"]") %>% # only dataset ID
##                 unlist() %>%
##                     stringr::str_replace(pattern = "meta.jsp[?]dataset=", replacement = "") %>%
##                         stringr::str_replace(pattern = "\"", replacement = "")
## c_rev(dataset_id)
ui.apiSTATAT.datasetname <- c(
    "OGD_f1197_Bev_Jahresdurchschn_1",
    "OGD_f1585_Stud_Abschl_1",
    "OGD_umwoekosteu_Oekosteuern_1",
    ## "OGD_vpi10_VPI_2010_1",             # duplicate 'row.names' are not allowed
    ## "OGD_konjidxhan10_Konjunktur_Handel_1", # character string is not in a standard unambiguous format: date not in first column
    "OGD_touextsai_Tour_HKL_1",
    "OGD_touextsai_Tour_UA_1",
    "OGD_f1531neu_Aussenhandel_1",
    "OGD_reg_udemo_Unternehmensdemografie_1",
    "OGD_vgr001_VGRJahresR_1"
)

ui.apiSTATAT.prefix <- "apistatat_"

ui.apiSTATAT.curl <- RCurl::getCurlHandle()

apiSTATAT.col1 <- column(width = 4,

                      box(width = 12,
                          selectInput("apistatat_datasetname", "Dataset",
                                      choices = ui.apiSTATAT.datasetname,
                                      selected = "",
                                      multiple = FALSE)
                         ,
                          uiOutput("uiSTATAT_filtervalues")
                          )

                      )


apiSTATAT.col2 <- column(width = 8,

                        box(width = 3,
                            downloadButton("download_data_apiSTATAT", "Download Data"),
                            downloadButton("download_plot_apiSTATAT", "Download Plot"))
                       ,
                        box(width = 9,
                            uiOutput("uiSTATAT_queryuri"))
                       ,
                        box(width = 12, collapsible = TRUE,
                            title = "Time Series Plot",
                            dygraphOutput("dygraphs_apiSTATAT",
                                          height = "400px"))
                       ,
                        box(width = 12,
                            collapsible = TRUE,
                            dataTableOutput("datatable_apiSTATAT"))

                        )
