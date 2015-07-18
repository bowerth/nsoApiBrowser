
apiCBS_queryData <- reactive({

    data <- stanApi::cbsODataAPI(api = ui.apiCBS.baseurl,
                                 DSD = input$apicbs_datasetname,
                                 scheme = "TypedDataSet",
                                 query = FALSE)

    return(data)

})

apiCBS_parameterlist <- reactive({

    queryData <- apiCBS_queryData()
    if (is.null(queryData) | length(queryData)==0) return()
    ## queryData <- read.csv(file.path(dlpath, "CBS_82572ENG.csv"))

    varcol.periods <- match("Periods", names(queryData))
    ## gather.cols <- names(queryData)[!names(queryData)%in%c("ID", "Periods")]
    ## gather.cols <- names(queryData)[varcol.begin:length(queryData)]
    ## id.cols <- names(queryData)[!names(queryData)%in%c("ID", "Periods", gather.cols)]
    parameterlist <- names(queryData)[1:varcol.periods]
    parameterlist <- parameterlist[!parameterlist%in%c("ID", "Periods")]

    return(parameterlist) # return "SectorBranchesSIC2008"

})



output$uiCBS_parametervalues <- renderUI({

    queryData <- apiCBS_queryData()
    if (is.null(queryData) | length(queryData)==0) return()

    parameterlist <- apiCBS_parameterlist()

    ui.all <- stanApi::selectInputs(data = queryData,
                                    names = parameterlist,
                                    prefix = ui.apiCBS.prefix,
                                    size = 10)

    return(ui.all)

})

apiCBS_queryData_filter <- reactive({

    queryData <- apiCBS_queryData()
    ## if (is.null(queryData) | length(queryData)==0) return()
    if (is.null(queryData) | length(queryData)==0 | is.null(input) | length(input)==0) return()

    parameterlist <- apiCBS_parameterlist() # needed for inputIds

    ## print(parameterlist)

    ## ## parameter <-  parameterlist[1]
    ## for (parameter in parameterlist) {
    ##     eval(parse(text = paste0('.', ui.apiCBS.prefix, parameter, ' <- input$', ui.apiCBS.prefix, parameter)))
    ## }

    ## print(input)

    filter.param <- paste(parameterlist, paste0('input$', ui.apiCBS.prefix, parameterlist), sep = ' %in% ')
    ## ## filter.param <- paste(parameterlist, paste0('.', ui.apiCBS.prefix, parameterlist), sep = ' %in% ')

    ## queryData.filter <-
    ##     queryData %>%
    ##         ## dplyr::filter_("SectorBranchesSIC2008 %in% c('300025')", "Periods %in% c('1995JJ00')")
    ##         dplyr::filter_(filter.param)

    ## queryData.filter <- subset(queryData, SectorBranchesSIC2008 %in% c("300025"))
    ## queryData.filter <- subset(queryData, SectorBranchesSIC2008 %in% input$apiCBS_SectorBranchesSIC2008)
    eval(parse(text =
                   paste0('queryData.filter <- subset(queryData, ', paste(filter.param, sep = " & "), ')')
               )
         )

    return(queryData.filter)

})

apiCBS_queryData_xts <- reactive({

    queryData.filter <- apiCBS_queryData_filter()

    data.xts <- stanApi::cbsOdataDFtoXTS(data = queryData.filter)

    return(data.xts)
})



## output$summary_apiCBS <- renderPrint({

##     queryData.xts <- apiCBS_queryData_xts()

##     summary.list = lapply(queryData.xts, summary)

##     return(summary.list)
## })

output$dygraphs_apiCBS <- renderDygraph({

    data.xts <- apiCBS_queryData_xts()

    d1 <- dygraph(data = data.xts) %>% dyLegend(width = 400,
                                                hideOnMouseOut = TRUE,
                                                show = "never"
                                                )


    return(d1)

    }
)

output$datatable_apiCBS <- renderDataTable({

    queryData <- apiCBS_queryData()
    queryData.filter <- apiCBS_queryData_filter()
    ## return(queryData)
    return(queryData.filter)

}, options = list(pageLength = 20,
                  lengthMenu = c(10, 20, 50, 100)))

output$download_apiCBS_data <- downloadHandler(
    filename = function() {
        paste0(
            'CBS_',
            input$apicbs_datasetname,
            '.csv')
    },
    content = function(file) {

      if (input$download_apiCBS_data_format=="df") {
          write.csv(apiCBS_queryData(), file, row.names = FALSE)
      } else if (input$download_apiCBS_data_format=="xts") {
          write.csv(as.data.frame(apiCBS_queryData_xts()), file, row.names = TRUE)
      }

}
  )


## output$download_apiCBS_parameter <- downloadHandler(
##     filename = function() {
##         paste0(
##           'apiCBS_parameter',
##           '.rdata')
##       },
##   content = function(file) {
##     api.param <- apiCBS_parametervalues()
##     save(api.param, file = file)
##   }
##   )
