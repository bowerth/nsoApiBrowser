## path <- file.path(dbpath, "GitHub", "nsoApi", "inst", "nsoApiBrowser")
## source(file.path(path, "widgets", "apiSTATAT_input.R"))

ui.apiSTATAT.curl <- RCurl::getCurlHandle()
RCurl::curlSetOpt(.opts = list(proxy = isolate(input$sidebar_httpproxy)), curl = ui.apiSTATAT.curl)

output$uiSTATAT_queryuri <- renderUI({

    apistatat_datasetname <- input$apistatat_datasetname
    if (is.null(apistatat_datasetname) | length(apistatat_datasetname)==0) return()

    queryURI <- nsoApi::statatAPI(dataset=apistatat_datasetname, query=TRUE)

    textInput("apistatat_queryuri", "Query URI", value = queryURI)

})

.apiSTATAT_queryData <- reactive({

    apistatat_datasetname <- input$apistatat_datasetname
    if (is.null(apistatat_datasetname) | length(apistatat_datasetname)==0) return()

    queryData <- nsoApi::statatAPI(dataset=apistatat_datasetname, query=FALSE, curl = ui.apiSTATAT.curl)
    ## queryData <- nsoApi::statatAPI(dataset=apistatat_datasetname, query=FALSE, curl = NULL)
    ## h(queryData)

    return(queryData)

})

.apiSTATAT_filterlist <- reactive({

    queryData <- .apiSTATAT_queryData()
    if (is.null(queryData) | length(queryData)==0) return()

    filterlist <- names(queryData)[!names(queryData)%in%c("date", "value")]

    return(filterlist)

})

output$uiSTATAT_filtervalues <- renderUI({

    filterlist <- .apiSTATAT_filterlist()
    if (is.null(filterlist) | length(filterlist)==0) return()

    queryData <- .apiSTATAT_queryData()

    filtervalues <- lapply(subset(queryData, select = filterlist), function (x) as.character(unique(x)))

    ui.all <- nsoApi::selectInputs(list = filtervalues,
                                   prefix = ui.apiSTATAT.prefix,
                                   minsize = input$sidebar_maxfilter
                                   )

    return(ui.all)

  })

.apiSTATAT_queryData_filter <- reactive({

    filterlist <- .apiSTATAT_filterlist()
    if (is.null(filterlist) | length(filterlist)==0) return()

    queryData <- .apiSTATAT_queryData()

    filter_param <- paste(filterlist, paste0('input$', ui.apiSTATAT.prefix, filterlist), sep = ' %in% ')
    eval(parse(text =
               paste0('queryData_filter <- subset(queryData, ',
                      paste(filter_param, collapse = " & ")
                      , ')')
               ))
    ## h(queryData)
    ## h(queryData_filter)
    return(queryData_filter)

})

.apiSTATAT_queryData_xts <- reactive({

    queryData_filter <- .apiSTATAT_queryData_filter()
    if (is.null(queryData_filter) | length(queryData_filter)==0) return()

    queryData_xts <- nsoApi::statatDFtoXTS(data = queryData_filter)

    return(queryData_xts)
})

.apiSTATAT_dygraph <- reactive({

    queryData_xts <- .apiSTATAT_queryData_xts()
    if (is.null(queryData_xts) | length(queryData_xts)==0) return()

    dygraph <- nsoApiBrowser_dygraph(
        data = queryData_xts,
        show.boolean = ifelse("legendshow" %in% input$sidebar_plotoptions, TRUE, FALSE)
    )

    return(dygraph)

})

output$dygraphs_apiSTATAT <- renderDygraph({

    dygraph <- .apiSTATAT_dygraph()
    if (is.null(dygraph) | length(dygraph)==0) return()

    return(dygraph)

})

output$datatable_apiSTATAT <- renderDataTable({

    queryData_filter <- .apiSTATAT_queryData_filter()
    if (is.null(queryData_filter) | length(queryData_filter)==0) return()

    return(queryData_filter)

}, options = ui.datatable.options)

output$download_data_apiSTATAT <- downloadHandler(
    filename = function() {
        paste0(
            'STATAT_',
            input$apistatat_datasetname,
            '.csv')
    },
    content = function(file) {

      if (input$sidebar_download_data_format=="df_all") {
        ## write.csv(.apiSTATAT_queryData(), file, row.names = FALSE)
        write.csv(.apiSTATAT_queryData_filter(), file, row.names = FALSE)

      } else if (input$sidebar_download_data_format=="df_filter") {
        write.csv(.apiSTATAT_queryData_filter(), file, row.names = FALSE)

      } else if (input$sidebar_download_data_format=="xts") {
        write.csv(as.data.frame(.apiSTATAT_queryData_xts()), file, row.names = TRUE)
      }

}
  )


output$download_plot_apiSTATAT <- downloadHandler(
    filename = function() {
        paste0(
            'STATAT_',
            input$apistatat_datasetname,
            '.html')
    },
    content = function(file) {

        htmlwidgets:::saveWidget(widget = .apiSTATAT_dygraph(), file = file, selfcontained = TRUE, libdir = NULL)

  }
  )

