## path <- file.path(dbpath, "GitHub", "nsoApi", "inst", "nsoApiBrowser")
## source(file.path(path, "widgets", "apiPXWEB_input.R"))

ui.apiPXWEB.curl <- RCurl::getCurlHandle()
RCurl::curlSetOpt(.opts = list(proxy = isolate(input$sidebar_httpproxy)), curl = ui.apiPXWEB.curl)

.apiPXWEB_queryData <- reactive({

    req.uri <- file.path(input$apipxweb_baseurl, input$apipxweb_datasetname)

    bottom_node <- pxweb::get_pxweb_metadata(req.uri)
    dims <- pxweb::get_pxweb_dims(bottom_node)
    dims_list <- as.list(rep("*", length(names(dims))))
    names(dims_list) <- names(dims)

    queryData <-
        pxweb::get_pxweb_data(
            url = req.uri,
            dims = dims_list,
            clean = TRUE)

    names(queryData) <- gsub("[ ]+", ".", names(queryData))
    names(queryData) <- gsub("[.]+", ".", names(queryData))

    return(queryData)

})

.apiPXWEB_filterlist <- reactive({

    queryData <- .apiPXWEB_queryData()
    if (is.null(queryData) | length(queryData)==0) return()

    filterlist <- names(queryData)[!names(queryData)%in%c("year", "values")]

    return(filterlist)

})



output$uiPXWEB_filtervalues <- renderUI({

    queryData <- .apiPXWEB_queryData()
    if (is.null(queryData) | length(queryData)==0) return()

    filterlist <- .apiPXWEB_filterlist()
    ## queryData.dimlist <- lapply(subset(queryData, select = filterlist), unique)
    queryData.dimlist <- lapply(subset(queryData, select = filterlist), function (x) as.character(unique(x)))
    ui.all <- nsoApi::selectInputs(list = queryData.dimlist,
                                   prefix = ui.apiPXWEB.prefix,
                                   minsize = input$sidebar_maxfilter
                                   )

    return(ui.all)

})

.apiPXWEB_queryData_filter <- reactive({

    queryData <- .apiPXWEB_queryData()
    if (is.null(queryData) | length(queryData)==0) return()

    filterlist <- .apiPXWEB_filterlist() # needed for inputIds

    filter.param <- paste(filterlist, paste0('input$', ui.apiPXWEB.prefix, filterlist), sep = ' %in% ')
    eval(parse(text =
               paste0('queryData.filter <- subset(queryData, ',
                      paste(filter.param, collapse = " & ")
                      , ')')
               ))
    return(queryData.filter)

})

.apiPXWEB_queryData_xts <- reactive({

    queryData.filter <- .apiPXWEB_queryData_filter()
    if (is.null(queryData.filter) | length(queryData.filter)==0) return()

    data.xts <- nsoApi::pxwebDFtoXTS(data = queryData.filter)

    return(data.xts)
})

## output$summary_apiPXWEB <- renderPrint({

##     queryData.xts <- .apiPXWEB_queryData_xts()

##     summary.list = lapply(queryData.xts, summary)

##     return(summary.list)
## })

output$uiPXWEB_queryuri <- renderUI({

    queryURI <- file.path(input$apipxweb_baseurl, input$apipxweb_datasetname)

    textInput("apipxweb_queryuri", "Query URI", value = queryURI)

})

.apiPXWEB_dygraph <- reactive({

    queryData.xts <- .apiPXWEB_queryData_xts()

    d1 <- nsoApiBrowser_dygraph(
        data = queryData.xts,
        show.boolean = ifelse("legendshow" %in% input$sidebar_plotoptions, TRUE, FALSE)
    )

    return(d1)

})

output$dygraphs_apiPXWEB <- renderDygraph({

    return(.apiPXWEB_dygraph())

})

output$datatable_apiPXWEB <- renderDataTable({

    queryData.filter <- .apiPXWEB_queryData_filter()
    return(queryData.filter)

}, options = ui.datatable.options)

output$download_data_apiPXWEB <- downloadHandler(
    filename = function() {
        paste0(
            'PXWEB_',
            input$apipxweb_datasetname,
            '.csv')
    },
    content = function(file) {

      if (input$sidebar_download_data_format=="df_all") {
        write.csv(.apiPXWEB_queryData(), file, row.names = FALSE)

      } else if (input$sidebar_download_data_format=="df_filter") {
        write.csv(.apiPXWEB_queryData_filter(), file, row.names = FALSE)

      } else if (input$sidebar_download_data_format=="xts") {
        write.csv(as.data.frame(.apiPXWEB_queryData_xts()), file, row.names = TRUE)
      }

}
  )


output$download_plot_apiPXWEB <- downloadHandler(
    filename = function() {
        paste0(
            'PXWEB_',
            input$apipxweb_datasetname,
            '.html')
    },
    content = function(file) {

        htmlwidgets:::saveWidget(widget = .apiPXWEB_dygraph(), file = file, selfcontained = TRUE, libdir = NULL)

  }
  )

## output$download_parameter_apiPXWEB <- downloadHandler(
##     filename = function() {
##         paste0(
##           'apiPXWEB_parameter',
##           '.rdata')
##       },
##   content = function(file) {
##     api.param <- .apiPXWEB_filtervalues()
##     save(api.param, file = file)
##   }
##   )
