## path <- file.path(dbpath, "GitHub", "nsoApi", "inst", "nsoApiBrowser")
## source(file.path(path, "widgets", "apiWDS_input.R"))

ui.apiWDS.curl <- RCurl::getCurlHandle()
RCurl::curlSetOpt(.opts = list(proxy = isolate(input$sidebar_httpproxy)), curl = ui.apiWDS.curl)

output$uiWDS_queryuri <- renderUI({

    apiwds_datasetname <- input$apiwds_datasetname
    if (is.null(apiwds_datasetname) | length(apiwds_datasetname)==0) return()

    ## queryURI <- file.path(ui.apiWDS.baseurl, apiwds_datasetname)
    queryURI <- wdsAPI(dataset = input$apiwds_datasetname, query = TRUE)
    textInput("apiwds_queryuri", "Query URI", value = queryURI)

})

## ## Downloads whole dataset - some large, 141 batches each 100.000 values

.apiWDS_queryData <- reactive({

    datasetname <- input$apiwds_datasetname
    if (is.null(datasetname) | length(datasetname)==0) return()

    ## queryData <- nsoApi::wdsAPI(dataset = datasetname, curl = ui.apiWDS.curl)
    ## queryData <- nsoApi::wdsAPI(dataset = datasetname, curl = NULL)
    queryData <- nsoApi::wdsAPI(dataset = datasetname)
    ## names(queryData)

    queryData$value <- as.numeric(as.character(queryData$value))

    ## names(queryData) <- gsub("[ ]+", ".", names(queryData))
    ## names(queryData) <- gsub("[.]+", ".", names(queryData))
    ## names(queryData) <- gsub("[ ]+", "_", names(queryData))
    ## names(queryData) <- gsub("[_]+", "_", names(queryData))
    ## names(queryData) <- gsub("[()]", "", names(queryData))
    ## names(queryData) <- gsub(")", "", names(queryData))
    return(queryData)

  })

.apiWDS_filterlist <- reactive({

  queryData <- .apiWDS_queryData()
  if (is.null(queryData) | length(queryData)==0) return()

  filterlist <- names(queryData)[!names(queryData)%in%c("ref_date", "value", "vector", "coordinate")]

  return(filterlist)

})

output$uiWDS_filtervalues <- renderUI({

  queryData <- .apiWDS_queryData()
  if (is.null(queryData) | length(queryData)==0) return()

  filterlist <- .apiWDS_filterlist()

  ## queryData.dimlist <- lapply(subset(queryData, select = filterlist), unique)

  queryData.dimlist <- lapply(subset(queryData, select = filterlist), function (x) as.character(unique(x)))
  ui.all <- nsoApi::selectInputs(list = queryData.dimlist,
                                 prefix = ui.apiWDS.prefix,
                                 minsize = input$sidebar_maxfilter
                                 )
  return(ui.all)
})

.apiWDS_queryData_filter <- reactive({

  queryData <- .apiWDS_queryData()
  if (is.null(queryData) | length(queryData)==0) return()

  filterlist <- .apiWDS_filterlist() # needed for inputIds
  filter.param <- paste(filterlist, paste0('input$', ui.apiWDS.prefix, filterlist), sep = ' %in% ')
  eval(parse(text =
             paste0('queryData_filter <- subset(queryData, ',
                      paste(filter.param, collapse = " & ")
                    , ')')
             ))
  return(queryData_filter)

})

.apiWDS_queryData_xts <- reactive({

    queryData_filter <- .apiWDS_queryData_filter()
    if (is.null(queryData_filter) | length(queryData_filter)==0) return()

    queryData_xts <- nsoApi::wdsDFtoXTS(data = queryData_filter)

    return(queryData_xts)
})

.apiWDS_dygraph <- reactive({

    queryData_xts <- .apiWDS_queryData_xts()
    if (is.null(queryData_xts) | length(queryData_xts)==0) return()

    dygraph <- nsoApiBrowser_dygraph(
        data = queryData_xts,
        show.boolean = ifelse("legendshow" %in% input$sidebar_plotoptions, TRUE, FALSE)
    )

    return(dygraph)

})

output$dygraphs_apiWDS <- renderDygraph({

    dygraph <- .apiWDS_dygraph()
    if (is.null(dygraph) | length(dygraph)==0) return()

    return(dygraph)

})

output$datatable_apiWDS <- renderDataTable({

    queryData_filter <- .apiWDS_queryData_filter()
    if (is.null(queryData_filter) | length(queryData_filter)==0) return()

    return(queryData_filter)

}, options = ui.datatable.options)

output$download_data_apiWDS <- downloadHandler(
    filename = function() {
        paste0(
            'WDS_',
            input$apiwds_datasetname,
            '.csv')
    },
    content = function(file) {

      if (input$sidebar_download_data_format=="df_all") {
        ## write.csv(.apiWDS_queryData(), file, row.names = FALSE)
        write.csv(.apiWDS_queryData_filter(), file, row.names = FALSE)

      } else if (input$sidebar_download_data_format=="df_filter") {
        write.csv(.apiWDS_queryData_filter(), file, row.names = FALSE)

      } else if (input$sidebar_download_data_format=="xts") {
        write.csv(as.data.frame(.apiWDS_queryData_xts()), file, row.names = TRUE)
      }

}
  )

output$download_plot_apiWDS <- downloadHandler(
    filename = function() {
        paste0(
            'WDS_',
            input$apiwds_datasetname,
            '.html')
    },
    content = function(file) {

        htmlwidgets:::saveWidget(widget = .apiWDS_dygraph(), file = file, selfcontained = TRUE, libdir = NULL)

  }
  )
