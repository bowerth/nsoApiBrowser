## path <- file.path(dbpath, "GitHub", "nsoApi", "inst", "nsoApiBrowser")
## source(file.path(path, "widgets", "apiSSB_input.R"))

ui.apiSSB.curl <- RCurl::getCurlHandle()
RCurl::curlSetOpt(.opts = list(proxy = isolate(input$sidebar_httpproxy)), curl = ui.apiSSB.curl)

output$uiSSB_queryuri <- renderUI({

    apissb_datasetname <- input$apissb_datasetname
    if (is.null(apissb_datasetname) | length(apissb_datasetname)==0) return()

    queryURI <- file.path(ui.apiSSB.baseurl, apissb_datasetname)
    textInput("apissb_queryuri", "Query URI", value = queryURI)

})

## ## Downloads whole dataset - some large, 141 batches each 100.000 values

.apiSSB_queryData <- reactive({

    datasetname <- input$apissb_datasetname
    if (is.null(datasetname) | length(datasetname)==0) return()

    queryData <- nsoApi::ssbAPI(dataset = datasetname, curl = ui.apiSSB.curl)

    ## names(queryData) <- gsub("[ ]+", ".", names(queryData))
    ## names(queryData) <- gsub("[.]+", ".", names(queryData))
    names(queryData) <- gsub("[ ]+", "_", names(queryData))
    ## names(queryData) <- gsub("[_]+", "_", names(queryData))
    names(queryData) <- gsub("[()]", "", names(queryData))
    ## names(queryData) <- gsub(")", "", names(queryData))
    return(queryData)

  })

.apiSSB_filterlist <- reactive({

  queryData <- .apiSSB_queryData()
  if (is.null(queryData) | length(queryData)==0) return()
  
  ## filterlist <- names(queryData)[!names(queryData)%in%c("year", "values")]
  filterlist <- names(queryData)[!names(queryData)%in%c("date", "quarter", "month", "values")]
  
  return(filterlist)
  
})

output$uiSSB_filtervalues <- renderUI({

  queryData <- .apiSSB_queryData()
  if (is.null(queryData) | length(queryData)==0) return()
  
  filterlist <- .apiSSB_filterlist()

  ## queryData.dimlist <- lapply(subset(queryData, select = filterlist), unique)

  queryData.dimlist <- lapply(subset(queryData, select = filterlist), function (x) as.character(unique(x)))
  ui.all <- nsoApi::selectInputs(list = queryData.dimlist,
                                 prefix = ui.apiSSB.prefix,
                                 minsize = input$sidebar_maxfilter
                                 )
  return(ui.all)
})

.apiSSB_queryData_filter <- reactive({

  queryData <- .apiSSB_queryData()
  if (is.null(queryData) | length(queryData)==0) return()

  filterlist <- .apiSSB_filterlist() # needed for inputIds
  filter.param <- paste(filterlist, paste0('input$', ui.apiSSB.prefix, filterlist), sep = ' %in% ')
  eval(parse(text =
             paste0('queryData_filter <- subset(queryData, ',
                      paste(filter.param, collapse = " & ")
                    , ')')
             ))
  return(queryData_filter)
  
})

## ## this returns a list, not a character vector!!
## ## names(filterlist) corresponds to other filterlist of other APIs
## .apiSSB_filterlist <- reactive({

##     datasetname <- input$apissb_datasetname
##     if (is.null(datasetname) | length(datasetname)==0) return()

##     nsoApi::ssbAPI(dataset = datasetname, curl = ui.apiSSB.curl)

##     bottom_node <- ssb::get_ssb_metadata(queryuri)
##     dims <- ssb::get_ssb_dims(bottom_node)

##     filterlist <- lapply(dims, function(x) x[["values"]])
##     ## names(filterlist) <- unname(sapply(dims, function(x) x[["text"]]))
##     names(filterlist) <- names(dims)

##     ## quarter?
##     ## names(filterlist) <- sub("Year", "Time", names(filterlist))
##     ## names(filterlist) <- sub("Vuosi", "Time", names(filterlist))
##     ## filterlist <- filterlist[names(filterlist)[!names(filterlist)=="Time"]]

##     return(filterlist)

## })

## output$uiSSB_filtervalues <- renderUI({

##     filterlist <- .apiSSB_filterlist()
##     if (is.null(filterlist) | length(filterlist)==0) return()

##     ## names(filterlist) <- sub("Vuosi", "Time", names(filterlist))
##     ## filterlist <- filterlist[names(filterlist)[!names(filterlist)=="Time"]]
##     filterlist_notime <- filterlist[names(filterlist)[!names(filterlist)%in%c("Vuosi", "Tid")]]

##     ui.all <- nsoApi::selectInputs(list = filterlist_notime,
##                                    prefix = ui.apiSSB.prefix,
##                                    minsize = input$sidebar_maxfilter
##                                    )

##     return(ui.all)

##   })

## .apiSSB_queryData_filter <- reactive({

##     ## req.uri <- file.path(input$apissb_baseurl, input$apissb_datasetname)

##     ## bottom_node <- ssb::get_ssb_metadata(req.uri)
##     ## dims <- ssb::get_ssb_dims(bottom_node)
##     ## dims_list <- as.list(rep("*", length(names(dims))))
##     ## names(dims_list) <- names(dims)

##     filterlist <- .apiSSB_filterlist()
##     if (is.null(filterlist) | length(filterlist)==0) return()

##     queryuri <- input$apissb_queryuri

##     ## subset filterlist to actual selection before get_ssb_data
##     filterlist_names <- names(filterlist)

##     ## filterlist_names <- sub("Vuosi", "Time", filterlist_names)
##     ## filterlist <- filterlist[names(filterlist)[!names(filterlist)=="Time"]]
##     ## filterlist_names <- filterlist_names[!filterlist_names=="Time"]
##     filterlist_names_notime <- filterlist_names[!filterlist_names%in%c("Vuosi", "Tid")]

##     filterlist_subset_notime <- NULL
##     ## dim <- filterlist_names[1]
##     for (dim in filterlist_names_notime) {
##       filterlist_dim <- input[[paste0(ui.apiSSB.prefix, dim)]]
##       filterlist_subset_notime <- c(filterlist_subset_notime, list(filterlist_dim))
##     }
##     names(filterlist_subset_notime) <- filterlist_names_notime
##     ## filterlist_subset

##     ## setdiff(names(filterlist), names(filterlist_subset))
##     if ("Vuosi" %in% names(filterlist)) filterlist_subset <- c(filterlist_subset_notime, list(Vuosi = c("*")))
##     if ("Tid" %in% names(filterlist)) filterlist_subset <- c(filterlist_subset_notime, list(Tid = c("*")))

##     ## queryData <-
##     queryData_filter <-
##         ssb::get_ssb_data(
##             url = queryuri,
##             dims = filterlist_subset,
##             clean = TRUE)

##     ## names(queryData) <- gsub("[ ]+", ".", names(queryData))
##     ## names(queryData) <- gsub("[.]+", ".", names(queryData))
##     names(queryData_filter) <- gsub("[ ]+", ".", names(queryData_filter))
##     names(queryData_filter) <- gsub("[.]+", ".", names(queryData_filter))

##     return(queryData_filter)

## })

.apiSSB_queryData_xts <- reactive({

    queryData_filter <- .apiSSB_queryData_filter()
    if (is.null(queryData_filter) | length(queryData_filter)==0) return()

    queryData_xts <- nsoApi::pxwebDFtoXTS(data = queryData_filter)

    return(queryData_xts)
})

## output$summary_apiSSB <- renderPrint({

##     queryData.xts <- .apiSSB_queryData_xts()

##     summary.list = lapply(queryData.xts, summary)

##     return(summary.list)
## })

.apiSSB_dygraph <- reactive({

    queryData_xts <- .apiSSB_queryData_xts()
    if (is.null(queryData_xts) | length(queryData_xts)==0) return()

    dygraph <- nsoApiBrowser_dygraph(
        data = queryData_xts,
        show.boolean = ifelse("legendshow" %in% input$sidebar_plotoptions, TRUE, FALSE)
    )

    return(dygraph)

})

output$dygraphs_apiSSB <- renderDygraph({

    dygraph <- .apiSSB_dygraph()
    if (is.null(dygraph) | length(dygraph)==0) return()

    return(dygraph)

})

output$datatable_apiSSB <- renderDataTable({

    queryData_filter <- .apiSSB_queryData_filter()
    if (is.null(queryData_filter) | length(queryData_filter)==0) return()

    return(queryData_filter)

}, options = ui.datatable.options)

output$download_data_apiSSB <- downloadHandler(
    filename = function() {
        paste0(
            'SSB_',
            input$apissb_datasetname,
            '.csv')
    },
    content = function(file) {

      if (input$sidebar_download_data_format=="df_all") {
        ## write.csv(.apiSSB_queryData(), file, row.names = FALSE)
        write.csv(.apiSSB_queryData_filter(), file, row.names = FALSE)

      } else if (input$sidebar_download_data_format=="df_filter") {
        write.csv(.apiSSB_queryData_filter(), file, row.names = FALSE)

      } else if (input$sidebar_download_data_format=="xts") {
        write.csv(as.data.frame(.apiSSB_queryData_xts()), file, row.names = TRUE)
      }

}
  )


output$download_plot_apiSSB <- downloadHandler(
    filename = function() {
        paste0(
            'SSB_',
            input$apissb_datasetname,
            '.html')
    },
    content = function(file) {

        htmlwidgets:::saveWidget(widget = .apiSSB_dygraph(), file = file, selfcontained = TRUE, libdir = NULL)

  }
  )

## output$download_parameter_apiSSB <- downloadHandler(
##     filename = function() {
##         paste0(
##           'apiSSB_parameter',
##           '.rdata')
##       },
##   content = function(file) {
##     api.param <- .apiSSB_filtervalues()
##     save(api.param, file = file)
##   }
##   )
