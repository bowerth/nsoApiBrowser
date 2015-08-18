## path <- file.path(dbpath, "GitHub", "nsoApi", "inst", "nsoApiBrowser")
## source(file.path(path, "widgets", "apiPXWEB_input.R"))

ui.apiPXWEB.curl <- RCurl::getCurlHandle()
RCurl::curlSetOpt(.opts = list(proxy = isolate(input$sidebar_httpproxy)), curl = ui.apiPXWEB.curl)

output$uiPXWEB_datasetname <- renderUI({

  baseurl <- input$apipxweb_baseurl
  if (baseurl=="http://api.scb.se/OV0104/v1/doris/en/ssd") {
    datasetname_choices <- list(
      NR0103ENS2010T08A = "NR/NR0103/NR0103E/NR0103ENS2010T08A", # Value added, detail components
      NR0103ENS2010T09A = "NR/NR0103/NR0103E/NR0103ENS2010T09A" # Output, intermediate consumption
      )
  } else if (baseurl=="http://pxnet2.stat.fi/PXWEB/api/v1/en/StatFin") {
    datasetname_choices <- list(
      PX_070_vtp_tau_071 = "kan/vtp/070_vtp_tau_071.px", # Value added, detail components
      PX_080_vtp_tau_081 = "kan/vtp/080_vtp_tau_081.px", # "Employment and hours worked 1975-2014"
      PX_180_vtp_tau_181 = "kan/vtp/180_vtp_tau_181.px", # "Gross fixed capital formation 1975-2014"
      PX_190_vtp_tau_191 = "kan/vtp/190_vtp_tau_191.px"  # "Gross capital, Net capital, consumption and retirements of fixed capital 1975
      )
  }

  selectInput("apipxweb_datasetname", "Dataset",
              choices = datasetname_choices,
              ## selected = "NR0103ENS2010T08A",
              ## selected = "NR/NR0103/NR0103E/NR0103ENS2010T08A",
              selected = datasetname_choices[1],
              multiple = FALSE)

})


output$uiPXWEB_queryuri <- renderUI({

    apipxweb_datasetname <- input$apipxweb_datasetname
    if (is.null(apipxweb_datasetname) | length(apipxweb_datasetname)==0) return()

    queryURI <- file.path(input$apipxweb_baseurl, apipxweb_datasetname)
    textInput("apipxweb_queryuri", "Query URI", value = queryURI)

})

## ## Downloads whole dataset - some large, 141 batches each 100.000 values
## .apiPXWEB_queryData <- reactive({
##     req.uri <- file.path(input$apipxweb_baseurl, input$apipxweb_datasetname)
##     bottom_node <- pxweb::get_pxweb_metadata(req.uri)
##     dims <- pxweb::get_pxweb_dims(bottom_node)
##     dims_list <- as.list(rep("*", length(names(dims))))
##     names(dims_list) <- names(dims)
##     queryData <-
##         pxweb::get_pxweb_data(
##             url = req.uri,
##             dims = dims_list,
##             clean = TRUE)
##     names(queryData) <- gsub("[ ]+", ".", names(queryData))
##     names(queryData) <- gsub("[.]+", ".", names(queryData))
##     return(queryData)
## })
## .apiPXWEB_filterlist <- reactive({
##     queryData <- .apiPXWEB_queryData()
##     if (is.null(queryData) | length(queryData)==0) return()
##     filterlist <- names(queryData)[!names(queryData)%in%c("year", "values")]
##     return(filterlist)
## })
## output$uiPXWEB_filtervalues <- renderUI({
##     queryData <- .apiPXWEB_queryData()
##     if (is.null(queryData) | length(queryData)==0) return()
##     filterlist <- .apiPXWEB_filterlist()
##     ## queryData.dimlist <- lapply(subset(queryData, select = filterlist), unique)
##     queryData.dimlist <- lapply(subset(queryData, select = filterlist), function (x) as.character(unique(x)))
##     ui.all <- nsoApi::selectInputs(list = queryData.dimlist,
##                                    prefix = ui.apiPXWEB.prefix,
##                                    minsize = input$sidebar_maxfilter
##                                    )
##     return(ui.all)
## })
## .apiPXWEB_queryData_filter <- reactive({
##     queryData <- .apiPXWEB_queryData()
##     if (is.null(queryData) | length(queryData)==0) return()
##     filterlist <- .apiPXWEB_filterlist() # needed for inputIds
##     filter.param <- paste(filterlist, paste0('input$', ui.apiPXWEB.prefix, filterlist), sep = ' %in% ')
##     eval(parse(text =
##                paste0('queryData.filter <- subset(queryData, ',
##                       paste(filter.param, collapse = " & ")
##                       , ')')
##                ))
##     return(queryData.filter)
## })

## this returns a list, not a character vector!!
## names(filterlist) corresponds to other filterlist of other APIs
.apiPXWEB_filterlist <- reactive({

    ## datasetname <- input$apipxweb_datasetname
    ## if (is.null(datasetname) | length(datasetname)==0) return()
    ## queryuri <- file.path(input$apipxweb_baseurl, datasetname)

    queryuri <- input$apipxweb_queryuri
    if (is.null(queryuri) | length(queryuri)==0) return()

    bottom_node <- pxweb::get_pxweb_metadata(queryuri)
    dims <- pxweb::get_pxweb_dims(bottom_node)

    filterlist <- lapply(dims, function(x) x[["values"]])
    ## names(filterlist) <- unname(sapply(dims, function(x) x[["text"]]))
    names(filterlist) <- names(dims)

    ## quarter?
    ## names(filterlist) <- sub("Year", "Time", names(filterlist))
    ## names(filterlist) <- sub("Vuosi", "Time", names(filterlist))
    ## filterlist <- filterlist[names(filterlist)[!names(filterlist)=="Time"]]

    return(filterlist)

})

output$uiPXWEB_filtervalues <- renderUI({

    filterlist <- .apiPXWEB_filterlist()
    if (is.null(filterlist) | length(filterlist)==0) return()

    ## names(filterlist) <- sub("Vuosi", "Time", names(filterlist))
    ## filterlist <- filterlist[names(filterlist)[!names(filterlist)=="Time"]]
    filterlist_notime <- filterlist[names(filterlist)[!names(filterlist)%in%c("Vuosi", "Tid")]]

    ui.all <- nsoApi::selectInputs(list = filterlist_notime,
                                   prefix = ui.apiPXWEB.prefix,
                                   minsize = input$sidebar_maxfilter
                                   )

    return(ui.all)

  })

.apiPXWEB_queryData_filter <- reactive({

    ## req.uri <- file.path(input$apipxweb_baseurl, input$apipxweb_datasetname)

    ## bottom_node <- pxweb::get_pxweb_metadata(req.uri)
    ## dims <- pxweb::get_pxweb_dims(bottom_node)
    ## dims_list <- as.list(rep("*", length(names(dims))))
    ## names(dims_list) <- names(dims)

    filterlist <- .apiPXWEB_filterlist()
    if (is.null(filterlist) | length(filterlist)==0) return()

    queryuri <- input$apipxweb_queryuri

    ## subset filterlist to actual selection before get_pxweb_data
    filterlist_names <- names(filterlist)

    ## filterlist_names <- sub("Vuosi", "Time", filterlist_names)
    ## filterlist <- filterlist[names(filterlist)[!names(filterlist)=="Time"]]
    ## filterlist_names <- filterlist_names[!filterlist_names=="Time"]
    filterlist_names_notime <- filterlist_names[!filterlist_names%in%c("Vuosi", "Tid")]

    filterlist_subset_notime <- NULL
    ## dim <- filterlist_names[1]
    for (dim in filterlist_names_notime) {
      filterlist_dim <- input[[paste0(ui.apiPXWEB.prefix, dim)]]
      filterlist_subset_notime <- c(filterlist_subset_notime, list(filterlist_dim))
    }
    names(filterlist_subset_notime) <- filterlist_names_notime
    ## filterlist_subset

    ## setdiff(names(filterlist), names(filterlist_subset))
    if ("Vuosi" %in% names(filterlist)) filterlist_subset <- c(filterlist_subset_notime, list(Vuosi = c("*")))
    if ("Tid" %in% names(filterlist)) filterlist_subset <- c(filterlist_subset_notime, list(Tid = c("*")))

    ## queryData <-
    queryData_filter <-
        pxweb::get_pxweb_data(
            url = queryuri,
            dims = filterlist_subset,
            clean = TRUE)

    ## names(queryData) <- gsub("[ ]+", ".", names(queryData))
    ## names(queryData) <- gsub("[.]+", ".", names(queryData))
    names(queryData_filter) <- gsub("[ ]+", ".", names(queryData_filter))
    names(queryData_filter) <- gsub("[.]+", ".", names(queryData_filter))

    return(queryData_filter)

})

.apiPXWEB_queryData_xts <- reactive({

    queryData_filter <- .apiPXWEB_queryData_filter()
    if (is.null(queryData_filter) | length(queryData_filter)==0) return()

    queryData_xts <- nsoApi::pxwebDFtoXTS(data = queryData_filter)

    return(queryData_xts)
})

## output$summary_apiPXWEB <- renderPrint({

##     queryData.xts <- .apiPXWEB_queryData_xts()

##     summary.list = lapply(queryData.xts, summary)

##     return(summary.list)
## })

.apiPXWEB_dygraph <- reactive({

    queryData_xts <- .apiPXWEB_queryData_xts()
    if (is.null(queryData_xts) | length(queryData_xts)==0) return()

    dygraph <- nsoApiBrowser_dygraph(
        data = queryData_xts,
        show.boolean = ifelse("legendshow" %in% input$sidebar_plotoptions, TRUE, FALSE)
    )

    return(dygraph)

})

output$dygraphs_apiPXWEB <- renderDygraph({

    dygraph <- .apiPXWEB_dygraph()
    if (is.null(dygraph) | length(dygraph)==0) return()

    return(dygraph)

})

output$datatable_apiPXWEB <- renderDataTable({

    queryData_filter <- .apiPXWEB_queryData_filter()
    if (is.null(queryData_filter) | length(queryData_filter)==0) return()

    return(queryData_filter)

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
        ## write.csv(.apiPXWEB_queryData(), file, row.names = FALSE)
        write.csv(.apiPXWEB_queryData_filter(), file, row.names = FALSE)

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
