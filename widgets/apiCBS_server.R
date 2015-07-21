ui.apiCBS.curl <- RCurl::getCurlHandle()
RCurl::curlSetOpt(.opts = list(proxy = isolate(input$sidebar_httpproxy)), curl = ui.apiCBS.curl)

.apiCBS_queryData <- reactive({

    data <- nsoApi::cbsODataAPI(api = ui.apiCBS.baseurl,
                                DSD = input$apicbs_datasetname,
                                scheme = "TypedDataSet",
                                query = FALSE,
                                curl = ui.apiCBS.curl)

    data.m <- nsoApi::cbsOdataDFgather(data)
    ## return(data)
    return(data.m)

})

.apiCBS_filterlist <- reactive({

    queryData <- .apiCBS_queryData()
    if (is.null(queryData) | length(queryData)==0) return()
    ## queryData <- read.csv(file.path(dlpath, "CBS_82572ENG.csv"))

    ## queryData <- data.m
    ## varcol.periods <- match("Periods", names(queryData))
    ## gather.cols <- names(queryData)[!names(queryData)%in%c("ID", "Periods")]
    ## gather.cols <- names(queryData)[varcol.begin:length(queryData)]
    filterlist <- names(queryData)[!names(queryData)%in%c("ID", "Periods", "VALUE")]
    ## filterlist <- names(queryData)[1:varcol.periods]
    ## filterlist <- filterlist[!filterlist%in%c("ID", "Periods")]
    ## filterlist <- filterlist[!filterlist%in%c("ID", "Periods")]

    return(filterlist) # return "SectorBranchesSIC2008"

})



output$uiCBS_filtervalues <- renderUI({

    queryData <- .apiCBS_queryData()
    if (is.null(queryData) | length(queryData)==0) return()
    filterlist <- .apiCBS_filterlist()
    queryData.dimlist <- lapply(subset(queryData, select = filterlist), unique)
    ui.all <- nsoApi::selectInputs(list = queryData.dimlist,
                                    prefix = ui.apiCBS.prefix,
                                    minsize = 10)
    return(ui.all)

})

.apiCBS_queryData_filter <- reactive({

    queryData <- .apiCBS_queryData()
    if (is.null(queryData) | length(queryData)==0) return()

    filterlist <- .apiCBS_filterlist() # needed for inputIds

    filter.param <- paste(filterlist, paste0('input$', ui.apiCBS.prefix, filterlist), sep = ' %in% ')
    eval(parse(text =
               paste0('queryData.filter <- subset(queryData, ',
                      paste(filter.param, collapse = " & ")
                      , ')')
               ))
    return(queryData.filter)

})

.apiCBS_queryData_xts <- reactive({

    queryData.filter <- .apiCBS_queryData_filter()
    if (is.null(queryData.filter) | length(queryData.filter)==0) return()

    ## queryData.filter <- read.csv(file.path(dlpath, "CBS_82572ENG.csv"))
    data.xts <- nsoApi::cbsOdataDFtoXTS(data = queryData.filter)

    return(data.xts)
})

## output$summary_apiCBS <- renderPrint({

##     queryData.xts <- .apiCBS_queryData_xts()

##     summary.list = lapply(queryData.xts, summary)

##     return(summary.list)
## })

output$uiCBS_queryuri <- renderUI({

    ## api.param <- .apiCBS_parametervalues()
    ## if (is.null(api.param) | length(api.param)==0) return()

    queryURI <- nsoApi::cbsODataAPI(api = ui.apiCBS.baseurl,
                                    DSD = input$apicbs_datasetname,
                                    scheme = "TypedDataSet",
                                    query = TRUE)

    textInput("apicbs_queryuri", "Query URI", value = queryURI)

    ## http://www.bea.gov/api/data/?&USERID=7023E825-15FF-488D-B8D9-D70E6F67D439&METHOD=GETDATA&DATASETNAME=NIPA&RESULTFORMAT=JSON&TableID=1&Frequency=A&Year=1970,1971,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015&
    ## return(queryURI)

})

.apiCBS_dygraph <- reactive({

    queryData.xts <- .apiCBS_queryData_xts()

    ## d1 <- dygraph(data = data.xts) %>% dyLegend(width = 400,
    ##                                             hideOnMouseOut = TRUE,
    ##                                             show = "never"
    ##                                             )

    d1 <- nsoApiBrowser_dygraph(
      data = queryData.xts,
      show.boolean = input$sidebar_dygraphlegendshow
      )

    return(d1)

})

output$dygraphs_apiCBS <- renderDygraph({

    return(.apiCBS_dygraph())

})

output$datatable_apiCBS <- renderDataTable({

    ## queryData <- .apiCBS_queryData()
    ## return(queryData)
    queryData.filter <- .apiCBS_queryData_filter()
    return(queryData.filter)

}, options = ui.datatable.options)

output$download_data_apiCBS <- downloadHandler(
    filename = function() {
        paste0(
            'CBS_',
            input$apicbs_datasetname,
            '.csv')
    },
    content = function(file) {

      if (input$download_data_format_apiCBS=="df_all") {
        write.csv(.apiCBS_queryData(), file, row.names = FALSE)

      } else if (input$download_data_format_apiCBS=="df_filter") {
        write.csv(.apiCBS_queryData_filter(), file, row.names = FALSE)

      } else if (input$download_data_format_apiCBS=="xts") {
        write.csv(as.data.frame(.apiCBS_queryData_xts()), file, row.names = TRUE)
      }

}
  )


output$download_plot_apiCBS <- downloadHandler(
    filename = function() {
        paste0(
            'CBS_',
            input$apicbs_datasetname,
            '.html')
    },
    content = function(file) {

        htmlwidgets:::saveWidget(widget = .apiCBS_dygraph(), file = file, selfcontained = TRUE, libdir = NULL)

  }
  )

## output$download_parameter_apiCBS <- downloadHandler(
##     filename = function() {
##         paste0(
##           'apiCBS_parameter',
##           '.rdata')
##       },
##   content = function(file) {
##     api.param <- .apiCBS_filtervalues()
##     save(api.param, file = file)
##   }
##   )
