ui.apiONS.curl <- RCurl::getCurlHandle()
RCurl::curlSetOpt(.opts = list(proxy = isolate(input$sidebar_httpproxy)), curl = ui.apiONS.curl)

.apiONS_queryData <- reactive({

    ## if (is.null(input$apions_context) | length(input$apions_context)==0) return()

    api.param <- list(
        context = as.character(ui.apiONS.datasetname$Context[ui.apiONS.datasetname$ID==input$apions_datasetname]),
        geog = "2011WARDH",
        totals = "false",
        apikey = apiKey$ONS$apikey)

    method = ifelse(Sys.info()[["sysname"]]=="Linux", "wget", "auto")

    data <- onsCsvData(api.param = api.param,
                       dataset = input$apions_datasetname,
                       curl = ui.apiONS.curl,
                       method = method
                       )

    data.m <- nsoApi::onsDFgather(data)
    ## return(data)
    return(data.m)

})

.apiONS_filterlist <- reactive({

    queryData <- .apiONS_queryData()
    if (is.null(queryData) | length(queryData)==0) return()

    ## filterlist <- names(queryData)[!names(queryData)%in%c("ID", "Periods", "VALUE")]
    filterlist <- names(queryData)[!names(queryData)%in%c("VALUE")]

    return(filterlist) # return "SectorBranchesSIC2008"

})



output$uiONS_filtervalues <- renderUI({

    queryData <- .apiONS_queryData()
    if (is.null(queryData) | length(queryData)==0) return()
    filterlist <- .apiONS_filterlist()
    queryData.dimlist <- lapply(subset(queryData, select = filterlist), unique)
    ui.all <- nsoApi::selectInputs(list = queryData.dimlist,
                                   prefix = ui.apiONS.prefix,
                                   ## minsize = 10
                                   minsize = input$sidebar_maxfilter
                                   )
    return(ui.all)

})

.apiONS_queryData_filter <- reactive({

    queryData <- .apiONS_queryData()
    if (is.null(queryData) | length(queryData)==0) return()

    filterlist <- .apiONS_filterlist() # needed for inputIds

    filter.param <- paste(filterlist, paste0('input$', ui.apiONS.prefix, filterlist), sep = ' %in% ')
    eval(parse(text =
               paste0('queryData.filter <- subset(queryData, ',
                      paste(filter.param, collapse = " & ")
                      , ')')
               ))
    return(queryData.filter)

})

## .apiONS_queryData_xts <- reactive({

##     queryData.filter <- .apiONS_queryData_filter()
##     if (is.null(queryData.filter) | length(queryData.filter)==0) return()

##     ## queryData.filter <- read.csv(file.path(dlpath, "ONS_82572ENG.csv"))
##     data.xts <- nsoApi::onsDFtoXTS(data = queryData.filter)

##     return(data.xts)
## })

.apiONS_queryData_dimple <- reactive({

    queryData.filter <- .apiONS_queryData_filter()
    if (is.null(queryData.filter) | length(queryData.filter)==0) return()

    ## queryData.filter <- read.csv(file.path(dlpath, "ONS_82572ENG.csv"))
    ## data.xts <- nsoApi::onsDFtoXTS(data = queryData.filter)
    data.dimple <- queryData.filter

    return(data.dimple)
})

## output$summary_apiONS <- renderPrint({

##     queryData.xts <- .apiONS_queryData_xts()

##     summary.list = lapply(queryData.xts, summary)

##     return(summary.list)
## })

output$uiONS_queryuri <- renderUI({

    ## if (input$apions_datasetname=="-- please select a dataset --") return()

    api.param <- list(
        context = as.character(ui.apiONS.datasetname$Context[ui.apiONS.datasetname$ID==input$apions_datasetname]),
        geog = "2011WARDH",
        totals = "false",
        apikey = apiKey$ONS$apikey)

    queryURI <- nsoApi::onsCsvData(api.param = api.param,
                                   dataset = input$apions_datasetname,
                                   curl = ui.apiONS.curl,
                                   query = TRUE
                                   )

    textInput("apions_queryuri", "Query URI", value = queryURI)

})

## .apiONS_dygraph <- reactive({

##     queryData.xts <- .apiONS_queryData_xts()

##     ## d1 <- dygraph(data = data.xts) %>% dyLegend(width = 400,
##     ##                                             hideOnMouseOut = TRUE,
##     ##                                             show = "never"
##     ##                                             )

##     d1 <- nsoApiBrowser_dygraph(
##       data = queryData.xts,
##       show.boolean = input$sidebar_dygraphlegendshow
##       )

##     return(d1)

## })

## output$dygraphs_apiONS <- renderDygraph({

##     return(.apiONS_dygraph())

## })

.apiONS_dimple <- reactive({

    queryData.dimple <- .apiONS_queryData_dimple()

    ## filter_id <- round(runif(n = 8, min = 0, max = length(unique(data$geographic_id))))
    ## filter_id <- unique(data$geographic_id)[filter_id]

    ## example 1 vt bar
    d1 <-
        queryData.dimple %>%
            ## dplyr::filter(TRANSACT != "total_all_categories_religion") %>%
            ## dplyr::filter(geographic_id %in% filter_id) %>%
            ## rcdimple::dimple(x ="TRANSACT", y = "VALUE", type = "bar")
            rcdimple::dimple(x ="geographic_id", y = "VALUE", group = "TRANSACT", type = "bar") ## %>%

    if ("percentaxis" %in% input$sidebar_plotoptions) {
        d1 <- d1 %>% yAxis( type = "addPctAxis" )
    }
    if ("legendshow" %in% input$sidebar_plotoptions) {
        d1 <- d1 %>% add_legend()
    }

    return(d1)
})


output$dimple_apiONS <- renderDimple({

    return(.apiONS_dimple())

})

output$datatable_apiONS <- renderDataTable({

    ## queryData <- .apiONS_queryData()
    ## return(queryData)
    queryData.filter <- .apiONS_queryData_filter()
    return(queryData.filter)

}, options = ui.datatable.options)

output$download_data_apiONS <- downloadHandler(
    filename = function() {
        paste0(
            'ONS_',
            input$apions_datasetname,
            '.csv')
    },
    content = function(file) {

      if (input$sidebar_download_data_format=="df_all") {
        write.csv(.apiONS_queryData(), file, row.names = FALSE)

      } else if (input$sidebar_download_data_format=="df_filter") {
        write.csv(.apiONS_queryData_filter(), file, row.names = FALSE)

      } else if (input$sidebar_download_data_format=="xts") {
        ## write.csv(as.data.frame(.apiONS_queryData_xts()), file, row.names = TRUE)
        write.csv(as.data.frame(.apiONS_queryData_dimple()), file, row.names = TRUE)
      }

}
  )


output$download_plot_apiONS <- downloadHandler(
    filename = function() {
        paste0(
            'ONS_',
            input$apions_datasetname,
            '.html')
    },
    content = function(file) {

        ## htmlwidgets:::saveWidget(widget = .apiONS_dygraph(), file = file, selfcontained = TRUE, libdir = NULL)
        htmlwidgets:::saveWidget(widget = .apiONS_dimple(), file = file, selfcontained = TRUE, libdir = NULL)

  }
  )

## output$download_parameter_apiONS <- downloadHandler(
##     filename = function() {
##         paste0(
##           'apiONS_parameter',
##           '.rdata')
##       },
##   content = function(file) {
##     api.param <- .apiONS_filtervalues()
##     save(api.param, file = file)
##   }
##   )
