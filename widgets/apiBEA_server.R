ui.apiBEA.curl <- RCurl::getCurlHandle()
RCurl::curlSetOpt(.opts = list(proxy = isolate(input$sidebar_httpproxy)), curl = ui.apiBEA.curl)


## input <- list(apibea_datasetname = "FixedAssets",
##               apibea_resultformat = "JSON")

.apiBEA_parameterlist <- reactive({

  api.param <- list(
    ## USERID = ui.apiBEA.userid,
    USERID = input$apibea_userid,
    METHOD = "GETPARAMETERLIST",
    DATASETNAME = input$apibea_datasetname,
    ## RESULTFORMAT = input$apibea_resultformat
    RESULTFORMAT = ui.apiBEA.resultformat
    )

    data <- nsoApi::beaAPI(api.param = api.param, curl = ui.apiBEA.curl)
    ## parameterlist <- sapply(data[[1]][[2]][[1]], function(x) x$ParameterName)
    parameterlist <- data$BEAAPI$Results$Parameter$ParameterName

    parameterlist <- parameterlist[!tolower(parameterlist)%in%c("year")]
    parameterlist <- parameterlist[!tolower(parameterlist)%in%c("showmillions")]

    return(parameterlist)
})

.apiBEA_parametervalues <- reactive({

    ## apibea_year <- input$apibea_year
    queryyear <- input$sidebar_queryyear
    ## apibea_method <- input$apibea_method
    apibea_datasetname <- input$apibea_datasetname
    ## apibea_resultformat <- input$apibea_resultformat

    apibea_parameterlist <- .apiBEA_parameterlist()

    parametervalue.all <- NULL
    for (d in seq(along = apibea_parameterlist)) {
        parametervalue <- eval(parse(text = paste0('input$apibea_dim', d)))
        parametervalue <- gsub(", ", ",", toString(parametervalue))
        parametervalue <- list(parametervalue)
        names(parametervalue)[1] <- apibea_parameterlist[d]
        parametervalue.all <- c(parametervalue.all, parametervalue)
    }

    ## queryyear <- c(1997,1999)
    ## Add year from time slider
    parametervalue.year <- gsub(" ", "", toString(c(queryyear[1]:queryyear[2])))
    parametervalue.year <- list(parametervalue.year)
    names(parametervalue.year)[1] <- "Year"
    parametervalue.all <- c(parametervalue.all, parametervalue.year)

    api.param <- list(
      ## USERID = ui.apiBEA.userid,
      USERID = input$apibea_userid,
      ## METHOD = apibea_method,
      METHOD = "GETDATA",
      DATASETNAME = apibea_datasetname,
      ## RESULTFORMAT = apibea_resultformat
      RESULTFORMAT = ui.apiBEA.resultformat
      )
    api.param <- c(api.param, parametervalue.all)

    return(api.param)

})

output$uiBEA_parameterlist <- renderUI({

    parameterlist <- .apiBEA_parameterlist()

    parameterlist <- parameterlist[!tolower(parameterlist)%in%c("year")]
    parameterlist <- parameterlist[!tolower(parameterlist)%in%c("showmillions")]

    list(
        selectInput("apibea_parameterlist", "Reduce Dataset",
                    choices = parameterlist,
                    selected = parameterlist, multiple = TRUE, selectize = FALSE)
    )

})

output$uiBEA_parametervalues <- renderUI({

    command.all <- NULL
    for (d in seq(along = input$apibea_parameterlist)) {

        if (input$apibea_parameterlist[d]=="TableID") {
            multiple <- ui.apiBEA.datasetname$multiple[ui.apiBEA.datasetname$name==input$apibea_datasetname]
        } else multiple <- TRUE

        api.param <- list(
          ## USERID = ui.apiBEA.userid,
          USERID = input$apibea_userid,
          METHOD = "GETPARAMETERVALUES",
          DATASETNAME = input$apibea_datasetname,
          PARAMETERNAME = input$apibea_parameterlist[d],
          ## RESULTFORMAT = input$apibea_resultformat
          RESULTFORMAT = ui.apiBEA.resultformat
          )

        data <- nsoApi::beaAPI(api.param = api.param, curl = ui.apiBEA.curl)

        ## print(data)

        ## dim.value <- nsoApi::beaJSONtoDF(List=data, third = 1)[, 1]
        ## dim.value <- data.frame(Key = data$BEAAPI$Results$ParamValue$Key)[ , 1]
        ## dim.value <- data$BEAAPI$Results$ParamValue$Key
        dim.value <- data$BEAAPI$Results$ParamValue[ , 1]
        dim.value <- as.character(dim.value)

        command <- paste0('selectInput("apibea_dim', d, '", "', input$apibea_parameterlist[d], '", ',
                          'choices = ', paste0('c("', gsub(', ', '", "', toString(dim.value)), '")'), ', ',
                          'selected = "', dim.value[1], '", multiple = ', multiple, ', selectize = FALSE)'
                          )
        command.all <- paste(c(command.all, command), collapse = ',\n')
    }

    command.all <- paste0('list(', command.all, ')')

    eval(parse(text = command.all))

})

## output$uiBEA_timeperiod <- renderUI({

##     dim.value <- as.numeric(c(1970:2013))

##     sliderInput("apibea_year", "Time Period:",
##                 min = dim.value[1], max = dim.value[length(dim.value)],
##                 value = c(2010, 2014),
##                 sep = "")
##                 ## format = "###0.")

## })

.apiBEA_queryData <- reactive({

    ## parameterlist <- .apiBEA_parameterlist()
    apibea_parameterlist <- input$apibea_parameterlist
    queryyear <- input$sidebar_queryyear
    ## apibea_method <- input$apibea_method
    apibea_datasetname <- input$apibea_datasetname
    ## apibea_resultformat <- input$apibea_resultformat
    ## apibea_resultraw <- input$apibea_resultraw

    api.param <- .apiBEA_parametervalues()

    if (is.null(api.param) | length(api.param)==0) return()

    ## if (apibea_resultraw) {
    ##     data <- beaAPI(api.param = api.param, curl = ui.apiBEA.curl, raw = TRUE)
    ## } else {
        List <- nsoApi::beaAPI(api.param = api.param, curl = ui.apiBEA.curl)
        ## data <- beaAPI(api.param = api.param, curl = ui.apiBEA.curl, raw = apibea_resultraw)

        ## ## data values are stored in different places of list - variation across datasetname
        ## valuefield <- as.numeric(as.character(ui.apiBEA.datasetname$valuefield[ui.apiBEA.datasetname$name==apibea_datasetname]))
        ## third = valuefield: 4: NIPA, GDPbyIndustry; 7: RegionalData
        ## data <- beaJSONtoDF(List=List, third = valuefield)

    ## data <- nsoApi::beaJSONtoDF(List=List, third = 4)
    ## data <- data.frame(c = c(1,2), d = c(2,3))
    data <- List$BEAAPI$Results$Data

    ## beaJSONtoDF(List=List, third = 2)

        ## distinct.var <- names(data)
        ## ## if ("TimePeriod"%in%names(data)) setnames(data, "TimePeriod", "Year") # NIPA
        ## if ("TimePeriod"%in%names(data)) names(data) <- sub("TimePeriod", "Year", names(data)) # NIPA
        ## distinct.var <- distinct.var[!distinct.var%in%c("Year", "IndustrYDescription", "DataValue", "NoteRef")]
        ## ## data.plots <- data
        ## distinct.col <- data[, colnames(data)%in%distinct.var]
        ## distinct.col2 <- data.frame(apply(distinct.col, 2, function(x) as.character(x)), stringsAsFactors = FALSE)
        ## data$variable <-  apply(distinct.col2, 1, function(x) gsub(", ", ".", toString(x)))
        ## data$DataValue <- as.numeric(as.character(data$DataValue))

    ## }

    return(data)

    ## return(data.d)

})

.apiBEA_filterlist <- reactive({

  filterlist <- as.character(ui.apiBEA.datasetname$filterlist[ui.apiBEA.datasetname$name==input$apibea_datasetname])
  return(filterlist)

})


output$uiBEA_filtervalues <- renderUI({

    queryData <- .apiBEA_queryData()
    if (is.null(queryData) | length(queryData)==0) return()

    filterlist <- .apiBEA_filterlist()

    queryData.dimlist <- lapply(subset(queryData, select = filterlist), unique)

    ## ui.apiBEA.prefix <- "apibea_"

    ui.all <- nsoApi::selectInputs(list = queryData.dimlist,
                                    ## names = filterlist,
                                   prefix = ui.apiBEA.prefix
                                   ,
                                   minsize = 20
                                   )

    return(ui.all)

})

.apiBEA_queryData_filter <- reactive({

    queryData <- .apiBEA_queryData()
    if (is.null(queryData) | length(queryData)==0) return()

    filterlist <- .apiBEA_filterlist() # needed for inputIds

    filter.param <- paste(filterlist, paste0('input$', ui.apiBEA.prefix, filterlist), sep = ' %in% ')
    eval(parse(text =
               paste0('queryData.filter <- subset(queryData, ',
                      paste(filter.param, collapse = " & ")
                      ,
                      ')')
               )
         )

    return(queryData.filter)

})


.apiBEA_queryData_xts <- reactive({

    ## queryData <- .apiBEA_queryData()
    ## ## data <- read.csv(file = file.path(dlpath, ".apiBEA_data.csv"))
    ## queryData.xts <- nsoApi::beaDFtoXTS(data = queryData)

    queryData.filter <- .apiBEA_queryData_filter()
    if (is.null(queryData.filter) | length(queryData.filter) == 0) return()

    queryData.xts <- nsoApi::beaDFtoXTS(data = queryData.filter)

    ## print(queryData.filter)
    ## print(queryData.xts)

    return(queryData.xts)

})

output$summary_apiBEA <- renderPrint({
    ## apibea_datasetname = input$apibea_datasetname
    ## queryyear = input$sidebar_queryyear
    ## apibea_parameterlist = input$apibea_parameterlist
    api.param <- .apiBEA_parametervalues()
    ## .apiBEA_queryData <- .apiBEA_queryData()

    summary.list <- list(
        Parameters = api.param
       ## ,
       ##  Data = h(.apiBEA_queryData)
    )

    ## queryData.xts <- as.data.frame(.apiBEA_queryData_xts())
    ## summary.list = lapply(queryData.xts, summary)

    ## queryData.xts <- read.csv(file.path(dlpath, "data_apiBEA (2).csv"))
    ## h(queryData.xts)


    return(summary.list)
})

output$uiBEA_queryuri <- renderUI({

    api.param <- .apiBEA_parametervalues()
    if (is.null(api.param) | length(api.param)==0) return()
    queryURI <- nsoApi::beaAPI(api.param = api.param, query = TRUE)

    textInput("apibea_queryuri", "Query URI", value = queryURI)

    ## http://www.bea.gov/api/data/?&USERID=7023E825-15FF-488D-B8D9-D70E6F67D439&METHOD=GETDATA&DATASETNAME=NIPA&RESULTFORMAT=JSON&TableID=1&Frequency=A&Year=1970,1971,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015&
    ## return(queryURI)

})

output$datatable_apiBEA <- renderDataTable({

  ## queryData <- .apiBEA_queryData()
  ## return(queryData)

  queryData.filter <- .apiBEA_queryData_filter()
  if (is.null(queryData.filter) | length(queryData.filter) == 0) return()

  drop.col <- c("TableID", "LineNumber", "CL_UNIT", "UNIT_MULT", "NoteRef")
  queryData.filter <- subset(queryData.filter, select = names(queryData.filter)[!names(queryData.filter)%in%drop.col])

  return(queryData.filter)

## }, options = list(
##      pageLength = 20,
##      lengthMenu = c(10, 20, 50, 100)
##      ))
}, options = ui.datatable.options)

.apiBEA_dygraph <- reactive({

    queryData.xts <- .apiBEA_queryData_xts()
    if (is.null(queryData.xts) | length(queryData.xts) == 0) return()

    d1 <- nsoApiBrowser_dygraph(
        data = queryData.xts,
        show.boolean = input$sidebar_dygraphlegendshow
    )

    return(d1)

})

output$dygraphs_apiBEA <- renderDygraph({

        ## require(dygraphs)
        ## require(xts)
        ## require(reshape2)
        ## data(sample_matrix)
        ## rownames(sample_matrix)

        ## apibea_resultraw <- input$apibea_resultraw
        ## queryData <- .apiBEA_queryData()
        ## queryData.xts <- .apiBEA_queryData_xts()
        ## if (is.null(queryData.xts) | length(queryData.xts) == 0) return()

        ## if (apibea_resultraw) return()

        ## queryData <- read.csv(file.path(dlpath, "data_apiBEA.csv"))

        ## queryData.d <- reshape2::dcast(queryData, Year ~ variable, value.var = "DataValue")
        ## h(data.d)

        ## queryData.d$Year <- as.numeric(as.character(queryData.d$Year))
        ## queryData.d$Year <- paste0(queryData.d$Year, '-01-01')

        ## rownames(queryData.d) <- queryData.d$Year
        ## ## queryData.d <- queryData.d[, colnames(queryData.d)!="Year"]
        ## ## need data frame
        ## queryData.d <- subset(queryData.d, select = names(queryData.d)[names(queryData.d)!="Year"])

        ## require(magrittr)
        ## require(dygraphs)

        ## show <- ifelse(input$sidebar_dygraphlegendshow, "always", "never")

        ## d1 <- dygraph(queryData.xts) %>%
        ##   dyLegend(
        ##     width = 400,
        ##     hideOnMouseOut = TRUE,
        ##     ## show = "onmouseover"
        ##     ## show = "never"
        ##     show = show
        ##     )

        ## d1 <- nsoApiBrowser_dygraph(
        ##   data = queryData.xts,
        ##   show.boolean = input$sidebar_dygraphlegendshow
        ##   )

        ## return(d1)

        return(.apiBEA_dygraph())

    }
)

output$download_data_apiBEA <- downloadHandler(
    filename = function() {
        paste0(
            'BEA_',
            input$apibea_datasetname,
            '.csv')
    },
    content = function(file) {

      if (input$apibea_download_data_format=="df_all") {
        write.csv(.apiBEA_queryData(), file, row.names = FALSE)

      } else if (input$apibea_download_data_format=="df_filter") {
        write.csv(.apiBEA_queryData_filter(), file, row.names = FALSE)

      } else if (input$apibea_download_data_format=="xts") {
        write.csv(as.data.frame(.apiBEA_queryData_xts()), file, row.names = TRUE)
      }

  }
  )


output$download_plot_apiBEA <- downloadHandler(
    filename = function() {
        paste0(
            'BEA_',
            input$apibea_datasetname,
            '.html')
    },
    content = function(file) {

        htmlwidgets:::saveWidget(widget = .apiBEA_dygraph(), file = file, selfcontained = TRUE, libdir = NULL)

  }
  )



## output$download_parameter_apiBEA <- downloadHandler(
##     filename = function() {
##         paste0(
##           '.apiBEA_parameter',
##           '.rdata')
##       },
##   content = function(file) {
##     api.param <- .apiBEA_parametervalues()
##     save(api.param, file = file)
##   }
##   )
