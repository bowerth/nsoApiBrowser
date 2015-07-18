

## input <- list(apibea_datasetname = "FixedAssets",
##               apibea_resultformat = "JSON")

apiBEA_parameterlist <- reactive({

    api.param <- list(USERID = ui.apiBEA.userid,
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

apiBEA_parametervalues <- reactive({

    apibea_year <- input$apibea_year
    ## apibea_method <- input$apibea_method
    apibea_datasetname <- input$apibea_datasetname
    ## apibea_resultformat <- input$apibea_resultformat

    apibea_parameterlist <- apiBEA_parameterlist()

    parametervalue.all <- NULL
    for (d in seq(along = apibea_parameterlist)) {
        parametervalue <- eval(parse(text = paste0('input$apibea_dim', d)))
        parametervalue <- gsub(", ", ",", toString(parametervalue))
        parametervalue <- list(parametervalue)
        names(parametervalue)[1] <- apibea_parameterlist[d]
        parametervalue.all <- c(parametervalue.all, parametervalue)
    }

    ## apibea_year <- c(1997,1999)
    ## Add year from time slider
    parametervalue.year <- gsub(" ", "", toString(c(apibea_year[1]:apibea_year[2])))
    parametervalue.year <- list(parametervalue.year)
    names(parametervalue.year)[1] <- "Year"
    parametervalue.all <- c(parametervalue.all, parametervalue.year)

    api.param <- list(USERID = ui.apiBEA.userid,
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

    parameterlist <- apiBEA_parameterlist()

    parameterlist <- parameterlist[!tolower(parameterlist)%in%c("year")]
    parameterlist <- parameterlist[!tolower(parameterlist)%in%c("showmillions")]

    list(
        selectInput("apibea_parameterlist", "Filter Dimensions:",
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

        api.param <- list(USERID = ui.apiBEA.userid,
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

        command <- paste0('selectInput("apibea_dim', d, '", "', input$apibea_parameterlist[d], ':", ',
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

apiBEA_queryData <- reactive({

    ## parameterlist <- apiBEA_parameterlist()
    apibea_parameterlist <- input$apibea_parameterlist
    apibea_year <- input$apibea_year
    ## apibea_method <- input$apibea_method
    apibea_datasetname <- input$apibea_datasetname
    ## apibea_resultformat <- input$apibea_resultformat
    ## apibea_resultraw <- input$apibea_resultraw

    api.param <- apiBEA_parametervalues()

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

apiBEA_queryData_xts <- reactive({

    queryData <- apiBEA_queryData()
    ## data <- read.csv(file = file.path(dlpath, "apiBEA_data.csv"))
    queryData.xts <- nsoApi::beaDFtoXTS(data = queryData)

    return(queryData.xts)

})

output$summary_apiBEA <- renderPrint({
    ## apibea_datasetname = input$apibea_datasetname
    ## apibea_year = input$apibea_year
    ## apibea_parameterlist = input$apibea_parameterlist
    apiBEA_parametervalues <- apiBEA_parametervalues()
    ## apiBEA_queryData <- apiBEA_queryData()

    summary.list <- list(
        Parameters = apiBEA_parametervalues
       ## ,
       ##  Data = h(apiBEA_queryData)
    )

    ## queryData.xts <- as.data.frame(apiBEA_queryData_xts())
    ## summary.list = lapply(queryData.xts, summary)

    ## queryData.xts <- read.csv(file.path(dlpath, "data_apiBEA (2).csv"))
    ## h(queryData.xts)


    return(summary.list)
})

output$datatable_apiBEA <- renderDataTable({
    ## queryData <- apiBEA_queryData()
    ## queryData <- cbind.data.frame(TIME = rownames(apiBEA_queryData()), apiBEA_queryData())
    queryData <- apiBEA_queryData()
    ## queryData <- subset(queryData, select = c("variable", "Year", "DataValue"))
    ## queryData$time <- rownames(queryData)
    ## queryData.d <- reshape2::dcast(queryData, Year ~ variable, value.var = "DataValue")
    return(queryData)
}, options = list(pageLength = 20,
                  lengthMenu = c(10, 20, 50, 100)))

output$dygraphs_apiBEA <- renderDygraph({

        ## require(dygraphs)
        ## require(xts)
        ## require(reshape2)
        ## data(sample_matrix)
        ## rownames(sample_matrix)

        ## apibea_resultraw <- input$apibea_resultraw
        ## queryData <- apiBEA_queryData()
        queryData.xts <- apiBEA_queryData_xts()

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
        d1 <- dygraph(queryData.xts) %>% dyLegend(width = 400,
                                                    hideOnMouseOut = TRUE,
                                                    ## show = "onmouseover"
                                                    show = "never"
                                                    )

        return(d1)

    }
)

output$download_apiBEA_data <- downloadHandler(
    filename = function() {
        paste0(
            'BEA_',
            input$apibea_datasetname,
            '.csv')
    },
    content = function(file) {

      if (input$download_apiBEA_data_format=="df") {
          write.csv(apiBEA_queryData(), file, row.names = FALSE)
      } else if (input$download_apiBEA_data_format=="xts") {
          write.csv(as.data.frame(apiBEA_queryData_xts()), file, row.names = TRUE)
      }

  }
  )


## output$download_apiBEA_parameter <- downloadHandler(
##     filename = function() {
##         paste0(
##           'apiBEA_parameter',
##           '.rdata')
##       },
##   content = function(file) {
##     api.param <- apiBEA_parametervalues()
##     save(api.param, file = file)
##   }
##   )
