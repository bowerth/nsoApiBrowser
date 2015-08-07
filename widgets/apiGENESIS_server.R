ui.apiGENESIS.curl <- RCurl::getCurlHandle()
## ui.proxy defined in global.R
## RCurl::curlSetOpt(.opts = list(proxy = ui.httpproxy), curl = ui.apiGENESIS.curl)
RCurl::curlSetOpt(.opts = list(proxy = isolate(input$sidebar_httpproxy)), curl = ui.apiGENESIS.curl)

## output$uiaB_timeperiod <- renderUI({

##     dim.value <- as.numeric(c(1970:2013))

##     sliderInput("apibea_year", "Time Period:",
##                 min = dim.value[1], max = dim.value[length(dim.value)],
##                 value = c(2010, 2014),
##                 sep = "")
##                 ## format = "###0.")

## })
## input <- list(apibea_
## output <- NULL


.apiGENESIS_parametervalues <- reactive({

    api.param.datenexport <- list(
        method = "DatenExport",
        ## kennung = ui.apiGENESIS.kennung,
        ## passwort = ui.apiGENESIS.passwort,
        kennung = input$apigenesis_kennung,
        passwort = input$apigenesis_passwort,
      ## namen = "81000BJ002",
        ## namen = "81000BJ105",
        ## namen = "81000BJ102",
        namen = input$apigenesis_datasetname,
        bereich = "oeffentlich",
        format = "csv",
        werte = "true",
        metadaten = "false",
        zusatz = "false",
        startjahr = "",
        endjahr = "",
        zeitscheiben = "",
        regionalschluessel = "",
        sachmerkmal = "",
        sachschluessel = "",
        stand = "01.01.2001",
        sprache = "de"
    )

    return(api.param.datenexport)

})

.apiGENESIS_queryData <- reactive({

    ## apibea_parameterlist <- input$apibea_parameterlist
    ## apibea_year <- input$apibea_year
    ## apibea_datasetname <- input$apibea_datasetname

    api.param.datenexport <- .apiGENESIS_parametervalues()
    ## if (is.null(api.param.datenexport) | length(api.param.datenexport)==0) return()

    xml.list.datenexport <- nsoApi::genesisAPI(api.param = api.param.datenexport,
                                                service = "ExportService",
                                                curl = ui.apiGENESIS.curl)

    ## data <- nsoApi::beaJSONtoDF(List=List, third = 4)
    data <- nsoApi::genesisXMLtoDF(xml.list = xml.list.datenexport)

    return(data)

})


.apiGENESIS_filterlist <- reactive({

    queryData <- .apiGENESIS_queryData()
    if (is.null(queryData) | length(queryData)==0) return()

    ## queryData <- read.csv(file.path(dlpath, "GENESIS_81000BJ002.csv"))
    ## h(queryData)
    ## names(queryData) <- sub("ZI.WERT", "ZI-WERT", names(queryData))

    varcol.ziwert <- match("ZI-WERT", names(queryData))
    ## gather.cols <- names(queryData)[!names(queryData)%in%c("ID", "ZI-WERT")]
    ## gather.cols <- names(queryData)[varcol.begin:length(queryData)]
    ## id.cols <- names(queryData)[!names(queryData)%in%c("ID", "ZI-WERT", gather.cols)]
    filterlist <- names(queryData)[1:(varcol.ziwert - 1)]
    ## filterlist <- filterlist[!filterlist%in%c("ZI-WERT")]

    ## UNIT added from at melting by nsoApi::genesisXMLtoDF()
    filterlist <- c("UNIT", filterlist)

    return(filterlist) # return "SectorBranchesSIC2008"

})

output$uiGENESIS_filtervalues <- renderUI({

    queryData <- .apiGENESIS_queryData()


    if (is.null(queryData) | length(queryData)==0) return()

    filterlist <- .apiGENESIS_filterlist()

    queryData.dimlist <- lapply(subset(queryData, select = filterlist), unique)

    ## ui.apiGENESIS.prefix <- "apigenesis_"

    ui.all <- nsoApi::selectInputs(list = queryData.dimlist,
                                    ## names = filterlist,
                                   prefix = ui.apiGENESIS.prefix
                                   ,
                                   ## minsize = 10
                                   minsize = input$sidebar_maxfilter
                                   )

    return(ui.all)

})

.apiGENESIS_queryData_filter <- reactive({

    queryData <- .apiGENESIS_queryData()
    if (is.null(queryData) | length(queryData)==0) return()

    filterlist <- .apiGENESIS_filterlist() # needed for inputIds

    ## querxyData <- read.csv(file = file.path(dlpath, "GENESIS_82572ENG.csv"))
    ## names(queryData)
    ## filterlist <- "SectorBranchesSIC2008"

    ## input <- list(apigenesis_VGRPB5 = c("VGRJPM"),
    ##               apigenesis_WZ08G2 = c("WZ08-A"),
    ##               apigenesis_DINSG = c("DG"))

    filter.param <- paste(filterlist, paste0('input$', ui.apiGENESIS.prefix, filterlist), sep = ' %in% ')
    eval(parse(text =
               paste0('queryData.filter <- subset(queryData, ',
                      paste(filter.param, collapse = " & ")
                      ,
                      ')')
               )
         )

    return(queryData.filter)

})


.apiGENESIS_queryData_xts <- reactive({

    ## queryData <- .apiGENESIS_queryData()

    queryData.filter <- .apiGENESIS_queryData_filter()
    if (is.null(queryData.filter) | length(queryData.filter)==0) return()

    ## queryData.filter <- read.csv(file.path(dlpath, "GENESIS_81000BJ002.csv"))
    ## h(queryData.filter)
    ## names(queryData.filter) <- sub("ZI.WERT", "ZI-WERT", names(queryData.filter))

    ## h(queryData.filter)
    ## ## nsoApi::genesisDFtoXTS
    ## data <- queryData.filter

    ## data <- subset(data, select = names(data)[!names(data) %in%
    ##     c("K", "QUALITAET", "GESPERRT", "WERT-VERFAELSCHT")])
    ## names(data) <- sub("ZI-WERT", "ZEIT", names(data))
    ## pivot.formula <- formula(paste("ZEIT ~", gsub(", ", " + ",
    ##     toString(names(data)[!names(data) %in% c("ZEIT", "WERT")]))))
    ## data.d <- reshape2::dcast(data, pivot.formula, value.var = "WERT")
    ## rownames(data.d) <- paste0(as.character(data.d$ZEIT), "-01-01")
    ## data.d <- subset(data.d, select = names(data.d)[names(data.d)!="ZEIT"])
    ## ## data.d <- data.d[, -1]
    ## data.xts <- xts::as.xts(data.d, dateFormate = "Date")

    ## data.xts <- nsoApi::genesisDFtoXTS(data = queryData.filter)

    ## h(data.xts)
    ## rownames(data.xts)
    ## write.csv(data.xts, file = file.path(dlpath, "apiGENESIS_data_xts.csv"))

    ## queryData.xts <- nsoApi::genesisDFtoXTS(data = queryData)
    queryData.xts <- nsoApi::genesisDFtoXTS(data = queryData.filter)
    return(queryData.xts)

})

## output$summary_apiGENESIS <- renderPrint({

##     queryData.xts <- .apiGENESIS_queryData_xts()
##     summary.list = lapply(queryData.xts, summary)
##     return(summary.list)

## })

output$uiGENESIS_queryuri <- renderUI({

    api.param.datenexport <- .apiGENESIS_parametervalues()
    if (is.null(api.param.datenexport) | length(api.param.datenexport)==0) return()

    queryURI <- nsoApi::genesisAPI(api.param = api.param.datenexport,
                                   service = "ExportService",
                                   query = TRUE)

    textInput("apigenesis_queryuri", "Query URI", value = queryURI)

    ## https://www-genesis.destatis.de/genesisWS/services/ExportService?method=DatenExport&kennung=GP103009&passwort=STI004&namen=81000BJ002&bereich=oeffentlich&format=csv&werte=true&metadaten=false&zusatz=false&startjahr=&endjahr=&zeitscheiben=&regionalschluessel=&sachmerkmal=&sachschluessel=&stand=01.01.2001&sprache=de

})

.apiGENESIS_dygraph <- reactive({

    queryData.xts <- .apiGENESIS_queryData_xts()
    if (is.null(queryData.xts) | length(queryData.xts)==0) return()

    d1 <- nsoApiBrowser_dygraph(
        data = queryData.xts,
        ## show.boolean = input$sidebar_legendshow
        show.boolean = ifelse("legendshow" %in% input$sidebar_plotoptions, TRUE, FALSE)
    )

    return(d1)

})

output$dygraphs_apiGENESIS <- renderDygraph({

    return(.apiGENESIS_dygraph())

    }
)

output$datatable_apiGENESIS <- renderDataTable({

    ## queryData <- .apiGENESIS_queryData()
    ## return(queryData)
    queryData.filter <- .apiGENESIS_queryData_filter()
    if (is.null(queryData.filter) | length(queryData.filter)==0) return()

    return(queryData.filter)

}, options = ui.datatable.options)

output$download_data_apiGENESIS <- downloadHandler(
    filename = function() {
        paste0(
          'GENESIS_',
            input$apigenesis_datasetname,
          '.csv')
      },
  content = function(file) {

      if (input$sidebar_download_data_format=="df_all") {
        write.csv(.apiGENESIS_queryData(), file, row.names = FALSE)

      } else if (input$sidebar_download_data_format=="df_filter") {
        write.csv(.apiGENESIS_queryData_filter(), file, row.names = FALSE)

      } else if (input$sidebar_download_data_format=="xts") {
        write.csv(as.data.frame(.apiGENESIS_queryData_xts()), file, row.names = TRUE)
      }

  }
  )

output$download_plot_apiGENESIS <- downloadHandler(
    filename = function() {
        paste0(
            'GENESIS_',
            input$apigenesis_datasetname,
            '.html')
    },
    content = function(file) {

        htmlwidgets:::saveWidget(widget = .apiGENESIS_dygraph(), file = file, selfcontained = TRUE, libdir = NULL)

  }
  )

## output$download_parameter_apiGENESIS <- downloadHandler(
##     filename = function() {
##         paste0(
##           'apiGENESIS_parameter',
##           '.rdata')
##       },
##   content = function(file) {
##     api.param <- .apiGENESIS_parametervalues()
##     save(api.param, file = file)
##   }
##   )
