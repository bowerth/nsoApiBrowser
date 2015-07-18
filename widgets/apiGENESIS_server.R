
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

apiGENESIS_parametervalues <- reactive({

    api.param.datenexport <- list(
        method = "DatenExport",
        kennung = ui.apiGENESIS.kennung,
        passwort = ui.apiGENESIS.passwort,
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

apiGENESIS_queryData <- reactive({

    ## apibea_parameterlist <- input$apibea_parameterlist
    ## apibea_year <- input$apibea_year
    ## apibea_datasetname <- input$apibea_datasetname

    api.param.datenexport <- apiGENESIS_parametervalues()

    xml.list.datenexport <- nsoApi::genesisAPI(api.param = api.param.datenexport,
                                                service = "ExportService",
                                                curl = ui.apiGENESIS.curl)

    ## data <- nsoApi::beaJSONtoDF(List=List, third = 4)
    data <- genesisXMLtoDF(xml.list = xml.list.datenexport)

    return(data)

})

apiGENESIS_queryData_xts <- reactive({

    queryData <- apiGENESIS_queryData()

    ## queryData <- read.csv(file.path(dlpath, "apiGENESIS_data.csv"))
    ## h(queryData)
    ## names(queryData) <- sub("ZI.WERT", "ZI-WERT", names(queryData))
    ## h(queryData)
    ## ## nsoApi::genesisDFtoXTS
    ## data.xts <- nsoApi::genesisDFtoXTS(data = queryData)
    ## h(data.xts)
    ## rownames(data.xts)
    ## write.csv(data.xts, file = file.path(dlpath, "apiGENESIS_data_xts.csv"))

    queryData.xts <- nsoApi::genesisDFtoXTS(data = queryData)
    return(queryData.xts)

})

## output$summary_apiGENESIS <- renderPrint({

##     queryData.xts <- apiGENESIS_queryData_xts()
##     summary.list = lapply(queryData.xts, summary)
##     return(summary.list)

## })

output$dygraphs_apiGENESIS <- renderDygraph({

    ## queryData <- apiGENESIS_queryData()
    ## data.xts <- genesisDFtoXTS(data = queryData)
    queryData.xts <- apiGENESIS_queryData_xts()

    d1 <- dygraph(data = queryData.xts) %>% dyLegend(width = 400,
                                                hideOnMouseOut = TRUE,
                                                show = "never"
                                                )

    return(d1)

    }
)

output$datatable_apiGENESIS <- renderDataTable({

    queryData <- apiGENESIS_queryData()
    return(queryData)

}, options = list(pageLength = 20,
                  lengthMenu = c(10, 20, 50, 100)))

output$download_apiGENESIS_data <- downloadHandler(
    filename = function() {
        paste0(
          'GENESIS_',
            input$apigenesis_datasetname,
          '.csv')
      },
  content = function(file) {

      if (input$download_apiGENESIS_data_format=="df") {
          write.csv(apiGENESIS_queryData(), file, row.names = FALSE)
      } else if (input$download_apiGENESIS_data_format=="xts") {
          write.csv(as.data.frame(apiGENESIS_queryData_xts()), file, row.names = TRUE)
      }

  }
  )


## output$download_apiGENESIS_parameter <- downloadHandler(
##     filename = function() {
##         paste0(
##           'apiGENESIS_parameter',
##           '.rdata')
##       },
##   content = function(file) {
##     api.param <- apiGENESIS_parametervalues()
##     save(api.param, file = file)
##   }
##   )
