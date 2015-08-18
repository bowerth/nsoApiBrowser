## path <- file.path(dbpath, "GitHub", "nsoApi", "inst", "nsoApiBrowser")
## source(file.path(path, "widgets", "apiNEWID_input.R"))

ui.apiNEWID.curl <- RCurl::getCurlHandle()
RCurl::curlSetOpt(.opts = list(proxy = isolate(input$sidebar_httpproxy)), curl = ui.apiNEWID.curl)

output$uiNEWID_datasetname <- renderUI({

    datasetname_choices <- c("")

    selectInput("apinewid_datasetname", "Dataset",
              choices = datasetname_choices,
              selected = datasetname_choices[1],
              multiple = FALSE)

})


output$uiNEWID_queryuri <- renderUI({

    apinewid_datasetname <- input$apinewid_datasetname
    if (is.null(apinewid_datasetname) | length(apinewid_datasetname)==0) return()

    queryURI <- NULL

    textInput("apinewid_queryuri", "Query URI", value = queryURI)

})

.apiNEWID_filterlist <- reactive({

    queryuri <- input$apinewid_queryuri
    if (is.null(queryuri) | length(queryuri)==0) return()

    filterlist <- NULL

    return(filterlist)

})

output$uiNEWID_filtervalues <- renderUI({

    filterlist <- .apiNEWID_filterlist()
    if (is.null(filterlist) | length(filterlist)==0) return()

    ui.all <- nsoApi::selectInputs(list = filterlist,
                                   prefix = ui.apiNEWID.prefix,
                                   minsize = input$sidebar_maxfilter
                                   )

    return(ui.all)

  })

.apiNEWID_queryData_filter <- reactive({

    filterlist <- .apiNEWID_filterlist()
    if (is.null(filterlist) | length(filterlist)==0) return()

    queryuri <- input$apinewid_queryuri

    queryData_filter <- NULL

    return(queryData_filter)

})

.apiNEWID_queryData_xts <- reactive({

    queryData_filter <- .apiNEWID_queryData_filter()
    if (is.null(queryData_filter) | length(queryData_filter)==0) return()

    queryData_xts <- nsoApi::newidDFtoXTS(data = queryData_filter)

    return(queryData_xts)
})

.apiNEWID_dygraph <- reactive({

    queryData_xts <- .apiNEWID_queryData_xts()
    if (is.null(queryData_xts) | length(queryData_xts)==0) return()

    dygraph <- nsoApiBrowser_dygraph(
        data = queryData_xts,
        show.boolean = ifelse("legendshow" %in% input$sidebar_plotoptions, TRUE, FALSE)
    )

    return(dygraph)

})

output$dygraphs_apiNEWID <- renderDygraph({

    dygraph <- .apiNEWID_dygraph()
    if (is.null(dygraph) | length(dygraph)==0) return()

    return(dygraph)

})

output$datatable_apiNEWID <- renderDataTable({

    queryData_filter <- .apiNEWID_queryData_filter()
    if (is.null(queryData_filter) | length(queryData_filter)==0) return()

    return(queryData_filter)

}, options = ui.datatable.options)

output$download_data_apiNEWID <- downloadHandler(
    filename = function() {
        paste0(
            'NEWID_',
            input$apinewid_datasetname,
            '.csv')
    },
    content = function(file) {

      if (input$sidebar_download_data_format=="df_all") {
        ## write.csv(.apiNEWID_queryData(), file, row.names = FALSE)
        write.csv(.apiNEWID_queryData_filter(), file, row.names = FALSE)

      } else if (input$sidebar_download_data_format=="df_filter") {
        write.csv(.apiNEWID_queryData_filter(), file, row.names = FALSE)

      } else if (input$sidebar_download_data_format=="xts") {
        write.csv(as.data.frame(.apiNEWID_queryData_xts()), file, row.names = TRUE)
      }

}
  )


output$download_plot_apiNEWID <- downloadHandler(
    filename = function() {
        paste0(
            'NEWID_',
            input$apinewid_datasetname,
            '.html')
    },
    content = function(file) {

        htmlwidgets:::saveWidget(widget = .apiNEWID_dygraph(), file = file, selfcontained = TRUE, libdir = NULL)

  }
  )

