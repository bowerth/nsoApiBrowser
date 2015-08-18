## install.packages("ISOweek")
## devtools::install_github("rstudio/shinydashboard")
## shiny::runGitHub(repo = "sdmxBrowser", username = "bowerth")
## setwd(file.path(dbpath, "GitHub", "sdmxBrowser"))
## source("app.R")
## shinyApp(ui, server)

## app.R ##
library(shiny)
library(shinydashboard)

source("global.R")

header <- dashboardHeader(title = "nsoApiBrowser", disable = FALSE)

sidebar <- dashboardSidebar(
    ## disable = TRUE,
    sidebarMenu(

        menuItem("StatAT: Statistics Austria", tabName = "apiSTATAT", icon = icon("line-chart"))
       ,

        menuItem("About", tabName = "about", icon = icon("info"))
        ,

        menuItem("PX-Web: Nordic Countries", tabName = "apiPXWEB", icon = icon("line-chart"))
       ,

        menuItem("GBR: Office of National Statistics", tabName = "apiONS", icon = icon("bar-chart"))
       ,
        menuItem("NLD: CBS STATLINE", tabName = "apiCBS", icon = icon("line-chart"))
       ,
        ## test server availability: https://www-genesis.destatis.de/genesis/
        menuItem("DEU: DESTATIS GENESIS", tabName = "apiGENESIS", icon = icon("line-chart"))
       ,
        menuItem("USA: Bureau of Economic Analysis", tabName = "apiBEA", icon = icon("line-chart"))
       ,
        radioButtons("sidebar_download_data_format", "Data Download Format",
                     choices = list(
                         "Data Table (all)" = "df_all",
                         "Data Table (filter)" = "df_filter",
                         ## "Time Series" = "xts"
                         "Plot Format" = "xts"
                     )
                     )
       ,
        checkboxGroupInput("sidebar_plotoptions", "Plot Options",
                           c("Show Legend" = "legendshow",
                             "Percentage Axis" = "percentaxis"),
                           ## selected = c("legendshow", "percentaxis")
                           selected = c("legendshow")
                           )
       ,
        sliderInput("sidebar_queryyear", "Time Period",
                    min = ui.queryyear.min,
                    max = ui.queryyear.max,
                    ## value = c(1970, 2014),
                    value = c(ui.queryyear.min, ui.queryyear.max),
                    sep = "")
       ,
        numericInput("sidebar_maxfilter", "Max. Filter Fields", value = 10)
       ,
        textInput("sidebar_httpproxy", "HTTP Proxy", ui.httpproxy) # ifelse(exists("ui.httpproxy"), ui.httpproxy, ""))

    )
)

source(file.path("widgets", "apiSTATAT_ui.R"))
source(file.path("widgets", "apiPXWEB_ui.R"))
source(file.path("widgets", "apiONS_ui.R"))
source(file.path("widgets", "apiCBS_ui.R"))
source(file.path("widgets", "apiGENESIS_ui.R"))
source(file.path("widgets", "apiBEA_ui.R"))
source(file.path("widgets", "about_ui.R"))

body <- dashboardBody(
    tabItems(

        tabItem(tabName = "apiSTATAT",
                fluidRow(apiSTATAT.col1,
                         apiSTATAT.col2)
                )

       ,
        tabItem(tabName = "about",
              about.output)

       ,
        tabItem(tabName = "apiPXWEB",
                fluidRow(apiPXWEB.col1,
                         apiPXWEB.col2)
                )

       ,
        tabItem(tabName = "apiONS",
                fluidRow(apiONS.col1,
                         apiONS.col2)
                )

       ,
        tabItem(tabName = "apiCBS",
                fluidRow(apiCBS.col1,
                         apiCBS.col2)
                )

       ,
        tabItem(tabName = "apiGENESIS",
                fluidRow(apiGENESIS.col1,
                         apiGENESIS.col2)
               ## ,
               ##  fluidRow(apiGENESIS.row2)
                )

        ,
        tabItem(tabName = "apiBEA",
                fluidRow(apiBEA.col1,
                         apiBEA.col2)
                ## ,
                ## fluidRow(apiBEA.row2)
                )



    )
)

ui <- dashboardPage(
    header,
    sidebar,
    body
)

server <- function(input, output) {

    source(file.path("widgets", "apiSTATAT_server.R"), local = TRUE)
    source(file.path("widgets", "apiPXWEB_server.R"), local = TRUE)
    source(file.path("widgets", "apiONS_server.R"), local = TRUE)
    source(file.path("widgets", "apiCBS_server.R"), local = TRUE)
    source(file.path("widgets", "apiGENESIS_server.R"), local = TRUE)
    source(file.path("widgets", "apiBEA_server.R"), local = TRUE)

}

shinyApp(ui, server)
