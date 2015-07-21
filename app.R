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

        menuItem("About", tabName = "about", icon = icon("info"))
        ,
        menuItem("NLD: CBS STATLINE", tabName = "apiCBS", icon = icon("line-chart"))
        ,

        ## test server availability: https://www-genesis.destatis.de/genesis/
        menuItem("DEU: DESTATIS GENESIS", tabName = "apiGENESIS", icon = icon("line-chart"))
        ,

        menuItem("USA: Bureau of Economic Analysis", tabName = "apiBEA", icon = icon("line-chart"))

      ,
      checkboxInput("sidebar_dygraphlegendshow", "Show Plot Legend", value = TRUE)
      ,
      sliderInput("sidebar_queryyear", "Time Period",
                  min = ui.queryyear.min,
                  max = ui.queryyear.max,
                  ## value = c(1970, 2014),
                  value = c(ui.queryyear.min, ui.queryyear.max),
                  sep = "")
      ,
      textInput("sidebar_httpproxy", "HTTP Proxy", ui.httpproxy) # ifelse(exists("ui.httpproxy"), ui.httpproxy, ""))

    )
)

source(file.path("widgets", "apiCBS_ui.R"))
source(file.path("widgets", "apiGENESIS_ui.R"))
source(file.path("widgets", "apiBEA_ui.R"))
source(file.path("widgets", "about_ui.R"))

body <- dashboardBody(
    tabItems(

      tabItem(tabName = "about",
              about.output)
      ,
      tabItem(tabName = "apiCBS",
                fluidRow(apiCBS.col1,
                         apiCBS.col2),
                fluidRow(apiCBS.row2)
                )

       ,
        tabItem(tabName = "apiGENESIS",
                fluidRow(apiGENESIS.col1,
                         apiGENESIS.col2),
                fluidRow(apiGENESIS.row2)
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

    source(file.path("widgets", "apiCBS_server.R"), local = TRUE)
    source(file.path("widgets", "apiGENESIS_server.R"), local = TRUE)
    source(file.path("widgets", "apiBEA_server.R"), local = TRUE)

}

shinyApp(ui, server)
