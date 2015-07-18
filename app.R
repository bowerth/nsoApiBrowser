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

header <- dashboardHeader(title = "apiBrowser")

sidebar <- dashboardSidebar(
    ## disable = TRUE,
    sidebarMenu(

        menuItem("NLD: CBS STATLINE", tabName = "apiCBS", icon = icon("line-chart"))
        ,
        menuItem("DEU: DESTATIS GENESIS", tabName = "apiGENESIS", icon = icon("line-chart"))
        ,
        menuItem("USA: Bureau of Economic Analysis", tabName = "apiBEA", icon = icon("line-chart"))

    )
)

source(file.path("widgets", "apiCBS_ui.R"))
source(file.path("widgets", "apiGENESIS_ui.R"))
source(file.path("widgets", "apiBEA_ui.R"))

body <- dashboardBody(
    tabItems(

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
                         apiBEA.col2),
                fluidRow(apiBEA.row2)
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
