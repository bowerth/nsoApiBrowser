## shiny::runApp(file.path(dbpath, "GitHub", "apiBrowser"), launch.browser = FALSE, port = 3838)
## source(file.path(dbpath, "GitHub", "apiBrowser", "master.R"))


path <- file.path(dbpath, "GitHub", "apiBrowser")
setwd(path)

source("app.R")
shinyApp(ui, server, options = list(port = 4848, launch.browser = FALSE))

## ?shiny::shinyApp
