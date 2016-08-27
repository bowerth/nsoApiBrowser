

about.output <- list(
  ## h3("ICIO Indicator 2015")
  ## ,
  shiny::p("Version 1.0 (19/07/2015)")
  ,
  shiny::p("Author: Bo Werth STI/EAS")
  ,
  shiny::p("Contact:", shiny::a(href = "mailto:bo.werth@oecd.org?subject=NSO%20API%20Browser", "bo.werth@oecd.org"))
  ,
  shiny::p("Source code and documentation on",
    shiny::a(href = "https://github.com/bowerth/nsoApiBrowser", "GitHub"))
  ,
    shiny::p("Data availability by provider at ",
      shiny::a(href = "http://bowerth.github.io/datascience/2015/04/07/Data-API/", "bowerth.github.io"))
,
  shiny::p("Built using", shiny::a(href = "http://www.rstudio.com/shiny/", "shiny"),
    "and", shiny::a(href = "http://rstudio.github.io/shinydashboard/", "shinydashboard"),
    "by", shiny::a(href = "http://www.rstudio.com/", "RStudio"))
  )
