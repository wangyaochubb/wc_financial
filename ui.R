library(ggvis)

# For dropdown menu
actionLink <- function(inputId, ...) {
  tags$a(href='javascript:void',
         id=inputId,
         class='action-button',
         ...)
}

shinyUI(fluidPage(
  titlePanel("GB WC data explorer"),
  fluidRow(
    column(3,
      wellPanel(
        h4("Filter panel"),
        selectInput("claim_file_status", "Claim file status", c("All","Closed","Open")),
        sliderInput("loss_year", "Loss year", 1987, 2016, value = c(2007, 2016), sep = "")#,
#        sliderInput("oscars", "Minimum number of Oscar wins (all categories)", 0, 4, 0, step = 1),
#        sliderInput("boxoffice", "Dollars at Box Office (millions)", 0, 800, c(0, 800), step = 1),
#        textInput("director", "Director name contains (e.g., Miyazaki)"),
#        textInput("cast", "Cast names contains (e.g. Tom Hanks)")
      ),
      wellPanel(
        selectInput("xvar", "X-axis variable", axis_vars, selected = "LS_YR"),
        selectInput("yvar", "Y-axis variable", axis_vars, selected = "CTTD_TOT_RPTD_A")#,
      #   tags$small(paste0(
      #     "Note: The Tomato Meter is the proportion of positive reviews",
      #     " (as judged by the Rotten Tomatoes staff), and the Numeric rating is",
      #     " a normalized 1-10 score of those reviews which have star ratings",
      #     " (for example, 3 out of 4 stars)."))
       )
     ),
     column(9,
      ggvisOutput("plot1"),
      wellPanel(
        span("Number of claims selected:", textOutput("num_fin_records"))
      )
    )
  )
))
