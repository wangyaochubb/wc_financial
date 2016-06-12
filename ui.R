library(ggvis)

# For dropdown menu
actionLink <- function(inputId, ...) {
  tags$a(href='javascript:void',
         id=inputId,
         class='action-button',
         ...)
}

shinyUI(pageWithSidebar(
  headerPanel("GB WC data explorer"),
  sidebarPanel(
    # h4("Filter panel"),
    selectInput("claim_file_status", "Claim file status", c("All","Closed","Open"),selected = "Closed"),
    sliderInput("loss_year", "Loss year", 1987, 2016, value = c(2005, 2016), sep = "",step = 1),
    radioButtons("total_loss", 
                 "Total incurred loss", 
                 list("All Claims" = "0,1000000000",
                      "Less than $5,000" = "0,5000",
                      "$5,000 to $10,000" = "5000,10000",
                      "$10,000 to $50,000" = "10000,50000",
                      "$50,000 to $100,000" = "50000,100000",
                      "$100,000 to $500,000" = "100000,500000",
                      "$500,000 to $1,000,000" = "500000,1000000",
                      "Greater than $1,000,000" = "1000000,1000000000"
                      ),
                 selected = "100000,500000"),
    textInput("insured_name", "Insured name", placeholder = "e.g. COMAIR, INC."),
    # actionButton("submit","Submit"),
    selectInput("xvar", "X-axis variable", axis_vars, selected = "LS_YR"),
    selectInput("yvar", "Y-axis variable", axis_vars, selected = "CTTD_TOT_RPTD_A"),
    tags$small(paste0("Note: The total loss is up to current reporting period. ",
                      "The axis variables inputs fields do not work ",
                      "when 'Summary' tab is selected."
               )
    )
  ),
mainPanel(
  tabsetPanel(
    tabPanel("Plot",ggvisOutput("plot1")),
    tabPanel("Summary",
             fluidRow(
               splitLayout(cellWidths = c("50%", "50%"), 
                           ggvisOutput("plot2"), 
                           ggvisOutput("plot3")
               )
             )
    ),
    tabPanel("Table", dataTableOutput("table1"))
  ),
  wellPanel(
    fluidRow(
      splitLayout(cellWidths = c("25%","20%","55%"),
                  span("Claims selected:", textOutput("num_fin_records")),
                  span("Average loss: ", textOutput("avg_loss")),
                  span("Top insured names:",textOutput("top_names"))
                  )
    )
  )
)
))
