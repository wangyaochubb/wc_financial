library(ggvis)
library(dplyr)
if (FALSE) library(RSQLite)

# Set up handles to database tables on app start
db <- src_sqlite("H:\\2016WC Unbundled Oversight\\shiny\\xiaoyun\\gbwc_fin.sqlite")
CPfindb <- tbl(db, "gbwc_financial")

# create loss year field
# mutate(CPfindb,LS_Y = as.numeric(as.Date(LS_D, "%Y-%m-%d")))

# select specified columns
all_fin_records <- select(CPfindb, CLM_SRC_SYS_UNQ_ID, LS_D,LS_YR, CLM_FILE_STAT_NA, 
          NA_INSD_NA, DAYS_LS_RPTD, CTTD_TOT_RPTD_A
#         ,LS_RPTD_D,CLMOC_CLSD_D,CLMOC_REOPN_D
)
          


shinyServer(function(input, output, session) {

  # Filter the claim records, returning a data frame
  fin_records <- reactive({
    # Due to dplyr issue #318, we need temp variables for input 
     min_ls_yr <- input$loss_year[1]
     max_ls_yr <- input$loss_year[2]

    # Apply filters
    w <- filter(all_fin_records,  LS_YR >=min_ls_yr & LS_YR <= max_ls_yr)

    # Optional: filter by Claim file status
    if (input$claim_file_status != "All") {
      claim_status <- toupper(input$claim_file_status)
      w <-  filter(w, toupper(CLM_FILE_STAT_NA) == claim_status)%>%arrange(LS_YR)
    }
    # # Optional: filter by director
    # if (!is.null(input$director) && input$director != "") {
    #   director <- paste0("%", input$director, "%")
    #   w <- w %>% filter(Director %like% director)
    # }
    # # Optional: filter by cast member
    # if (!is.null(input$cast) && input$cast != "") {
    #   cast <- paste0("%", input$cast, "%")
    #   w <- w %>% filter(Cast %like% cast)
    # }
    w <- collect(w)
    w <- as.data.frame(w)

    # Add column which says whether the movie won any Oscars
    # Be a little careful in case we have a zero-row data frame
    w$NUM_CLM <- character(nrow(w))
    # w$has_oscar[w$Oscars == 0] <- "No"
    # w$has_oscar[w$Oscars >= 1] <- "Yes"
    w
  })

  # Function for generating tooltip text
  records_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$CLM_SRC_SYS_UNQ_ID)) return(NULL)

    # Pick out the movie with this ID
    selected_fin_records <- isolate(fin_records())
    records <- selected_fin_records[selected_fin_records$CLM_SRC_SYS_UNQ_ID == x$CLM_SRC_SYS_UNQ_ID, ]

    paste0("<b>", records$NA_INSD_NA, "</b><br>",
      records$LS_D, "<br>",
      "$", format(records$CTTD_TOT_RPTD_A, big.mark = ",", scientific = FALSE)
    )
  }

  # A reactive expression with the ggvis plot
  vis <- reactive({
    # Lables for axes
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]

    # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
    # but since the inputs are strings, we need to do a little more work.
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))

    fin_records %>%
      ggvis(x = xvar, y = yvar) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5, 
        stroke = ~CLM_FILE_STAT_NA, key := ~CLM_SRC_SYS_UNQ_ID) %>%
      add_tooltip(records_tooltip, "hover") %>%
      add_axis("x", title = xvar_name,format="####") %>%
      add_axis("y", title = yvar_name) %>%
      add_legend("stroke", title = "Claim file status", values = c("Open", "Closed")) %>%  
      scale_nominal("stroke", domain = c("Open", "Closed"),range = c( "orange","#aaa")) %>% 
      set_options(width = 500, height = 500)
  })

  vis %>% bind_shiny("plot1")

  output$num_fin_records<- renderText({ nrow(fin_records()) })
})
