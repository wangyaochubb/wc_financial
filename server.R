library(ggvis)
library(dplyr)
if (FALSE) library(RSQLite)

# Set up handles to database tables on app start
db <- src_sqlite("H:\\2016WC Unbundled Oversight\\shiny\\xiaoyun\\gbwc_fin.sqlite")
CPfindb <- tbl(db, "gbwc_financial")

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

     # Radio buttion: filter by total incurred loss
     bracket <- input$total_loss
     brlbd <- as.numeric(strsplit(bracket,",")[[1]][1])
     brubd <- as.numeric(strsplit(bracket,",")[[1]][2])
     
    # Apply filters
    w <- filter(all_fin_records,
                LS_YR >=min_ls_yr & LS_YR <= max_ls_yr,
                CTTD_TOT_RPTD_A>=brlbd & CTTD_TOT_RPTD_A <= brubd
                )
    w <- group_by(w,LS_YR)

    # Optional: filter by Claim file status
    if (input$claim_file_status != "All") {
      claim_status <- toupper(input$claim_file_status)
      w <-  filter(w, toupper(CLM_FILE_STAT_NA) == claim_status)#%>%arrange(LS_YR)
    }
    
    # Optional: filter by insured name
    if (!is.null(input$insured_name) && input$insured_name != "") {
      insured_name <- paste0("%", input$insured_name, "%")
      w <- w %>% filter(NA_INSD_NA %like% insured_name)
    }
    # Will not get data until here
    w <- collect(w)
    w <- as.data.frame(w)
    w
  })
  # summarize the data frame fin_records 
  fin_records_sum <- reactive({
    # note that to pass results to this reactive function
    # we use fin_records() 
    s <- group_by(fin_records(),LS_YR)
    s <- summarise(s,
                   NUMCLM_PER_YR = n(),
                   AVGLS_PER_YR = mean(CTTD_TOT_RPTD_A,na.rm = TRUE),
                   AVGDAYS_LS_RPTD = mean(DAYS_LS_RPTD,na.rm = TRUE)
    )
    s <- select(s,LS_YR,NUMCLM_PER_YR,AVGLS_PER_YR,AVGDAYS_LS_RPTD)
    s
  })

  # Function for generating tooltip text for points chart
  records_tooltip_points <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$CLM_SRC_SYS_UNQ_ID)) return(NULL)

    # Pick out the movie with this ID
    selected_fin_records <- isolate(fin_records())
    records <- selected_fin_records[selected_fin_records$CLM_SRC_SYS_UNQ_ID == x$CLM_SRC_SYS_UNQ_ID, ]

    paste0("<b>", records$NA_INSD_NA, "</b><br>",
      "Loss Date:" ,records$LS_D, "<br>",
      "Claim File Status: ",records$CLM_FILE_STAT_NA, "<br>",
      "Total Loss: ","$", format(records$CTTD_TOT_RPTD_A, big.mark = ",", scientific = FALSE)
    )
  }

  # reactive expression with the ggvis plot
  plot1 <- reactive({
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
      add_tooltip(records_tooltip_points, "hover") %>%
      add_axis("x", title = xvar_name,format="####") %>%
      add_axis("y", title = yvar_name) %>%
      add_legend("stroke", title = "Claim file status", values = c("Open", "Closed")) %>%  
      scale_nominal("stroke", domain = c("Open", "Closed"),range = c( "red","#aaa")) %>% 
      set_options(width = 750, height = 600)
  })

  plot1 %>% bind_shiny("plot1")
  
  plot2 <- reactive({
    # Lables for axes
    xvar_name <- "Loss Year"
    yvar_name <- "Number of claims"

    xvar <- prop("x", as.symbol("LS_YR"))
    yvar <- prop("y", as.symbol("NUMCLM_PER_YR"))
    
    fin_records_sum %>%
      ggvis(x = xvar, y = yvar) %>%
      layer_bars(opacity := 0.5,fill := "#3299CC") %>%
      #add_tooltip(records_tooltip, "hover") %>%
      add_axis("x", title = xvar_name,format="####",grid=FALSE) %>%
      add_axis("y", title = yvar_name) %>%
      # add_legend("stroke", title = "Claim file status", values = c("Open", "Closed")) %>%
      # scale_nominal("stroke", domain = c("Open", "Closed"),range = c( "red","#aaa")) %>%
      set_options(width = 450, height = 500)
  })
  
  plot2 %>% bind_shiny("plot2")
  
  output$num_fin_records<- renderText({ nrow(fin_records()) })
  
  # note that on ui.R we use dataTableOutput$table1
  output$table1 <- renderDataTable(fin_records(),
                                   options = list(
                                     pageLength = 15
                                   )
  )
})
