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

     # Radio buttion: filter by total incurred loss
     bracket <- input$total_loss
     brlbd <- as.numeric(strsplit(bracket,",")[[1]][1])
     brubd <- as.numeric(strsplit(bracket,",")[[1]][2])
     
    # Apply filters
    w <- filter(all_fin_records,LS_YR >=min_ls_yr & LS_YR <= max_ls_yr,CTTD_TOT_RPTD_A>=brlbd & CTTD_TOT_RPTD_A <= brubd)
    w <- group_by(w,LS_YR)
#     w_smr <- summarise(w,
#                    num_clm_yr = n(),
#                    avgloss_clm_yr = mean(CTTD_TOT_RPTD_A)
#     )

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

    # Add columns which contains number of the claims and
    # average loss per loss year
    # w$NUM_CLM<- 
    # w$MV_AVG <- 
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
      "Loss Date:" ,records$LS_D, "<br>",
      "Claim File Status: ",records$CLM_FILE_STAT_NA, "<br>",
      "Total Loss: ","$", format(records$CTTD_TOT_RPTD_A, big.mark = ",", scientific = FALSE)
    )
  }

  # reactive expression with the ggvis plot
  plot1 <- reactive({
    # Lables for axes
    xvar1_name <- names(axis_vars)[axis_vars == input$xvar1]
    yvar1_name <- names(axis_vars)[axis_vars == input$yvar1]

    # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
    # but since the inputs are strings, we need to do a little more work.
    xvar1 <- prop("x", as.symbol(input$xvar1))
    yvar1 <- prop("y", as.symbol(input$yvar1))

    fin_records %>%
      ggvis(x = xvar1, y = yvar1) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5, 
        stroke = ~CLM_FILE_STAT_NA, key := ~CLM_SRC_SYS_UNQ_ID) %>%
      add_tooltip(records_tooltip, "hover") %>%
      add_axis("x", title = xvar1_name,format="####") %>%
      add_axis("y", title = yvar1_name) %>%
      add_legend("stroke", title = "Claim file status", values = c("Open", "Closed")) %>%  
      scale_nominal("stroke", domain = c("Open", "Closed"),range = c( "red","#aaa")) %>% 
      set_options(width = 750, height = 600)
  })

  plot1 %>% bind_shiny("plot1")
  
  plot2 <- reactive({
    # Lables for axes
    xvar2_name <- names(axis_vars)[axis_vars == input$xvar1]
    yvar2_name <- names(axis_vars)[axis_vars == input$yvar1]
    
    # Normally we could do something like
    #     xvar2 <- props(x = ~xvar1)
    #     yvar2 <- props(y = ~yvar1)
    # but since the inputs are strings, we need to do a little more work.

    xvar2 <- prop("x", as.symbol(input$xvar1))
    yvar2 <- prop("y", as.symbol(input$yvar1))
    
    fin_records %>%
      ggvis(x = xvar2, y = yvar2) %>%
      layer_points(size := 50, size.hover := 200,
                   fillOpacity := 0.2, fillOpacity.hover := 0.5, 
                   stroke = ~CLM_FILE_STAT_NA, key := ~CLM_SRC_SYS_UNQ_ID) %>%
      add_tooltip(records_tooltip, "hover") %>%
      add_axis("x", title = xvar2_name,format="####") %>%
      add_axis("y", title = yvar2_name) %>%
      add_legend("stroke", title = "Claim file status", values = c("Open", "Closed")) %>%  
      scale_nominal("stroke", domain = c("Open", "Closed"),range = c( "red","#aaa")) %>% 
      set_options(width = 750, height = 600)
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
