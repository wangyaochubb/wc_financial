library(ggvis)
library(dplyr)
if (FALSE) library(RSQLite)

# Set up handles to database tables on app start
db <- src_sqlite("C:\\Users\\u041018\\Documents\\RShiny\\wc_financial\\gbwc_fin.sqlite")
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
    s <- as.data.frame(s)
    # create new column
    s$AVG_LS <- numeric(nrow(s))
    s$AVG_LS <- mean(s$AVGLS_PER_YR)
    
    s <- select(s,LS_YR,NUMCLM_PER_YR,AVGLS_PER_YR,AVGDAYS_LS_RPTD,AVG_LS)
    s
  })
  
  # get top 5 (or less) names in NA_INSD_NA
  top_names <- reactive({
    t <- group_by(fin_records(),NA_INSD_NA)
    t <- summarise(t, 
                   INSD_NA_FREQ = n())%>%arrange(desc(INSD_NA_FREQ))
    t <- as.data.frame(t)
    t <- select(t,NA_INSD_NA)
    if (nrow(t)>5){
      t <- head(t,5)
    } else{
      t <- head(t,nrow(t))
    }
    t
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
      layer_points(size := 80, size.hover := 250,
        fillOpacity := 0.2, fillOpacity.hover := 0.5, 
        stroke = ~CLM_FILE_STAT_NA, key := ~CLM_SRC_SYS_UNQ_ID) %>%
      add_tooltip(records_tooltip_points, "hover") %>%
      add_axis("x", 
               title = xvar_name,
               format="####",
               #subdivide = 1,
               values = seq(min(fin_records()$LS_YR),
                            max(fin_records()$LS_YR),
                            by=1
               ),
               tick_size_major = 10
               #tick_size_minor = 5
      ) %>%
      add_axis("y", 
               title = yvar_name,
               title_offset = 65
      ) %>%
      add_legend("stroke", 
                 title = "Claim file status", 
                 values = c("Open", "Closed")
                 ) %>%  
      scale_nominal("stroke", 
                    domain = c("Open", "Closed"),
                    range = c( "red","#aaa")
                    ) %>% 
      set_options(width = 800, height = 550)
  })
  plot1 %>% bind_shiny("plot1")
  
  
  # plot2 the frequency bar chart
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
      add_axis("x", 
               title = xvar_name,
               title_offset = 50,
               format="####",
               subdivide = 1,
               values = seq(min(fin_records()$LS_YR),
                            max(fin_records()$LS_YR),
                            by=1),
               tick_size_major = 10,
               tick_size_minor = 5,
               properties = axis_props(labels = list(angle = -45, align = "right")),
               grid=FALSE
      ) %>%
      add_axis("y", 
               title = yvar_name,
               title_offset = 50
      ) %>%
      # add_legend("stroke", title = "Claim file status", values = c("Open", "Closed")) %>%
      # scale_nominal("stroke", domain = c("Open", "Closed"),range = c( "red","#aaa")) %>%
      set_options(width = 450, height = 580)
  })
  plot2 %>% bind_shiny("plot2")
  
  # plot3: points line chart that shows average loss per year
  plot3 <- reactive({
    
    # Lables for axes
    xvar_name <- "Loss Year"
    yvar_name <- "Average Loss"
    
    xvar <- prop("x", as.symbol("LS_YR"))
    yvar <- prop("y", as.symbol("AVGLS_PER_YR"))
    
    fin_records_sum %>%
      ggvis(x = xvar, y = yvar) %>%
      layer_lines() %>%
      layer_points(stroke := "#FFA500",
                   fill := "#FFA500") %>%
      #add_tooltip(records_tooltip, "hover") %>%
      # add a horizontal line represents the grand mean
      layer_paths(x=~LS_YR,y=~AVG_LS,stroke := "green",fill :="green")%>%
      add_axis("x", 
               title = xvar_name,
               title_offset = 50,
               properties = axis_props(labels = list(angle = -45, align = "right")),
               format="####",
               values = seq(min(fin_records()$LS_YR),
                            max(fin_records()$LS_YR),
                            by=1),
               tick_size_major = 10,
               tick_size_minor = 5,
               grid=TRUE
               ) %>%
      add_axis("y", 
               title = yvar_name,
               title_offset = 60,
               orient = "right") %>%
      # add_legend("stroke", title = "Claim file status", values = c("Open", "Closed")) %>%
      # scale_nominal("stroke", domain = c("Open", "Closed"),range = c( "red","#aaa")) %>%
      set_options(width = 430, height = 580)
  })
  plot3 %>% bind_shiny("plot3")
  # other ouput fields
                  
  output$top_names <- renderText({paste(top_names()$NA_INSD_NA,sep = ", ")})
  output$num_fin_records<- renderText({ nrow(fin_records()) })
  output$avg_loss <- renderText(paste0("$ ",
                       as.character(
                         format(ceiling(sum(fin_records()$CTTD_TOT_RPTD_A)/nrow(fin_records())),big.mark = ",")
                       ))
                     )
  
  # note that on ui.R we use dataTableOutput$table1
  output$table1 <- renderDataTable(fin_records(),
                                   options = list(
                                     pageLength = 15
                                   )
                   )
})
