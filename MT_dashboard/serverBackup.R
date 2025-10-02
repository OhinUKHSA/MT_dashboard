# =============================================================================
# App: Mandatory Training Dashboard — Server
# Purpose: Handle server-side processing, including data validation, wrangling,
#          and generating interactive outputs for the CSAC performance report.
#
# Author: Owen Williams
# Created: 2025-09-26
#
#
#
#
# How to run:
#   1) Ensure packages are installed by using 'Load_packages.R'.
#   2) Run the app via 'ui.R' + 'server.R' or 'app.R' script.
# =============================================================================


# shiny server side ------------------------------------------------------

function(input, output, session) {
  
  ##################################
  # Pull in imported data and tidy #
  ##################################
  
  # Pull in data and validate columns
  data = reactive({
    req(input$file1)
    
    ext = file_ext(input$file1$name)
    
    df = switch(
      tolower(ext),
      "csv"  = read.csv(input$file1$datapath, check.names = FALSE),
      "xls"  = read_excel(input$file1$datapath),
      "xlsx" = read_excel(input$file1$datapath),
      {
        validate(need(FALSE, "Invalid file; please upload a .csv, .xls, or .xlsx file."))
        return(NULL)
      }
    )
    
  })
  
  
  #############
  # tidy data #
  #############
  
  tidy_data = reactive({
    req(data())
    df = data()     %>%
      mutate(Date = my(Date),
             Date = as.Date(Date))
  })
  
  
  ##################
  # Observe Events #
  ##################
  
  observeEvent(tidy_data(), {
    req(tidy_data())
    
    updateNumericInput(
      session,
      "filterDates",
      max   = length(unique(sort(tidy_data()$Date))),
      value = length(unique(sort(tidy_data()$Date)))
    )
  })
  
  
  ###########################  
  # Create summarised Table #
  ###########################
  
  summarisedTable = reactive({
    req(tidy_data())
    
    # Apply filters
    dat = tidy_data() %>%
      mutate(Date = yearquarter(Date, fiscal_start = 4))
    
  })
  
  
  
  ######################
  # Mandatory Training #
  ######################
  
  
  # Table -------------------------------------------------------------------
  
  
  output$contents = renderTable({
    req(summarisedTable())
    
    summarisedTable() %>%
      filter(Date %in% tail(unique(sort(.$Date)), 6)) %>%
      mutate(Date_time = factor(Date)) %>%
      select(Date_time, everything(), -Date) %>%
      rename(Date = Date_time)
    
    
    
    
  })
  
  
  # Create Figure -----------------------------------------------------------
  
  
  
  output$plot <- renderPlot({
    req(summarisedTable())
    df <- summarisedTable()
    
    summarisedTable() %>%
      filter(Date %in% tail(unique(sort(.$Date)), 6)) %>%
      mutate(Date = factor(Date)) %>%
      
      
      ggplot(aes(x = Training, y = Complicance, fill = Date)) +  
      geom_col(position = position_dodge2(width = 0.8, preserve = "single"), width = 0.7) +  
      
      geom_hline(yintercept = 85, linetype = "dashed", color = "darkgreen", size = 1) +
      geom_hline(yintercept = 70, linetype = "dashed", color = "#CC6600", size = 1) +
      
      geom_text(aes(label = paste0(Complicance, "%")), 
                position = position_dodge2(width = 0.8, preserve = "single"), 
                vjust = -0.5, size = input$textsize) +
      
      annotate("text", x = Inf, y = 85, label = "85% Compliance", vjust = -0.5, hjust = 1, color = "darkgreen", fontface = "bold") +
      annotate("text", x = Inf, y = 70, label = "70% Compliance", vjust = 1.5, hjust = 1, color = "#CC6600", fontface = "bold") +
      
      # Use scale_fill_discrete and set legend labels only
      scale_fill_discrete_af( 'main6',
                              name = "Date",
                              labels = paste0(as.character(yearquarter(unique(df$Date), fiscal_start = 4)),
                                              ' (', format(unique(df$Date), "%b"), ')')
      ) +
      labs(
        title    = input$figureTitle,
        subtitle = input$subHeadingTitle,
        caption  = input$captionTitle,
        x        = "Training",
        y = "Compliance (%)",
        fill     = input$LegendTitle
      ) +
      theme_af() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 10), 
            axis.text.y = element_text(size = 10),
            axis.title.y = element_text(angle = 90),
            legend.title = element_text(face = "bold"),
            legend.text = element_text(size = 8)) +
      ylim(0, 100)
  }, height = function() input$plotheight, width = function() input$plotwidth)
  
  
  
  ##############
  # Compliance #
  ##############
  
  # Create Table ------------------------------------------------------------
  
  MT_Table = reactive({
    req(tidy_data())
    
    tidy_data() %>%
      select(Training, Date, Complicance) %>%
      filter(Date %in% tail(unique(sort(.$Date)), input$filterDates)) %>%
      mutate(Date = format(as.Date(Date), "%b %Y")) %>%  # Convert Date to 'Monthname Year'
      pivot_wider(
        names_from = Date,  # This creates columns for each unique Date
        values_from = Complicance  # This populates the table with compliance values
      ) %>%
      mutate(across(where(is.numeric), ~ paste0(.x, "%")))
    
  })
  
  
  # Render Formattable table ------------------------------------------------
  
  output$formattable_Table = renderFormattable({
    req(MT_Table())
    
    # Define color formatting conditions with text centered
    color_formatter <- formatter(
      "span",
      style = x ~ style(
        display = "block",
        "border-radius" = "4px",
        "padding-right" = "4px",
        "text-align" = "center",  # Center the text in the cells
        background = ifelse(as.numeric(sub("%", "", x)) > 85, "green",
                            ifelse(as.numeric(sub("%", "", x)) >= 70 & as.numeric(sub("%", "", x)) <= 85, "orange", "red")),
        color = "white"  # To ensure text is visible
      )
    )
    
    # Apply the formatting to each column manually (except the Training column)
    formattable(
      MT_Table(), 
      setNames(
        rep(list(color_formatter), ncol(MT_Table()) - 1),  # Repeat the formatter for each applicable column
        names(MT_Table())[-1]  # Exclude the first column (Training)
      )
    )
  })
  
  
  ################
  # Safeguarding #
  ################
  
  
  # Create reactive summary table -------------------------------------------
  
  SafeguardTable = reactive({
    req(tidy_data())
    
    tidy_data() %>% 
      filter(Training == 'Safeguarding') %>%
      pivot_longer(
        cols = 2:(length(.)-1),
        names_to = 'Employee.Type',
        values_to = 'Compliance')  %>%
      mutate(Date = factor(paste0(
        as.character(yearquarter(Date), fiscal_start = 4),
        ' (', format(Date, "%b"), ')')
      ))
  })
  
  
  # render table ------------------------------------------------------------
  
  output$SafeguardTable_Render = renderTable({
    req(SafeguardTable())
    
    SafeguardTable() 
    
  })
  
  # Render safeguardPlot ----------------------------------------------------
  
  
  output$plot_SLA <- renderPlot({
    req(SafeguardTable())
    # plot
    ggplot() +
      geom_col(
        data = SafeguardTable() %>% filter(Employee.Type == 'Complicance'), 
        aes(x = Date, y = Compliance,
            fill = 'Overall Compliance')
      ) +
      geom_label(
        data = SafeguardTable() %>% filter(Employee.Type == 'Complicance'),
        aes(x = Date, y = Compliance, label = paste0(Compliance, '%')),
        vjust = 1.5,
        fill = "white",     
        color = "black",
        label.size = 0,
        size = 7
      ) +
      geom_line(
        data = SafeguardTable() %>% filter(Employee.Type != 'Complicance'),
        aes(x = Date, y = Compliance, group = Employee.Type, color = Employee.Type), size = 1
      ) +
      geom_text(
        data = SafeguardTable() %>% filter(Employee.Type != 'Complicance'),
        aes(x = Date, y = Compliance, label = paste0(Compliance, '%')),
        vjust = -1,
        size = input$textsize
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme_af(legend = 'bottom') +
      theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 10), 
            axis.text.y = element_text(size = 10),
            axis.title.y = element_text(angle = 90),
            legend.title = element_text(face = "bold"),
            legend.text = element_text(size = 15)) +
      afcharts::scale_colour_discrete_af('main') +
      scale_fill_manual(name = "Compliance", 
                        values = c('Overall Compliance' = '#999999')) +
      ylim(0,100) +
      labs(
        title    = input$figureTitle,
        subtitle = input$subHeadingTitle,
        caption  = input$captionTitle,
        x        = "Date",
        y = "Compliance (%)",
        fill     = input$LegendTitle
      ) 
    
  }, height = function() input$plotheight, width = function() input$plotwidth)
  
  
  
  
  
  
  
  
  
  
  
}


