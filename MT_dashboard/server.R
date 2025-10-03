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
    
    updateNumericInput(
      session,
      "filterDates_SG",
      max   = length(unique(sort(tidy_data()$Date))),
      value = length(unique(sort(tidy_data()$Date)))
    )
    
    
    updateSelectInput(
      session,
      'Training_Course',
      choices = unique(tidy_data()$Training)
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
  
  output$plot = renderPlot({
    req(summarisedTable())

    # Get last 6 unique dates in order and lock them as factor levels
    last6 = tail(sort(unique(summarisedTable()$Date)),6)
    
    df = summarisedTable() %>%
      filter(Date %in% last6) %>%
      mutate(Date = factor(Date, levels = last6))
    
    # Prebuild legend labels using real Date objects (avoids format.default warning)
    legend_labels = paste0(
      as.character(yearquarter(last6, fiscal_start = 4)),
      " (", format(last6, "%b"), ")"
    )
    
    # set standard position (dodge for columns and geom_labels)
    pos = position_dodge2(width = 0.7, preserve = "single", padding = 0)
    
    # Create Figure
    df %>%
      ggplot(aes(x = Training, y = Complicance, fill = Date)) +
      geom_col(position = pos, width = 0.7) +
      # Labels inside bars, halfway down each column
      geom_label(
        aes(
          y = Complicance / 2,
          label = paste0(Complicance, "%"),
          group = Date               
        ),
        position   = pos, 
        colour     = "black",
        fill       = "white",
        label.size = 0,
        angle      = 90,
        size       = input$textsize,
        show.legend = FALSE
      ) +
      # Inline annotations for thresholds
      geom_hline(yintercept = 85, linetype = "dashed", color = "darkgreen", size = 1) +
      geom_hline(yintercept = 70, linetype = "dashed", color = "#CC6600", size = 1) +
      annotate("text", x = Inf, y = 85, label = "85% Compliance",
               vjust = -0.5, hjust = 1, color = "darkgreen", fontface = "bold") +
      annotate("text", x = Inf, y = 70, label = "70% Compliance",
               vjust = 1.5, hjust = 1, color = "#CC6600", fontface = "bold") +
      
      # Palette + legend labels
      scale_fill_discrete_af(
        'main6',
        name   = "Date",
        labels = legend_labels
      ) +
      
      labs(
        title    = input$figureTitle,
        subtitle = input$subHeadingTitle,
        caption  = input$captionTitle,
        x        = "Training",
        y        = "Compliance (%)",
        fill     = input$LegendTitle
      ) +
      theme_af() +
      theme(
        axis.text.x  = element_text(angle = 30, hjust = 1, size = 10),
        axis.text.y  = element_text(size = 10),
        axis.title.y = element_text(angle = 90),
        legend.title = element_text(face = "bold"),
        legend.text  = element_text(size = 8)
      ) +
      coord_cartesian(ylim = c(0, 100))
    
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
  

# render basic table ------------------------------------------------------

  output$MT_basicTable = renderTable({
    req(MT_Table())
    
    MT_Table()
   
    
  })  
  

  ################
  # Safeguarding #
  ################
  

# Create reactive summary table -------------------------------------------

  SafeguardTable = reactive({
    req(tidy_data())
    
    tidy_data() %>% 
      filter(Training == 'Safeguarding',
             Date %in% tail(unique(sort(.$Date)), input$filterDates_SG)) %>%
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

  
  output$plot_SG <- renderPlot({
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
  
  
  #####################
  # MT - Per Training #
  #####################
  
  output$MT_trainingPlot = renderPlot({
    req(tidy_data())
    
    
    tidy_data() %>% 
      filter(Training == input$Training_Course,
             Date %in% tail(unique(sort(.$Date)), 6)) %>%
      select(Training, Complicance, Date) %>%
      rename(Compliance = Complicance) %>%
      mutate(Date = yearquarter(Date, fiscal_start = 4),
             .x_lab = paste0("Q", quarter(Date, fiscal_start = 4), ' ', year(Date) %% 100, '/', year(Date) %% 100 + 1)) %>%
      ggplot(aes(.x_lab, Compliance)) +
      geom_col(fill = af_colour_values["dark-blue"]) +
      # Inline annotations for thresholds
      geom_hline(yintercept = 85, linetype = "dashed", color = "darkgreen", size = 1) +
      geom_hline(yintercept = 70, linetype = "dashed", color = "#CC6600", size = 1) +
      annotate("text", x = Inf, y = 85, label = "85% Compliance", , size = 6,
               vjust = -0.5, hjust = 1, color = "darkgreen", fontface = "bold") +
      annotate("text", x = Inf, y = 70, label = "70% Compliance", size = 6,
               vjust = 1.5, hjust = 1, color = "#CC6600", fontface = "bold") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme_af(legend = 'bottom') +
      theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 10), 
            axis.text.y = element_text(size = 10),
            axis.title.y = element_text(angle = 90),
            legend.title = element_text(face = "bold"),
            legend.text = element_text(size = 15)) +
      geom_label(
        aes(
          label = paste0(Compliance, "%"),
        ),
        colour     = "black",
        fill       = "white",
        size       = input$textsize,
        show.legend = FALSE
      ) +
      labs(
        x = 'Date',
        y = 'Compliance (%)' ,
        title    = input$figureTitle,
        subtitle = input$subHeadingTitle,
        caption  = input$captionTitle,
      ) +
      coord_cartesian(ylim = c(0, 100))
    
    
  }, height = function() input$plotheight, width = function() input$plotwidth)
  
  
  
  
  
  
  
  
}


