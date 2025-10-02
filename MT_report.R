require(tidyverse)
require(afcharts)
require(tsibble)
require(formattable)


df = read.csv('MT.csv') %>%
  mutate(Date = my(Date),
         Date = as.Date(Date))



# Bar chart for all compliance --------------------------------------------

df %>%
  filter(Date %in% tail(unique(sort(.$Date)), 6)) %>%
  mutate(Date = factor(Date)) %>%
  
  
  ggplot(aes(x = Training, y = Complicance, fill = Date)) +  
  geom_col(position = position_dodge2(width = 0.8, preserve = "single"), width = 0.7) +  
  
  geom_hline(yintercept = 85, linetype = "dashed", color = "darkgreen", size = 1) +
  geom_hline(yintercept = 70, linetype = "dashed", color = "#CC6600", size = 1) +
  
  geom_text(aes(label = paste0(Complicance, "%")), 
            position = position_dodge2(width = 0.8, preserve = "single"), 
            vjust = -0.5, size = 3) +
  
  annotate("text", x = Inf, y = 85, label = "85% Compliance", vjust = -0.5, hjust = 1, color = "darkgreen", fontface = "bold") +
  annotate("text", x = Inf, y = 70, label = "70% Compliance", vjust = 1.5, hjust = 1, color = "#CC6600", fontface = "bold") +
  
  # scale_y_continuous(labels = function(x) paste0(x, "%")) +
  
  # Use scale_fill_discrete and set legend labels only
  scale_fill_discrete_af( 'main6',
                          name = "Date",
                          labels = paste0(as.character(yearquarter(unique(df$Date), fiscal_start = 4)),
                                          ' (', format(unique(df$Date), "%b"), ')')
  ) +

  labs(title = "Mandatory Training",
       x = "Training",
       y = "Compliance (%)") + 
  theme_af() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(angle = 90),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 8)) +
  ylim(0, 100)



# Create Table for compliance ---------------------------------------------



df_wide <- df %>%
  select(Training, Date, Complicance) %>%
  mutate(Date = format(as.Date(Date), "%b %Y")) %>%  # Convert Date to 'Monthname Year'
  pivot_wider(
    names_from = Date,  # This creates columns for each unique Date
    values_from = Complicance  # This populates the table with compliance values
  ) %>%
  mutate(across(where(is.numeric), ~ paste0(.x, "%")))  # Convert numeric values to percentages

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

Manadatory_Table = formattable(
  df_wide, 
  setNames(
    rep(list(color_formatter), ncol(df_wide) - 1),  # Repeat the formatter for each applicable column
    names(df_wide)[-1]  # Exclude the first column (Training)
  )
)

Manadatory_Table

Sys.sleep(time = 3)

# Safeguarding ------------------------------------------------------------


# Filter data specific to Safeguarding
df_filtered = df %>%
  filter(Training == 'Safeguarding') %>%
  pivot_longer(
    cols = 2:(length(df)-1),
    names_to = 'Employee.Type',
    values_to = 'Compliance')  %>%
  mutate(Date = factor(paste0(
    as.character(yearquarter(Date), fiscal_start = 4),
    ' (', format(Date, "%b"), ')')
  ))


# create Plot
ggplot() +
  geom_col(
    data = df_filtered %>% filter(Employee.Type == 'Complicance'),  # fixed typo
    aes(x = Date, y = Compliance,
        fill = 'Overall Compliance')
  ) +
  geom_label(
    data = df_filtered %>% filter(Employee.Type == 'Complicance'),
    aes(x = Date, y = Compliance, label = paste0(Compliance, '%')),
    vjust = 1.5,
    fill = "white",     # label background
    color = "black",    # text color
    label.size = 0      # no border
  ) +
  
  geom_line(
    data = df_filtered %>% filter(Employee.Type != 'Complicance'),
    aes(x = Date, y = Compliance, group = Employee.Type, color = Employee.Type), size = 1
  ) +
  geom_text(
    data = df_filtered %>% filter(Employee.Type != 'Complicance'),
    aes(x = Date, y = Compliance, label = paste0(Compliance, '%')),
    vjust = -1,
    size = 2
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_af(legend = 'bottom') +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(angle = 90),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 8)) +
  afcharts::scale_colour_discrete_af('main') +
  scale_fill_manual(name = "Compliance", 
                    values = c('Overall Compliance' = '#999999')) +
  ylim(0,100) +
  labs(title = "Safeguard MT Compliance",
       x = NULL,
       y = "Compliance (%)") 


  