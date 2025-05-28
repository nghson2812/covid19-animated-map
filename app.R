# Install packages (run only once if not installed)
# install.packages("shiny")
# install.packages("ggplot2")
# install.packages("sf")
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("shinyWidgets")

library(shiny)
library(ggplot2)
library(sf)
library(dplyr)
library(lubridate)
library(shinyWidgets)

# Load your processed data
covid_map_month <- readRDS("covid_map_month.rds")
covid_map_month$month_date <- as.Date(covid_map_month$month_date)

# Prepare a character vector of unique months
month_choices <- format(sort(unique(covid_map_month$month_date)), "%Y-%m")

timeline_events <- data.frame(
  event = c(
    "Mar 2020: WHO declares pandemic",
    "Dec 2020: Pfizer vaccine approved (US)",
    "Jan 2021: Global vaccine rollout begins",
    "Apr 2021: More vaccines approved",
    "Jul 2021: Delta variant surge",
    "Nov 2021: Omicron variant detected",
    "Feb 2022: Booster doses widely used",
    "Jun 2022: Restrictions lifted in many countries"
  ),
  date = as.Date(c(
    "2020-03-11",
    "2020-12-14",
    "2021-01-01",
    "2021-04-01",
    "2021-07-01",
    "2021-11-24",
    "2022-02-01",
    "2022-06-01"
  )),
  stringsAsFactors = FALSE
)

# UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600&family=Roboto&display=swap", rel = "stylesheet"),
    tags$style(HTML("
      /* Global fonts */
      body, label, input, button, select {
        font-family: 'Roboto', sans-serif;
      }
      h1, h2, h3, h4 {
        font-family: 'Montserrat', sans-serif;
        font-weight: 600;
        color: #2c3e50;
      }
      
      /* Page background and container */
      body {
        color: #34495e;
      }
      
      /* Title panel */
      .shiny-title-panel {
        background-color: #2a9df4;
        color: white;
        padding: 20px 15px;
        font-size: 2.5rem;
        font-weight: 700;
        border-radius: 0 0 15px 15px;
        box-shadow: 0 5px 15px rgba(42, 157, 244, 0.4);
        margin-bottom: 30px;
      }
      
      /* Sidebar */
      .sidebar .well {
        background: white;
        border-radius: 15px;
        box-shadow: 0 5px 15px rgba(0, 0, 0, 0.1);
        padding: 25px;
        margin-bottom: 30px;
      }
      
      /* Slider styling */
      .irs-bar, .irs-bar-edge {
        background: #2a9df4 !important;
      }
      .irs-from, .irs-to, .irs-single {
        background: #2a9df4 !important;
        border-radius: 10px;
      }
      .irs-grid-pol {
        background: #2a9df4 !important;
      }
      
      /* Selectize Input */
      .selectize-control.multi .selectize-input > div {
        background: #2a9df4 !important;
        color: white !important;
        border-radius: 8px;
        padding: 5px 10px;
        margin-right: 5px;
      }
      
      /* Plot titles */
      .shiny-plot-output h4 {
        font-weight: 600;
        margin-bottom: 10px;
        color: #2a9df4;
      }
      
      /* Main panel spacing */
      .main-panel {
        padding-left: 30px;
        padding-right: 30px;
      }
      
    "))
  ),
  
  titlePanel("COVID-19 Data Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      sliderTextInput(
        inputId = "selected_month",
        label = tags$span(style = "font-weight:600; font-size:18px; color:#2a9df4;", "Select Month:"),
        choices = month_choices,
        selected = month_choices[1],
        animate = animationOptions(interval = 1500),
        width = "100%"
      ),
      br(),
      selectizeInput(
        inputId = "selected_country",
        label = tags$span(style = "font-weight:600; font-size:18px; color:#2a9df4;", "Select Country (optional, multiple allowed):"),
        choices = sort(unique(covid_map_month$location)),
        selected = NULL,
        multiple = TRUE,
        options = list(
          placeholder = 'Select one or more countries...',
          maxOptions = 10
        )
      ),
      br(),
      htmlOutput("timeline_note"),
      uiOutput("timeline_ui")
    ),
    
    mainPanel(
      width = 9,
      class = "main-panel",
      fluidRow(
        column(width = 6,
               h4("New Cases per 100K"),
               plotOutput("new_cases_pc100k_plot", height = "450px")),
        column(width = 6,
               h4("New Deaths per 100K"),
               plotOutput("new_deaths_pc100k_plot", height = "450px"))
      ),
      br(),
      fluidRow(
        column(width = 12,
               h4("Vaccination Coverage"),
               plotOutput("vax_pct_plot", height = "450px"))
      ),
      hr(style = "border-color: #2a9df4; border-width: 2px; margin-top: 40px; margin-bottom: 25px;"),
      h4(style = "font-weight:700; margin-bottom: 15px;", "Timeline: Cases, Deaths (log10), and Vaccination Rate"),
      plotOutput("line_plot1", height = "500px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  lapply(1:nrow(timeline_events), function(i) {
    observeEvent(input[[paste0("timeline_", i)]], {
      selected_month <- format(timeline_events$date[i], "%Y-%m")
      updateSliderTextInput(
        session = session,
        inputId = "selected_month",
        selected = selected_month
      )
    })
  })
  
  filtered_data <- reactive({
    selected_date <- as.Date(paste0(input$selected_month, "-01"))
    data <- covid_map_month %>%
      filter(month_date == selected_date)
    
    # if (input$selected_country != "All") {
    #   data <- data %>% filter(location == input$selected_country)
    # }
    
    if (is.null(input$selected_country) || length(input$selected_country) == 0) {
      return(data)
    } else {
      return(data %>% filter(location %in% input$selected_country))
    }
    
    data
  })
  
  # filtered_line_data <- reactive({
  #   data <- covid_map_month
  #   
  #   if (input$selected_country == "All") {
  #     # Aggregate (sum) all numeric fields by month_date when "All" is selected
  #     data %>%
  #       group_by(month_date) %>%
  #       summarise(
  #         new_cases_pc100k = sum(new_cases_pc100k, na.rm = TRUE),
  #         new_deaths_pc100k = sum(new_deaths_pc100k, na.rm = TRUE),
  #         vax_pct = sum(vax_pct * population, na.rm = TRUE) / sum(population, na.rm = TRUE), # weighted avg vaccination %
  #         population = sum(population, na.rm = TRUE),
  #         .groups = "drop"
  #       ) %>%
  #       arrange(month_date)
  #   } else {
  #     # Filter by selected country and just arrange
  #     data %>%
  #       filter(location == input$selected_country) %>%
  #       arrange(month_date)
  #   }
  # })
  
  filtered_line_data <- reactive({
    data <- covid_map_month
    
    if (is.null(input$selected_country) || length(input$selected_country) == 0) {
      # Aggregate all countries
      data %>%
        group_by(month_date) %>%
        summarise(
          new_cases_pc100k = sum(new_cases_pc100k, na.rm = TRUE),
          new_deaths_pc100k = sum(new_deaths_pc100k, na.rm = TRUE),
          vax_pct = sum(vax_pct * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
          population = sum(population, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(month_date)
    } else {
      # Filter and aggregate by selected countries
      data %>%
        filter(location %in% input$selected_country) %>%
        group_by(month_date) %>%
        summarise(
          new_cases_pc100k = sum(new_cases_pc100k, na.rm = TRUE),
          new_deaths_pc100k = sum(new_deaths_pc100k, na.rm = TRUE),
          vax_pct = sum(vax_pct * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
          population = sum(population, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(month_date)
    }
  })
  
  
  # output$line_plot <- renderPlot({
  #   data <- filtered_line_data()
  #   
  #   # Find max value of cases to rescale vaccination percentage
  #   max_cases <- max(data$new_deaths_pc100k, na.rm = TRUE)
  #   
  #   ggplot(data, aes(x = month_date)) +
  #     geom_line(aes(y = new_deaths_pc100k), color = "red", size = 1) +
  #     geom_line(aes(y = vax_pct * max_cases / 100), color = "blue", size = 1) +  # scale vax_pct to cases scale
  #     scale_y_continuous(
  #       name = "New Cases per 100K",
  #       sec.axis = sec_axis(~ . * 100 / max_cases, name = "Vaccination %")  # reverse scale for axis labels
  #     ) +
  #     theme_minimal(base_size = 14) +
  #     labs(
  #       title = paste("Trend in New Cases and Vaccination Percentage"),
  #       x = "Month"
  #     ) +
  #     theme(
  #       axis.title.y.left = element_text(color = "red"),
  #       axis.title.y.right = element_text(color = "blue"),
  #       axis.text.y.left = element_text(color = "red"),
  #       axis.text.y.right = element_text(color = "blue")
  #     )
  # })
  
  output$line_plot1 <- renderPlot({
    data <- filtered_line_data() %>%
      mutate(
        new_cases_log = log10(new_cases_pc100k + 1),
        death_cases_log = log10(new_deaths_pc100k + 1)
      ) %>%
      # Pivot longer so we can map color to "type"
      tidyr::pivot_longer(
        cols = c(new_cases_log, death_cases_log),
        names_to = "type",
        values_to = "value"
      )
    
    max_log <- max(c(data$value), na.rm = TRUE)
    
    # Add vaccination data as separate dataframe with scaled values for plotting
    vax_data <- filtered_line_data() %>%
      mutate(value = vax_pct * max_log / 100, type = "Vaccination %") %>%
      select(month_date, value, type)
    
    # Combine with cases/deaths data
    plot_data <- bind_rows(data, vax_data)
    
    ggplot(plot_data, aes(x = month_date, y = value, color = type)) +
      geom_line(size = 1) +
      scale_color_manual(values = c(
        "new_cases_log" = "red",
        "death_cases_log" = "green",
        "Vaccination %" = "blue"
      ),
      labels = c(
        "new_cases_log" = "New Cases (log10)",
        "death_cases_log" = "Deaths (log10)",
        "Vaccination %" = "Vaccination %"
      )) +
      scale_y_continuous(
        name = "Deaths/Cases Per 100K (Log scale)",
        sec.axis = sec_axis(~ . * 100 / max_log, name = "Vaccination %")
      ) +
      labs(
        title = "Trend in New Cases, Deaths (log scale) and Vaccination",
        x = "Time",
        color = "Legend"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        axis.title.y.left = element_text(color = "black"),
        axis.title.y.right = element_text(color = "blue"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 1),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
      )
  })
  
  
  output$timeline_note <- renderUI({
    month_str <- input$selected_month
    
    note_text <- switch(month_str,
                        "2020-03" = "<b>🌍 Mar 2020:</b> <span style='color:#ff4500;'>WHO declares COVID-19 a global pandemic.</span>",
                        "2020-12" = "<b>💉 Dec 2020:</b> <span style='color:#1e90ff;'>Pfizer vaccine approved for emergency use in the US.</span>",
                        "2021-01" = "<b>🚀 Jan 2021:</b> <span style='color:#228b22;'>Global COVID-19 vaccine rollout begins.</span>",
                        "2021-04" = "<b>🧪 Apr 2021:</b> <span style='color:#ffa500;'>Multiple vaccines gain emergency approval worldwide.</span>",
                        "2021-07" = "<b>🔬 Jul 2021:</b> <span style='color:#32cd32;'>Delta variant causes global surge.</span>",
                        "2021-11" = "<b>⚠️ Nov 2021:</b> <span style='color:#dc143c;'>Omicron variant detected.</span>",
                        "2022-02" = "<b>🛡️ Feb 2022:</b> <span style='color:#4169e1;'>Booster doses widely administered.</span>",
                        "2022-06" = "<b>🌈 Jun 2022:</b> <span style='color:#ff69b4;'>Many countries lift restrictions.</span>",
                        ""
    )
    
    if (note_text == "") {
      return(NULL)
    } else {
      tags$div(
        style = "background-color: #e6f2ff; 
               border-left: 5px solid #3399ff; 
               padding: 15px; 
               margin-top: 10px; 
               font-size: 16px; 
               font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;",
        HTML(note_text)
      )
    }
  })
  
  output$timeline_ui <- renderUI({
    current_month <- as.Date(paste0(input$selected_month, "-01"))
    
    tagList(
      # Header with some style
      tags$h3("📅 Key Timeline Events", style = "font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; color: #2c3e50; margin-bottom: 15px;"),
      
      # CSS styling for timeline list and links
      tags$style(HTML("
      .timeline-container {
        background: #ffffff;
        border-radius: 12px;
        box-shadow: 0 4px 10px rgba(0, 0, 0, 0.1);
        padding: 20px;
        max-height: 350px;
        overflow-y: auto;
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }
      .timeline-list {
        list-style: none;
        padding-left: 0;
        margin: 0;
      }
      .timeline-item {
        position: relative;
        padding-left: 20px;
        margin-bottom: 14px;
        cursor: pointer;
        color: #34495e;
        transition: background-color 0.3s ease, color 0.3s ease;
        border-radius: 8px;
      }
      .timeline-item:hover {
        background-color: #ecf0f1;
        color: #2980b9;
        text-decoration: underline;
      }
      .timeline-item::before {
        content: '';
        position: absolute;
        left: 0;
        top: 50%;
        transform: translateY(-50%);
        width: 10px;
        height: 10px;
        border-radius: 50%;
        background-color: #3498db;
        transition: background-color 0.3s ease;
      }
      .timeline-current {
        font-weight: 700;
        color: #e74c3c !important;
        background-color: #fdecea;
      }
      .timeline-current::before {
        background-color: #e74c3c !important;
      }
    ")),
      
      # Timeline container with scroll if needed
      tags$div(class = "timeline-container",
               tags$ul(class = "timeline-list",
                       lapply(1:nrow(timeline_events), function(i) {
                         event_date <- timeline_events$date[i]
                         is_current <- format(event_date, "%Y-%m") == input$selected_month
                         
                         tags$li(
                           class = paste("timeline-item", if (is_current) "timeline-current"),
                           actionLink(
                             inputId = paste0("timeline_", i),
                             label = timeline_events$event[i],
                             style = "display: block; width: 100%; text-align: left; border: none; background: none; padding: 8px 0; font-size: 16px;"
                           )
                         )
                       })
               )
      )
    )
  })
  
  
  
  
  output$new_cases_pc100k_plot <- renderPlot({
    selected_date <- as.Date(paste0(input$selected_month, "-01"))
    
    valid_cases <- covid_map_month$new_cases_pc100k[
      !is.na(covid_map_month$new_cases_pc100k) & covid_map_month$new_cases_pc100k > 0
    ]
    
    max_cases <- quantile(valid_cases, 0.99, na.rm = TRUE)
    
    ggplot(filtered_data()) +
      geom_sf(aes(geometry = geometry, fill = new_cases_pc100k),
              color = "white", size = 0.1) +
      scale_fill_distiller(palette = "YlOrRd", na.value = "gray90", 
                           limits = c(1, max_cases), oob = scales::squish,
                           trans = "log10", direction = 1) +
      theme_void(base_size = 14) +
      labs(
        title = paste("New COVID-19 Cases per 100K in", format(selected_date, "%B %Y")),
        fill = "Cases/100K",
        caption = "Source: Our World in Data"
      ) +
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 1),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.title.position = "right",
      )
  })
  
  output$new_deaths_pc100k_plot <- renderPlot({
    selected_date <- as.Date(paste0(input$selected_month, "-01"))
    
    valid_cases <- covid_map_month$new_deaths_pc100k[
      !is.na(covid_map_month$new_deaths_pc100k) & covid_map_month$new_deaths_pc100k > 0
    ]
    
    max_cases <- quantile(valid_cases, 0.99, na.rm = TRUE)
    
    ggplot(filtered_data()) +
      geom_sf(aes(geometry = geometry, fill = new_deaths_pc100k),
              color = "white", size = 0.1) +
      scale_fill_distiller(palette = "YlOrRd", na.value = "gray90",
                           limits = c(1, max_cases), oob = scales::squish,
                           trans = "log10", direction = 1) +
      theme_void(base_size = 14) +
      labs(
        title = paste("New COVID-19 Deaths per 100K in", format(selected_date, "%B %Y")),
        fill = "Deaths/100K",
        caption = "Source: Our World in Data"
      ) + 
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 1),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.title.position = "right",
      )
  })
  
  output$vax_pct_plot <- renderPlot({
    selected_date <- as.Date(paste0(input$selected_month, "-01"))
    
    ggplot(filtered_data()) +
      geom_sf(aes(geometry = geometry, fill = vax_pct),
              color = "white", size = 0.1) +
      scale_fill_distiller(palette = "plasma", na.value = "gray90",
                           direction = 1, limits = c(0, 100)) +
      theme_void(base_size = 14) +
      labs(
        title = paste("Vaccination Coverage in", format(selected_date, "%B %Y")),
        fill = "% Fully Vaccinated",
        caption = "Source: Our World in Data"
      ) +
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 1),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
      )
  })
}

# Run the app
shinyApp(ui, server)
