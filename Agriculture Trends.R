# Load required packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(DT)
library(dplyr)

# Sample data
set.seed(123)
agri_trends <- data.frame(
  Trend = c("Smart Farming", "Organic Practices", "Drone Usage", "Climate-Adaptive Crops",
            "Precision Irrigation", "AI in Farming", "Agroforestry", "Hydroponics",
            "Soil Health Monitoring", "Blockchain in Agri"),
  Region = sample(c("Africa", "Asia", "Europe", "North America", "South America"), 10, replace = TRUE),
  Crop = sample(c("Wheat", "Corn", "Rice", "Fruits", "Vegetables", "Soybeans"), 10, replace = TRUE),
  TechLevel = sample(c("Low", "Medium", "High"), 10, replace = TRUE),
  Popularity = sample(5000:90000, 10),
  Engagement = round(runif(10, 1, 10), 2)
)

# UI
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Agriculture Trends 2025 Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Trends Overview", tabName = "overview", icon = icon("chart-bar")),
      menuItem("Engagement Analysis", tabName = "engagement", icon = icon("chart-line")),
      menuItem("Trend Comparisons", tabName = "comparison", icon = icon("balance-scale"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("overview",
              fluidRow(
                box(width = 12, title = "Filters", status = "primary", solidHeader = TRUE,
                    pickerInput("region_filter", "Select Region:", choices = unique(agri_trends$Region),
                                selected = unique(agri_trends$Region), multiple = TRUE),
                    pickerInput("crop_filter", "Select Crop Type:", choices = unique(agri_trends$Crop),
                                selected = unique(agri_trends$Crop), multiple = TRUE),
                    pickerInput("tech_filter", "Select Tech Level:", choices = unique(agri_trends$TechLevel),
                                selected = unique(agri_trends$TechLevel), multiple = TRUE)
                )
              ),
              fluidRow(
                box(width = 6, title = "Trend Popularity", solidHeader = TRUE, status = "primary",
                    plotlyOutput("popularity_plot", height = 400)),
                box(width = 6, title = "Top Performing Trends", solidHeader = TRUE, status = "primary",
                    DTOutput("top_table"))
              )
      ),
      tabItem("engagement",
              fluidRow(
                box(width = 6, title = "Engagement Metrics", solidHeader = TRUE, status = "success",
                    plotlyOutput("engagement_plot", height = 400)),
                box(width = 6, title = "Trend Engagement Comparison", solidHeader = TRUE, status = "success",
                    plotlyOutput("comparison_plot", height = 400))
              )
      ),
      tabItem("comparison",
              fluidRow(
                box(width = 12, title = "Trend Engagement Distribution", solidHeader = TRUE, status = "info",
                    plotlyOutput("dist_plot", height = 400))
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Filtered data based on inputs
  filtered_data <- reactive({
    agri_trends %>%
      filter(
        Region %in% input$region_filter,
        Crop %in% input$crop_filter,
        TechLevel %in% input$tech_filter
      )
  })
  
  output$popularity_plot <- renderPlotly({
    plot_ly(filtered_data(), x = ~Trend, y = ~Popularity, type = 'bar', marker = list(color = 'darkgreen')) %>%
      layout(title = "Popularity by Trend", xaxis = list(title = ""), yaxis = list(title = "Mentions"))
  })
  
  output$top_table <- renderDT({
    filtered_data() %>% arrange(desc(Popularity)) %>% datatable()
  })
  
  output$engagement_plot <- renderPlotly({
    plot_ly(filtered_data(), x = ~Trend, y = ~Engagement, type = 'bar', marker = list(color = 'forestgreen')) %>%
      layout(title = "Engagement Score by Trend", xaxis = list(title = ""), yaxis = list(title = "Engagement Score"))
  })
  
  output$comparison_plot <- renderPlotly({
    plot_ly(filtered_data(), x = ~Popularity, y = ~Engagement, text = ~Trend,
            type = 'scatter', mode = 'markers',
            marker = list(size = 10, color = 'green')) %>%
      layout(title = "Popularity vs Engagement", xaxis = list(title = "Popularity"), yaxis = list(title = "Engagement"))
  })
  
  output$dist_plot <- renderPlotly({
    plot_ly(filtered_data(), x = ~Engagement, type = 'histogram', marker = list(color = 'seagreen')) %>%
      layout(title = "Engagement Score Distribution", xaxis = list(title = "Engagement"), yaxis = list(title = "Count"))
  })
}

# Run the app
shinyApp(ui, server)
