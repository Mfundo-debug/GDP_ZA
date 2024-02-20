# Load the necessary libraries
library(shiny)
library(shinydashboard)
library(bslib)
library(dplyr)
library(echarts4r)
library(highcharter)
library(gtrendsR)

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "GDP and Unemployment in South Africa"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("GDP Trends", tabName = "gdp", icon = icon("line-chart")),
      menuItem("Unemployment Trends", tabName = "unemployment", icon = icon("line-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(echarts4rOutput("mergedPlot", height = "400px")),
                box(plotOutput("emptyBox", height = "400px"))
              )
      ),
      tabItem(tabName = "gdp",
              h2("GDP Trends"),
              echarts4rOutput("gdpPlot")
      ),
      tabItem(tabName = "unemployment",
              h2("Unemployment Trends"),
              highchartOutput("unemploymentPlot")
      )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Fetch the Google Trends data
  gdp_trends <- gtrends("GDP", geo = "ZA")$interest_over_time
  unemployment_trends <- gtrends("Unemployment", geo = "ZA")$interest_over_time
  
  # Merge the trends
  merged_trends <- merge(gdp_trends, unemployment_trends, by = "date", suffixes = c("_GDP", "_Unemployment"))
  
  # Generate the plots
  output$gdpPlot <- renderEcharts4r({
    gdp_trends %>%
      e_charts(date) %>%
      e_line(hits) %>%
      e_title("GDP Trends in South Africa")
  })
  
  output$unemploymentPlot <- renderHighchart({
    unemployment_trends %>%
      hchart(.,"line", hcaes(x = date, y = hits)) %>%
      hc_title(text = "Unemployment Trends in South Africa")
  })
  
  output$mergedPlot <- renderEcharts4r({
    merged_trends %>%
      e_charts(date) %>%
      e_line(hits_GDP, name = "GDP") %>%
      e_line(hits_Unemployment, name = "Unemployment") %>%
      e_tooltip(trigger = "axis") %>%
      e_title("GDP and Unemployment in South Africa") %>%
      e_y_axis(name = "GDP and Unemployment") %>%
      e_theme("dark")
  })
  
  output$emptyBox <- renderPlot({
    plot.new()
    title(main = "Empty Box", col.main = "grey", font.main = 4)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
