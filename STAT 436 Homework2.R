library(shiny)
library(tidyverse)
library(lubridate)
data <- read_csv("https://raw.githubusercontent.com/jxu626/STAT-436/main/seattle-weather.csv")
data<-data%>%mutate(Year=year(date),diff=temp_max-temp_min)
types=unique(data$weather)%>% sort()
reset_selection <- function(x, brush) {
  brushedPoints(x, brush, allRows = TRUE)$selected_
}
scatter <- function(x, selected_, var1, var2,type) {
  x %>%
    mutate(selected_ = selected_) %>%
    filter(weather %in% type)%>%
    ggplot(aes_string(var1, var2)) +
    geom_point(aes(alpha = as.numeric(selected_), col = weather)) +
    scale_alpha(range = c(0.1, 1))
}

ui <- fluidPage(
  h1("Seattle's weather data analysis from 2012 to 2015"),
  p("Source code for this app can be found on Github: https://github.com/jxu626/STAT-436."),
  p("Application created by Jinwen Xu !"),
  tabsetPanel(type = "tabs",
              tabPanel("Time and factors for each type of weather",sidebarLayout(
                sidebarPanel(
                  selectInput("weather","Select a type of weather (or more):",choices = types,selected = "rain",multiple = TRUE),
                  sliderInput("n", "Year", min=2012, max=2015,c(2012, 2015), sep = "")
                ),
                
                mainPanel(
                  tabsetPanel(type = "tabs",
                              tabPanel("weather",tableOutput("data")),
                              tabPanel("Summary",tableOutput("totals"))
                  )
                )
              )),
              tabPanel("Plot of precipitation",sidebarLayout(
                sidebarPanel(
                  selectInput("weather1","Select a type of weather (or more):",choices = types,selected = "rain",multiple = TRUE),
                  sliderInput("a", "Year", min=2012, max=2015,c(2012, 2015), sep = ""),
                  sliderInput("b", "Month", min=1, max=12,c(1, 12), sep = "")
                ),
                mainPanel(
                  tabsetPanel(
                  tabPanel("Visualize",plotOutput("scatter1")
                  ))
                )),),
              tabPanel("Plot of Temp_max",sidebarLayout(
                sidebarPanel(
                  selectInput("weather2","Select a type of weather (or more):",choices = types,selected = "rain",multiple = TRUE),
                  sliderInput("c", "Year", min=2012, max=2015,c(2012, 2015), sep = ""),
                  sliderInput("d", "Month", min=1, max=12,c(1, 12), sep = "")
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Visualize",plotOutput("scatter2")
                    ))
                )),),
              tabPanel("Plot of temperature difference",sidebarLayout(
                sidebarPanel(
                  selectInput("weather3","Select a type of weather (or more):",choices = types,selected = "rain",multiple = TRUE),
                  sliderInput("e", "Year", min=2012, max=2015,c(2012, 2015), sep = ""),
                  sliderInput("f", "Month", min=1, max=12,c(1, 12), sep = "")
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Visualize",plotOutput("scatter3")
                    ))
                )),),
              tabPanel("Plot of wind",sidebarLayout(
                sidebarPanel(
                  selectInput("weather4","Select a type of weather (or more):",choices = types,selected = "rain",multiple = TRUE),
                  sliderInput("g", "Year", min=2012, max=2015,c(2012, 2015), sep = ""),
                  sliderInput("h", "Month", min=1, max=12,c(1, 12), sep = "")
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Visualize",plotOutput("scatter4")
                    ))
                )),),
              tabPanel("Temperature difference in relation to precipitation and wind speed ",sidebarLayout(
                sidebarPanel(
                  selectInput("weather5","Select a type of weather (or more):",choices = types,selected = "rain",multiple = TRUE),
                ),
                mainPanel(
                  tabsetPanel(type = "tabs",
                              tabPanel("Visualize",fluidRow(column(6,plotOutput("relation1", brush = "plot_brush")),
                                                            column(6,plotOutput("relation2", brush = "plot_brush"))
                                                                     ),
                                       dataTableOutput("table1")),
                  ))
              ))))




server <- function(input, output) {
  
  output$data = renderTable(
    data %>% 
      filter(weather %in% input$weather & Year <=input$n[2] & Year>=input$n[1])%>%
      mutate(date1=as.character(date))%>%
      select(date1,weather,precipitation,temp_max,temp_min,wind))
  output$totals = renderTable(
    data %>% 
      filter(weather %in% input$weather & Year <= input$n[2] &Year >=input$n[1]) %>%
      mutate(date1=as.character(date))%>%
      summarise(numbers = n(),
                time_begin= date1[1],
                time_end = date1[numbers])
  )
  output$scatter1<- renderPlot(
    data%>%
      mutate(Year=year(date),Day=day(date),Month=month(date))%>%
      filter(weather %in%  input$weather1 & Year <= input$a[2] &Year >=input$a[1] & Month <= input$b[2] & Month >= input$b[1])%>%
      ggplot(aes(x=Day,y=precipitation,col= weather))+
      geom_line()+
      facet_grid(Year~Month)
  )
  output$scatter2<- renderPlot(
    data%>%
      mutate(Year=year(date),Day=day(date),Month=month(date))%>%
      filter(weather %in%  input$weather2 & Year <= input$c[2] &Year >=input$c[1] & Month <= input$d[2] & Month >= input$d[1])%>%
      ggplot(aes(x=Day,y=temp_max,col= weather))+
      geom_line()+
      facet_grid(Year~Month)
  )
  output$scatter3<- renderPlot(
    data%>%
      mutate(Year=year(date),Day=day(date),Month=month(date))%>%
      filter(weather %in%  input$weather3 & Year <= input$e[2] &Year >=input$e[1] & Month <= input$f[2] & Month >= input$f[1])%>%
      ggplot(aes(x=Day,y=diff,col= weather))+
      geom_line()+
      facet_grid(Year~Month)
  )
  output$scatter4<- renderPlot(
    data%>%
      mutate(Year=year(date),Day=day(date),Month=month(date))%>%
      filter(weather %in%  input$weather4 & Year <= input$g[2] &Year >=input$g[1] & Month <= input$h[2] & Month >= input$h[1])%>%
      ggplot(aes(x=Day,y=wind,col= weather))+
      geom_line()+
      facet_grid(Year~Month)
  )
  selected <- reactiveVal(rep(TRUE, nrow(data)))
  observeEvent(
    input$plot_brush,
    selected(reset_selection(data, input$plot_brush))
  )
  output$relation1 <- renderPlot({
    scatter(data, selected(), "diff", "precipitation",input$weather5)
  })
  output$relation2 <- renderPlot({
    scatter(data, selected(), "diff", "wind",input$weather5)
  })
  output$table1 <- renderDataTable(filter(data, selected()))
  
  
}

# Run the application 

shinyApp(ui = ui, server = server)
