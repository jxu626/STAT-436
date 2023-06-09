library(DT)
library(shiny)
library(tidyverse)
airbnb <- read.csv("https://raw.githubusercontent.com/jxu626/data/main/airbnb.csv")

library(ggeasy)
airbnb_new<-airbnb[,c(1,2,6,7,8,9,10,11,17)]%>%
  mutate(log_number=log(1+minimum_nights))

reset_selection <- function(x, brush) {
  brushedPoints(x, brush, allRows = TRUE)$selected_
}

scatterplot1 <- function(data, selected_) {
  data %>%
    mutate(selected_ = selected_) %>%
    ggplot() +
    geom_point(aes(log_price, log_number,col=neighbourhood,alpha = as.numeric(selected_))) +
    scale_alpha(range = c(0, 1))
}

scatterplot2 <- function(data, selected_) {
  data %>%
    mutate(selected_ = selected_) %>%
    ggplot() +
    geom_point(aes(longitude, latitude,color = room_type ,alpha = as.numeric(selected_))) +
    scale_alpha(range = c(0, 1))
}

ui <- fluidPage(
  fluidRow(
    column(12, plotOutput("scatterplot1",brush = "plot_brush"))),
    column(12, plotOutput("scatterplot2"))
  )
server <- function(input, output) {
  selected <- reactiveVal(rep(TRUE, nrow(airbnb_new)))
  
  observeEvent(
    input$plot_brush,
    selected(reset_selection(airbnb_new, input$plot_brush))
  )
  
  output$scatterplot1 <- renderPlot(scatterplot1(airbnb_new, selected()))
  output$scatterplot2 <- renderPlot(scatterplot2(airbnb_new, selected()))
}

shinyApp(ui, server)

