library(DT)
library(shiny)
library(tidyverse)
library(lubridate)
library(tidymodels)
library(readr)
library(tidytext)
library(shinythemes)



wine <- read_csv("https://raw.githubusercontent.com/jxu626/data/main/WineQT.csv")
wine<-wine%>%
  select(-"Id")
colnames(wine) <- gsub(" ", "", colnames(wine))
wine$quality<-as.factor(wine$quality)
pca_rec <- recipe(~., data = wine) %>%
  update_role(quality,new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())
pca_prep <- prep(pca_rec)
pca_result <- tidy(pca_prep, 2) %>%
  mutate(terms = str_replace(terms, " \\(g\\)", ""))
scores = juice(pca_prep)
colnames(scores) <- str_replace(colnames(scores), "PC0", "PC")
variances = tidy(pca_prep, 2, type = "variance")
reset_selection <- function(x, brush) {
  brushedPoints(x, brush, allRows = TRUE)$selected_
}




histogram <- function(wine, selected_) {
  sub_counts <- wine %>%
    filter(selected_) %>%
    count(quality)
  
  wine %>%
    count(quality) %>%
    ggplot(aes(quality, n)) +
    geom_bar(stat = "identity", fill = "#d3d3d3", width = 1) +
    geom_bar(data = sub_counts, stat = "identity", width = 1) +
    scale_y_continuous(expand = c(0, 0))+
    labs(x = "Quality", y = "Count", title = "Distribution of Wine Quality")+
    theme_minimal()
}




scatterplot <- function(wine,x,y,z,selected_) {
  wine %>%
    mutate(selected_ = selected_,quality=z) %>%
    ggplot() +
    geom_point(aes_string(x,y,col=z,alpha = as.numeric(selected_))) +
    scale_alpha(range = c(0.05, 1))+
    labs(x = x, y = y, title = "Scatter Plot of Wine Data")+
    theme_minimal() +
    theme(
      legend.position = "bottom"
    )
}




ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("Wine Data Analysis"),
  sidebarPanel(
    selectInput("var1", "First Variable", colnames(wine)[1:11]),
    selectInput("var2", "Second Variable", colnames(wine)[1:11]),
    selectInput("species", "Species", dist <- str_c("PC", 1:9), selected = "PC1", multiple = TRUE),
    selectInput("var3", "X axis Variable", str_c("PC", 1:5)),
    selectInput("var4", "Y axis Variable", str_c("PC", 1:5))
  ),
  mainPanel(
    fluidRow(
      column(6,h3("Scatter Plot of Wine Data"),plotOutput("scatterplot", brush = brushOpts("plot_brush"))),
      column(6, h3("Distribution of Wine Quality"),plotOutput("histogram", brush = brushOpts("plot_brush", direction = "x")))
    ),
    column(12,h3("Principal Component Analysis"),plotOutput("PCAplot")),
    column(12,h3("PCA Scores Plot"),plotOutput("PCAplot2"))
  )
)





server <- function(input, output) {
  selected <- reactiveVal(rep(TRUE, nrow(wine)))
  z=wine$quality
  observeEvent(
    input$plot_brush,
    selected(reset_selection(wine, input$plot_brush))
  )
  output$histogram <- renderPlot(histogram(wine, selected()))
  output$scatterplot <- renderPlot({
    scatterplot(wine,input$var1,input$var2,z,selected())
  }
  )
  output$PCAplot <- renderPlot({
    ggplot(pca_result %>% filter(component %in% input$species)%>%mutate(terms = reorder_within(terms, abs(value), component))) +
      geom_col(aes(x = value, y = terms)) +
      facet_wrap(~ component,scales = "free_y") +
      labs(x = "Component", y = "Features",title = "Per Component Coefficients of Drink Variables")+
      theme_bw()+
      scale_y_reordered() 
  })
  
  output$PCAplot2<-renderPlot({
    result1<-strsplit(input$var3, "(?<=PC)(?=\\d)", perl=TRUE)[[1]]
    result2<-strsplit(input$var4, "(?<=PC)(?=\\d)", perl=TRUE)[[1]]
    ggplot(scores, aes_string(input$var3, input$var4)) +
      geom_point(aes(color = quality), alpha = 0.7, size = 1.5) +
      coord_fixed(sqrt(variances$value[as.numeric(result2[2])]/variances$value[as.numeric(result1[2])]))+theme_minimal()+
      theme(
        legend.position = "bottom"
      )
  })
}

shinyApp(ui, server)


