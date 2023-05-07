library(tidyverse)
library(stringr)
library(ggplot2)
library(scales)
library(patchwork)
library(shiny)
library(lubridate)
library(tidyverse)
library(tsibble)
library(shiny)
library(shinyWidgets)
options(scipen = 999)
text = p("Are the mortality and welfare cost follow the same trend for each country? Only countries with full records from 1990 to 2019 are kept to make it consistent with the country selection in evironmental policy stringency index. Select the countries and year range to compare the trend of premature death per million inhabitant and percent of welfare cost on PM-related premature death of total welfare.")
text2=p("Does the effectiveness of PM prevention and control follow the same trend in every country?Except for 1990, there are continuously recorded data starting from 2005. So we keep the countries with complete records from 2005 to 2019.
The range of years selected is used to compare the top 10 countries with the earliest deaths caused by environmental particulate matter (PM) and the top 10 countries with the earliest deaths caused by PM per million inhabitants, separately.
")
data = read_csv("https://uwmadison.box.com/shared/static/w9v71xbpuvb6kuilm6bfiz9va5lvfn08.csv")
# tidying the data to have the type of emission under the "Type" column
data = data %>%
  rename(Carbon_Dioxide = "Carbon dioxide_value") %>%
  rename(Nitrous_Oxide = "Nitrous oxide_Value") %>%
  rename(Methane = Methane_Value) %>%
  pivot_longer(cols = c("Carbon_Dioxide", "Methane", "Nitrous_Oxide"), names_to = "Type", values_to = "Value") %>%
  select(Country, Year, Environmental_Policy_Stringency_Value, Type, Value) %>%
  mutate(Log10_Value = log10(Value))
# get the types into a list and the countries into a list from the dataset
types <- pull(data, Type) %>%
  unique() %>%
  na.omit()
countries <- pull(data, Country) %>%
  unique() %>%
  na.omit()
mortality <- read.csv("https://github.com/AndreaChen0301/stat436/raw/main/mortality_ph2.5.csv")%>%
  rename_at(vars(starts_with('X')), funs(sub("X", " ", .))) %>% 
  pivot_longer(!Country, names_to = "Year", values_to = "prematured_death")
welfare.percent <- read.csv("https://github.com/AndreaChen0301/stat436/raw/main/percent_welfare.csv")%>%
  rename_at(vars(starts_with('X')), funs(sub("X", " ", .))) %>% 
  pivot_longer(!Country, names_to = "Year", values_to = "cost_percent")
mortality_per <- read.csv("https://github.com/AndreaChen0301/stat436/raw/main/mortality_per_mili.csv") %>%
  rename_at(vars(starts_with('X')), funs(sub("X", "", .))) %>% 
  pivot_longer(!Country, names_to = "Year", values_to = "death_per_million")
mortality_per$Country[mortality_per$Country=="T\x9frkiye"]="Türkiye"
health = right_join(welfare.percent, mortality, c("Country", "Year")) %>%
  mutate(Year=as.numeric(Year))
welfare.percent <- read.csv("https://github.com/AndreaChen0301/stat436/raw/main/percent_welfare.csv")%>%
  rename_at(vars(starts_with('X')), funs(sub("X", "", .))) %>% 
  pivot_longer(!Country, names_to = "Year", values_to = "cost_percent")

health2 = right_join(welfare.percent, mortality_per, c("Country", "Year")) %>%
  mutate(Year = as.numeric(Year))

complete.country = read.csv("https://github.com/AndreaChen0301/stat436/raw/main/co_emmit.csv") %>%
  group_by(Country) %>%
  mutate(nyear = n()) %>%
  filter(nyear==31) %>%
  pull(Country) %>%
  unique()
emissions = read_csv("https://raw.githubusercontent.com/mrbarron3/group_project/main/data.csv")
emissions = emissions %>%
  group_by(Country) %>%
  mutate(avg_epsv = mean(Environmental_Policy_Stringency_Value)) %>%
  ungroup() %>%
  mutate(total_avg_epsv = median(avg_epsv)) %>%
  arrange(desc(avg_epsv), Year) 

header_eps = function() {
  headerPanel("Environmental Policy Stringency Index by Country")
}
text_eps = function() {
  mainPanel("This data represents the environmental policy stringency index of countries from the years of 1990-2020.
              
Users can select a beginning and ending year to depict on the bar graphs below. The first bar graph depicts the Environmental Policy Stringency Indexes of countries where data from the starting and ending year of the Year Range slider are both available. The second graph shows the difference between the two selected years. Both graphs are sorted based on the difference shown in the second graph.")
}


ui <- fluidPage(
  setBackgroundColor(
    color = "lightgrey",
    gradient = c("linear", "radial"),
    direction = c("bottom", "top", "right", "left"),
    shinydashboard = FALSE),
  headerPanel("Impacts of Climate Change"),
  tabsetPanel(type = "tabs",
              tabPanel("Greenhouse",
                       headerPanel("Global Greenhouse Gas Emissions by Country"),
                       # explanatory text
                       mainPanel("This data represents greenhouse gas emissions of countries from the years of 1990-2020.
              
Users can select one type of emission source and a time range to depict on the heat map below. The heat map shows the value for the selected emission type and the bar graph shows the average of all the emission types over the given time range. Note: There are some datapoints missing for certain types of CO2 emission sources for some years and countries.",
                                 
                                 tags$head(tags$style(HTML('* {font-family: "Comic Sans"};'))),
                                 helpText("Create a heat map and bar graph visualizing the type of emmissions for countries with a specified time periods."),
                                 # Creating tabs with each graph
                                 tabsetPanel(
                                   tabPanel("Heat Map", plotOutput("heat")), 
                                   tabPanel("Bar Graph", plotOutput("bar"))
                                 )),
                       
                       # creating the modular query for user selected variables
                       sidebarPanel(
                         sliderInput("slider", label = h4("Year Range"), min = 1990, 
                                     max = 2020, value = c(1990, 2020), sep = ""),
                         selectInput("types", "Type", types, 
                                     selected = "Carbon_Dioxide",
                                     multiple = FALSE)
                         
                       )
              ),
              tabPanel("Trend in Mortality Rate",
                       headerPanel("Premature Death Caused by Ambient Particulate Matter (PM)"),
                       sidebarLayout(
                         sidebarPanel(
                           text2,
                           sliderInput("year", "Year Range", min =2005, max = 2019,value = 2019, sep = "")
                         ),
                         mainPanel(
                           tabsetPanel(
                             tabPanel("Visualize",
                                      plotOutput("ratings_scatter"),
                                      plotOutput("ratings_scatter2"),
                                      plotOutput("ratings_scatter3")
                             ))
                         )),),
              tabPanel("Mortality and Welfare Cost Comparison",
                       headerPanel("Mortality and Welfare Cost Comparison"),  
                       sidebarLayout(
                         sidebarPanel(
                           text,
                           selectInput("country", "Select a country", unique(complete.country), selected = c("Austria", "Belgium", "United States"), multiple = TRUE),
                           sliderInput("year1", "Select a year range", min = min(health2$Year), max = max(health2$Year), value = c(1990, 2019), step = 1, sep = "")
                         ),
                         mainPanel(
                           tabsetPanel(
                             tabPanel("Mortality and Welfare Cost Comparison",
                                      plotOutput("mortality"),
                                      plotOutput("welfare"),
                                      dataTableOutput("table")
                             )
                           )
                         )
                       ))
              ,
              tabPanel("EPS Index",
                       header_eps(),
                       sidebarPanel(
                         sliderInput("slider2", label = h4("Year Range"), min = 1990, 
                                     max = 2020, value = c(1990, 2020), sep = "")),
                       text_eps(),
                       tabsetPanel(
                         tabPanel("barplot", plotOutput("bar2")), 
                         tabPanel("barplot2", plotOutput("bar3"))
                       ))
  ))

server <- function(input, output) {
  output$heat <- renderPlot({
    data %>%
      filter(Year >= input$slider[1],
             Year <= input$slider[2],
             Type == input$types) %>%
      ggplot( aes(Year, y = reorder(Country, desc(-Log10_Value)))) +
      geom_tile(aes(fill = Log10_Value)) +
      scale_fill_gradient(low = "blue",
                          high = "yellow",
                          guide = "colorbar") +
      theme(panel.grid.major = element_blank(),
            plot.background = element_rect(fill='transparent',color="grey")) +
      scale_x_continuous(expand = c(0,0)) +
      ylab("Country") +
      labs(title= "Country's Emissions",
           xlab = "Year", ylab = "Kilotons") 
  }
  )
  output$bar<- renderPlot({
    data %>%
      filter(
        Year >= input$slider[1],
        Year <= input$slider[2],
      ) %>%
      group_by(Country, Type) %>%
      mutate(Average = mean(Value)) %>%
      arrange(desc(Average)) %>%
      ggplot(aes(x=Average, y=reorder(Country, desc(-Average)), fill = Type)) +
      geom_bar(position = "stack", stat="identity")+
      labs(title= "Average Emissions Over the Given Time Range")+
      xlab("Average Emissions (Kilotons)") +
      ylab("Country") +
      theme_bw() +
      scale_fill_brewer(palette="Dark2") +
      scale_x_continuous(expand = c(0,0)) +
      theme(plot.background = element_rect(fill='transparent', color="grey"))
    
  } )
  mortality_subset <- reactive({
    health %>%
      filter(Country %in% complete.country) %>%
      mutate(Year = as.character(Year)) %>%
      filter(Year == 1990 | Year == input$year)%>%
      filter(Country !="Russia")
  })
  top.10 <- reactive({
    mortality_subset() %>%
      group_by(Country) %>%
      summarise(death = mean(prematured_death))%>%
      arrange(desc(death))%>%
      slice_head(n=10)%>%
      pull(Country)
  })
  
  mortality_subset2 <- reactive({
    health2 %>%
      filter(Country %in% complete.country) %>%
      mutate(Year = as.character(Year)) %>%
      filter(Year == 1990 | Year == input$year)%>%
      filter(Country !="Russia")
  })
  
  per_top.10 <- reactive({
    mortality_subset2() %>%
      group_by(Country) %>%
      summarise(death = mean(death_per_million))%>%
      arrange(desc(death))%>%
      slice_head(n=10)%>%
      pull(Country)
  })
  
  output$ratings_scatter <- renderPlot({
    mortality_subset()%>%
      filter(Country %in% top.10()) %>%
      ggplot(aes(x=reorder(Country, prematured_death), y=prematured_death, fill = Year))+
      geom_col(position="dodge", stat = "identity", width=0.7)+
      scale_fill_manual(values=c("#D95F02", "#1B9E77"))+
      geom_text(aes(label=prematured_death), position=position_dodge(width=0.75), vjust=-0.75, size=3)+
      labs(x="Country", y="Number of Premature Deaths", title = "Top 10 Country with Most Premature Death Caused by Ambient Particulate Matter (PM) ")+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=15),
            axis.ticks = element_blank(),
            panel.grid.major.x = element_blank())
  })
  
  output$ratings_scatter2 <- renderPlot({
    mortality_subset2() %>%
      filter(Country %in% top.10()) %>%
      ggplot(aes(x=reorder(Country, death_per_million), y=death_per_million, fill = Year))+
      scale_fill_manual(values=c("#D95F02", "#1B9E77"))+
      geom_col(position="dodge", width=0.7)+
      geom_text(aes(label=death_per_million), position=position_dodge(width=0.75), hjust=0.4, vjust=-0.65, size=3)+
      scale_y_continuous(expand = c(0, 0, 0.1, 0.1))+
      labs(x="Country", y="Premature Deaths per Million", title = "Premature Death Per Million Inhabitants of Top10 Countries with Most Premature Death")+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=15),
            axis.ticks = element_blank(),
            panel.grid.major.x = element_blank())
  })
  
  output$ratings_scatter3 <- renderPlot({
    mortality_subset2() %>%
      filter(Country %in% per_top.10()) %>%
      ggplot(aes(x=reorder(Country, death_per_million), y=death_per_million, fill = Year))+
      scale_fill_manual(values=c("#D95F02", "#1B9E77"))+
      geom_col(position="dodge", width=0.7)+
      geom_text(aes(label=death_per_million), position=position_dodge(width=0.75), hjust=0.4, vjust=-0.65, size=3)+
      scale_y_continuous(expand = c(0, 0, 0.1, 0.1))+
      labs(x="Country", y="Premature Deaths per Million", title = "Top 10 Countries with Most Premature Death Per Million Inhabitant Caused by PM ")+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=15),
            axis.ticks = element_blank(),
            panel.grid.major.x = element_blank())
  })
  
  filtered_data <- reactive({
    health2 %>%
      filter(Country %in% input$country,
             Year >= input$year1[1] & Year <= input$year1[2])
  })
  
  # Plot 1: Mortality vs. year
  output$mortality <- renderPlot({
    ggplot(filtered_data(), aes(x = Year, y = death_per_million, color = Country)) +
      geom_line() +
      labs(x = "Year", y = "Premature Death Per Million", title = "Mortality Comparison of Selected Countries by Year")
  })
  
  # Plot 2: Welfare Cost vs. year
  output$welfare <- renderPlot({
    ggplot(filtered_data(), aes(x = Year, y = cost_percent, color = Country)) +
      geom_line() +
      labs(x = "Year", y = "Percent of Welfare Cost", title = "% Cost on PM Premature Death of Total Welfare Comparison of Selected Countries by Year")
  })
  
  # Table
  output$table <- renderDataTable({
    filtered_data()
  })
  output$bar2 <- renderPlot({
    emissions %>%
      filter(
        Year == input$slider2[1] |
          Year == input$slider2[2]
      ) %>%
      group_by(Country) %>%
      mutate(year_max = max(Year), year_min = min(Year)) %>%
      select(Country, Year, Environmental_Policy_Stringency_Value) %>%
      pivot_wider(names_from = Year, values_from = Environmental_Policy_Stringency_Value) %>%
      rename(`First Year` = as.character(input$slider2[1]), `Last Year` = as.character(input$slider2[2])) %>%
      mutate(diff = `Last Year` - `First Year`) %>%
      drop_na() %>%
      pivot_longer(c(`Last Year`, `First Year`), names_to = "Year", values_to = "epsi") %>%
      mutate(Year = str_replace(Year, "Last Year", as.character(input$slider2[2])), Year = str_replace(Year, "First Year", as.character(input$slider2[1]))) %>%
      ggplot() +
      geom_col(aes(x = reorder(Country, diff), y = epsi, fill = Year), width = 0.7, position = "dodge") + 
      scale_fill_manual(values = c("#D95F02", "#1B9E77")) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      scale_y_continuous(expand = c(0, 0, 0.03, 0.03)) +
      labs(x = "Country", y = "EPS Index", title = "Environmental Policy Stringency (EPS) Index by Country and Year", subtitle = "Only Includes Countries Where Data from Both Years is Available") +
      theme(plot.background = element_rect(fill='transparent', color="grey"))
  })
  
  output$bar3  <- renderPlot({
    emissions %>%
      filter(
        Year == input$slider2[1] |
          Year == input$slider2[2]
      ) %>%
      select(Country, Year, Environmental_Policy_Stringency_Value) %>%
      pivot_wider(names_from = Year, values_from = Environmental_Policy_Stringency_Value) %>%
      rename(`First Year` = as.character(input$slider2[1]), `Last Year` = as.character(input$slider2[2])) %>%
      mutate(diff = `Last Year` - `First Year`) %>%
      drop_na() %>%
      ggplot() +
      geom_col(aes(x = reorder(Country, diff), y = diff), fill = "#7570B3") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      labs(x = "Country", y = "Difference Amount", title = "Difference Between Selected Years EPS Indexes") +
      scale_y_continuous(expand = c(0, 0, 0.03, 0.03)) +
      theme(plot.background = element_rect(fill='transparent', color="grey"))
  })
  
}
shinyApp(ui = ui, server = server)