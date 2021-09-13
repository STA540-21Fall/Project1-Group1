library(maps)
library(ggplot2)
library(ggiraph)
library(dplyr)
library(rlang)
library(shiny)
library(DT)
library(shinyjs)
library(shinyalert)
library(stringr)

#for closing window
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

# Define UI for application that draws a histogram
ui <- fluidPage(
  #stop the app when the window is closed
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  div(align = "right",
      actionButton("close", strong("Close"), icon("anchor"), 
                   style="color: #fff; background-color: #337ab7;
                                 border-color: #2e6da4")
  ),
  
  useShinyalert(),
  
  # Application title
  titlePanel("Covid Cases and Vaccinations by Country"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("DatesMerge",
                  "Dates:",
                  min = as.Date("2020-01-01","%Y-%m-%d"),
                  max = as.Date("2021-08-01","%Y-%m-%d"),
                  value=as.Date("2021-01-01"),
                  timeFormat="%Y-%m-%d") ,
      
      selectInput(
        inputId = "Variable",
        label = "Variable:",
        choices = c("", list("New Cases" = "new_cases"),
          "New Deaths" = "new_deaths",
          "People Fully Vaccinated" = "people_fully_vaccinated",
          "Patients Hospitalized" = "hosp_patients")
        
      ),
      selectInput(
        inputId = "Continent",
        label = "Continent:",
        choices = c(
          "",
          Africa = "Africa",
          Asia = "Asia",
          Australia = "Australia",
          Europe = "Europe",
          list("North America" = "North America",
         "South America" = "South America"),
          World = "World"
        )
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        tags$div(style = "margin: 1%",
                 htmlOutput(outputId = "InfoTitle1"),
                 tabsetPanel(
                   tabPanel(title = "Worldwide Covid19 Cases and Vaccination rate",
                            tags$div(
     
                              plotOutput("distPlot"),
                              DT::dataTableOutput(outputId = "tb"),
                             
                            )),
                   tabPanel(title = "Worldwide Covid 19 Trend Plot",
                            br(),
                            tags$div(
                              plotOutput("trendPlot")
                              #DT::dataTableOutput(outputId = "tb"),
                            ))
                 )
        ),
        htmlOutput(outputId = "text.Overview"),
        
      ) # fluidRow
      
      
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  world_data <- ggplot2::map_data('world')
  world_data <- fortify(world_data)
  covid_data <- read.csv("data/covid_data_country_clean.csv")
  covid_data_others <- read.csv("data/covid_data_clean.csv")
  
  covid_data <- covid_data %>% 
    select(c("location", "date", 
             "new_cases", "total_cases",
             "new_deaths", "total_deaths",
             "people_fully_vaccinated", "hosp_patients"))
  
  covid_data_others <- covid_data_others %>% 
    select(c("location", "date", 
             "new_cases", "total_cases",
             "new_deaths", "total_deaths",
             "people_fully_vaccinated", "hosp_patients"))
  
  covid_data$location[covid_data$location == "United States"] <- "USA"
  covid_data$location[covid_data$location == "United Kingdom"] <- "UK"
  # covid_data$location[covid_data$location == "Samoa"] <- "American Samoa"
  covid_data$location[covid_data$location == "Cote d'Ivoire"] <- "Ivory Coast"
  covid_data$location[covid_data$location == "Czechia"] <- "Czech Republic"
  covid_data$location[covid_data$location == "Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
  
  
  world_data_short <- world_data %>% 
    select(c("long", "lat", "group", "region"))
  
  world_data_short$region[world_data_short$region == "Antigua"] <- "Antigua and Barbuda"
  world_data_short$region[world_data_short$region == "Barbuda"] <- "Antigua and Barbuda"
  
  
  
  output$distPlot <- renderPlot({
    
    temp <- covid_data %>% 
      filter(date == input$DatesMerge)
    
    temp_covid <- left_join(x = world_data_short, y = temp, by = c("region" = "location"))
    
    if (input$Continent == "North America") {
      temp_covid <- temp_covid %>%
        filter((lat >= 8 & lat <= 90)) %>% 
        filter((long <= -20 & long >= -170.1667))
      
    } else if (input$Continent == "South America"){
      temp_covid <- temp_covid %>%
        filter((lat >= -60 & lat <= 20)) %>% 
        filter((long >= -100 & long <= -30))
      
    } else if (input$Continent == "Asia") {
      temp_covid <- temp_covid %>%
        filter((lat >= -10 & lat <= 90)) %>% 
        filter((long >= 50 & long <= 180))
      
    } else if (input$Continent == "Europe") {
      temp_covid <- temp_covid %>%
        filter((lat >= 15 & lat <= 90)) %>% 
        filter((long >= -20 & long <= 50))
      
    } else if (input$Continent == "Africa") {
      temp_covid <- temp_covid %>%
        filter((lat >= -40 & lat <= 40)) %>% 
        filter((long >= -30 & long <= 60))  
      
    } else if (input$Continent == "Australia") {
      temp_covid <- temp_covid %>%
        filter((lat >= -50 & lat <= -10)) %>% 
        filter((long >= 80 & long <= 180))  
      
    } else {
      temp_covid <- temp_covid %>%
        filter((lat >= -60 & lat <= 90))
    }
    
    
    
    ggplot() +
      geom_polygon_interactive(data = temp_covid, size = 0.1,
                               aes(x = long, y = lat, 
                                   fill = !! sym(input$Variable), group = group)) +
      theme(legend.position="bottom") + theme_bw() + coord_fixed() +
      labs(title = "Covid Cases and Vaccinations", subtitle = "by Country", 
           x = "Lat", y = "Long") +
      scale_fill_gradient(low = "#CC6666", high = "#000066")
  })
  
  
  
  output$tb <- DT::renderDataTable({
    
    temp <- covid_data %>% 
      filter(date == input$DatesMerge)
    
    temp_covid <- left_join(x = world_data_short, y = temp, by = c("region" = "location"))
    
    if (input$Continent == "North America") {
      temp_covid <- temp_covid %>%
        filter((lat >= 8 & lat <= 90)) %>% 
        filter((long <= -20 & long >= -170.1667))
      
    } else if (input$Continent == "South America"){
      temp_covid <- temp_covid %>%
        filter((lat >= -60 & lat <= 20)) %>% 
        filter((long >= -100 & long <= -30))
      
    } else if (input$Continent == "Asia") {
      temp_covid <- temp_covid %>%
        filter((lat >= -10 & lat <= 90)) %>% 
        filter((long >= 50 & long <= 170))
      
    } else if (input$Continent == "Europe") {
      temp_covid <- temp_covid %>%
        filter((lat >= 15 & lat <= 90)) %>% 
        filter((long >= -20 & long <= 50))
      
    } else if (input$Continent == "Africa") {
      temp_covid <- temp_covid %>%
        filter((lat >= -40 & lat <= 40)) %>% 
        filter((long >= -30 & long <= 60))  
      
    } else if (input$Continent == "Australia") {
      temp_covid <- temp_covid %>%
        filter((lat >= -50 & lat <= -10)) %>% 
        filter((long >= 80 & long <= 180))  
      
    } else {
      temp_covid <- temp_covid %>%
        filter((lat >= -60 & lat <= 90))
    }
    
    temp_covid %>% select(region,input$Variable) %>% unique()
  })
  
  output$trendPlot <- renderPlot({
    
    temp <- covid_data_others %>% 
      filter(location == input$Continent)
  
    
    ggplot(data = temp) +
      geom_line(aes(x = as.Date(date), y = !! sym(input$Variable))) +
      theme_bw()+
      labs(title = input$Variable, 
           x = "Date", y = "Counts")
  })
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)