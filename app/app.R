library(maps)
library(ggplot2)
library(ggiraph)
library(dplyr)
library(rlang)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("New Covid Cases by Country"),

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
          choices = c(
            "",
            new_cases  = "new_cases",
            new_deaths = "new_deaths",
            people_fully_vaccinated = "people_fully_vaccinated"
          )
        )
      ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    world_data <- ggplot2::map_data('world')
    world_data <- fortify(world_data)
    covid_data <- read.csv("data/covid_data_country_clean.csv")
    
    covid_data <- covid_data %>% 
      select(c("location", "date", 
               "new_cases", "total_cases",
               "new_deaths", "total_deaths",
               "people_fully_vaccinated"))
    
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
      
      ggplot() + 
        geom_polygon_interactive(data = subset(temp_covid, lat >= -60 & lat <= 90), size = 0.1, 
                                 aes(x = long, y = lat, fill = !! sym(input$Variable), group = group)) +
        theme(legend.position="bottom") + theme_bw()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
