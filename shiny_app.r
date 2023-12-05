# Load the necessary libraries
library(shiny)
library(readr)
library(leaflet) # Added library for map visualization
library(sf)
library(httr)
library(ggplot2)

# Load the data
data <- read_csv("C:/Users/shuey/Downloads/Income Analytics.csv")

# No longer removing "SpatialObj" as it is needed for map visualization
data <- data[, !(names(data) %in% c("Key", "Name", "Adult_Population", "Households", "Avg_Percentage_Women"))]
data <- data[, !(names(data) %in% c("Avg_Percentage_Men", "StdDev_Percentage_Women", "StdDev_Percentage_Men"))]
names(data)[names(data) == "AverageIncomePerHouse"] <- "Average Household Income"

# Define UI for application
ui <- fluidPage(
   titlePanel("Women's Presence in the Household"),
   sidebarLayout(
       mainPanel(
           leafletOutput("map"), # Added map output
           plotOutput("graph")
       ),
       sidebarPanel(
         "Hypothesis: If a higher proportion of women live in an area, then a higher average household income will be present",
           textOutput("avgIncome"), # Added panel for average income
           textOutput("avgPercentageWomen"), # Added panel for average percentage of women
              sliderInput("womenPresence", "Women's Presence", min=min(data$Percentage_Women), max=max(data$Percentage_Women), value=c(min(data$Percentage_Women), max(data$Percentage_Women)),
               step=1),
            verbatimTextOutput("summaryStats"),  # Added verbatimTextOutput for summary statistics
            
            "Result: No statistically significant relationship between proportion of women and higher average income, however; isolated geographies may yield different results. Run the analysis for yourself!"
       )
   )
)

# Define server logic
server <- function(input, output, session) {
   output$map <- renderLeaflet({
       leaflet(data) %>% 
       addTiles() %>% 
       addCircleMarkers(lng = ~Longitude, lat = ~Latitude, radius = 5, color = ~cut(`Average Household Income`, breaks = c(0, 20000, 40000, 60000, 80000, 100000), labels = c("white", "green", "yellow", "orange", "red")), opacity = 1, fillOpacity = 0.8, popup = ~paste("Income: $", format(`Average Household Income`, big.mark = ","))) 
   })
   output$view <- renderTable({
       head(data)
   })
   observe({
       map_bounds <- input$map_bounds
       if (!is.null(map_bounds)) {
           data_subset <- data[data$Longitude > map_bounds$west & data$Longitude < map_bounds$east & data$Latitude > map_bounds$south & data$Latitude < map_bounds$north,]
           if (!is.null(data_subset)) {
               data_subset <- data_subset[data_subset$Percentage_Women >= input$womenPresence[1] & data_subset$Percentage_Women <= input$womenPresence[2],]
               output$avgIncome <- renderText({
                   paste("Average Household Income: $", format(round(mean(data_subset$`Average Household Income`, na.rm = TRUE), digits = 0), big.mark = ",")) # Added average income calculation
               })
               output$avgPercentageWomen <- renderText({
                   paste("Average Percentage of Women: ", round(mean(data_subset$`Percentage_Women`, na.rm = TRUE), 2), "%") # Enhanced average percentage of women calculation
               })
               data_subset$color <- cut(data_subset$`Average Household Income`, breaks = c(0, 20000, 40000, 60000, 80000, 100000), labels = c("white", "green", "yellow", "orange", "red"))
               data_subset$fillOpacity <- ifelse(data_subset$Percentage_Women >= input$womenPresence[1] & data_subset$Percentage_Women <= input$womenPresence[2], 0.8, 0) # Set fillOpacity based on Women's Presence filter
               
               # Update data_subset based on Women's Presence filter
               data_subset <- data_subset[data_subset$Percentage_Women >= input$womenPresence[1] & data_subset$Percentage_Women <= input$womenPresence[2],]
               # Create a graph based on the filtered data_subset
               output$graph <- renderPlot({
                    ggplot(data_subset, aes(x = `Average Household Income`, y = Percentage_Women, color = color)) +
                        geom_point(size = 3) +
                        geom_smooth(method = "lm", se = FALSE) +  # Add a linear trend line without confidence interval
                        labs(title = "Income vs. Percentage of Women", x = "Average Household Income", y = "Percentage of Women", color = "Income Level") +
                        theme_minimal()
                })
                output$summaryStats <- renderPrint({
                    summary(lm(Percentage_Women ~ Aggregate_Income, data = data_subset))  # Calculate and display summary statistics for regression analysis between Aggregate_Income and Percentage_Women
                })
               
               # Clear existing markers
               leafletProxy("map") %>% clearMarkers()

               # Add markers if data_subset is not empty
               if (nrow(data_subset) > 0) {
                   leafletProxy("map", data = data_subset) %>% 
                       addCircleMarkers(
                           lng = ~Longitude, 
                           lat = ~Latitude, 
                           radius = 5, 
                           color = ~color, 
                           opacity = 1, 
                           fillOpacity = 0.8,  # Set fillOpacity to 0.8 for all markers
                           popup = ~paste("Income: $", format(`Average Household Income`, big.mark = ","))
                       ) 
               }
           }
       }
   })
}

shinyApp(ui = ui, server = server)