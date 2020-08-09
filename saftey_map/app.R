#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## Data Preprocessing and Wrangling
library(tidyverse)

sb <- read_csv("sb.csv") 


## open google maps
library(googleAuthR)
library(googleAnalyticsR)
library(searchConsoleR)

library(sp)
library(raster)
library(rasterVis)
library(maptools)
library(rgeos)
library(dismo)
library(mapsapi)
library(devtools)
library(RgoogleMaps)
library(ggmap)

register_google(key='AIzaSyBOc4kTJ68iuOw5EvHcjPp7dLZ_WQ9sBh4')



##shiny
# Load the shiny package
library(shiny)

# Define UI for the application
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput("lon_x", "Enter Longitude",127.0278, min(sb$lon)-0.05, max(sb$lon)+0.05),
      numericInput("lat_x", "Enter Latitude", 37.5880, min(sb$lat)-0.05, max(sb$lat)+0.05),
      numericInput("zoom_x", "Enter Zoom", 15, 10, 18),
      radioButtons("color_x", "Do you want color?", c("color"="color", "black & White"="bw"))
      ),
    mainPanel(
      plotOutput("safemap"),
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$ safemap <- renderPlot({
    base_map <-	get_map(c(lon=input$lon_x, lat=input$lat_x),zoom=input$zoom_x, scale=1,maptype='roadmap', color= input$color_x)
    layer_map <- ggmap(base_map)+
      stat_density2d(aes(x=lon, y=lat, fill=..level..), alpha=0.1, geom = "polygon", data = filter(sb, type=="lights"))+
      scale_fill_gradient(low="yellow", high="red") +
      geom_point(aes(color = type, size = type), data = filter(sb, type!="lights"))+
      scale_size_manual(values=c(2,1,5))
    plot(layer_map)
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)

