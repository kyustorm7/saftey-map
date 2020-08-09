#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
## Data Preprocessing and Wrangling
library(tidyverse)

bell <- read_csv("bell_sb.csv") %>%
  dplyr::select(lat, lon) %>%
  mutate(type="bell")
cctv <- read_csv("cctv_sb.csv")%>%
  dplyr::select(lat, lon) %>%
  mutate(type="cctv")
lights <- read_csv("lights_sb.csv")%>%
  dplyr::select(lat, lon) %>%
  mutate(type="lights")
police <- read_csv("police_sb.csv")%>%
  dplyr::select(lat, lon) %>%
  mutate(type="police")

sb <- na.omit(rbind(bell, cctv, lights, police))

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
library(ggplot2)
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
      numericInput("zoom_x", "Enter Zoom", 15, 10, 18)
    ),
    mainPanel(
      plotOutput("safemap"),
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$ safemap <- renderPlot({
    base_map <-	get_map(c(lon=input$lon_x, lat=input$lat_x),zoom=input$zoom_x, scale=1,maptype='roadmap')
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

