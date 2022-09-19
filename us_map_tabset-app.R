library(shiny)
library(leaflet)
library(rgdal)
library(maptools)
library(mapproj)
library(rgeos)
library(tigris)
library(sf)
library(tidyverse)
library(sp)
library(tmap)

#read data
test_shp<-st_read("C:/Users/rvp8/OneDrive - CDC/Data/output_data.shp")
nis<-read.csv(file="C:/Users/rvp8/OneDrive - CDC/Data/nis.csv")
merged_nis<-test_shp %>%
  left_join(., nis, by = c("NAME" = "State"))

indicator<-unique(merged_nis$Indicator)
year<-unique(merged_nis$Year)

indicator1<-unique(merged_nis$Indicator)
year1<-unique(merged_nis$Year)
Mypal <- c('#f0f9e8','#a0e5a9','#61d2b4','#2b8cbe')

#manage data
#merged<-test_shp %>%
#  left_join(., test, by = c("NAME" = "NAME"))

ui <- fluidPage(
    tabsetPanel(
      tabPanel("Map - State Map", 
         h4("This is where you can show state-level data"),
         sidebarLayout(
           sidebarPanel(
             
             h4("Choose your state"),
             h1("Breastfeeding"),
             
             selectInput(
               "indi",
               "choose health behavioral indicator here",
               choices = indicator
             ),
             selectInput(
               "yr",
               "choose year",
               choices = year
             )
             
             
           ),
           
           
           mainPanel(
             h3("This is a figure"),
             plotOutput("state_map")
           )
         )

         ),
      tabPanel("Map - County Map",
               h4("This is where you can show county-level data"),
               sidebarLayout(
                 sidebarPanel(
                   
                   h4("Choose your state"),
                   h1("Breastfeeding"),
                   
                   selectInput(
                     "indi1",
                     "choose health behavioral indicator here",
                     choices = indicator1
                   ),
                   selectInput(
                     "yr1",
                     "choose year",
                     choices = year1
                   )
                   
                   
                 ),
                 
                 
                 mainPanel(
                   h3("This is a figure"),
                   plotOutput("state_tmap")
                 )
               )),
      tabPanel("Raw Data",h4("This is where you can show some data."),
               tableOutput("table1"))
    )
)

#ggplot() + 
#  geom_sf(aes(fill = Percent), size = .1, color = "grey65")  +
#  theme_void() +
#  labs(fill = "Percent")

server <- function(input, output, session) {
  output$state_map <- renderPlot({ggplot(subset(merged_nis, Indicator %in% input$indi & Year %in% input$yr)) + 
      geom_sf(aes(fill = Percent), size = .1, color = "grey65")  +
      theme_void() +
      labs(fill = "Percent")
  })
  
  output$state_tmap<-renderPlot({tm_shape(subset(merged_nis, Indicator %in% input$indi1 & Year %in% input$yr1)) + 
      tm_polygons("Percent", n=4, palette=Mypal) +
      tm_text("State_Abbr", remove.overlap = TRUE )
    tmap_mode("view")
    tmap_last()}
    
  )
  output$table1 <- renderTable({head(mtcars)})
}


shinyApp(ui = ui, server = server)
