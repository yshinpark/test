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
test_shp<-st_read("C:/Users/pengo/Documents/GitHub/dashboard/test/output_data.shp")
nis<-read.csv(file="C:/Users/pengo/Documents/GitHub/dashboard/test/nis.csv")
merged_nis<-test_shp %>%
  left_join(., nis, by = c("NAME" = "State"))

indicator<-unique(merged_nis$Indicator)
year<-sort(unique(merged_nis$Year))

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
             h3("This is a Choropleth map"),
             plotOutput("state_map"),
             h3("This is a bar chart"),
             plotOutput("bar_chart")
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
                   h3("This is a figure")
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
  
  output$bar_chart<-renderPlot({ggplot(subset(merged_nis, Indicator %in% input$indi & Year %in% input$yr),aes(x=reorder(NAME, Percent), y=Percent)) +
      geom_bar(stat = "identity") +
      coord_flip()
    
  })
  output$table1 <- renderTable({head(subset(merged_nis, Indicator %in% input$indi & Year %in% input$yr))})
}


shinyApp(ui = ui, server = server)
