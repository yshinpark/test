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

unique(merged_nis$Indicator)

#[1] "Percent of breastfed infants who were supplemented with infant formula before 6 months"      
#[2] "Percent of infants who were exclusively breastfed through 3 months"                          
#[3] "Percent of breastfed infants who were supplemented with infant formula within 2 days of life"
#[4] "Percent of infants who were breastfed at 12 months"                                          
#[5] "Percent of infants who were breastfed at 6 months"                                           
#[6] "Percent of infants who were ever breastfed"                                                  
#[7] "Percent of infants who were exclusively breastfed through 6 months"                          
#[8] "Percent of breastfed infants who were supplemented with infant formula before 3 months"  

sort(unique(merged_nis$Year)) #2000 to 2019

###subset

merged_nis_sub<-merged_nis[merged_nis$Indicator=="Percent of infants who were ever breastfed"& merged_nis$Year== 2019, ]
unique(merged_nis_sub$Year)
unique(merged_nis_sub$Indicator)

library(ggplot2)

ggplot(merged_nis_sub, aes(x=reorder(NAME, Percent), y=Percent)) +
  geom_bar(stat = "identity") +
  coord_flip()


fig1 <- plot_ly(merged_nis_sub, x = ~NAME, y = ~Percent, name = 'Percent of infants who were ever breastfed',
                type = 'bar', orientation = 'h',
                marker = list(color = 'rgba(50, 171, 96, 0.6)',
                              line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) 
fig1



#interactive bar chart
library(plotly)

y <- c('Japan', 'United Kingdom', 'Canada', 'Netherlands', 'United States', 'Belgium', 'Sweden', 'Switzerland')
x_saving <- c(1.3586, 2.2623000000000002, 4.9821999999999997, 6.5096999999999996,
              7.4812000000000003, 7.5133000000000001, 15.2148, 17.520499999999998)
x_net_worth <- c(93453.919999999998, 81666.570000000007, 69889.619999999995, 78381.529999999999,
                 141395.29999999999, 92969.020000000004, 66090.179999999993, 122379.3)
data <- data.frame(y, x_saving, x_net_worth)

fig1 <- plot_ly(x = ~x_saving, y = ~reorder(y, x_saving), name = 'Household savings, percentage of household disposable income',
                type = 'bar', orientation = 'h',
                marker = list(color = 'rgba(50, 171, 96, 0.6)',
                              line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) 
fig1 <- fig1 %>% layout(yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
                        xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) 
fig1 <- fig1 %>% add_annotations(xref = 'x1', yref = 'y',
                                 x = x_saving * 2.1 + 3,  y = y,
                                 text = paste(round(x_saving, 2), '%'),
                                 font = list(family = 'Arial', size = 12, color = 'rgb(50, 171, 96)'),
                                 showarrow = FALSE)

fig2 <- plot_ly(x = ~x_net_worth, y = ~reorder(y, x_saving), name = 'Household net worth, Million USD/capita',
                type = 'scatter', mode = 'lines+markers',
                line = list(color = 'rgb(128, 0, 128)')) 
fig2 <- fig2 %>% layout(yaxis = list(showgrid = FALSE, showline = TRUE, showticklabels = FALSE,
                                     linecolor = 'rgba(102, 102, 102, 0.8)', linewidth = 2,
                                     domain = c(0, 0.85)),
                        xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE,
                                     side = 'top', dtick = 25000)) 
fig2 <- fig2 %>% add_annotations(xref = 'x2', yref = 'y',
                                 x = x_net_worth, y = y,
                                 text = paste(x_net_worth, 'M'),
                                 font = list(family = 'Arial', size = 12, color = 'rgb(128, 0, 128)'),
                                 showarrow = FALSE)

fig <- subplot(fig1, fig2) 
fig <- fig %>% layout(title = 'Household savings & net worth for eight OECD countries',
                      legend = list(x = 0.029, y = 1.038,
                                    font = list(size = 10)),
                      margin = list(l = 100, r = 20, t = 70, b = 70),
                      paper_bgcolor = 'rgb(248, 248, 255)',
                      plot_bgcolor = 'rgb(248, 248, 255)')
fig <- fig %>% add_annotations(xref = 'paper', yref = 'paper',
                               x = -0.14, y = -0.15,
                               text = paste('OECD (2015), Household savings (indicator), Household net worth (indicator). doi: 10.1787/cfc6f499-en (Accessed on 05 June 2015)'),
                               font = list(family = 'Arial', size = 10, color = 'rgb(150,150,150)'),
                               showarrow = FALSE)

fig