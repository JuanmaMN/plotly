# Packages used  ----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(plotly)
library(scales)
library(ggthemes)
library(hrbrthemes)

library(readxl)


# Upload the data ---------------------------------------------------------


Avg <- read_excel("Rugby World Cup/Avg.xlsx")


# Data cleaning and filtering ---------------------------------------------

names(Avg)[2]<-"Total_Points"
names(Avg)[4]<-"Average_Points_Game"
names(Avg)[6]<-"Average_Tries_Game"

Avg2<- Avg %>% select(1,4,6) %>% filter(Team %in% c("South Africa", "Australia", "Ireland",
                                                    "Japan", "New Zealand",
                             "England", "France","Wales"))



# Plotly --------------------------------------------------------------


# Logo for the graph
RWlogo <- png::readPNG("C:/RepTemplates/plotly/Rugby World Cup/RW.png")



# First plot
pA <- plot_ly(Avg2, x = ~Average_Points_Game, y = ~reorder(Team, Average_Points_Game), 
              name = 'Average Points per game -  Pool stages',
              type = 'bar', orientation = 'h',
              text =  ~paste('</br> Team: ', Team,
                             '</br> Avg. Points per Game: ', round(Average_Points_Game,2)),
              hoverinfo = "text",
              marker = list(color = '#add8e6',
                            line = list(color = '#add8e6', width = 1)))%>%
  layout(yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(20, 50)),
         xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = TRUE),
         margin = list(l=100, r=50, b=50, t=50, pad=10)) %>%
 add_annotations(xref = 'x1', yref = 'y',
                  x = ~Average_Points_Game * 1.1 + 1,  y = ~Team,
                   text = ~paste(round(Average_Points_Game,2)),
                   font = list(family = 'Arial', size = 12, color = 'rgb(50, 171, 96)'),
                   showarrow = FALSE)

# Second plot

pB1 <- plot_ly(Avg2,x = ~Average_Tries_Game, y = ~reorder(Team, Average_Points_Game), 
              name = 'Average Tries per game -  Pool stages',
              type = 'scatter',mode = 'markers',
              text =  ~paste('</br> Team: ', Team,
                             '</br> Avg. Tries per Game: ', round(Average_Tries_Game,2)),
              hoverinfo = "text",
              textposition = 'top right',
              marker = list(size = 10,
                            color = '#f08080',
                            line = list(color = '#f08080',
                                        width = 1))) %>%
  layout(yaxis = list(showgrid = FALSE, showline = TRUE, showticklabels = FALSE,
                      domain = c(0, 10)),
         xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = TRUE,
                      side = 'top', dtick = 1))  %>%
  add_annotations(xref = 'x2', yref = 'y',
                  x =  ~Average_Tries_Game* 1.05, y = ~Team,
                  text = ~paste(round(Average_Tries_Game,2)),
                  font = list(family = 'Arial', size = 12, color = 'rgb(50, 171, 96)'),
                  showarrow = FALSE)


# Both plots

p <- subplot(pA, pB1) %>%
  layout(title = 'Rugby World Cup 2019  - Teams in Quarter-finals - Pool stages stats',autosize = T,
         legend = list(x = 0.6, y = -0.15,
                       font = list(size = 12)),
         margin = list(l = 100, r = 50, t = 100, b = 100, pad=6),
         paper_bgcolor = '#fffcf8',
         plot_bgcolor = '#fffcf8') %>%
  add_annotations(xref = 'paper', yref = 'paper',
                  x = -0.07, y = -0.15,
                  text = paste('Rugby World Cup (Japan 2019). Games cancelled not considered in calculation. (Data updated on 13 October 2019)'),
                  font = list(family = 'Arial', size = 12, color = 'rgb(150,150,150)'),
                  showarrow = FALSE)  %>% 
  layout(showlegend = TRUE)%>% 
  layout(
    images = list(
      source = raster2uri(as.raster(RWlogo)),
      x = 1, y = -0.15, 
      sizex = 0.2, sizey = 0.2,
      xref = "paper", yref = "paper", 
      xanchor = "right", yanchor = "bottom"
    ),
    margin = list(t = 50)
  )

f <- list(
  family = "Arial",
  size = 20,
  color = "#000000"
)

p %>% layout(list(font = f))



