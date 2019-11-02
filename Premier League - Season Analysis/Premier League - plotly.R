# Completed on August 18th 2019
# Set working directory ---------------------------------------------------


# Packages used ------------------------------------------------------------


library(readxl)
library(tidyverse)
library(dplyr)
library(plotly)
library(scales)
library(ggthemes)
library(hrbrthemes)

# Upload the file ---------------------------------------------------------

library(readxl)
PremierLeague <- read_excel("Premier League/PremierLeague.xlsx")
View(PremierLeague)



# Plotly --------------------------------------------------------------

PLlogo <- png::readPNG("C:/RepTemplates/plotly/Premier League/PL.png")



p5 <-   plot_ly(PremierLeague, 
                x = ~Goals, 
                y = ~Wins, 
                color = ~Club, 
                frame = ~Season, 
                size = ~Shots,
                text =  ~paste('</br> Club: ', Club,
                               '</br> Season: ', Season,
                               '</br> Shots: ', comma_format()(Shots),
                               '</br> Goals: ', Goals,
                               '</br> Wins: ', Wins), 
                
                
                hoverinfo = "text",
                type = 'scatter',
                mode = 'markers'
) %>%
  layout(xaxis = list(range = c(20, 110),  title = 'Goals'),
         yaxis = list(range = c(0, 40), title = 'Wins')) %>%
  layout(title = 
           list(
             text = "Premier League - Season analysis", 
             xanchor = "middle",
             font = list(
               family = "times New Roman", 
               color = "#1E86FF", 
               size = 20
             )
           )
  ) %>% 
  layout(showlegend = FALSE)%>% 
    layout(
    images = list(
      source = raster2uri(as.raster(PLlogo)),
      x = 1, y = 0.9, 
      sizex = 0.15, sizey = 0.15,
      xref = "paper", yref = "paper", 
      xanchor = "right", yanchor = "bottom"
    ),
    margin = list(t = 50)
  )

p5

