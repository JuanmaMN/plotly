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





# Upload the data ---------------------------------------------------------


#devtools::install_github("gadenbuie/ggpomological")

library(ggpomological)
library(readxl)
library(tidyverse)
Avg_2 <- read_excel("Avg_2.xlsx")
View(Avg_2)


Avg_22<- Avg_2 %>% select(1,6,7) %>% filter(Team %in% c("South Africa", "Australia", "Ireland",
                                                    "Japan", "New Zealand",
                                                    "England", "France","Wales"))

colnames(Avg_22)

Avg_223<-Avg_22%>% mutate (diff=round(Avg_Tries_Pool-Avg_Tries_KO,1),
                  label=ifelse(diff>0, paste0("+",diff), paste0(diff)))




library(dumbbel)

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggalt)
library(scales)
library(hrbrthemes)

g<-ggplot(Avg_223, aes(x = Avg_Tries_Pool, xend = Avg_Tries_KO, y=reorder(Team,Avg_Tries_Pool))) + 
  geom_dumbbell(colour = "#dddddd",
                size = 3,
                colour_x = "#FAAB18",
                colour_xend = "#1380A1")+
  labs(x=NULL, y=NULL, title="ggplot2 geom_dumbbell with dot guide")  +
  labs(
    title = "Rugby World Cup 2019",
    subtitle = "Teams in Quarter-finals - Pool stages VS KO stages stats",
    caption = "\n Source: Rugby World Cup
      Visualization: JuanmaMN (Twitter @Juanma_MN)",
    x = "",
    y = "") + theme(legend.position = "bottom",
                    legend.box = "vertical")  + geom_text(data = filter(Avg_22, Team == "New Zealand"),
                                                          aes(x = Avg_Tries_Pool, y = Team),
                                                          label = "Pool", fontface = "bold",
                                                          color = "#395B74",
                                                          vjust = -1.8) +
  geom_text(data = filter(Avg_22, Team == "New Zealand"),
            aes(x = Avg_Tries_KO, y = Team),
            label = "KO", fontface = "bold",
            color = "#F7BC08",
            vjust = -1.8)

g2<-g + 
  geom_rect(aes(xmin=8, xmax=10, ymin=-Inf, ymax=Inf), fill="grey80") +
  geom_text(aes(label=diff, y=Team, x=9), fontface="bold", size=4) +
  geom_text(aes(x=9, y=7.7, label="Difference"),
            color="grey20", size=4, vjust=-3, fontface="bold") +
  scale_x_continuous(breaks = c(1.5:7.5), limits = c(1, 10),expand = c(0, 0))   + 
  theme_ipsum()


g2



# Waffle --------------------------------------------------------------


library(readxl)
Avg_waffle <- read_excel("~/R/Avg_waffle.xlsx")
View(Avg_waffle)

library(tidyverse)


Avg_waffle_2<-Avg_waffle%>%select(1,5,8) %>% filter (Team %in% c("South Africa",
                                                                 "New Zealand",
                                                                 "Australia",
                                                                 "Wales",
                                                                 "Ireland",
                                                                 "England",
                                                                 "Japan",
                                                                 "France"))

head(Avg_waffle_2)
Avg_waffle_3<-Avg_waffle_2%>% pivot_longer(-Team, names_to="Round") 

head(Avg_waffle_3)
Avg_waffle_3$Round[Avg_waffle_3$Round == "Total tries"] <- "Pool Stages"
Avg_waffle_3$Round[Avg_waffle_3$Round == "Total_tries_in_K0"] <- "Knock-out Stages"




#Avg_waffle_3 %>%   count(Team, Round) -> Avg_waffle_3f

#View(Avg_waffle_3f)

ggplot(Avg_waffle_3, aes(fill = Team, values = value)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(~Round, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  ggthemes::scale_fill_tableau(name=NULL) +
  coord_equal() +
  labs(
    title = "Faceted Waffle Bar Chart",
    subtitle = "{dplyr} storms data",
    x = "Year",
    y = "Count"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
  guides(fill = guide_legend(reverse = TRUE))




