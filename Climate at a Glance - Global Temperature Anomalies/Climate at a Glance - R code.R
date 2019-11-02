library(ggplot2)
library(plotly)
library(readxl)

Temperature <- read_excel("Climate at a Glance - Global Temperature Anomalies/Temperature.xlsx")
View(Temperature)


Temperature$colour <- ifelse(Temperature$Value < 0, "firebrick1","steelblue")

Temperature_2<-select(Temperature,1:2)


### Plotly



plot_ly(Temperature, x=~Year, y=~Value, mode="markers", type="bar", color=~colour , text = ~paste('Year: ', Year, '</br> Value(°C): ', Value)) %>% 
  layout(title = "Global Land and Ocean Temperature Anomalies",xaxis = list(title = ""), yaxis = list(title = "anomaly(°C)"),
         annotations = 
           list(text = "Data from www.ncdc.noaa.gov ", 
                showarrow = F, xref='paper', yref='paper', 
                xref = 'paper', x = 0,
                yref = 'paper', y = 1,
                font=list(size=10, color="black"))
  )  %>% hide_legend()



### Plotly with slider




plot_ly(Temperature, x=~Year, y=~Value, mode="markers", type="bar", color=~colour , text = ~paste('Year: ', Year, '</br> Value(°C): ', Value)) %>% 
  layout(title = "Global Land and Ocean Temperature Anomalies",xaxis = list(title = ""), yaxis = list(title = "anomaly(°C)"),
         annotations = 
           list(text = "Data from www.ncdc.noaa.gov ", 
                showarrow = F, xref='paper', yref='paper', 
                xref = 'paper', x = 0,
                yref = 'paper', y = 1,
                font=list(size=10, color="black"))
  )  %>% hide_legend()%>%rangeslider(Temperature$Year[1880],Temperature$Year[2019])



### Gplot




g<- ggplot(Temperature,aes(x=Year, y=Value))+
  geom_bar(stat="identity",position="identity",aes(fill = colour), show.legend = FALSE) + 
  labs(title = "Global Time Series", subtitle = "Global Land and Ocean Temperature Anomalies", caption = "based on data from www.ncdc.noaa.gov") + ylab("anomaly(°C)")+xlab("")

g



## Code, questions and feedback

The code for this article is publish in my [github account](https://github.com/JuanmaMN).

Please feel free to reach out for any question or feedback.