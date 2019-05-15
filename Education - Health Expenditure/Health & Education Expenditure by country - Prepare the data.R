library(readr)
library(tidyr)
library(dplyr)
library(plotly)
library(scales)


# Upload Education and prepare --------------------------------------------------

Education_expenditure <- read_csv("Education - Health Expenditure/Education_expenditure.csv")
View(Education_expenditure)
#colnames(Education_expenditure)


Education_expenditure_gather<-gather(Education_expenditure, Year, Education_expenditure, '2000':'2016')
Education_expenditure_gather<-Education_expenditure_gather %>% select(1,4,5)
View(Education_expenditure_gather)

# Country -----------------------------------------------------------------

library(readr)
Continent_Country <- read_csv("Continent_Country.csv")
#View(Continent_Country)

## name
names(Continent_Country)[1]<- "Country"
names(Continent_Country)[2]<- "Continent"

# Join --------------------------------------------------------------------

Education_expenditure_join<- Education_expenditure_gather%>% inner_join(Continent_Country, by= "Country")

View(Education_expenditure_join)



# Health_expenditure ---------------------------------------------------------

Health_expenditure <- read_csv("Education - Health Expenditure/Health_expenditure.csv")
View(Health_expenditure)


## gather

Health_expenditure_gather<- gather(Health_expenditure, Year, Health_expenditure, '2000':'2016')%>% select(1,"Year","Health_expenditure")

View(Health_expenditure_gather)


# Join 2 ------------------------------------------------------------------

Education_Health_join<- Education_expenditure_join%>% inner_join(Health_expenditure_gather, by= c("Country","Year"))

View(Education_Health_join)



# Upload Population -------------------------------------------------------

Population <- read_csv("Population.csv")
View(Population)

## gather

Population<-gather (Population, Year, Population, `2000`:`2016`)%>% select(1, "Year", "Population")
names(Population)[1]<-"Country"

View(Population)



# Join --------------------------------------------------------------------

#############   Join

Education_Health_join_POP<-Education_Health_join %>%inner_join(Population, by=c("Country", "Year")) 


View(Education_Health_join_POP)


data<-Education_Health_join_POP

colnames(data)

# Prepare the data for plotly ---------------------------------------------

#############   reorder

data <-data[c(1,4,2,3,5,6)]

View(data)


#############   omit na rows

data_plotly_Health_Education<-na.omit(data)

View(data_plotly_Health_Education)
colnames(data_plotly_Health_Education)


#############   Round Health and Education to two number


data_plotly_Health_Education$Education_expenditure<-round(data_plotly_Health_Education$Education_expenditure, 2)
data_plotly_Health_Education$Health_expenditure<-round(data_plotly_Health_Education$Health_expenditure, 2)

View(data_plotly_Health_Education)

#############   Create csv file

write.csv(data_plotly_Health_Education, file="data_plotly_Education_Health.csv", row.names = FALSE)

colnames(data_plotly_Health_Education)




# Plotly  -----------------------------------------------------------------

p_Education_Health <-   plot_ly(data_plotly_Health_Education, 
                      x = ~Education_expenditure, 
                      y = ~Health_expenditure, 
                      color = ~Continent, 
                      frame = ~Year, 
                      size = ~Population,
                      text =  ~paste('</br> Country: ', Country,
                                     '</br> Year: ', Year,
                                     '</br> Population: ', comma_format()(Population),
                                     '</br> Government expenditure on education (% of GDP): ', Education_expenditure,
                                     '</br> Current health expenditure (% of GDP): ', Health_expenditure), 
                      
                      
                      hoverinfo = "text",
                      type = 'scatter',
                      mode = 'markers'
) %>%
  animation_slider(
    currentvalue = list(prefix = "Year ", font = list(color="red"))
  ) %>%
  
  layout(xaxis = list(range = c(0, 15), title = 'Government expenditure on education (% of GDP)'),
         yaxis = list(range = c(0,20), title = 'Current health expenditure (% of GDP)'),
         title = 'Health & Education Expenditure by country',
         annotations = 
           list(text = "Data from data.worldbank.org", 
                showarrow = F, xref='paper', yref='paper', 
                xref = 'paper', x = 1,
                yref = 'paper', y = 0,
                font=list(size=12, color="black")))


p_Education_Health
