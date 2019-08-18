library(readxl)
library(tidyr)
library(dplyr)
library(plotly)
library(scales)


# Upload GNI and prepare --------------------------------------------------

  library(readxl)
  
  GNI_per_capita <- read_excel("GNI per capita.xlsx")
  View(GNI_per_capita)
  colnames(GNI_per_capita)
  
  GNI_per_capita<-gather(GNI_per_capita, Year, GNI, 3:30)
  GNI_per_capita<-GNI_per_capita %>% select(1,4:5)
  View(GNI_per_capita)

# Country -----------------------------------------------------------------

library(readr)
Continent_Country <- read_csv("Continent_Country.csv")
View(Continent_Country)

## name
names(Continent_Country)[1]<- "Country"
names(Continent_Country)[2]<- "Continent"

# Join --------------------------------------------------------------------

GNI_per_capita_join<- GNI_per_capita%>% inner_join(Continent_Country, by= "Country")

View(GNI_per_capita_join)



# life Expectancy ---------------------------------------------------------

LifeExpectancy <- read_csv("LifeExpectancy.csv")
View(LifeExpectancy)

## name

names(LifeExpectancy)[1]<-"Country"

## gather

LifeExpectancy_gather<- gather(LifeExpectancy, Year, Life_Expectancy, '1990':'2016')%>% select(1,"Year","Life_Expectancy")

View(LifeExpectancy_gather)


# Join 2 ------------------------------------------------------------------

GNI_per_capita_LE_join<- GNI_per_capita_join%>% inner_join(LifeExpectancy_gather, by= c("Country","Year"))

View(GNI_per_capita_LE_join)



# Upload Population -------------------------------------------------------

Population <- read_csv("Population.csv")
View(Population)

## gather

Population<-gather (Population, Year, Population, `1990`:`2016`)%>% select(1, "Year", "Population")
names(Population)[1]<-"Country"

View(Population)



# Join --------------------------------------------------------------------

#############   Join

GNI_per_capita_LE_join_POP<-GNI_per_capita_LE_join %>%inner_join(Population, by=c("Country", "Year")) 


View(GNI_per_capita_LE_join_POP)

data<-GNI_per_capita_LE_join_POP

colnames(data)

# Prepare the data for plotly ---------------------------------------------

#############   reorder

data <-data[c(1,4,2,3,5,6)]

View(data)


#############   omit na rows

data_plotly_GNI_LE<-na.omit(data)

#############   Round Life Expectancy to two number

data_plotly_GNI_LE$Life_Expectancy<-round(data_plotly_GNI_LE$Life_Expectancy, 2)

View(data_plotly_GNI_LE)


#############   Create csv file

write.csv(data_plotly_GNI_LE, file="data_plotly_GNIPC_LE.csv", row.names = FALSE)

colnames(data_plotly_GNI_LE)




# Plotly  -----------------------------------------------------------------

p_LE_GNI <-   plot_ly(data_plotly_GNI_LE, 
               x = ~GNI, 
               y = ~Life_Expectancy, 
               color = ~Continent, 
               frame = ~Year, 
               size = ~Population,
               text =  ~paste('</br> Country: ', Country,
                              '</br> Year: ', Year,
                              '</br> Population: ', comma_format()(Population),
                              '</br> GNI per capita ($): ', comma_format()(GNI),
                              '</br> Life Expectancy (years): ', round(Life_Expectancy,2)), 
               
               
               hoverinfo = "text",
               type = 'scatter',
               mode = 'markers'
) %>%
  animation_slider(
    currentvalue = list(prefix = "Year ", font = list(color="red"))
  ) %>%
  
  layout(xaxis = list(range = c(0, 130000), title = 'GNI per capita($)'),
         yaxis = list(range = c(15, 95), title = 'Life Expectancy'),
         title = 'Life Expectancy -  GNI per capita ($) - 1990 - 2016')

  

p_LE_GNI
