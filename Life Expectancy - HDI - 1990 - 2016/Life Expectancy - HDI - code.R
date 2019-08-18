### Set working directory


### Packages to upload

library(dplyr)
library(plotly)
library(tidyr)
library(scales)
library(readr)

library(readxl)


####################################################################
#############   Upload the files and join the tables   #############
####################################################################


#############   Upload the file: HDI:  First Document

Human_development_index <- read_excel("Human development index.xlsx")
View(Human_development_index)
colnames(Human_development_index)
## gather

Human_Development_Index_HDI<-gather(Human_development_index, Year, HDI, 2:28)
View(Human_Development_Index_HDI)


#############  Upload the file: Continent and country: Second Document


Continent_Country <- read_csv("Continent_Country.csv")
View(Continent_Country)

## name
names(Continent_Country)[1]<- "Country"
names(Continent_Country)[2]<- "Continent"


#############  join

Human_Development_Index_HDI_join<- Human_Development_Index_HDI%>% inner_join(Continent_Country, by= "Country")

View(Human_Development_Index_HDI_join)


#############   Upload the file: Life Expectacnty: Third Document


LifeExpectancy <- read_csv("LifeExpectancy.csv")
View(LifeExpectancy)

## name

names(LifeExpectancy)[1]<-"Country"

## gather

LifeExpectancy_gather<- gather(LifeExpectancy, year, Life_Expectancy, '1990':'2016')%>% select(1,"year","Life_Expectancy")

View(LifeExpectancy_gather)

names(LifeExpectancy_gather)[2]<-"Year"


#############   Join

Human_Development_Index_HDI_join_LE<- Human_Development_Index_HDI_join%>% inner_join(LifeExpectancy_gather, by= c("Country","Year"))

View(Human_Development_Index_HDI_join_LE)


#############  Upload the file: Population: Fourth Document

Population <- read_csv("Population.csv")
View(Population)

## gather

Population<-gather (Population, Year, Population, `1990`:`2016`)%>% select(1, "Year", "Population")
names(Population)[1]<-"Country"

View(Population)


#############   Join

Human_Development_Index_HDI_join_LE_POP<- Human_Development_Index_HDI_join_LE%>%inner_join(Population, by=c("Country", "Year")) 


View(Human_Development_Index_HDI_join_LE_POP)

data<-Human_Development_Index_HDI_join_LE_POP


####################################################################
#############   Prepare the data for plotly            #############
####################################################################


#############   reorder

data <-data[c(1,4,2,3,5,6)]

View(data)


#############   omit na rows

data_plotly<-na.omit(data)  

View(data_plotly)

#############   Round Life Expectancy to two number

data_plotly$Life_Expectancy<-round(data_plotly$Life_Expectancy, 2)

View(data_plotly)


#############   Create csv file

write.csv(data_plotly, file="data_plotly.csv", row.names = FALSE)

colnames(data_plotly)



####################################################################
#############   Prepare the data for plotly            #############
####################################################################

p <-   plot_ly(data_plotly, 
               x = ~HDI, 
               y = ~Life_Expectancy, 
               color = ~Continent, 
               frame = ~Year, 
               size = ~Population,
               text =  ~paste('</br> Country: ', Country,
                              '</br> Year: ', Year,
                              '</br> Population: ', comma_format()(Population),
                              '</br> HDI: ', HDI,
                              '</br> Life Expectancy (years): ', round(Life_Expectancy,2)), 
               
               
               hoverinfo = "text",
               type = 'scatter',
               mode = 'markers'
) %>%
  layout(xaxis = list(range = c(0, 1), title = 'Human Development Index (HDI)'),
         yaxis = list(range = c(0, 100), title = 'Life Expectancy'),
         title = 'Life Expectancy -  HDI - 1990 - 2016')

p
