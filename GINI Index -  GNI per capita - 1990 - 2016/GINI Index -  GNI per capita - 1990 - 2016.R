library(readxl)
library(tidyr)
library(dplyr)
library(plotly)
library(scales)


# Upload GNI and prepare --------------------------------------------------


GNI_per_capita <- read_excel("GNI per capita.xlsx")
View(GNI_per_capita)
colnames(GNI_per_capita)

GNI_per_capita<-gather(GNI_per_capita, Year, GNI, 3:30)
GNI_per_capita<-GNI_per_capita %>% select(1,4:5)
View(GNI_per_capita)


# Upload GINI  --------------------------------------------------


library(readxl)
GINI_index <- read_excel("GINI index.xlsx")
View(GINI_index)
colnames(GINI_index)

GINI_index<-gather(GINI_index, Year, GINI, 3:30)
View(GINI_index)
GINI_index<-GINI_index%>%select(1,3:4)
GINI_index
names(GINI_index)[1]<- "Country"


# Country -----------------------------------------------------------------

library(readr)
Continent_Country <- read_csv("Continent_Country.csv")
View(Continent_Country)

## name
names(Continent_Country)[1]<- "Country"
names(Continent_Country)[2]<- "Continent"


# Join --------------------------------------------------------------------

GINI_index_join<- GINI_index%>% inner_join(Continent_Country, by= "Country")

View(GINI_index_join)


# Life Expectancy ---------------------------------------------------------

LifeExpectancy <- read_csv("LifeExpectancy.csv")
View(LifeExpectancy)

## name

names(LifeExpectancy)[1]<-"Country"

## gather

LifeExpectancy_gather<- gather(LifeExpectancy, year, Life_Expectancy, '1990':'2017')%>% select(1,"year","Life_Expectancy")

View(LifeExpectancy_gather)

names(LifeExpectancy_gather)[2]<-"Year"



# Join 2 ------------------------------------------------------------------

GNI_index_GNI_per_capita<- GINI_index_join%>% inner_join(GNI_per_capita, by= c("Country","Year"))

View(GNI_index_GNI_per_capita)


# Population --------------------------------------------------------------

Population <- read_csv("Population.csv")
View(Population)

## gather

Population<-gather (Population, Year, Population, `1990`:`2017`)%>% select(1, "Year", "Population")
names(Population)[1]<-"Country"

View(Population)


# Join 3 ------------------------------------------------------------------


GNI_index_GNI_per_capita_POP<- GNI_index_GNI_per_capita%>%inner_join(Population, by=c("Country", "Year")) 

View(GNI_index_GNI_per_capita_POP)

data<-GNI_index_GNI_per_capita_POP
head(data)

# Prepare data for plotly -------------------------------------------------

# Reodrder

data <-data[c(1,4,2,3,5,6)]

View(data)

data_plotly<-na.omit(data)  

#data_plotly$GNI<-round(data_plotly$GNI, 2)

View(data_plotly)


# Create csv --------------------------------------------------------------

#############   Create csv file

write.csv(data_plotly, file="data_plotly_GNIIndex.csv", row.names = FALSE)


colnames(data_plotly)

# plotly ------------------------------------------------------------------

pGINI <-   plot_ly(data_plotly, 
               x = ~GINI, 
               y = ~GNI, 
               color = ~Continent, 
               frame = ~Year, 
               size = ~Population,
               text =  ~paste('</br> Country: ', Country,
                              '</br> Year: ', Year,
                              '</br> Population: ', comma_format()(Population),
                              '</br> GINI index: ', GINI,
                              '</br> GNI per capita ($): ', round(GNI,2)), 
               
               
               hoverinfo = "text",
               type = 'scatter',
               mode = 'markers'
) %>%
  layout(xaxis = list(range = c(10, 80), title = 'GINI index'),
         yaxis = list(range = c(0, 80000), title = 'GNI'),
         title = 'GINI Index -  GNI per capita - 1990 - 2016') %>%
  animation_slider(
    currentvalue = list(prefix = "Year ", font = list(color="red"))
  )


pGINI 


## Test


pGINI_bar <-   plot_ly(data_plotly, 
                   x = ~GINI, 
                   y = ~GNI, 
                   color = ~Continent, 
                   frame = ~Year, 
                   size = ~Population,
                   text =  ~paste('</br> Country: ', Country,
                                  '</br> Year: ', Year,
                                  '</br> Population: ', comma_format()(Population),
                                  '</br> GINI index: ', GINI,
                                  '</br> GNI per capita ($): ', round(GNI,2)), 
                   
                   
                   hoverinfo = "text",
                   type = 'bar',
                   mode = 'markers'
) %>%
  layout(xaxis = list(range = c(10, 80), title = 'GINI index'),
         yaxis = list(range = c(0, 80000), title = 'GNI'),
         title = 'GINI Index -  GNI per capita - 1990 - 2016')


pGINI_bar
