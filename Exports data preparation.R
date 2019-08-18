
library(readxl)
library(readr)
library(tidyr)
library(dplyr)
Exports <- read_excel("Exports.xls", col_types = c("text", 
                                                   "text", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", 
                                                   "text", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric"))

names(Exports)[1]<- "Country"


Continent_Country <- read_csv("Continent_Country.csv")
View(Continent_Country)

## name
names(Continent_Country)[1]<- "Country"
names(Continent_Country)[2]<- "Continent"



Exports_join<- Exports%>% inner_join(Continent_Country, by= "Country") %>% select(Country, Continent, everything())
View(Exports_join)

Exports_Data<- Exports_join%>%
  gather(Year,Exports, `1960`:`2017`)%>%
  select(1,2,4,5)


View(Exports_Data)
write.csv(Exports_Data, file="Exports_Data.csv", row.names = FALSE)

