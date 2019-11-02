####################################################################
####################          FIRST FILE        ####################
####################################################################

# Upload the data ---------------------------------------------------------

library(readxl)
Out_of_school <- read_excel("Out_of_school.xls")
View(Out_of_school)
colnames(Out_of_school)
names(Out_of_school)[2]<- "Indicator"

unique(Out_of_school$Indicator)





# Male data ---------------------------------------------------------------

library(tidyverse)
Male<-Out_of_school %>% filter(Indicator =="Children out of school, primary, male")
# # View(Male)


Male_gather<-gather(Male, Year, Children_out_of_school_male, '1970':'2017')

Children_out_of_school_gender_Male<-Male_gather%>%select(1,2,4,5)%>%na.omit()

colnames(Children_out_of_school_gender_Male)

names(Children_out_of_school_gender_Male)[1]<-"Country"

Children_out_of_school_gender_Male_for_pyramid<-Children_out_of_school_gender_Male%>%
  mutate(Male=-(Children_out_of_school_male)) %>% select(1,3,5)

# # View(Children_out_of_school_gender_Male_for_pyramid)



# Female data -------------------------------------------------------------



library(tidyverse)
Female<-Out_of_school %>% filter(Indicator =="Children out of school, primary, female")
# # View(Female)


Female_gather<-gather(Female, Year, Female, '1970':'2017')
# # View(Female_gather)

Children_out_of_school_gender_Female<-Female_gather%>%select(1,4,5)%>%na.omit()


# # View(Children_out_of_school_gender_Female)

names(Children_out_of_school_gender_Female)[1]<-"Country"

colnames(Children_out_of_school_gender_Female)


# Join for pyramid--------------------------------------------------------------------

head(Children_out_of_school_gender_Male_for_pyramid)
head(Children_out_of_school_gender_Female)

Children_out_of_schoolpyra<-Children_out_of_school_gender_Male_for_pyramid%>%inner_join(Children_out_of_school_gender_Female, by=c("Country", "Year"))

head(Children_out_of_schoolpyra)


Children_out_of_schoolpyra_2<-Children_out_of_schoolpyra%>%
  filter (Year %in% c(2000:2017) & Country == "World") %>%
  gather(Gender, Value, 3:4)

#Children_out_of_schoolpyra_2$Value<-comma_format()(Children_out_of_schoolpyra_2$Value)

#names(Children_out_of_schoolpyra_2)[4]<- "Children out of school(primary)"

# View(Children_out_of_schoolpyra_2)





# pyramid -----------------------------------------------------------------

library(ggthemes)

# X Axis Breaks and Labels 
breaks <- seq(-100000000, 100000000, 20000000)
axis_labels <- paste0(as.character(c(seq(100, 0, -20), seq(20, 100, 20))), "m")

head(Children_out_of_schoolpyra_2)

names(Children_out_of_schoolpyra_2)[4]<-"Total"

# Plot

library(hrbrthemes)

library(plotly)


g2<-ggplot(Children_out_of_schoolpyra_2, 
           aes(x = Year, y =Total, fill = Gender)) +   
  geom_bar(stat = "identity", width = .6) +   
  scale_y_continuous(breaks = breaks,   
                   labels = axis_labels) + 
  coord_flip() +
  theme_ipsum() +  
  theme(legend.title=element_blank())+
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank()) +   
  scale_fill_brewer(palette = "Set1")  +
  ylab("Source: data.worldbank.org") +
  xlab("") +
  labs( color = " " ) +
  
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text( size=48 )
  ) 

ggplotly(g2)  %>%
  layout(legend = list(orientation = "h",   # show entries horizontally
                       xanchor = "center",  # use center of legend as anchor
                       x = 0,
                       y=0))  %>% 
  layout(title = list(text = "Children out of school (primary) - World - Gender analysis", y = 1)) 











# https://stackoverflow.com/questions/39668369/plotly-in-r-listing-legend-items-horizontally-and-centered-below-a-plot
