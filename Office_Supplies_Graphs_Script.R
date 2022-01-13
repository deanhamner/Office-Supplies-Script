library(tidyverse)
library(readxl)
library(treemapify)

Sales_Table <- read_csv("C:/Users/deanh/Downloads/office_supplies.csv")
Sales_Table <- Sales_Table %>%
  mutate(Profit=Sales*(1-Discount), Year=as.numeric(format(as.Date(Sales_Table$`Order Date`, format="%Y-%m-%d"), "%Y")))

Sales_Table_Furniture <- Sales_Table %>%
  filter(Category=='Furniture') %>%
  group_by(Year, Region, Category, `Sub-Category`) %>%
  summarise(Total_Quantity=sum(Quantity))

Sales_Table_Furniture_Tots <-Sales_Table %>%
  filter(Category=='Furniture') %>%
  group_by(Region, `Sub-Category`) %>%
  summarise(Total_Quantity=sum(Quantity))

Sales_Table_Furniture_Profs <- Sales_Table %>%
  filter(Category=='Furniture') %>%
  group_by(Year, Region, Category, `Sub-Category`) %>%
  summarise(Total_Profit=sum(Profit))

Sales_Table_Office <- Sales_Table %>%
  filter(Category=='Office Supplies') %>%
  group_by(Year, Region, Category, `Sub-Category`) %>%
  summarise(Total_Quantity=sum(Quantity))

Sales_Table_Office_Tots <- Sales_Table %>%
  filter(Category=='Office Supplies') %>%
  group_by(Region, `Sub-Category`) %>%
  summarise(Total_Quantity=sum(Quantity))

Sales_Table_Office_Profs <- Sales_Table %>%
  filter(Category=='Office Supplies') %>%
  group_by(Year, Region, Category, `Sub-Category`) %>%
  summarise(Total_Profit=sum(Profit))

Sales_Table_Tech <- Sales_Table %>%
  filter(Category=='Technology') %>%
  group_by(Year, Region, Category, `Sub-Category`) %>%
  summarise(Total_Quantity=sum(Quantity))

Sales_Table_Tech_Tots <- Sales_Table %>%
  filter(Category=='Technology') %>%
  group_by(Region, `Sub-Category`) %>%
  summarise(Total_Quantity=sum(Quantity))

Sales_Table_Tech_Profs <- Sales_Table %>%
  filter(Category=='Technology') %>%
  group_by(Year, Region, Category, `Sub-Category`) %>%
  summarise(Total_Profit=sum(Profit))

ggplot(Sales_Table_Furniture_Tots, aes(area=Total_Quantity, fill=Total_Quantity, label = paste(`Sub-Category`, Total_Quantity, sep="\n"), subgroup=Region))+ geom_treemap()+ geom_treemap_subgroup_border(colour = "white", size = 5)+ geom_treemap_subgroup_text(place = "centre", grow = TRUE, alpha = 0.5, colour = "orange", fontface = "italic")+geom_treemap_text(colour = "white", place = "centre", size = 15, grow = TRUE) +  ggtitle("Furniture Sales by Region") 

ggplot(Sales_Table_Office_Tots, aes(area=Total_Quantity, fill=Total_Quantity, label = paste(`Sub-Category`, Total_Quantity, sep="\n"), subgroup=Region))+ geom_treemap()+ geom_treemap_subgroup_border(colour = "white", size = 5)+ geom_treemap_subgroup_text(place = "centre", grow = TRUE, alpha = 0.5, colour = "orange", fontface = "italic")+geom_treemap_text(colour = "white", place = "centre", size = 15, grow = TRUE) + ggtitle("Office Supplies Sales by Region")

ggplot(Sales_Table_Tech_Tots, aes(area=Total_Quantity, fill=Total_Quantity, label = paste(`Sub-Category`, Total_Quantity, sep="\n"), subgroup=Region))+ geom_treemap()+ geom_treemap_subgroup_border(colour = "white", size = 5)+ geom_treemap_subgroup_text(place = "centre", grow = TRUE, alpha = 0.5, colour = "orange", fontface = "italic")+geom_treemap_text(colour = "white", place = "centre", size = 15, grow = TRUE) + ggtitle("Technology Sales by Region")

ggplot(Sales_Table_Furniture, aes(x=Year, y=Total_Quantity, color=`Sub-Category`))+geom_line()+facet_wrap(vars(Region)) + ggtitle("Furniture Sales Over Time") + ylab("Units Sold")

ggplot(Sales_Table_Office, aes(x=Year, y=Total_Quantity, color=`Sub-Category`))+geom_line()+facet_wrap(vars(Region)) + ggtitle("Office Supplies Sales Over Time") + ylab("Units Sold")

ggplot(Sales_Table_Tech, aes(x=Year, y=Total_Quantity, color=`Sub-Category`))+geom_line()+facet_wrap(vars(Region)) + ggtitle("Technology Sales Over Time")+ ylab("Units Sold")

ggplot(Sales_Table_Furniture_Profs, aes(x=Year, y=Total_Profit, color=`Sub-Category`))+geom_line()+facet_wrap(vars(Region)) + ggtitle("Furniture Profits Over Time") + ylab("Total Profit")

ggplot(Sales_Table_Office_Profs, aes(x=Year, y=Total_Profit, color=`Sub-Category`))+geom_line()+facet_wrap(vars(Region)) + ggtitle("Office Supplies Profits Over Time") + ylab("Total Profit")

ggplot(Sales_Table_Tech_Profs, aes(x=Year, y=Total_Profit, color=`Sub-Category`))+geom_line()+facet_wrap(vars(Region)) + ggtitle("Technology Profits Over Time") + ylab("Total Profit")