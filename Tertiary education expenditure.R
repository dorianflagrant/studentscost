install.packages("tidyverse")
library("tidyverse")

##Public expenditure on education as percentage of total government expenditure

expenditures <- read.csv("Data/EAG_FIN_RATIO_CATEGORY_23012019145211397.csv",
                         sep = ",",header = TRUE,
                         dec = ".")

View(expenditures)
names(expenditures)

##Narrow it down to tertiary education expenditures

tertiaryexp <- filter(expenditures, 
                      Education.level.and.programe.orientation == "Total tertiary education (ISCED2011 levels 5 to 8)")

##Narrow it down to tertiary education expenditures in 2015

tertiaryexp <- filter(expenditures, 
Education.level.and.programe.orientation == "Total tertiary education (ISCED2011 levels 5 to 8)",
Year == 2015, Institution.type == "All sectors")

names(tertiaryexp)

##Keep only the country and the total amount dedicated to tertiary education expenditures

tertiaryexpsimple <- tertiaryexp[c(2,15)]

##Graphs

tertiaryexpsimple %>%
  filter(Value>0) %>%
  ggplot() +
  geom_bar(aes(x = reorder(Country, Value), y = Value, fill = Value), stat = "identity", 
           position = position_stack(reverse = TRUE)) +  
  coord_flip() +
  ggtitle("Tertiary education expenditure in OECD countries") +
  xlab("OECD countries") +
  ylab("% tertiary education expenditure / Total public expenditure") + 
  labs(fill = "%")

