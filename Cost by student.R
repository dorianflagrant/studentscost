Coststudent <- read.csv("Data/Cost-by-student.csv",
                         sep = ",",header = TRUE,
                         dec = ".")

##Narrow down to tertiary education, all expenditure types, year 2015 [there was no data for 2016]

Coststudenttertiary <- Coststudent %>% filter(Education.level.and.programe.orientation == 
                                                "Total tertiary education (ISCED2011 levels 5 to 8)",
                                              Type.of.expenditure == "All expenditure types",
                                              Year == 2015)

##Differentiating ALL institutions / PUBLIC institutions / PRIVATE institutions

CoststudenttertiaryPUB <- Coststudenttertiary %>% filter(Institution.type == "Public educational institutions")

CoststudenttertiaryALL <- Coststudenttertiary %>% filter(Institution.type == "All public and private educational institutions")

CoststudenttertiaryPRIV <- Coststudenttertiary %>% filter(Institution.type == "All private educational institutions")

##Graphs

CoststudenttertiaryPUB %>%
  filter(Value>0) %>%
  ggplot() +
  geom_bar(aes(x = reorder(Country, Value), y = Value, fill = Value), stat = "identity", 
           position = position_stack(reverse = TRUE)) +  
  coord_flip() +
  theme()

CoststudenttertiaryPRIV %>%
  filter(Value>0) %>%
  ggplot() +
  geom_bar(aes(x = reorder(Country, Value), y = Value, fill = Value), stat = "identity", 
           position = position_stack(reverse = TRUE)) +  
  coord_flip() +
  theme()

CoststudenttertiaryALL %>%
  filter(Value>0) %>%
  ggplot() +
  geom_bar(aes(x = reorder(Country, Value), y = Value, fill = Value), stat = "identity", 
           position = position_stack(reverse = TRUE)) +  
  coord_flip() +
  theme()

