### Enrolment Rate of French Students Comparing with Other OECD Countries ###

## Step 1. Open the table of enrolment rate in OECD countries
  library(readr)
  library(readxl)
  Table_of_enrolment_rate_OECD <- read_csv("D:/R/Datathon/Table of enrolment rate_OECD.csv")
  View(Table_of_enrolment_rate_OECD)

## Step 2. We choose age group 18 among age group 17-19 to make the comparison as it is more common and easy to compare
  library(tidyverse)
  # extract the age group 18 from the dataset and make a new one named "OECD_Comparison"
   Table_of_enrolment_rate_OECD %>% 
   filter(SUBJECT == "AGE_18") -> OECD_Comparison  
   View(OECD_Comparison) 

## Step 3. Make a bar chart of the enrolement rate of OECD countries
  library(hrbrthemes)
   OECD_Comparison %>% 
    group_by(LOCATION) %>% 
    summarise(mean_value = mean(Value)) %>% 
    filter(mean_value > 0) %>%
    # filter(mean_value != NA) %>%
    ggplot(aes(x = LOCATION, y = mean_value)) +
    # changing the color of the bars - typing "blue / etc.." or using "color picker" in google to "fill")
    geom_bar(stat = "identity", width = 0.8, fill = "#3519c1") +
    coord_flip() +
    geom_vline(OECD_Comparison, xintercept = mean(Value))
    # "::"means extracting sth. from a package and comma is how you label the numbers (like 40000 to 40,000)
    scale_y_continuous(labels = scales::comma) +
     +
    # geom_text() = creating new geometry of type 'text' onto plot
    #     don't have to indicate x position as it was indicated in initial ggplot
    #     have to change y position because we want labels 
    #     y indicates the CENTER of the label 
    #    hjust = horizontal justification 
    geom_text(aes(label = countries, y = LOCATION, colour = colour), hjust = "inward", 
              size = 2)  +
      # ipsum theme is used for adjusting fonts
      theme_ipsum(grid = "X")  +
      # "grid = "X" " means that we remove the grid of "X"
      labs (title = "Mean Enrolement Rate of OECD Countries",
            subtille = "The Percentage of French Students Compared with the OECD average", 
            caption="Source:OECD Stat")
