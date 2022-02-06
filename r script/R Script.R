rm(list=ls())
library(dplyr) # for data cleaning
library(tidyverse) # fir data cleaning
library(bookdown)    # for cross referencing figures and graphs; referencing
library(scales)      # for fixing date axes
library(DescTools)   # for capitalizing graph labels
library(lubridate)   # extract month from date_scored
library(kableExtra)  # for nicer tables
library(opendatatoronto) #read data directly from the data portal
library(ggplot2) # for graphs
library(reshape2) # for data cleaning

Toronto_Crime <- read.csv(here::here("~/inputs/data/raw_data.csv"))
head(Toronto_Crime)

Clean_Toronto_Crime <- Toronto_Crime %>%
  as_tibble() %>%
  select(Neighbourhood, Assault_2014, Assault_2015, Assault_2016, Assault_2017, Assault_2018, Assault_2019, AutoTheft_2014, AutoTheft_2015, AutoTheft_2016, AutoTheft_2017, AutoTheft_2018, AutoTheft_2019, BreakAndEnter_2014, BreakAndEnter_2015, BreakAndEnter_2016, BreakAndEnter_2017, BreakAndEnter_2018, BreakAndEnter_2019, Homicide_2014, Homicide_2015, Homicide_2016, Homicide_2017, Homicide_2018, Homicide_2019, TheftOver_2014, TheftOver_2015, TheftOver_2016, TheftOver_2017, TheftOver_2018, TheftOver_2019, Robbery_2014, Robbery_2015, Robbery_2016, Robbery_2017, Robbery_2018, Robbery_2019)

### top 5 neighbourhoods with the most crimes ###
top5_toronto_crime <- Clean_Toronto_Crime %>%
  melt(id = c("Neighbourhood"), # using `reshape2` to change dataframe to long-format data for plotting. kept Neighbourhood as the ID
       variable.name = "Year",
       value.name = "Number_of_Crimes") %>%
  group_by(Neighbourhood) %>%
  summarise(Number_of_Crimes = sum(Number_of_Crimes)) %>% # summing crime by neighbourhood 
  slice_max(Number_of_Crimes, n = 5) # selecting 5 neighbourhoods with the most crime

### Finding population data of the top 5 neighbourhoods ###
toronto_crime_pop <- Toronto_Crime %>%
  as_tibble() %>%
  select(Neighbourhood, F2020_Population_Projection) %>%
  filter(Neighbourhood %in% c("Waterfront Communities-The Island", "Bay Street Corridor", "Church-Yonge Corridor", "West Humber-Clairville", "Moss Park"))

### Merging population and crime dataframes ###
compare_pop_crime <- merge(x = top5_toronto_crime, y = toronto_crime_pop, by = "Neighbourhood", all = TRUE)

### Graphing neighbourhoods with the highest crime numbers and their population ###
compare_pop_crime %>% # plotting the number of crimes with their population
  ggplot(aes(x = reorder(Neighbourhood, -F2020_Population_Projection), y = Number_of_Crimes, fill = F2020_Population_Projection)) + # ordered by population size
  geom_bar(stat = "identity", position = position_dodge(), color="red") +
  labs(x = "Top 5 Neighbourhood",
       y = "Number of Crimes",
       title = "Top 5 neighbourhoods with the most crimes 2014-2019") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Understanding neighbourhood population ###
toronto_crime_pop_overall <- Toronto_Crime %>%
  as_tibble() %>%
  select(Neighbourhood, F2020_Population_Projection) %>%
  summarize(
    The_min = min(F2020_Population_Projection),
    The_max = max(F2020_Population_Projection),
    Mean = mean(F2020_Population_Projection),
    Std_dev = sd(F2020_Population_Projection)) %>%
  arrange(desc(Mean))

toronto_crime_pop_overall %>%
  knitr::kable(caption = " Summary of Neighbourhood Populations.", 
               col.names = c(" Minimum", "Maximum", "Average", "Standard Deviation"),
               align = c('c', 'c', 'c', 'c'),
               booktabs = T) %>%
  kable_styling(full_width = T)

### Selected 5 Neighbourhood's populations ###
toronto_crime_pop %>%
  arrange(desc(F2020_Population_Projection)) %>%
  knitr::kable(caption = "Summary of Top5 Neighbourhod Populations.", 
               col.names = c("Top 5 Neighbourhood", "Population"),
               align = c('c', 'c'),
               booktabs = T) %>%
  kable_styling(full_width = T) %>%
  column_spec(1, width = "10cm")

per_capita <- compare_pop_crime%>%
  transform(per_capita = (Number_of_Crimes / F2020_Population_Projection) * 100000)

#graph of Per capita  crime rates in top 5 neighbourhood 
per_capita %>%
  ggplot(aes(x = reorder(Neighbourhood, -per_capita), y = per_capita, fill = F2020_Population_Projection,)) +
  geom_bar(stat = "identity", position = position_dodge(), color= "red") +
  labs(x = "Top 5 Neighbourhood",
       y = "Per CapitaCrimes",
       title = " Top 5 Neighbourhood Crime Per Capita at Toronto 2014-2019")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))