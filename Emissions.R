library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
library(dplyr) #helps in data manipulation
library(tidyr)

getwd() #displays your working directory
setwd("/Users/vincent/Documents/Coursera/Data Analytics/Final Project/Emissions") #sets your working directory to simplify calls to data)

## Read CSV file and import data
# Upload Emissions data (csv files) here
world_emissions <- read_csv("global_emissions.csv")
population <- read_csv("population.csv")
colnames(world_emissions)
colnames(population)

#Data Cleanup
str(world_emissions)
head(world_emissions)
summary(world_emissions)

##filtering World CO2 Emission
only_world_emissions <- filter(world_emissions, Entity == "World" & Year >= 2000)
world_population <- filter(population, Entity == "World" & Year >= 2000)

##Calculate the percentage increase in emissions compared to every year
emission_rate = only_world_emissions %>%
  # first sort by year
  arrange(Year) %>%
  mutate(Diff_Year = Year - lag(Year),  # Difference in time (just in case there are gaps)
         Diff_emission = annual_co2_emissions - lag(annual_co2_emissions), # Difference in route between years
         Rate_percent_emission = (Diff_emission / Diff_Year)/annual_co2_emissions * 100)# growth rate in percent


##Calculate the percentage increase in population compared to every year
population_rate = world_population %>%
  # first sort by year
  arrange(Year) %>%
  mutate(Diff_Year = Year - lag(Year),  # Difference in time (just in case there are gaps)
         Diff_Population = Population - lag(Population), # Difference in route between years
         Rate_percent_population = (Diff_Population / Diff_Year)/Population * 100) # growth rate in percent

emissions_population <- full_join(emission_rate, population_rate) %>% 
  arrange(Year)

Avergae_Gobal_emission = mean(emissions_population$annual_co2_emissions)/mean(emissions_population$Population)

write.csv(emissions_population,"/Users/vincent/Documents/Coursera/Data Analytics/Final Project/emission_population", row.names=FALSE)

emissions_population_v2 <- full_join(world_emissions, population) %>% 
  arrange(Entity)

emission_per_capita <- filter (emissions_population_v2, Year >= 2000) %>%
  filter(!is.na(Code) & Code != "OWID_WRL") %>% 
  arrange(desc(annual_co2_emissions))



