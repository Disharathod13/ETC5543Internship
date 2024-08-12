library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

library(tidyverse)

# Read the Excel file
sa_regions <- read_excel("SAregions_AUS.xlsx")

# Read the CSV file
complete_table <- read.csv("complete_table.csv")

complete_table <- complete_table %>%
  mutate(sa1reg = as.numeric(sa1reg))

merged_data <- left_join(complete_table, sa_regions, by = c("sa1reg" = "SA1_CODE_2021"))


glimpse(merged_data)


# Reshape the data
merged_data <- merged_data %>%
  pivot_longer(cols = starts_with("X20"), 
               names_to = "Year", 
               values_to = "Value") %>%
  mutate(Year = as.integer(sub("X", "", Year))) 

ggplot(merged_data, aes(x = Year, y = Value, color = SA4_NAME_2021, group = SA4_NAME_2021)) +
  geom_line() +
  geom_point() +  # Add points for each year
  scale_x_continuous(breaks = seq(2021, 2030, by = 1)) +  # Ensure each year is shown separately
  labs(title = "Yearly Trends by SA4 Region",
       x = "Year",
       y = "Value",
       color = "SA4 Region") +
  theme_minimal() +
  facet_wrap(~ SA4_NAME_2021)  # Facet wrap by SA4_NAME


