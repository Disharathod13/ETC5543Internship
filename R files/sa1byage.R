library(tidyverse)
library(tibble)
library(ggplot2)
library(zoo)
library(readxl)
library(knitr)
library(kableExtra)

# Read the CSV file without headers and define column names manually
sa1_age_data <- read.csv("Data/SA1_age.csv", skip = 10, header = FALSE, check.names = FALSE, row.names = NULL)



# Define the column names
colnames(sa1_age_data) <- c("SA1reg", "Age", "Count", "OtherColumn") 

sa1_age_data <- sa1_age_data %>%
  slice(-1)  

# Remove the last column using indexing
sa1_age_data <- sa1_age_data[, -ncol(sa1_age_data)]

# Fill down the first row values in the SA1reg column
sa1_age_data$SA1reg[sa1_age_data$SA1reg == ""] <- NA
sa1_age_data <- sa1_age_data %>%
  fill(SA1reg, .direction = "down")

sa1_age_data <- sa1_age_data |> 
  filter(SA1reg != 'Total')

total_population <- sum(sa1_age_data$Count, na.rm = TRUE)
total_population



glimpse(sa1_age_data)


# First, ensure the 'Age' column is numeric
sa1_age_data$Age <- as.numeric(sa1_age_data$Age)

# Create new columns for the years 2021 to 2030
sa1_age_data_new <- sa1_age_data %>%
  mutate(
    `2021` = case_when(
      Age %in% c(17, 18, 19) ~ Count,
      TRUE ~ 0
    ),
    `2022` = case_when(
      Age %in% c(16, 17, 18) ~ Count,
      TRUE ~ 0
    ),
    `2023` = case_when(
      Age %in% c(15, 16, 17) ~ Count,
      TRUE ~ 0
    ),
    `2024` = case_when(
      Age %in% c(14, 15, 16) ~ Count,
      TRUE ~ 0
    ),
    `2025` = case_when(
      Age %in% c(13, 14, 15) ~ Count,
      TRUE ~ 0
    ),
    `2026` = case_when(
      Age %in% c(12, 13, 14) ~ Count,
      TRUE ~ 0
    ),
    `2027` = case_when(
      Age %in% c(11, 12, 13) ~ Count,
      TRUE ~ 0
    ),
    `2028` = case_when(
      Age %in% c(10, 11, 12) ~ Count,
      TRUE ~ 0
    ),
    `2029` = case_when(
      Age %in% c(9, 10, 11) ~ Count,
      TRUE ~ 0
    ),
    `2030` = case_when(
      Age %in% c(8, 9, 10) ~ Count,
      TRUE ~ 0
    )
  ) %>%
  # Calculate rolling averages for each year
  # mutate(
  #   `2021` = rollmean(`2021`, k = 1, fill = NA, align = "right"),
  #   `2022` = rollmean(`2022`, k = 1, fill = NA, align = "right"),
  #   `2023` = rollmean(`2023`, k = 1, fill = NA, align = "right"),
  #   `2024` = rollmean(`2024`, k = 1, fill = NA, align = "right"),
  #   `2025` = rollmean(`2025`, k = 1, fill = NA, align = "right"),
  #   `2026` = rollmean(`2026`, k = 1, fill = NA, align = "right"),
  #   `2027` = rollmean(`2027`, k = 1, fill = NA, align = "right"),
  #   `2028` = rollmean(`2028`, k = 1, fill = NA, align = "right"),
  #   `2029` = rollmean(`2029`, k = 1, fill = NA, align = "right"),
  #   `2030` = rollmean(`2030`, k = 1, fill = NA, align = "right")
  # ) %>%
  # # Group by SA1reg to calculate the totals for each region
  group_by(SA1reg) %>%
  summarise(
    `2021` = sum(`2021`) / 3,
    `2022` = sum(`2022`)/ 3,
    `2023` = sum(`2023`)/ 3,
    `2024` = sum(`2024`)/ 3,
    `2025` = sum(`2025`)/ 3,
    `2026` = sum(`2026`)/ 3,
    `2027` = sum(`2027`)/ 3,
    `2028` = sum(`2028`)/ 3,
    `2029` = sum(`2029`)/ 3,
    `2030` = sum(`2030`)/ 3,
    .groups = 'drop'
  )


sa1_age_data_new <- sa1_age_data_new |> 
  mutate(SA1reg = as.numeric(SA1reg)) |> 
  filter(!is.na(SA1reg))



# Check the updated structure
glimpse(sa1_age_data_new)

#Pivot longer to the year and the values 
data_long <-  sa1_age_data_new %>%
  pivot_longer(cols = `2021`:`2030`, names_to = "Year", values_to = "Value")

#Read the excel file
SAregions_AUS <- read_excel("Data/SAregions_AUS.xlsx")
SAregions_AUS <- SAregions_AUS |> filter(STATE_NAME_2021 == "Victoria")





# If the column name in SAregions_AUS is different, rename it to match
colnames(SAregions_AUS)[colnames(SAregions_AUS) == "SA1_CODE_2021"] <- "SA1reg"


combined_data <- left_join(data_long, SAregions_AUS, by = "SA1reg")

glimpse(combined_data)

# Summarize the data by Year
thetable <- combined_data %>%
  group_by(Year) %>%
  summarize(Total_Value = sum(Value, na.rm = TRUE)) %>%
  arrange(Year)  

thetable

# Create a table
kable(thetable, caption = "Total Values Summarized by Year") %>%
  kable_styling("striped", full_width = F)

library(ggplot2)

# Create a line graph
ggplot(thetable, aes(x = Year, y = Total_Value)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(color = "red", size = 2) +  
  labs(title = "Total Values Summarized by Year",
       x = "Year",
       y = "Total Value") +
  theme_minimal()  


glimpse(combined_data)


library(ggplot2)

library(ggplot2)

# Create the plot with facet_wrap, line graph, and points
ggplot(combined_data, aes(x = as.numeric(Year), y = Value, color = SA4_NAME_2021, group = SA4_NAME_2021)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2021, 2030, by = 1)) +  
  scale_y_continuous(labels = scales::comma) + 
  labs(title = "Value by Year for Each SA4 Region",
       x = "Year",
       y = "Value",
       color = "SA4 Region") +
  theme_minimal() +
  facet_wrap(~ SA4_NAME_2021, scales = "free_y")

