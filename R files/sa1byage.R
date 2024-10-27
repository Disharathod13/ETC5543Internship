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
    `2021` = case_when(Age %in% c(17, 18, 19) ~ Count, TRUE ~ 0),
    `2022` = case_when(Age %in% c(16, 17, 18) ~ Count, TRUE ~ 0),
    `2023` = case_when(Age %in% c(15, 16, 17) ~ Count, TRUE ~ 0),
    `2024` = case_when(Age %in% c(14, 15, 16) ~ Count, TRUE ~ 0),
    `2025` = case_when(Age %in% c(13, 14, 15) ~ Count, TRUE ~ 0),
    `2026` = case_when(Age %in% c(12, 13, 14) ~ Count, TRUE ~ 0),
    `2027` = case_when(Age %in% c(11, 12, 13) ~ Count, TRUE ~ 0),
    `2028` = case_when(Age %in% c(10, 11, 12) ~ Count, TRUE ~ 0),
    `2029` = case_when(Age %in% c(9, 10, 11) ~ Count, TRUE ~ 0),
    `2030` = case_when(Age %in% c(8, 9, 10) ~ Count, TRUE ~ 0
    )
  ) %>%
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





# The column name in SAregions_AUS is different, rename it 
colnames(SAregions_AUS)[colnames(SAregions_AUS) == "SA1_CODE_2021"] <- "SA1reg"


combined_data <- left_join(data_long, SAregions_AUS, by = "SA1reg")

glimpse(combined_data)

# Summarize the data by Year
thetable <- combined_data %>%
  group_by(Year) %>%
  summarize(Total_Value = sum(Value, na.rm = TRUE)) %>%
  arrange(Year)  |> 
  ungroup()

thetable

# Create a table
kable(thetable, caption = "Total Values Summarized by Year") %>%
  kable_styling("striped", full_width = F)



ggplot(thetable, aes(x = Year, y = Total_Value, group = 1)) +  
  geom_line(color = "blue", size = 1) + 
  geom_point(color = "red", size = 2) +  
  labs(title = "The Trend for Year 12 Population",
       x = "Year",
       y = "Total Population") +
  theme_minimal()



glimpse(combined_data)




# Summarize the data by SA4_NAME_2021 and Year
summarized_data <- combined_data %>%
  group_by(SA4_NAME_2021, Year) %>%
  summarize(Total_Value = sum(Value, na.rm = TRUE)) %>%
  ungroup()  

# Convert Year to numeric in the data frame
summarized_data <- summarized_data %>%
  mutate(Year = as.numeric(Year))

# Check the structure of the summarized data
str(summarized_data)

# Create the plot with facet_wrap, line graph, and points
ggplot(summarized_data, aes(x = Year, y = Total_Value)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2021, 2030, by = 1)) +  
  scale_y_continuous(labels = scales::comma) + 
  labs(title = "Total Value by Year for Each SA4 Region",
       x = "Year",
       y = "Total Value",
       color = "SA4 Region") +
  theme_minimal() +
  facet_wrap(~ SA4_NAME_2021, scales = "free_y")

percentile_invic <- read.csv("Data/SA1_rankingwstate.csv")

glimpse(percentile_invic)



percentile_invic <- percentile_invic %>%
  rename(SA1reg = X2021.Statistical.Area.Level.1..SA1.) %>%
  filter(State == "VIC")

glimpse(percentile_invic)




# Perform the left join
 vicdata_new <- data_long %>%
left_join(percentile_invic, by = "SA1reg") 
 
 # Remove rows with any NA values
 vicdata_new <- na.omit(vicdata_new)
 
 library(dplyr)
 
#Join the SA4_NAME_2021 column from SAregions_AUS to vicdata_new
 vicdata_new <- vicdata_new %>%
   left_join(SAregions_AUS %>% select(SA1reg, SA4_NAME_2021), by = "SA1reg") %>%
   na.omit()
 
 # Create a new column based on Percentile.within.State ranges
 vicdata_new <- vicdata_new %>%
   mutate(Percentile_Category = case_when(
     Percentile.within.State >= 0 & Percentile.within.State <= 25 ~ 1,
     Percentile.within.State >= 26 & Percentile.within.State <= 50 ~ 2,
     Percentile.within.State >= 51 & Percentile.within.State <= 75 ~ 3,
     Percentile.within.State >= 76 & Percentile.within.State <= 100 ~ 4
   ))
 
#Check the updated dataset
 glimpse(vicdata_new)
 
 
 
#Convert Year to numeric if it's not already
 vicdata_new$Year <- as.numeric(vicdata_new$Year)
 
#Summarize the data to get the sum of Values by SA4, Year, and Percentile_Category
 vicdata_sum <- vicdata_new %>%
   group_by(SA4_NAME_2021, Year, Percentile_Category) %>%
   summarise(Sum_Value = sum(Value)) %>%
   ungroup()
 
#Create the faceted line graph with adjusted x-axis breaks
 ggplot(vicdata_sum, aes(x = Year, y = Sum_Value, color = factor(Percentile_Category))) +
   geom_line() +
   facet_wrap(~ SA4_NAME_2021, scales = "free_y") +
   scale_x_continuous(breaks = seq(2021, 2030, 1)) +
   labs(title = "Population by Year for SA4 Region in VIC",
        x = "Year",
        y = "Population",
        color = "Percentile Category") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 
