library(tidyverse)
library(readr)
library(readxl)

# Read the CSV file
sa1_yr12 <- read_csv('sa1_edu_age 2.csv', skip = 10)


# Fill down the missing values in the first two columns and rename first col
sa1_yr12 <- sa1_yr12 %>%
  rename(sa1reg = 1, Count = 4) %>%
  fill(sa1reg:2, .direction = "down")

# Selecting the Years for 2021
sa1_yr12 <- sa1_yr12 |> 
  mutate(Count = as.numeric(gsub(",","",Count)))

sa1_for2021 <- sa1_yr12 |> 
  filter(`3-digit level HEAP Level of Highest Educational Attainment` == "Year 12" & `AGEP Age` <= 21) |> 
  group_by(sa1reg) |> summarise(Count = sum(Count))

# Selecting the Years for 2022
sa1_yr12 <- sa1_yr12 |> 
  mutate(Count = as.numeric(gsub(",","",Count)))

sa1_for2022 <- sa1_yr12 |> 
  filter(`3-digit level HEAP Level of Highest Educational Attainment` == "Year 11" & `AGEP Age` <= 20) |> 
  group_by(sa1reg) |> summarise(Count = sum(Count))

# Selecting the Years for 2023
sa1_yr12 <- sa1_yr12 |> 
  mutate(Count = as.numeric(gsub(",","",Count)))

sa1_for2023 <- sa1_yr12 |> 
  filter(`3-digit level HEAP Level of Highest Educational Attainment` == "Year 10" & `AGEP Age` <= 19) |> 
  group_by(sa1reg) |> summarise(Count = sum(Count))

# Selecting the Years for 2024
sa1_yr12 <- sa1_yr12 |> 
  mutate(Count = as.numeric(gsub(",","",Count)))

sa1_for2024 <- sa1_yr12 |> 
  filter(`3-digit level HEAP Level of Highest Educational Attainment` == "Year 9" & `AGEP Age` <= 18) |> 
  group_by(sa1reg) |> summarise(Count = sum(Count))

# Selecting the Years for 2025
sa1_yr12 <- sa1_yr12 |> 
  mutate(Count = as.numeric(gsub(",","",Count)))

sa1_for2025 <- sa1_yr12 |> 
  filter(`AGEP Age` == 14, `3-digit level HEAP Level of Highest Educational Attainment` == "Not applicable")|> 
  group_by(sa1reg) |> summarise(Count = sum(Count))

# Selecting the Years for 2026
sa1_yr12 <- sa1_yr12 |> 
  mutate(Count = as.numeric(gsub(",","",Count)))

sa1_for2026 <- sa1_yr12 |> 
  filter(`AGEP Age` == 13, `3-digit level HEAP Level of Highest Educational Attainment` == "Not applicable")|> 
  group_by(sa1reg) |> summarise(Count = sum(Count))

# Selecting the Years for 2027
sa1_yr12 <- sa1_yr12 |> 
  mutate(Count = as.numeric(gsub(",","",Count)))

sa1_for2027 <- sa1_yr12 |> 
  filter(`AGEP Age` == 12, `3-digit level HEAP Level of Highest Educational Attainment` == "Not applicable")|> 
  group_by(sa1reg) |> summarise(Count = sum(Count))

# Selecting the Years for 2028
sa1_yr12 <- sa1_yr12 |> 
  mutate(Count = as.numeric(gsub(",","",Count)))

sa1_for2028 <- sa1_yr12 |> 
  filter(`AGEP Age` == 11, `3-digit level HEAP Level of Highest Educational Attainment` == "Not applicable" )|> 
  group_by(sa1reg) |> summarise(Count = sum(Count))

# Selecting the Years for 2029
sa1_yr12 <- sa1_yr12 |> 
  mutate(Count = as.numeric(gsub(",","",Count)))

sa1_for2029 <- sa1_yr12 |> 
  filter(`AGEP Age` == 10, `3-digit level HEAP Level of Highest Educational Attainment` == "Not applicable" )|> 
  group_by(sa1reg) |> summarise(Count = sum(Count))

# Selecting the Years for 2030
sa1_yr12 <- sa1_yr12 |> 
  mutate(Count = as.numeric(gsub(",","",Count)))

sa1_for2030 <- sa1_yr12 |> 
  filter(`AGEP Age` == 9, `3-digit level HEAP Level of Highest Educational Attainment` == "Not applicable" )|> 
  group_by(sa1reg) |> summarise(Count = sum(Count))


# Merging all the yearly dataframes
complete_table <- sa1_for2021 %>%
  left_join(sa1_for2022, by = "sa1reg") %>%
  left_join(sa1_for2023, by = "sa1reg") %>%
  left_join(sa1_for2024, by = "sa1reg") %>%
  left_join(sa1_for2025, by = "sa1reg") %>%
  left_join(sa1_for2026, by = "sa1reg") %>%
  left_join(sa1_for2027, by = "sa1reg") %>%
  left_join(sa1_for2028, by = "sa1reg") %>%
  left_join(sa1_for2029, by = "sa1reg") %>%
  left_join(sa1_for2030, by = "sa1reg")

# Rename the columns
complete_table <- complete_table %>%
  rename(
    `2021` = Count.x,
    `2022` = Count.y,
    `2023` = Count.x.x,
    `2024` = Count.y.y,
    `2025` = Count.x.x.x,
    `2026` = Count.y.y.y,
    `2027` = Count.x.x.x.x,
    `2028` = Count.y.y.y.y,
    `2029` = Count.x.x.x.x.x,
    `2030` = Count.y.y.y.y.y
  )

# Display the combined dataframe with new column names
print(complete_table)

# Save as a CSV file
write.csv(complete_table, "complete_table.csv")


