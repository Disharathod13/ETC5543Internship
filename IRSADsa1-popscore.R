library(tidyverse)
library(readr)
library(readxl)

# Read the Excel file
sa1_popscore <- read_excel("SA1_popscore.xlsx")

glimpse(sa1_popscore)

rankingwaus <- sa1_popscore |> 
  select('2021 Statistical Area Level 1 (SA1)', 'Usual Resident Population', 'Score', 'Percentile...7') %>%
  rename(`Percentile within Australia` = `Percentile...7`)


# Save the data to a CSV file
write_csv(rankingwaus, "SA1_rankingwaus.csv")

rankingwstate <- sa1_popscore |> 
  select('2021 Statistical Area Level 1 (SA1)', 'State', 'Usual Resident Population', 'Score', 'Percentile...12') %>%
  rename(`Percentile within State` = `Percentile...12`)

# Save the data to a CSV file
write_csv(rankingwstate, "SA1_rankingwstate.csv")