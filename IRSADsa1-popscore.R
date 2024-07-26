library(tidyverse)
library(readr)
library(readxl)

# Read the Excel file
sa1_popscore <- read_excel("SA1_popscore.xlsx")

glimpse(sa1_popscore)

rankingwaus <- sa1_popscore |> 
  select('2021 Statistical Area Level 1 (SA1)', 'Usual Resident Population', 'Score', 'Percentile...7') %>%
  rename(`Percentile within Australia` = `Percentile...7`)


