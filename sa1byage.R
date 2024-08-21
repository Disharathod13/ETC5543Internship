library(tidyverse)
library(tibble)
library(ggplot2)

# Read the CSV file without headers and define column names manually
sa1_age_data <- read.csv("Data/SA1_age.csv", skip = 10, header = FALSE, check.names = FALSE, row.names = NULL)



# Define the column names
colnames(sa1_age_data) <- c("SA1reg", "Age", "AGEP Age", "OtherColumn") 

sa1_age_data <- sa1_age_data %>%
  slice(-1)  

# Remove the last column using indexing
sa1_age_data <- sa1_age_data[, -ncol(sa1_age_data)]

# Fill down the first row values in the SA1reg column
sa1_age_data$SA1reg[sa1_age_data$SA1reg == ""] <- NA
sa1_age_data <- sa1_age_data %>%
  fill(SA1reg, .direction = "down")


glimpse(sa1_age_data)


