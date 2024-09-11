# Mapping SA1

install.packages(c("sf", "ggplot2", "dplyr"))

# Load the libraries
library(sf)
library(ggplot2)
library(dplyr)


# Read the shapefile 
vic_sa1_shapefile <- st_read("Data/SA1_2021_AUST_GDA2020.shp")


vicdata_map <- vic_sa1_shapefile %>% filter(STE_NAME21 == "Victoria")




vicdata_wide <- vicdata_new %>%
  pivot_wider(names_from = Year, values_from = Value, names_prefix = "Year_")

# Convert SA1reg from double to character
vicdata_wide <- vicdata_wide %>%
  mutate(SA1reg = as.character(SA1reg))


vicdata_map <- vicdata_map %>%
left_join(vicdata_wide, by = c("SA1_CODE21" = "SA1reg"))


# Plot the map using ggplot2
ggplot(data = vicdata_map) +
  geom_sf(aes(fill = as.factor(Percentile_Category))) +  
  scale_fill_viridis_d(name = "Percentile Category") +  
  labs(title = "SA1 Regions in Victoria by Percentile Category",
       subtitle = "Mapping Percentile Categories across Victoria",
       caption = "Source: ABS & Custom Data") +
  theme_minimal() +
  theme(legend.position = "bottom")

library(ggplot2)





# SA3

SA3_shapefile <- st_read("/Users/disharathod/Desktop/ETC5543Internship/Data/SA3_2021_AUST_SHP_GDA2020/SA3_2021_AUST_GDA2020.shp")

vicdata_map_sa3 <- SA3_shapefile %>% filter(STE_NAME21 == "Victoria")

# Perform a left join on the datasets by matching SA3_NAME_2021 in combined_data with SA3_NAME21 in vicdata_map_sa3
merged_data_forsa3 <- combined_data %>%
  left_join(vicdata_map_sa3, by = c("SA3_NAME_2021" = "SA3_NAME21"))


# Perform a left join on the datasets by matching SA1reg
dataforsa3map <- merged_data_forsa3 %>%
  left_join(percentile_invic, by = "SA1reg")
