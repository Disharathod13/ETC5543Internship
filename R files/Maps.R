# Mapping SA1


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






zoom_lon_min <- 144.8  
zoom_lon_max <- 145.15
zoom_lat_min <- -38.0  
zoom_lat_max <- -37.7

# Coordinates for Monash University Clayton Campus
monash_lat <- -37.9139
monash_lon <- 145.1317


ggplot(data = vicdata_map) +
  geom_sf(aes(fill = as.factor(Percentile_Category))) +  
  scale_fill_viridis_d(name = "Percentile Category") +  
  labs(title = "SA1 Regions in Victoria by Percentile Category",
       subtitle = "Zoomed into Central Victoria (SA1 regions)",
       caption = "Source: ABS & Custom Data") +
  coord_sf(xlim = c(zoom_lon_min, zoom_lon_max), 
           ylim = c(zoom_lat_min, zoom_lat_max), 
           expand = FALSE) +  
  geom_point(aes(x = monash_lon, y = monash_lat), color = "red", size = 3) +  
  geom_text(aes(x = monash_lon, y = monash_lat, label = "Monash University Clayton Campus"), 
            hjust = 1.5, vjust = -0.5, color = "black", size = 3) + 
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

# Create a new column based on Percentile.within.State ranges
dataforsa3map <- dataforsa3map %>%
  mutate(Percentile_Category = case_when(
    Percentile.within.State >= 0 & Percentile.within.State <= 25 ~ 1,
    Percentile.within.State >= 26 & Percentile.within.State <= 50 ~ 2,
    Percentile.within.State >= 51 & Percentile.within.State <= 75 ~ 3,
    Percentile.within.State >= 76 & Percentile.within.State <= 100 ~ 4
  ))




dataforsa3map_wide <- dataforsa3map %>%
  pivot_wider(names_from = Year, values_from = Value)

# Check the reshaped data
glimpse(dataforsa3map_wide)


# Select the desired columns
dataforsa3map_new <- dataforsa3map_wide %>%
  select(SA3_NAME_2021, Usual.Resident.Population, Percentile_Category)

# Display the selected data
glimpse(dataforsa3map_new)

# Make sure the population data has SA3_CODE21
vicdata_map_sa3 <- vicdata_map_sa3 %>%
  mutate(SA3_NAME_2021 = SA3_NAME21) 




sa3_combined <- vicdata_map_sa3 %>%
  left_join(dataforsa3map_new, by = "SA3_NAME_2021")


glimpse(sa3_combined)

# Filter the data to remove rows with NA values in the population column
sa3_combined_filtered <- sa3_combined %>%
  filter(!is.na(Usual.Resident.Population))


# Plot the map with a population gradient
ggplot(data = sa3_combined_filtered) +
  geom_sf(aes(fill = Usual.Resident.Population), color = "black") +  
  scale_fill_gradient(low = "lightblue", high = "darkblue", 
                      name = "Population", 
                      na.value = "grey50") + 
  theme_minimal() +  
  labs(title = "SA3 Regions Colored by Population",
       subtitle = "Population distribution across SA3 regions") +
  coord_sf()  


