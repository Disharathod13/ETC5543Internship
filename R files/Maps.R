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
monash_lat_cl <- -37.9139
monash_lon_cl <- 145.1317
monash_lat_caul <- -37.8768
monash_lon_caul <- 145.0458
monash_lat_pen <- - 38.1527
monash_lon_pen <- 145.1360
monash_lat_park <- -37.7838
monash_lon_park <- 144.9586


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

library(tidyverse)

library(sf)

# SA3

SA3_shapefile <- st_read("/Users/disharathod/Desktop/ETC5543Internship/Data/SA3_2021_AUST_SHP_GDA2020/SA3_2021_AUST_GDA2020.shp")


glimpse(SA3_shapefile)

vicdata_map_sa3 <- SA3_shapefile %>% filter(STE_NAME21 == "Victoria")

# Perform a left join on 'SA1reg'
combined_data_new <- combined_data %>%
  left_join(percentile_invic, by = "SA1reg")

vicdata_map_sa3$SA3_NAME21 <- as.character(vicdata_map_sa3$SA3_NAME21)
combined_data_new$SA3_NAME_2021 <- as.character(combined_data_new$SA3_NAME_2021)

combined_data_new <-  combined_data_new |> 
  select(Year, Value, SA3_NAME_2021, Usual.Resident.Population, Score, Percentile.within.State) |> 
  mutate(Percentile_Category = case_when(
    Percentile.within.State >= 0 & Percentile.within.State <= 25 ~ 1,
    Percentile.within.State >= 26 & Percentile.within.State <= 50 ~ 2,
    Percentile.within.State >= 51 & Percentile.within.State <= 75 ~ 3,
    Percentile.within.State >= 76 & Percentile.within.State <= 100 ~ 4
  )) |> 
  pivot_wider(names_from = Year, values_from = Value)



merged_data_forsa3 <- combined_data_new %>%
  group_by(SA3_NAME_2021) |> 
  summarise(Usual.Resident.Population = sum(Usual.Resident.Population,  na.rm = TRUE),
            Percentile_Category = median(Percentile_Category, na.rm = TRUE)) |> 
  ungroup() |> 
  left_join(vicdata_map_sa3, by = c("SA3_NAME_2021" = "SA3_NAME21"))


merged_data_forsa3 <- st_as_sf(merged_data_forsa3)



library(ggplot2)
library(viridis)  

# Plot the map using ggplot2
ggplot(data = merged_data_forsa3) +
  geom_sf(aes(fill = as.factor(Percentile_Category), geometry = geometry)) +  # geometry is now explicitly specified
  scale_fill_viridis_d(name = "Percentile Category") +  
  labs(title = "SA3 Regions in Victoria by Percentile Category",
       subtitle = "Mapping Percentile Categories across Victoria",
       caption = "Source: ABS & Custom Data") +
  theme_minimal() +
  theme(legend.position = "bottom")



zoom_lon_min <- 143  
zoom_lon_max <- 146
zoom_lat_min <- -39  
zoom_lat_max <- -37

ggplot(data = merged_data_forsa3) +
  geom_sf(aes(fill = as.factor(Percentile_Category))) +  
  scale_fill_viridis_d(name = "Percentile Category") +  
  labs(title = "SA3 Regions in Victoria by Percentile Category",
       subtitle = "Zoomed into Central Victoria (SA3 regions)",
       caption = "Source: ABS & Custom Data") +
  coord_sf(xlim = c(zoom_lon_min, zoom_lon_max), 
           ylim = c(zoom_lat_min, zoom_lat_max), 
           expand = FALSE) +  
  theme_minimal() +
  theme(legend.position = "bottom")


## SA3 map for  2021

 sa3_ses21<- vicdata_map |> 
  filter(Percentile_Category == 1) |>      
  group_by(SA3_NAME21) |>        
  summarise(Sum_Year_2021_low_ses = sum(Year_2021, na.rm = TRUE)) |> 
  ungroup()  |> 
   right_join(vicdata_map |> 
                distinct(SA3_NAME21), by = "SA3_NAME21") |>  
   mutate(Sum_Year_2021_low_ses = ifelse(is.na(Sum_Year_2021_low_ses), 0, Sum_Year_2021_low_ses))  |> 
   rename(SA3_NAME_2021 = SA3_NAME21)

 sa3_total_21 <- vicdata_map |>     
   group_by(SA3_NAME21) |>        
   summarise(Sum_Year_2021 = sum(Year_2021, na.rm = TRUE)) |> 
   ungroup()  |> 
   right_join(vicdata_map |> 
                distinct(SA3_NAME21), by = "SA3_NAME21") |>  
   mutate(Sum_Year_2021 = ifelse(is.na(Sum_Year_2021), 0, Sum_Year_2021))  |> 
   rename(SA3_NAME_2021 = SA3_NAME21)
 
sa3_ses21_map <- merged_data_forsa3 |> 
  left_join(sa3_ses21 |> 
              tibble()|>
              select(-geometry) 
            , by = c("SA3_NAME_2021" = "SA3_NAME_2021"))|> 
  left_join(sa3_total_21 |> 
              tibble()|>
              select(-geometry) 
            , by = c("SA3_NAME_2021" = "SA3_NAME_2021"))

### Low ses map 21
 
ggplot(data = sa3_ses21_map) +
  geom_sf(aes(fill = Sum_Year_2021_low_ses, geometry = geometry)) +  
  labs(title = "SA3 Regions in Victoria by Year 12 population in 2021",
       subtitle = "Low SES",
       caption = "Source: ABS & Custom Data") +
  coord_sf(xlim = c(zoom_lon_min, zoom_lon_max), ylim = c(zoom_lat_max, zoom_lat_min)) + 
  geom_point(aes(x = monash_lon_cl, y = monash_lat_cl), color = "red", size = 2) + 
  geom_point(aes(x = monash_lon_caul, y = monash_lat_caul), color = "yellow", size = 2) +
  geom_point(aes(x = monash_lon_pen, y = monash_lat_pen), color = "lightgreen", size = 2) +
  geom_point(aes(x = monash_lon_park, y = monash_lat_park), color = "orange", size = 2) +
  # geom_text(aes(x = monash_lon_cl, y = monash_lat_cl, label = "Monash Clayton"), 
            #hjust = 1.5, vjust = -0.5, color = "black", size = 2) + 
  theme_minimal() +
  theme(legend.position = "bottom")


### Total pop map 21
 
ggplot(data = sa3_ses21_map) +
  geom_sf(aes(fill = Sum_Year_2021, geometry = geometry)) +  # geometry is now explicitly specified
  labs(title = "SA3 Regions in Victoria by Year 12 population in 2021",
       subtitle = "Total Population",
       caption = "Source: ABS & Custom Data") +
  coord_sf(xlim = c(zoom_lon_min, zoom_lon_max), ylim = c(zoom_lat_max, zoom_lat_min)) + 
  geom_point(aes(x = monash_lon_cl, y = monash_lat_cl), color = "red", size = 2) + 
  geom_point(aes(x = monash_lon_caul, y = monash_lat_caul), color = "yellow", size = 2) +
  geom_point(aes(x = monash_lon_pen, y = monash_lat_pen), color = "lightgreen", size = 2) +
  geom_point(aes(x = monash_lon_park, y = monash_lat_park), color = "orange", size = 2) +
  theme_minimal() +
  theme(legend.position = "bottom")


## SA3 map for 2025

sa3_ses25<- vicdata_map |> 
  filter(Percentile_Category == 1) |>      
  group_by(SA3_NAME21) |>        
  summarise(Sum_Year_2025_low_ses = sum(Year_2025, na.rm = TRUE)) |> 
  ungroup()  |> 
  right_join(vicdata_map |> 
               distinct(SA3_NAME21), by = "SA3_NAME21") |>  
  mutate(Sum_Year_2025_low_ses = ifelse(is.na(Sum_Year_2025_low_ses), 0, Sum_Year_2025_low_ses))  |> 
  rename(SA3_NAME_2021 = SA3_NAME21)

sa3_total_25 <- vicdata_map |>     
  group_by(SA3_NAME21) |>        
  summarise(Sum_Year_2025 = sum(Year_2025, na.rm = TRUE)) |> 
  ungroup()  |> 
  right_join(vicdata_map |> 
               distinct(SA3_NAME21), by = "SA3_NAME21") |>  
  mutate(Sum_Year_2025 = ifelse(is.na(Sum_Year_2025), 0, Sum_Year_2025))  |> 
  rename(SA3_NAME_2021 = SA3_NAME21)

sa3_ses25_map <- merged_data_forsa3 |> 
  left_join(sa3_ses25 |> 
              tibble()|>
              select(-geometry) 
            , by = c("SA3_NAME_2021" = "SA3_NAME_2021"))|> 
  left_join(sa3_total_25 |> 
              tibble()|>
              select(-geometry) 
            , by = c("SA3_NAME_2021" = "SA3_NAME_2021"))


### Low ses map 25
ggplot(data = sa3_ses25_map) +
  geom_sf(aes(fill = Sum_Year_2025_low_ses, geometry = geometry)) + 
  labs(title = "SA3 Regions in Victoria by Year 12 population in 2025",
       subtitle = "Low SES",
       caption = "Source: ABS & Custom Data") +
  coord_sf(xlim = c(zoom_lon_min, zoom_lon_max), ylim = c(zoom_lat_max, zoom_lat_min)) + 
  geom_point(aes(x = monash_lon_cl, y = monash_lat_cl), color = "red", size = 2) + 
  geom_point(aes(x = monash_lon_caul, y = monash_lat_caul), color = "yellow", size = 2) +
  geom_point(aes(x = monash_lon_pen, y = monash_lat_pen), color = "lightgreen", size = 2) +
  geom_point(aes(x = monash_lon_park, y = monash_lat_park), color = "orange", size = 2) +
  theme_minimal() +
  theme(legend.position = "bottom")

### Total pop map 25

ggplot(data = sa3_ses25_map) +
  geom_sf(aes(fill = Sum_Year_2025_low_ses, geometry = geometry)) +  
  labs(title = "SA3 Regions in Victoria by Year 12 population in 2025",
       subtitle = "Total Population",
       caption = "Source: ABS & Custom Data") +
  coord_sf(xlim = c(zoom_lon_min, zoom_lon_max), ylim = c(zoom_lat_max, zoom_lat_min)) + 
  geom_point(aes(x = monash_lon_cl, y = monash_lat_cl), color = "red", size = 2) + 
  geom_point(aes(x = monash_lon_caul, y = monash_lat_caul), color = "yellow", size = 2) +
  geom_point(aes(x = monash_lon_pen, y = monash_lat_pen), color = "lightgreen", size = 2) +
  geom_point(aes(x = monash_lon_park, y = monash_lat_park), color = "orange", size = 2) +
  theme_minimal() +
  theme(legend.position = "bottom")



## SA3 map for 2030

sa3_ses30<- vicdata_map |> 
  filter(Percentile_Category == 1) |>      
  group_by(SA3_NAME21) |>        
  summarise(Sum_Year_2030_low_ses = sum(Year_2030, na.rm = TRUE)) |> 
  ungroup()  |> 
  right_join(vicdata_map |> 
               distinct(SA3_NAME21), by = "SA3_NAME21") |>  
  mutate(Sum_Year_2030_low_ses = ifelse(is.na(Sum_Year_2030_low_ses), 0, Sum_Year_2030_low_ses))  |> 
  rename(SA3_NAME_2021 = SA3_NAME21)

sa3_total_30 <- vicdata_map |>     
  group_by(SA3_NAME21) |>        
  summarise(Sum_Year_2030 = sum(Year_2030, na.rm = TRUE)) |> 
  ungroup()  |> 
  right_join(vicdata_map |> 
               distinct(SA3_NAME21), by = "SA3_NAME21") |>  
  mutate(Sum_Year_2030 = ifelse(is.na(Sum_Year_2030), 0, Sum_Year_2030))  |> 
  rename(SA3_NAME_2021 = SA3_NAME21)

sa3_ses30_map <- merged_data_forsa3 |> 
  left_join(sa3_ses30 |> 
              tibble()|>
              select(-geometry) 
            , by = c("SA3_NAME_2021" = "SA3_NAME_2021"))|> 
  left_join(sa3_total_30 |> 
              tibble()|>
              select(-geometry) 
            , by = c("SA3_NAME_2021" = "SA3_NAME_2021"))


### Low ses map 30
ggplot(data = sa3_ses30_map) +
  geom_sf(aes(fill = Sum_Year_2030_low_ses, geometry = geometry)) + 
  labs(title = "SA3 Regions in Victoria by Year 12 population in 2030",
       subtitle = "Low SES",
       caption = "Source: ABS & Custom Data") +
  coord_sf(xlim = c(zoom_lon_min, zoom_lon_max), ylim = c(zoom_lat_max, zoom_lat_min)) + 
  geom_point(aes(x = monash_lon_cl, y = monash_lat_cl), color = "red", size = 2) + 
  geom_point(aes(x = monash_lon_caul, y = monash_lat_caul), color = "yellow", size = 2) +
  geom_point(aes(x = monash_lon_pen, y = monash_lat_pen), color = "lightgreen", size = 2) +
  geom_point(aes(x = monash_lon_park, y = monash_lat_park), color = "orange", size = 2) +
  theme_minimal() +
  theme(legend.position = "bottom")

### Total pop map 30

ggplot(data = sa3_ses30_map) +
  geom_sf(aes(fill = Sum_Year_2030, geometry = geometry)) +  
  labs(title = "SA3 Regions in Victoria by Year 12 population in 2030",
       subtitle = "Total Population",
       caption = "Source: ABS & Custom Data") +
  coord_sf(xlim = c(zoom_lon_min, zoom_lon_max), ylim = c(zoom_lat_max, zoom_lat_min)) + 
  geom_point(aes(x = monash_lon_cl, y = monash_lat_cl), color = "red", size = 2) + 
  geom_point(aes(x = monash_lon_caul, y = monash_lat_caul), color = "yellow", size = 2) +
  geom_point(aes(x = monash_lon_pen, y = monash_lat_pen), color = "lightgreen", size = 2) +
  geom_point(aes(x = monash_lon_park, y = monash_lat_park), color = "orange", size = 2) + 
  theme_minimal() +
  theme(legend.position = "bottom")









 