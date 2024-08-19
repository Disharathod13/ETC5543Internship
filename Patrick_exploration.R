library(zoo)

sa1_yr12 |>
  filter(`3-digit level HEAP Level of Highest Educational Attainment` == "Year 12" & `AGEP Age` <= 21) |>
  group_by(`AGEP Age`) |> 
  summarise(Count = sum(Count, na.rm=TRUE)) |> 
  ungroup() |> 
  filter(Count != 0) 


sa1_yr12 |> 
  filter(`3-digit level HEAP Level of Highest Educational Attainment` == "Year 11" & `AGEP Age` <= 20) |>
  group_by(`AGEP Age`) |> 
  summarise(Count = sum(Count, na.rm=TRUE)) |> 
  ungroup() |> 
  filter(Count != 0)



sa1_yr12 |> 
  filter(`3-digit level HEAP Level of Highest Educational Attainment` == "Year 10" & `AGEP Age` <= 19) |> 
  group_by(`AGEP Age`) |> 
  summarise(Count = sum(Count, na.rm=TRUE)) |> 
  ungroup() |> 
  filter(Count != 0)

sa1_yr12 |> 
  filter(`AGEP Age` <= 15) |> 
  group_by(`3-digit level HEAP Level of Highest Educational Attainment`) |> 
    summarise(Count = sum(Count, na.rm=TRUE)) |> 
    ungroup() |> 
  arrange(-Count) |> 
  filter(Count != 0)
  

sa1_yr12 |> 
  filter(sa1reg != 'Total')|> 
  # group_by(sa1reg, `AGEP Age`) |> 
  group_by(`AGEP Age`) |> 
  # filter(between(`AGEP Age`,10,20)) |> 
  summarise(Count = sum(Count, na.rm=TRUE)) |>
  ungroup() |> 
  ggplot(aes(x=`AGEP Age`, y=Count)) +
  geom_line()+
  scale_y_continuous(limits = c(0,110000))

sa1_yr12 |> 
  filter(sa1reg == 'Total')

sa1_yr12 |> 
  filter(`AGEP Age` == 'Total')

df_test <- read_csv('/Users/patrickl/Downloads/vic_age_edu_population.csv',
                    skip = 10)

df_test_2 <- read_csv('/Users/patrickl/Downloads/SA1_age.csv',
                      skip = 10)|>
  fill(c(`SA1 (UR)`,
         `AGEP Age`), .direction = "down") |>
  rename('Count' = '...3') |> 
  mutate(Count = as.numeric(gsub(",","",Count))) |> 
  filter(`SA1 (UR)` != 'Total')


df_test_3 <- read_csv('/Users/patrickl/Downloads/Age_0_40_SA1_Edu.csv', skip = 10)

df_test_3 <- df_test_3|>
  fill(c(`SA1 (UR)`,
         `AGEP Age`), .direction = "down") |>
  rename('Count' = '...4') |> 
  mutate(Count = as.numeric(gsub(",","",Count))) |> 
  filter(`AGEP Age` != 'Total') |> 
  select(`SA1 (UR)`, `AGEP Age`,`3-digit level HEAP Level of Highest Educational Attainment`, Count) 

df_test_4 <- read_csv('/Users/patrickl/Downloads/vic_age_edu_population 2.csv', skip = 10)|>
  fill(c(`STATE (UR)`,
         `AGEP Age`), .direction = "down") |>
  rename('Count' = '...4') |> 
  mutate(Count = as.numeric(gsub(",","",Count))) |> 
  filter(`STATE (UR)` != 'Total')

df_test |>
  fill(c(`STATE (UR)`,
         `AGEP Age`), .direction = "down") |>
  rename('Count' = '...4') |> 
  mutate(Count = as.numeric(gsub(",","",Count))) |> 
  filter(`STATE (UR)` != 'Total') |>
  group_by(`AGEP Age`) |> 
  summarise(Count = sum(Count, na.rm=TRUE)) |>
  ungroup() |> 
  filter(between(`AGEP Age`, 0 , 50)) |> 
  ggplot(aes(x=`AGEP Age`, y=Count)) +
  geom_line()+
  scale_y_continuous(limits = c(0,110000))


df_test_1 <- df_test |>
  fill(c(`STATE (UR)`,
         `AGEP Age`), .direction = "down") |>
  rename('Count' = '...4') |> 
  mutate(Count = as.numeric(gsub(",","",Count))) |> 
  filter(`STATE (UR)` != 'Total')


sa1_yr12 |> 
  filter(sa1reg != 'Total') |> 
  summarise(Count = sum(Count, na.rm=TRUE)) 

sa1_yr12 |> distinct(`AGEP Age`) |> 
  arrange( - `AGEP Age`)

sa1_yr12 |> 
  group_by(`3-digit level HEAP Level of Highest Educational Attainment`) |> 
   summarise(Count_sa1 = sum(Count, na.rm=TRUE)) |> 
   left_join(df_test_1|> 
               filter(`AGEP Age` <= 50) |> 
               group_by(`3-digit level HEAP Level of Highest Educational Attainment`) |> 
               summarise(Count = sum(Count, na.rm=TRUE)), 
             by = '3-digit level HEAP Level of Highest Educational Attainment') |> 
  View()
  

df_test_1 |> 
  # group_by(`AGEP Age`) |>
  summarise(Count_sa1 = sum(Count, na.rm=TRUE))

 df_test_1 |> 
  group_by(`AGEP Age`) |>
  summarise(Count_sa1 = sum(Count, na.rm=TRUE))|> 
  mutate(type = 'Age - Edu') |> 
  rbind(df_test_2  |>
          group_by(`AGEP Age`) |>
          summarise(Count_sa1 = sum(Count, na.rm=TRUE)) |>
          mutate(type = 'SA1 - Age')) |>
  rbind(sa1_yr12 |>
          filter(sa1reg != 'Total') |>
          group_by(`AGEP Age`) |>
          summarise(Count_sa1 = sum(Count, na.rm=TRUE))|>
          mutate(type = 'SA1 - Age - Edu'))|>
  # rbind(df_test_3  |>
  #         group_by(`AGEP Age`) |>
  #         mutate(`AGEP Age` = as.numeric(`AGEP Age`)) |>
  #         summarise(Count_sa1 = sum(Count, na.rm=TRUE)) |>
  #         mutate(type = 'SA1 - Edu - New')) |>
  # rbind(df_test_4  |> 
  #         group_by(`AGEP Age`) |>
  #         mutate(`AGEP Age` = as.numeric(`AGEP Age`)) |> 
  #         summarise(Count_sa1 = sum(Count, na.rm=TRUE)) |> 
  #         mutate(type = 'Age - Edu'))  |> 
  ggplot(aes(x = `AGEP Age`, y = Count_sa1, color = type)) +
  geom_line()


 df_test_4 |> 
  filter(`AGEP Age` == 16) |> 
  group_by(`3-digit level HEAP Level of Highest Educational Attainment`) |> 
  summarise(Count_sa1 = sum(Count, na.rm=TRUE)) |> 
  filter(Count_sa1 != 0) |> 
   arrange(-Count_sa1) 
 
 df_test_3 |> 
   filter(`AGEP Age` == 16) |> 
   group_by(`3-digit level HEAP Level of Highest Educational Attainment`) |> 
   summarise(Count_sa1 = sum(Count, na.rm=TRUE)) |> 
   filter(Count_sa1 != 0) |> 
   arrange(-Count_sa1) 

  
 df_test_3 |> 
   filter(`AGEP Age` == 16) |> 
   group_by(`SA1 (UR)`) |> 
   summarise(Count_sa1 = sum(Count, na.rm=TRUE)) |> 
   arrange(-Count_sa1) 
 
 df_test_2  |>
   group_by(`AGEP Age`) |>
   summarise(Count_sa1 = sum(Count, na.rm=TRUE)) |> 
   ungroup() |> 
   mutate(roll_count = rollmean(Count_sa1, 3, fill = NA, align = 'center')) |> 
   # filter(`AGEP Age` < 40) |>
   mutate(Year = 2021 + (20 - `AGEP Age`)) |> 
   filter(!is.na(roll_count)) |>
   # filter(between(Year, 2013, 2040)) |>
   ggplot(aes(x=Year, y=roll_count))+
   geom_line()+
   geom_vline(xintercept = 2023, linetype = 'dashed')+
   scale_y_continuous(limits = c(5 * 10000, 9 * 10000))+
   scale_x_continuous(limits = c(2016, 2040), breaks = seq(2016, 2040, 2))

              