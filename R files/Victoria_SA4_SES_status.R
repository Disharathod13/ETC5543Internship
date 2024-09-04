### SES status - Aus vs Vic calculation
percentile_invic_state <- read.csv("/Users/patrickl/Desktop/Intern/Repo/ABS_project/Data/SA1_rankingwstate.csv") |> 
  filter(State == 'VIC') |> 
  distinct(`X2021.Statistical.Area.Level.1..SA1.`, `Percentile.within.State`) |> 
  rename('SA1reg' = 'X2021.Statistical.Area.Level.1..SA1.')

# percentile_invic_state |> group_by(`X2021.Statistical.Area.Level.1..SA1.`) |> 
#   summarise(n = n_distinct(`Percentile.within.State`)) |> 
#   ungroup() |> 
#   filter(n>1)
percentile_invic_aus <- read.csv("/Users/patrickl/Desktop/Intern/Repo/ABS_project/Data/SA1_rankingwaus.csv") |> 
  distinct(`X2021.Statistical.Area.Level.1..SA1.`, `Percentile.within.Australia`)|> 
  rename('SA1reg' = 'X2021.Statistical.Area.Level.1..SA1.')

data_long <- data_long |> 
  rename('SA1reg' = 'SA1')

# percentile_invic_aus |> group_by(`X2021.Statistical.Area.Level.1..SA1.`) |> 
#   summarise(n = n_distinct(`Percentile.within.Australia`)) |> 
#   ungroup() |> 
#   filter(n>1)
percentile_invic_aus |> colnames()

df_temp <- data_long |> left_join(percentile_invic_state,
                                  by = 'SA1reg')|> 
  left_join(percentile_invic_aus,
            by = 'SA1reg') |> 
  pivot_longer(cols = c('Percentile.within.State', 
                        'Percentile.within.Australia'), 
               names_to = 'Method', 
               values_to = 'Percentile') |> 
  mutate(Method = str_replace(Method, 'Percentile.within.', '')) %>%
  mutate(Percentile_Category = case_when(
    Percentile >= 0 & Percentile <= 25 ~ 4,
    Percentile >= 26 & Percentile <= 50 ~ 3,
    Percentile >= 51 & Percentile <= 75 ~ 2,
    Percentile >= 76 & Percentile <= 100 ~ 1
  )) |> 
  mutate(Year = as.numeric(Year))

df_temp |> 
  group_by(Year, Method, Percentile_Category) |>
  summarise(Value = sum(Value)) |>
  ungroup() |> 
  group_by(Year, Method) |>
  mutate(prop = Value/sum(Value)) |>
  ungroup() |> 
  filter(!is.na(Percentile_Category)) |> 
  ggplot(aes(x=Year, 
             # y=Value, 
             y=prop,
             group=Percentile_Category, 
             color=as.factor(Percentile_Category),
             fill=as.factor(Percentile_Category))) +
  # geom_line()+
  geom_bar(stat = "identity")+
  scale_y_continuous(breaks = seq(0,1,0.05))+
  facet_wrap(~Method)

# percentile_invic |> 
#   group_by(Percentile.within.State) |> 
#   summarise(population = sum(Usual.Resident.Population),
#             n=n_distinct(SA1reg)) |> 
#   ungroup() |> 
#   View()

## Focus on the State Method

## Victoria Total
df_temp  %>%
  left_join(SAregions_AUS %>% 
              distinct(SA1reg,
                       SA2_NAME_2021, 
                       SA3_NAME_2021, 
                       SA4_NAME_2021), 
            by = "SA1reg") %>%
  na.omit() |> 
  filter(Method == 'State') |> 
  group_by(Year) |> 
  summarise(Value = sum(Value)) |>
  ungroup() |> 
  ggplot(aes(x=Year, 
             y=Value)) +
  geom_line()
## Victoria Total - By SES

## SA4 / SA3 Total

df_temp_SA4_name <- df_temp  %>%
  left_join(SAregions_AUS %>% 
              distinct(SA1reg,
                       SA2_NAME_2021, 
                       SA3_NAME_2021, 
                       SA4_NAME_2021), 
            by = "SA1reg") %>%
  na.omit() |> 
  filter(Method == 'State') 

## SA4 / SA3 Total - By SES


df_SA4_plot <- df_temp_SA4_name |> 
  group_by(Year, SA4_NAME_2021, Percentile_Category) |> 
  summarise(Value = sum(Value)) |>
  ungroup() |> 
  group_by(Year, SA4_NAME_2021) |> 
  mutate(prop = Value/sum(Value)) |>
  ungroup()

df_SA4_plot |> 
  group_by(Year, SA4_NAME_2021) |>
  summarise(Value = sum(Value)) |>
  ungroup() |> 
  mutate(Year = as.numeric(Year)) |>
  ggplot(aes(x=Year, 
             y=Value)) +
  geom_line()+
  facet_wrap(~SA4_NAME_2021)

df_SA4_plot |> 
  ggplot(aes(x=Year, 
             y=prop,
             group=Percentile_Category, 
             color=as.factor(Percentile_Category),
             fill=as.factor(Percentile_Category))) +
  geom_bar(stat = "identity")+
  scale_y_continuous(breaks = seq(0,1,0.25))+
  facet_wrap(~SA4_NAME_2021)

df_SA4_plot |> 
  filter(Percentile_Category == 4) |> 
  ggplot(aes(x=Year, 
             y=Value,
             group=Percentile_Category, 
             color=as.factor(Percentile_Category),
             fill=as.factor(Percentile_Category))) +
  # geom_bar(stat = "identity")+
  geom_line()+
  # scale_y_continuous(breaks = seq(0,1,0.25))+
  facet_wrap(~SA4_NAME_2021)


SAregion