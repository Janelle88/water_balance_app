past_day <- read_csv(here("water_balance_app", "past_day.csv")) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date,
                                  label = TRUE,
                                  abbr = TRUE),
         doy = lubridate::yday(date))





future_day <- read_csv(here("water_balance_app","future_day.csv"))%>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date,
                                  label = TRUE,
                                  abbr = TRUE),
         doy = lubridate::yday(date))




past_month <- read_csv(here("water_balance_app", "past_month.csv"))%>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date,
                                  label = TRUE,
                                  abbr = TRUE),
         doy = lubridate::yday(date))



future_month <- read_csv(here("water_balance_app", "future_month.csv"))%>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date,
                                  label = TRUE,
                                  abbr = TRUE),
         doy = lubridate::yday(date))


# -------------------------
# -------------------------
# data wrangling for annual_averages time series graph
# -------------------------
# -------------------------


annual_past <- past_day %>% 
  group_by(year, park) %>% 
  filter(!year == 2020) %>% 
  summarize(annual_soil_water = mean(soil_water_daily, na.rm = TRUE),
            annual_runoff = sum(runoff_daily, na.rm = TRUE), 
            annual_rain = sum(rain_daily, na.rm = TRUE),
            annual_accumswe = max(accumswe_daily, na.rm = TRUE),
            annual_pet = sum(pet_daily, na.rm = TRUE),
            annual_deficit = sum(deficit_daily, na.rm = TRUE), 
            #annual_agdd = max(agdd_daily, na.rm = TRUE), 
            #put this back in when agdd is working
            annual_aet = sum(aet_daily, na.rm = TRUE))


# -------------
# long format future data daily
# -------------

# -------------
# worst case
# -------------

wc_annual_future <- future_day %>%
  select(date, soil_water_daily_wc:month) %>% # selecting for variables for 8.5 only
  filter(!year == 2100) %>% 
  group_by(year, park) %>% 
  summarize(annual_soil_water_wc = mean(soil_water_daily_wc, na.rm = TRUE),
            annual_runoff_wc = sum(runoff_daily_wc, na.rm = TRUE),
            annual_rain_wc = sum(rain_daily_wc, na.rm = TRUE),
            annual_accumswe_wc = max(accumswe_daily_wc, na.rm = TRUE),
            annual_pet_wc = sum(pet_daily_wc, na.rm = TRUE),
            annual_deficit_wc = sum(deficit_daily_wc, na.rm = TRUE),
            #annual_agdd_wc = max(agdd_daily_wc, na.rm = TRUE), 
            #put this back in when agdd is working
            annual_aet_wc = sum(aet_daily_wc, na.rm = TRUE))

#-------------
# best case
# ------------

bc_annual_future <- future_day %>% 
  select(date, soil_water_daily_bc:month) %>% #selecting for 4.5 only
  filter(!year == 2100) %>% 
  group_by(year, park) %>% 
  summarize(annual_soil_water_bc = mean(soil_water_daily_bc, na.rm = TRUE),
            annual_runoff_bc = sum(runoff_daily_bc, na.rm = TRUE),
            annual_rain_bc = sum(rain_daily_bc, na.rm = TRUE),
            annual_accumswe_bc = max(accumswe_daily_bc, na.rm = TRUE),
            annual_deficit_bc = sum(deficit_daily_bc, na.rm = TRUE),
            annual_pet_bc = sum(pet_daily_bc, na.rm = TRUE),
            #annual_agdd_bc = max(agdd_daily_bc, na.rm = TRUE),
            #put this back in when agdd is working
            annual_aet_bc = sum(aet_daily_bc, na.rm = TRUE))

# -----------
# make values into one df
# -----------

# -----------
# best case
# ----------

bc_annual_values <- annual_past %>%
  full_join(bc_annual_future) %>% 
  pivot_longer(`annual_soil_water`:`annual_aet_bc`, # The columns I'm gathering together
               names_to = "variable", # new column name for existing names
               values_to = "annual_value") %>% 
  ungroup(year, park) %>% 
  na.omit(annual_values) %>% # new column name to store values
  mutate(variable = case_when(
    .$variable %in% c("annual_runoff", "annual_runoff_bc") ~ "Runoff",
    #.Svariable calls the variable, then it can be renamed
    #.$variable %in% c("annual_agdd", "annual_agdd_bc") ~ "AGDD",
    .$variable %in% c("annual_soil_water", "annual_soil_water_bc") ~ "Soil Water",
    .$variable %in% c("annual_rain", "annual_rain_bc") ~ "Rain",
    .$variable %in% c("annual_accumswe", "annual_accumswe_bc") ~ "Accumulated SWE",
    .$variable %in% c("annual_pet", "annual_pet_bc") ~ "PET",
    .$variable %in% c("annual_deficit", "annual_deficit_bc") ~ "Deficit",
    TRUE ~ "AET" # last ifelse is just labeled as TRUE
  )) %>% 
  mutate(averages = case_when(
    .$year %in% c(1980:2019) ~ "annual_avg",
    TRUE ~ "annual_avg_bc"
  )) %>% 
  mutate(decades = case_when( #don't need individual decades for this data
    .$year %in% c(1980:2019) ~ "1980-2019",
    .$year %in% c(2020:2059) ~ "2020-2059",
    TRUE ~ "2060-2099")) #decades by 39 yr chunks

# ------------
# worst case
# ------------

wc_annual_values <- annual_past %>% 
  full_join(wc_annual_future) %>%
  pivot_longer(`annual_soil_water`:`annual_aet_wc`, # The columns I'm gathering together
               names_to = "variable", # new column name for existing names
               values_to = "annual_value") %>% 
  na.omit(annual_values) %>% # new column name to store values
  ungroup(year, park) %>% 
  mutate(variable = case_when( #make names more readable
    .$variable %in% c("annual_runoff", "annual_runoff_wc") ~ "Runoff",
    #.$variable calls the variable, then it can be renamed
    .$variable %in% c("annual_agdd", "annual_agdd_wc") ~ "AGDD",
    .$variable %in% c("annual_soil_water", "annual_soil_water_wc") ~ "Soil Water",
    .$variable %in% c("annual_rain", "annual_rain_wc") ~ "Rain",
    .$variable %in% c("annual_accumswe", "annual_accumswe_wc") ~ "Accumulated SWE",
    .$variable %in% c("annual_pet", "annual_pet_wc") ~ "PET",
    .$variable %in% c("annual_deficit", "annual_deficit_wc") ~ "Deficit",
    TRUE ~ "AET" # last ifelse is just labeled as TRUE
  ))  %>% 
  mutate(averages = case_when(
    .$year %in% c(1980:2019) ~ "annual_avg",
    TRUE ~ "annual_avg_wc"
  )) %>% 
  mutate(decades = case_when( #don't need individual decades for this data
    .$year %in% c(1980:2019) ~ "1980-2019",
    .$year %in% c(2020:2059) ~ "2020-2059",
    TRUE ~ "2060-2099")) #decades by 39 yr chunks

# ---------------
# all together now!
# ---------------

annual_values <- bc_annual_values %>% 
  full_join(wc_annual_values) 

write.csv(annual_values, "annual_values.csv")


# ----------------------------
# ----------------------------
# data wrangling for scatterplot
# ----------------------------
# ----------------------------




# ----------------------------
# ----------------------------
# data wrangling for seasonality
# ----------------------------
# ----------------------------

doy_avg_1980_2019 <-  past_day %>% 
  filter(year %in% c(1980:2019)) %>%
  pivot_longer(`soil_water_daily`:`aet_daily`, # The columns I'm gathering together
               names_to = "variable", # new column name for existing names
               values_to = "value") %>%  # new column name to store values) 
  #for grouping by doy
  group_by(doy, variable, park) %>% 
  summarize(`1980-2019` = mean(value, na.rm = TRUE),
            month = unique(month))

# --------
# Best case means
# --------

# 2020 - 2059 Best case
# first, rename variables to be able to combine them later

doy_avg_2020_2059_bc <- future_day %>% 
  filter(year %in% c(2020:2059)) %>%
  select(soil_water_daily_bc:doy) %>% 
  pivot_longer(`soil_water_daily_bc`:`aet_daily_bc`, # The columns I'm gathering together
               names_to = "variable", # new column name for existing names
               values_to = "value") %>% # new column name to store values)
  mutate(variable = case_when(.$variable == "aet_daily_bc" ~ "aet_daily",
                              .$variable == "rain_daily_bc" ~ "rain_daily",
                              .$variable == "runoff_daily_bc" ~ "runoff_daily",
                              .$variable == "pet_daily_bc" ~ "pet_daily",
                              .$variable == "accumswe_daily_bc" ~ "accumswe_daily",
                              .$variable == "soil_water_daily_bc" ~ "soil_water_daily",
                              .$variable == "agdd_daily_bc" ~ "agdd_daily",
                              TRUE ~ "deficit_daily")) %>% 
  group_by(doy, variable, park) %>% 
  summarize(`2020-2059` = mean(value, na.rm = TRUE),
            month = unique(month)) 


# 2060 - 2099 Best case df
# first, rename variables to be able to combine them later

doy_avg_2060_2099_bc <-  future_day %>% 
  filter(year %in% c(2060:2099)) %>%
  select(soil_water_daily_bc:doy) %>%
  pivot_longer(`soil_water_daily_bc`:`aet_daily_bc`, # The columns I'm gathering together
               names_to = "variable", # new column name for existing names
               values_to = "value") %>%  # new column name to store values) 
  mutate(variable = case_when(.$variable == "aet_daily_bc" ~ "aet_daily",
                              .$variable == "rain_daily_bc" ~ "rain_daily",
                              .$variable == "runoff_daily_bc" ~ "runoff_daily",
                              .$variable == "pet_daily_bc" ~ "pet_daily",
                              .$variable == "accumswe_daily_bc" ~ "accumswe_daily",
                              .$variable == "soil_water_daily_bc" ~ "soil_water_daily",
                              .$variable == "agdd_daily_bc" ~ "agdd_daily",
                              TRUE ~ "deficit_daily")) %>% #for grouping by doy
  group_by(doy, variable, park) %>% 
  summarize(`2060-2099` = mean(value, na.rm = TRUE),
            month = unique(month)) 

#full doy dataframe Best case

doy_avg_bc <- doy_avg_1980_2019 %>% 
  full_join(doy_avg_2020_2059_bc) %>% 
  full_join(doy_avg_2060_2099_bc) %>% 
  pivot_longer(c(`1980-2019`,`2020-2059`,`2060-2099`), # The columns I'm gathering together
               names_to = "decades", # new column name for existing names
               values_to = "value") %>% # new column name to store values 
  na.omit(doy_avg_bc) %>%  # because of the way the dataframe was made, there's NA's for second and third 40, just don't want those there
  pivot_wider(names_from = variable,
              values_from = value)

write.csv(doy_avg_bc, "doy_avg_bc.csv")


# --------
# wc means
# --------

# 2020 - 2059 wc df
# first, rename variables to be able to combine them later

doy_avg_2020_2059_wc <- future_day %>% 
  filter(year %in% c(2020:2059)) %>%
  select(soil_water_daily_wc:doy) %>% 
  pivot_longer(`soil_water_daily_wc`:`aet_daily_wc`, # The columns I'm gathering together
               names_to = "variable", # new column name for existing names
               values_to = "value") %>% # new column name to store values)
  mutate(variable = case_when(.$variable == "aet_daily_wc" ~ "aet_daily",
                              .$variable == "rain_daily_wc" ~ "rain_daily",
                              .$variable == "runoff_daily_wc" ~ "runoff_daily",
                              .$variable == "pet_daily_wc" ~ "pet_daily",
                              .$variable == "accumswe_daily_wc" ~ "accumswe_daily",
                              .$variable == "soil_water_daily_wc" ~ "soil_water_daily",
                              .$variable == "agdd_daily_wc" ~ "agdd_daily",
                              TRUE ~ "deficit_daily")) %>% 
  group_by(doy, variable, park) %>% 
  summarize(`2020-2059` = mean(value, na.rm = TRUE),
            month = unique(month)) 


# 2060 - 2099 wc df
# first, rename variables to be able to combine them later

doy_avg_2060_2099_wc <-  future_day %>% 
  filter(year %in% c(2060:2099)) %>%
  select(soil_water_daily_wc:doy) %>%
  pivot_longer(`soil_water_daily_wc`:`aet_daily_wc`, # The columns I'm gathering together
               names_to = "variable", # new column name for existing names
               values_to = "value") %>%  # new column name to store values) 
  mutate(variable = case_when(.$variable == "aet_daily_wc" ~ "aet_daily",
                              .$variable == "rain_daily_wc" ~ "rain_daily",
                              .$variable == "runoff_daily_wc" ~ "runoff_daily",
                              .$variable == "pet_daily_wc" ~ "pet_daily",
                              .$variable == "accumswe_daily_wc" ~ "accumswe_daily",
                              .$variable == "soil_water_daily_wc" ~ "soil_water_daily",
                              .$variable == "agdd_daily_wc" ~ "agdd_daily",
                              TRUE ~ "deficit_daily")) %>% 
  group_by(doy, variable, park) %>% 
  summarize(`2060-2099` = mean(value, na.rm = TRUE),
            month = unique(month)) 


#full day dataset wc

doy_avg_wc <- doy_avg_1980_2019 %>% 
  full_join(doy_avg_2020_2059_wc) %>% 
  full_join(doy_avg_2060_2099_wc) %>% 
  pivot_longer(c(`1980-2019`,`2020-2059`,`2060-2099`), # The columns I'm gathering together
               names_to = "decades", # new column name for existing names
               values_to = "value") %>% # new column name to store values 
  na.omit(doy_avg_wc) %>%  # because of the way the dataframe was made, there's NA's for second and third 40, just don't want those there
  pivot_wider(names_from = variable,
              values_from = value) 

write.csv(doy_avg_wc, "doy_avg_wc.csv")