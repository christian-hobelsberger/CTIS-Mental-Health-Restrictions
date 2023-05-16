# import packages
install.packages("timetk")
library(timetk)
library(lubridate)
library(tidyverse)

# read microdata
CTIS_microdata <- readRDS(file = "data/protected_data/CTIS_microdata.RDS")

# Create and save tables

# Obs and NAs -----------------------------------------------------------------------------

table_Obs_D1_Date <- CTIS_microdata %>%
  select(RecordedDate, D1) %>%
  drop_na() %>%
  summarise_by_time(.date_var = RecordedDate,
                    .by = "day",
                    count = n())
saveRDS(object = table_Obs_D1_Date, file = "tables/analysis/table_Obs_D1_Date.RDS")

table_NA_D1_Date <- CTIS_microdata %>%
  select(RecordedDate, D1) %>%
  filter(is.na(D1)) %>%
  summarise_by_time(.date_var = RecordedDate,
                    .by = "day",
                    count_NA = n())
saveRDS(object = table_NA_D1_Date, file = "tables/analysis/table_NA_D1_Date.RDS")

table_Obs_D2_Date <- CTIS_microdata %>%
  select(RecordedDate, D2) %>%
  drop_na() %>%
  summarise_by_time(.date_var = RecordedDate,
                    .by = "day",
                    count = n())
saveRDS(object = table_Obs_D2_Date, file = "tables/analysis/table_Obs_D2_Date.RDS")

table_NA_D2_Date <- CTIS_microdata %>%
  select(RecordedDate, D2) %>%
  filter(is.na(D2)) %>%
  summarise_by_time(.date_var = RecordedDate,
                    .by = "day",
                    count_NA = n())
saveRDS(object = table_NA_D2_Date, file = "tables/analysis/table_NA_D2_Date.RDS")

table_Obs_D4_Date <- CTIS_microdata %>%
  select(RecordedDate, D4) %>%
  drop_na() %>%
  summarise_by_time(.date_var = RecordedDate,
                    .by = "day",
                    count = n())
saveRDS(object = table_Obs_D4_Date, file = "tables/analysis/table_Obs_D4_Date.RDS")

table_NA_D4_Date <- CTIS_microdata %>%
  select(RecordedDate, D4) %>%
  filter(is.na(D4)) %>%
  summarise_by_time(.date_var = RecordedDate,
                    .by = "day",
                    count_NA = n())
saveRDS(object = table_NA_D4_Date, file = "tables/analysis/table_NA_D4_Date.RDS")

table_Obs_D5_Date <- CTIS_microdata %>%
  select(RecordedDate, D5) %>%
  drop_na() %>%
  summarise_by_time(.date_var = RecordedDate,
                    .by = "day",
                    count = n())
saveRDS(object = table_Obs_D5_Date, file = "tables/analysis/table_Obs_D5_Date.RDS")

table_NA_D5_Date <- CTIS_microdata %>%
  select(RecordedDate, D5) %>%
  filter(is.na(D5)) %>%
  summarise_by_time(.date_var = RecordedDate,
                    .by = "day",
                    count_NA = n())
saveRDS(object = table_NA_D5_Date, file = "tables/analysis/table_NA_D5_Date.RDS")

# table_Obs_region_agg_Date <- CTIS_microdata %>%
#   select(region_agg, Date) %>%
#   drop_na() %>%
#   group_by(region_agg, Date) %>%
#   summarize(count = n())
# saveRDS(object = table_Obs_region_agg_Date, file = "tables/analysis/table_Obs_region_agg_Date.RDS")

table_Obs_region_agg_Date_weekly <- CTIS_microdata %>%
  select(region_agg, Date) %>%
  drop_na() %>%
  group_by(region_agg) %>%
summarise_by_time(.date_var = Date,
                  .by = "week",
                  count = n())
saveRDS(object = table_Obs_region_agg_Date_weekly, file = "tables/analysis/table_Obs_region_agg_Date_weekly.RDS")

# table_Obs_region_agg <- CTIS_microdata %>%
#   select(region_agg) %>%
#   drop_na() %>%
#   group_by(region_agg) %>%
#   summarise(count = n())
# saveRDS(object = table_Obs_region_agg, file = "tables/analysis/table_Obs_region_agg.RDS")

table_D1_weekday <- CTIS_microdata %>%
  select(RecordedDate, D1) %>%
  drop_na() %>%
  filter(RecordedDate <= "2022-06-15") %>%
  mutate(day_of_week = wday(RecordedDate, label = TRUE)) %>%
  select(day_of_week, D1) %>%
  group_by(day_of_week) %>%
  summarise(n = n()) %>%
  mutate(perc = n/sum(n))
saveRDS(object = table_D1_weekday, file = "tables/analysis/table_D1_weekday.RDS")

table_D1_weekday_per_D1 <- CTIS_microdata %>%
  select(RecordedDate, D1) %>%
  drop_na() %>%
  filter(RecordedDate <= "2022-06-15") %>%
  mutate(day_of_week = wday(RecordedDate, label = TRUE)) %>%
  select(day_of_week, D1) %>%
  group_by(day_of_week, D1) %>%
  summarise(n = n()) %>%
  mutate(perc = n/sum(n))
saveRDS(object = table_D1_weekday_per_D1, file = "tables/analysis/table_D1_weekday_per_D1.RDS")
