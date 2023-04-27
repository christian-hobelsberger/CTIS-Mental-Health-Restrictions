# import packages
install.packages("timetk")
library(timetk)
library(tidyverse)

# read microdata
CTIS_microdata <- readRDS(file = "data/protected_data/CTIS_microdata.RDS")

# Create and save tables

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
