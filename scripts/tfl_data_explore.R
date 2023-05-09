# Load packages.
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(ggplot2)

# Set language for Dutch server.
Sys.setlocale("LC_TIME", "English")

# Sam: the download link does not work as of 15 March 2023.
# https://data.gov.uk/dataset/9e14de72-df2a-4bbe-b131-1844394e8368/lower-layer-super-output-area-2011-to-ward-2019-lookup-in-england-and-wales
# lsoa_table <- read_csv("data/Lower_Layer_Super_Output_Area__2011__to_Ward__2019__Lookup_in_England_and_Wales.csv")

# Alternative:
# https://geoportal.statistics.gov.uk/datasets/ons::lower-layer-super-output-area-2011-to-ward-2019-lookup-in-england-and-wales-1/explore
lsoa_table <- read_csv("data/lookups/Lower_Layer_Super_Output_Area_(2011)_to_Ward_(2019)_Lookup_in_England_and_Wales.csv")

# Load data handled in tfl_data_load.R.
load(file = 'data/rdata/tfl_data_2.Rdata')

# Brief explore.
glimpse(tfl_data_2)
head(tfl_data_2)

# Sort dates.
tfl_data_2 <- tfl_data_2 %>% 
  mutate(trafficdate_lub = ymd(trafficdate))

# load google data
GB_mob_data_20 <- read.csv("data/Region_Mobility_Report_CSVs/2020_GB_Region_Mobility_Report.csv", stringsAsFactors = F)
GB_mob_data_21 <- read.csv("data/Region_Mobility_Report_CSVs/2021_GB_Region_Mobility_Report.csv", stringsAsFactors = F)

# Bind years together.
all_mob_data <-  bind_rows(GB_mob_data_20, GB_mob_data_21)

# Identify the LSOAs we have the TFL data.
tfl_LSOAs <-  tfl_data_2 %>%
  select(lsoa_2011) %>%
  distinct() %>%
  mutate(lsoa_in_tfl = T)

count(tfl_LSOAs, lsoa_in_tfl)

# check that tfl data sufficiently covers sub_regions of interest  
region_tfl_coverage <- all_mob_data %>%
  filter(str_detect(sub_region_1, 'London')) %>% # filter only those labeled as london
  mutate(sub_region_2_clean = trimws(str_remove_all(sub_region_2, "City of |London Borough of |Royal Borough of "))) %>% # clean names of extra detail
  select(sub_region_2_clean)%>%
  left_join(lsoa_table, by = c('sub_region_2_clean' = 'LAD19NM')) %>% # combine with the LSOA data
  left_join(tfl_LSOAs, by = c('LSOA11CD' = 'lsoa_2011')) %>%
  filter(!sub_region_2_clean == 'London') %>% # remove those that refer to whole of london
  filter(!sub_region_2_clean == '')  %>%
  select(sub_region_2_clean, LSOA11CD, lsoa_in_tfl)%>%
  distinct() %>%
  group_by(sub_region_2_clean,) %>%
  summarise(count = n(), present = sum(lsoa_in_tfl, na.rm = T))%>%
  ungroup()%>%
  mutate(perc = 100*present/ count)

# sub_regions to be used from google data
ldn_mob_data <-  all_mob_data %>% filter(str_detect(sub_region_1, 'London')) %>% # filter only those labeled as london
  mutate(sub_region_2_clean = trimws(str_remove_all(sub_region_2, "City of |London Borough of |Royal Borough of "))) %>% # clean names of extra detail
  filter(!sub_region_2_clean == 'London') %>% # remove those that refer to whole of london
  filter(!sub_region_2_clean == '') %>%
  select(sub_region_2_clean, date,contains('baseline')) %>% 
  as_tibble()

# Tidy the names
names(ldn_mob_data) <-  str_remove(names(ldn_mob_data), "_percent_change_from_baseline") 

# Make sure it is a date.
ldn_mob_data <- ldn_mob_data %>% 
  mutate(date_lub = ymd(date))
  
# Test plot.
ldn_mob_data %>% 
  group_by(date_lub) %>% 
  summarise(mean_transit = mean(transit_stations)) %>% 
  ungroup() %>%
  ggplot(data = .) +
  geom_line(mapping = aes(x = date_lub, y = mean_transit)) +
  theme(legend.position = "none")

# Save.
# save(ldn_mob_data, file = 'data/rdata/ldn_mob_data_15052023.Rdata')

# ==============================================================================

# Load
# load('data/rdata/ldn_mob_data_15052023.Rdata')

max(ldn_mob_data$date)
max(ldn_mob_data$date_lub)

min(ldn_mob_data$date)
min(ldn_mob_data$date_lub)

max(tfl_data_2$trafficdate)
max(tfl_data_2$trafficdate_lub)

min(tfl_data_2$trafficdate_lub)
min(tfl_data_2$trafficdate)

# paste('The date period of interest is from ', min(ldn_mob_data$date), ' to ' , max(tfl_data_2$date) )

pan_tfl <-  tfl_data_2 %>%
  group_by(date, LAD19NM, entryexit, mode) %>%
  summarise(count = sum(n,na.rm = T))%>%
  filter(date > as.Date("2020-02-14") ) %>%
  ungroup()

bus_exit_missing_dates <- pan_tfl %>%
  filter(mode == 'Bus') %>%
  select(date, entryexit) %>%
  distinct() %>%
  mutate(n = 1) %>% 
  pivot_wider(id_cols = c(date), names_from = entryexit, values_from = n, values_fill = list(n = 0)) %>%
  filter(`Exit/Alighting` == 0) %>%
  pull(date)

rail_exit_missing_dates <- pan_tfl %>%
  filter(mode == 'Rail') %>%
  select(date, entryexit) %>% distinct() %>% mutate(n = 1) %>% 
  pivot_wider(id_cols = c(date), names_from = entryexit, values_from = n, values_fill = list(n = 0))%>%
  filter(`Exit/Alighting` == 0) %>%
  pull(date)

pan_tfl <- tfl_data_2 %>%
  group_by(date, LAD19NM, entryexit, mode) %>%
  summarise(count = sum(n,na.rm = T)) %>% 
  filter(date > as.Date("2020-02-14") ) %>%
  ungroup() %>%
  filter(!date %in% bus_exit_missing_dates) %>%
  #filter(!date %in% c(ymd("2020-02-29"))) %>%  # remove leap year day as no comparrison data
  mutate(week = week(date)) %>%
  mutate(dow = wday(date)) %>%
  mutate(year = year(date))%>%
  mutate(year_2 = ifelse(year == '2020',1,2))%>%
  mutate(rel_date = paste(year_2, week, dow, sep = '-'))


# the tfl has missing exit dates so remove all of this data

# to match feb 2020 to april 19th
x <- tfl_data_2 %>% 
  group_by(date, LAD19NM, entryexit, mode) %>%
  summarise(count = sum(n,na.rm = T))%>%
  ungroup()%>%
  filter(date > as.Date("2019-02-14") & date < as.Date("2019-04-20")) %>%
  mutate(year = 1)

# to match may 25th to end 2019
y <- tfl_data_2%>% 
  group_by(date, LAD19NM, entryexit, mode) %>%
  summarise(count = sum(n,na.rm = T))%>%
  ungroup()%>%
  filter(date > as.Date("2019-05-24") & date < as.Date("2020-01-01"))%>%
  mutate(year = 1)

# to match beginning 2021 to Aug 2021
z <- tfl_data_2%>% 
  group_by(date, LAD19NM, entryexit, mode) %>%
  summarise(count = sum(n,na.rm = T))%>%
  ungroup()%>%
  filter(date > as.Date("2018-12-31") & date < as.Date("2019-09-01"))%>%
  mutate(year = 2)

# Join and create time measures.
pre_pan_tfl_2019_match <- bind_rows(x,y,z)%>%
  filter(!date %in% bus_exit_missing_dates) %>%
  #filter(!date %in% c(ymd("2020-02-29"))) %>%  # remove leap year day as no comparrison data
  mutate(week = week(date)) %>%
  mutate(dow = wday(date)) %>%
  mutate(rel_date = paste(year, week, dow, sep = '-'))

# Join.
Both_data <-  full_join(pre_pan_tfl_2019_match, pan_tfl, by = c("rel_date" = "rel_date", "LAD19NM" = "LAD19NM", "entryexit" = "entryexit", 'mode' = 'mode' )) %>%
  filter(complete.cases(.)) %>%
  select(rel_date, LAD19NM, entryexit, mode,count.x, count.y, date.x, date.y) %>%
  mutate(change = 100*(count.y - count.x )/ count.x)

# Final handling.
boroughs <-  unique(ldn_mob_data$sub_region_2_clean)

counts <-  Both_data %>% filter(mode == 'Rail') %>%
  filter(entryexit == 'Entry/Boarding') %>%
  select(date.y, change, LAD19NM)

mob_change <-  ldn_mob_data  %>%
  mutate(date = ymd(date)) %>%
  select(date, transit_stations, sub_region_2_clean)

joined_data <- left_join(counts, mob_change, by = c('LAD19NM'= 'sub_region_2_clean', 'date.y' = 'date')) %>%
  filter(LAD19NM %in% boroughs) %>% 
  rename(google_measure = transit_stations,
         tfl_measure    = change,
         date_day       = date.y,
         LocalAuthority = LAD19NM)

# Make a long version as and when needed.
joined_long_data <- joined_data %>% 
  pivot_longer(cols = c(-LocalAuthority, -date_day),
               names_to = "measure", values_to = "value")

# ==============================================================================

# Line plot over time.
time_compare_borough_gg <- joined_long_data %>% 
  ggplot(data = .) +
  geom_point(mapping = aes(x = date_day, y = value,
                          group = measure, colour = measure),
            alpha = 1, size = 0.01) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(~LocalAuthority, ncol = 4) +
  theme_bw() +
  scale_y_continuous(limits = c(-110,20)) +
  scale_colour_viridis_d() +
  # scale_x_date(date_breaks = "3 months", date_labels = "%b %y") +
  scale_x_date(date_labels = "%b %Y") +
  labs(colour = NULL, y = NULL, x = NULL) +
  guides(colour = guide_legend(override.aes = list(size=6))) +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 7),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 7)) 

# Save.
ggsave(plot = time_compare_borough_gg,
       filename = "visuals/time_compare_borough.png",
       height = 28, width = 15, unit = "cm", dpi = 300)

# Bus handling for plot.
boroughs <-  unique(ldn_mob_data$sub_region_2_clean)

counts <-  Both_data %>% filter(mode == 'Bus') %>%
  filter(entryexit == 'Entry/Boarding') %>%
  select(date.y, change, LAD19NM)

mob_change <- ldn_mob_data  %>%
  mutate(date = ymd(date)) %>%
  select(date, transit_stations, sub_region_2_clean)

joined_data <-  left_join(counts, mob_change, by = c('LAD19NM'= 'sub_region_2_clean', 'date.y' = 'date')) %>%
  filter(LAD19NM %in% boroughs) %>%
  rename(google_measure = transit_stations,
         tfl_measure    = change,
         date_day       = date.y,
         LocalAuthority = LAD19NM)

# Make a long version as and when needed.
joined_long_data <- joined_data %>% 
  pivot_longer(cols = c(-LocalAuthority, -date_day),
               names_to = "measure", values_to = "value")


# Plot.
# Line plot over time.
bus_time_compare_borough_gg <- joined_long_data %>% 
  ggplot(data = .) +
  geom_point(mapping = aes(x = date_day, y = value,
                           group = measure, colour = measure),
             alpha = 1, size = 0.01) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(~LocalAuthority, ncol = 4) +
  theme_bw() +
  scale_y_continuous(limits = c(-110,20)) +
  scale_colour_viridis_d() +
  # scale_x_date(date_breaks = "3 months", date_labels = "%b %y") +
  scale_x_date(date_labels = "%b %Y") +
  labs(colour = NULL, y = NULL, x = NULL) +
  guides(colour = guide_legend(override.aes = list(size=6))) +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 7),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 7)) 
  
# Save.
ggsave(plot = bus_time_compare_borough_gg,
       filename = "visuals/bus_time_compare_borough.png",
       height = 28, width = 15, unit = "cm", dpi = 300)

# Explore correlation analysis.
barnet_tfl_df <- joined_long_data %>% 
  filter(LocalAuthority == "Barnet" & measure == "tfl_measure") %>% 
  arrange(value) %>% 
  mutate(date_day_char = as.character(date_day))


tfl_test_df <- barnet_tfl_df %>% 
  mutate(date_day_fac = forcats::fct_relevel(date_day_char, unique(date_day_char)))

barnet_goo_df <- joined_long_data %>% 
  filter(LocalAuthority == "Barnet" & measure == "google_measure") %>% 
  arrange(value) %>% 
  mutate(date_day_char = as.character(date_day))


goo_test_df <- barnet_goo_df %>% 
  mutate(date_day_fac = forcats::fct_relevel(date_day_char, unique(date_day_char)))

# Bind.
goo_tfl_test_df <- bind_rows(tfl_test_df, goo_test_df)

ggplot(data = goo_tfl_test_df) +
  geom_point(mapping = aes(x = date_day_fac, y = value, group = measure,
                           colour = measure))

# Test 2.
goo_test_2_df <- barnet_goo_df %>% 
  mutate(date_day_fac = forcats::fct_relevel(date_day_char, unique(barnet_tfl_df$date_day_char)))

ggplot(data = goo_test_2_df,
       mapping = aes(x = as.numeric(date_day_fac), y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) 


