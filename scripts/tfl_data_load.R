library(tidyverse)
library(janitor)

##### LSOA data #######

# Sam: the download link does not work as of 15 March 2023.
# https://data.gov.uk/dataset/9e14de72-df2a-4bbe-b131-1844394e8368/lower-layer-super-output-area-2011-to-ward-2019-lookup-in-england-and-wales
lsoa_table <- read_csv("data/Lower_Layer_Super_Output_Area__2011__to_Ward__2019__Lookup_in_England_and_Wales.csv")

# Alternative:
# https://geoportal.statistics.gov.uk/datasets/ons::lower-layer-super-output-area-2011-to-ward-2019-lookup-in-england-and-wales-1/explore
# lsoa_table <- read_csv("data/Lower_Layer_Super_Output_Area_(2011)_to_Ward_(2019)_Lookup_in_England_and_Wales.csv")

######## tfl data ########

path = "data/TFL_data/second/taps"
files = list.files(path = path)

list_of_data = list()

for(file in files){
  
  x = read.csv(paste0(path,'/',file), header = T, stringsAsFactors = F) %>% 
    clean_names()%>%
    mutate(date = as.Date(trafficdate))
  
  month = str_sub(file, 1,10)
  
  list_of_data[[month]] = x
  
}


tfl_data_2 = bind_rows(list_of_data) %>%
  left_join(lsoa_table %>% select(LSOA11CD, LAD19NM), by = c('lsoa_2011' = 'LSOA11CD'))

save(tfl_data_2, file = 'tfl_data_2.Rdata')




#########  load google data ####### 

GB_mob_data_20<- read.csv("data/Region_Mobility_Report_CSVs/2020_GB_Region_Mobility_Report.csv", stringsAsFactors = F)

head(GB_mob_data_20)

max(GB_mob_data_20$date)


GB_mob_data_21<- read.csv("data/Region_Mobility_Report_CSVs/2021_GB_Region_Mobility_Report.csv", stringsAsFactors = F)


all_mob_data = bind_rows(GB_mob_data_20, GB_mob_data_21)


###### reduce google data for only those LADs that we have complete data for ######

# get the distinct LSOAs
tfl_LSOAs = tfl_data_2 %>% select(lsoa_2011) %>% distinct() %>% mutate(lsoa_in_tfl = T)

# check that tfl data sufficiently covers sub_regions of interest  
region_tfl_coverage = all_mob_data %>% filter(str_detect(sub_region_1, 'London')) %>% # filter only those labeled as london
  mutate(sub_region_2_clean = trimws(str_remove_all(sub_region_2, "City of |London Borough of |Royal Borough of "))) %>% # clean names of extra detail
  select(sub_region_2_clean)%>%
  left_join(lsoa_table, by = c('sub_region_2_clean' = 'LAD19NM')) %>%#combine with the LSOA data
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

ldn_mob_data = all_mob_data %>% filter(str_detect(sub_region_1, 'London')) %>% # filter only those labeled as london
  mutate(sub_region_2_clean = trimws(str_remove_all(sub_region_2, "City of |London Borough of |Royal Borough of "))) %>% # clean names of extra detail
  filter(!sub_region_2_clean == 'London') %>% # remove those that refer to whole of london
  filter(!sub_region_2_clean == '') %>%
  select(sub_region_2_clean, date,contains('baseline'))

names(ldn_mob_data) = str_remove(names(ldn_mob_data), "_percent_change_from_baseline") #tidy the names

save(ldn_mob_data, file = 'ldn_mob_data.Rdata')


