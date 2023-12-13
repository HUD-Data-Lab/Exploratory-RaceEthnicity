library(networkD3)
options(scipen=999)
set.seed(2024)

# Pre-set Categories -- categories are pre-grouped for analysis and resposnes are coded into those bins. Following APR/CAPER Specs

#Disparities analysis
## Compare to general population (we don't have)
## Descriptive (who is in the system) -- By project type and compare disparities
## Compare Program participation length of time to permanent exit

#Source("CLassifyingMulti_race.R)

#Overall Race and ethnicity 
# Only using the following project types: ES (and ES NbN), RRH, TH, PH
# ES(0,1); RRH (13); TH(2); PH(3,9,10)

Q12_detail <- recent_program_enrollment_allDemograhics %>% 
  select(all_of(standard_detail_columns),Destination,ProjectType) %>%
  left_join(Client %>%
              select(PersonalID, all_of(unname(race_columns)), RaceNone),
            by = "PersonalID")

Q12_counts <- Q12_detail %>%
  ifnull(., 0) %>%
  left_join(race_info, #Race_info created from DataLab_lists.R line 261
            by = all_of(unname(race_columns))) %>% 
  mutate(across(
    all_of(unname(race_columns)),
    ~ as.numeric(.)),
    race_count = rowSums(across(all_of(unname(race_columns))),
                         na.rm = TRUE),
    race_tabulation = case_when(
      race_count %in% 1:2 ~ race_list,
      race_count > 2 &
        HispanicLatinaeo == 1 ~ "Multiracial – more than 2 races/ethnicity, with one being Hispanic/Latina/e/o",
      race_count > 2 & HispanicLatinaeo != 1 ~ "Multiracial – more than 2 races, where no option is Hispanic/Latina/e/o",
      RaceNone %in% c(8, 9) ~ "Client Doesn’t Know/Prefers Not to Answer",
      TRUE ~ "Data Not Collected")) %>% 
  filter(ProjectType %in% c(0,1,2,3,9,10,13),
         EntryDate <= report_end_date & (is.na(ExitDate) | ExitDate >= report_start_date)) %>% 
  mutate(ProjectType_name= case_when(
    ProjectType == 0 | ProjectType == 1 ~ "ES",
    ProjectType == 2 ~ "TH",
    ProjectType == 13 ~ "RRH",
    ProjectType == 3 | ProjectType == 9 | ProjectType == 10  ~ "PH"))

Pre_set_categories_Overall <- Q12_counts %>% 
  group_by(race_tabulation) %>%
  summarise(APR.Total = n()) %>% 
  filter(race_tabulation != "-",
         race_tabulation != "Client Doesn’t Know/Prefers Not to Answer",
         race_tabulation != "Data Not Collected",
         is.na(race_tabulation) == FALSE) %>% 
  adorn_totals("row") %>% 
  mutate(APRQ12.per = paste0(round(100* APR.Total/sum(APR.Total[race_tabulation != "Total"],na.rm=TRUE),2),'%')) %>%
  ungroup()

#Race and Ethnicity By project type ----

PreCat_Count_ProjectType <- Q12_counts %>% 
  group_by(ProjectType_name,race_list) %>% 
  summarise(Total_response_total = n()) %>%
  mutate(Total_response_Percent = paste0(round(100* Total_response_total/sum(Total_response_total[race_list != "Total"],
                                                                             na.rm = TRUE), 2),'%')) %>%
  ungroup()

# Compare Project Type to overall Population ----
# Null hypothesis there is no difference from the overall distribution of racial identity to project type X

PreCat_T.Tests_Standard <- PreCat_Count_ProjectType %>% 
  group_by(race_list) %>% 
  summarise(Total_response_total = n()) %>%
  mutate(Total_response_Percent = paste0(round(Total_response_total/sum(Total_response_total[race_list != "Total"],
                                                                        na.rm = TRUE), 4))) %>%
  ungroup()

T.Tests_Detail <- PreCat_Count_ProjectType %>% 
  select(PersonalID,ProjectType_name,race_list) %>% 
  mutate("AmIndAKNative" =case_when(race_list == "AmIndAKNative"~1, TRUE ~0), #### This could be a loop *************
         
         
         
         
         
         
         
         
         