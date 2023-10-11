# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>. 


source("https://raw.githubusercontent.com/HUD-Data-Lab/DataLab/main/00_read_2024_csv.R") # Question for Gwen. How did you get this link? Can I change all source() into links?
source("https://github.com/HUD-Data-Lab/DataLab/blob/33f9e0615a5857ab2f31f03fe1266444e665be99/DataLab.R")

# Goals to do Three method tests for each set up. 
#### Descriptive measures: (1) (percent of population) (2) Length of time spent homeless
#### Method testing 1: Detailed Ethnicity/Complex (Every combination is unique)
#### Method testing 2: Total Response grouping (half white, half Asian would be counted in both White and Asian)
#### Method testing 3: Household estimate (LSA and HUD Racial Equity Analysis tool)
#Comparison and discussion: APR/CAPER, LSA, Census


#Set up data to analyze and compare ----
# Program enrollment and household information from the APR and CAPER PY24 reporting
{
all_program_enrollments <- Enrollment %>% 
  #filter(ProjectID %in% project_list) %>% # Commented out to include all projects
  left_join(Project %>%
              select(ProjectID, ProjectType, ProjectName),
            by = "ProjectID") %>%
  left_join(Exit %>%
              select(-PersonalID), # Are we removing personalID to avoid the duplication problem? (i.e., PersonalID.X and PersonalID.Y)
            by = "EnrollmentID")

recent_program_enrollment <- all_program_enrollments %>%
  group_by(PersonalID) %>%
  arrange(desc(EntryDate)) %>% #arrange by most recent entry date  
  slice(1L) %>% #what does 1L do?
  ungroup() 

# get additional client information (age for reporting)
client_plus <- add_client_info(recent_program_enrollment)  #View(add_client_info) Getting a warning about mac(age, na.rm=TRUE) is returning -inf

annual_assessment_dates <- Enrollment %>% #What is this trying to get?
  group_by(HouseholdID) %>%
  mutate(start_for_annual = max(EntryDate[RelationshipToHoH == 1]), #Max entry date for each head of household
         years_in_project = trunc((start_for_annual %--% report_end_date) / years(1))) %>% # What does %--% do?
  filter(years_in_project > 0) %>%
  mutate(annual_due = start_for_annual %m+% years(years_in_project)) %>%
  select(HouseholdID, annual_due) %>%
  distinct()

household_info <- get_household_info(all_program_enrollments,
                                     return_type = "household")

recent_program_enrollment_r <- recent_program_enrollment %>%
  left_join(client_plus, by = "PersonalID") %>%
  left_join(household_info, by = "HouseholdID") %>%
  mutate(MoveInDateAdj = case_when(
    !is.na(HoH_HMID) &
      HoH_HMID >= DOB &
      HoH_HMID >= EntryDate &
      (HoH_HMID <= ExitDate |
         is.na(ExitDate)) ~ HoH_HMID,
    !is.na(HoH_HMID) &
      (HoH_HMID <= ExitDate |
         is.na(ExitDate)) ~ EntryDate),
    leaver = ExitDate >= report_start_date &
      ExitDate <= report_end_date & 
      !is.na(ExitDate)) %>%
  left_join(chronicity_data, by = "EnrollmentID")

detail_columns <- c("ProjectName","HouseholdID","PersonalID","EnrollmentID","RelationshipToHoH", "EntryDate","ExitDate")

recent_program_enrollment_allDemograhics <- recent_program_enrollment_r %>%
  select(all_of(detail_columns), age_group, HoH_HMID) %>%
  left_join(Client %>%
              select(PersonalID, all_of(unname(race_columns)), RaceNone),
            by = "PersonalID")
}

#Set up race details

Race_detail <- recent_program_enrollment_allDemograhics %>% 
  select("PersonalID","EnrollmentID","RelationshipToHoH","EntryDate","ExitDate","AmIndAKNative","Asian","BlackAfAmerican","HispanicLatinaeo",
         "MidEastNAfrican", "NativeHIPacific","White","RaceNone") %>% 
  mutate(RaceCount = rowSums(across(all_of(unname(race_columns))),na.rm=TRUE)) %>%
  mutate(AmIndAKNative = case_when(AmIndAKNative == 1 ~ "AmIndAKNative", AmIndAKNative == 0 ~ NA),
         Asian = case_when(Asian == 1 ~ "Asian", Asian == 0 ~ NA),
         BlackAfAmerican = case_when(BlackAfAmerican == 1 ~ "BlackAfAmerican", BlackAfAmerican == 0 ~ NA),
         HispanicLatinaeo = case_when(HispanicLatinaeo == 1 ~ "HispanicLatinaeo", HispanicLatinaeo == 0 ~ NA),
         MidEastNAfrican = case_when(MidEastNAfrican == 1 ~ "MidEastNAfrican", MidEastNAfrican == 0 ~ NA),
         NativeHIPacific = case_when(NativeHIPacific == 1 ~ "NativeHIPacific", NativeHIPacific == 0 ~ NA),
         White = case_when(White == 1 ~ "White", White == 0 ~ NA)) %>% 
  mutate(race_list = apply(Race_detail[c(6:12)], 1, #is there a way to do this from the column select? 
                                    function(x) paste(x[!is.na(x)], collapse = "/"))) #how does this work?

View(Race_detail)

#Set up Q12 APR totals

standard_detail_columns2 <- standard_detail_columns[c(1:6)] #removed Household_type


Q12_detail <- recent_program_enrollment %>%
  select(all_of(standard_detail_columns2)) %>% #removed age_group and HoH_HMID
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
      race_count %in% 1:2 ~ race_name_list,
      race_count > 2 &
        HispanicLatinaeo == 1 ~ "Multiracial – more than 2 races/ethnicity, with one being Hispanic/Latina/e/o",
      race_count > 2 ~ "Multiracial – more than 2 races, where no option is Hispanic/Latina/e/o",
      RaceNone %in% c(8, 9) ~ "Client Doesn’t Know/Prefers Not to Answer",
      TRUE ~ "Data Not Collected"
    )
  ) %>% 
  adorn_totals("row")


# Tables to compare ----

#APR
APRQ12 <- Q12_counts %>% 
  group_by(race_list) %>%
  summarise(APR.Total = n()) %>% 
  filter(race_list != "-",
         is.na(race_list) == FALSE) %>% 
  mutate(APRQ12.per = paste0(round(100* APR.Total/sum(APR.Total),2),'%')) %>%
  adorn_totals("row") %>%
  ungroup()

#Method 1

M1_method <- Race_detail %>% 
  group_by(race_list) %>% 
  summarise(M1.Total = n()) %>%
  filter(race_list != "") %>% #what is the random blank? Is it the raceNone/missing data elements
  adorn_totals("row") %>% 
  mutate(M1.per = paste0(round(100* M1.Total/sum(M1.Total),2),'%')) %>%
  ungroup()


# Total Descriptoives Compared to APR ----
M1_APR_compare <- M1_method %>% 
  left_join(APRQ12, by= "race_list")

# Other calcs
Race_detail %>% 
  #filter(RaceCount >=3) %>% 
  n_distinct("PersonalID")

Q12_counts %>% 
  filter(#race_count >= 3,
         race_count < 100) %>% 
  n_distinct("PersonalID")





