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


source("https://raw.githubusercontent.com/HUD-Data-Lab/DataLab/main/00_read_2024_csv.R") # Can we switch all to be source links?
source("https://raw.githubusercontent.com/HUD-Data-Lab/DataLab/main/DataLab.R")

# Goals to do Three method tests for each set up. 
#### Descriptive measures: (1) (percent of population) (2) Length of time spent homeless
#### Method testing 1: Detailed Ethnicity/Complex (Every combination is unique)
#### Method testing 2: Total Response grouping (half white, half Asian would be counted in both White and Asian)
#### Method testing 3: Household estimate (LSA and HUD Racial Equity Analysis tool)
#Comparison and discussion: APR/CAPER, LSA, Census


#Set up data to analyze and compare ----
# Program enrollment and household information from the APR and CAPER PY24 reporting
{

  project_list <- c(
    234,	#"DataLab - ES-EE ESG I",
    93,	#"DataLab - ES-NbN ESG",
    1002,	#"DataLab - HP ESG",
    1625,	#"DataLab - PSH CoC I",
    1343,	#"DataLab - RRH CoC I",
    1492,	#"DataLab - RRH CoC II", # set this to RRH-SSO
    1051,	#"DataLab - RRH ESG I",
    1647,	#"DataLab - SO ESG",
    1615,	#"DataLab - SSO CoC",
    389	#"DataLab - TH CoC" # update the funding source to 5
  )
  
  
all_program_enrollments <- Enrollment %>% 
  filter(ProjectID %in% project_list) %>% # Match project list to APR
  left_join(Project %>%
              select(ProjectID, ProjectType, ProjectName),
            by = "ProjectID") %>%
  left_join(Exit %>%
              select(-PersonalID),
            by = "EnrollmentID")

recent_program_enrollment <- all_program_enrollments %>%
  group_by(PersonalID) %>%
  arrange(desc(EntryDate)) %>% #arrange by most recent entry date  
  slice(1L) %>% #Keep the most recent entry
  ungroup() 

# get additional client information (age for reporting)
client_plus <- add_client_info(recent_program_enrollment)  #View(add_client_info) Getting a warning about mac(age, na.rm=TRUE) is returning -inf

# annual_assessment_dates <- Enrollment %>% #Not currently using this.
#   group_by(HouseholdID) %>%
#   mutate(start_for_annual = max(EntryDate[RelationshipToHoH == 1]), #Max entry date for each head of household
#          years_in_project = trunc((start_for_annual %--% report_end_date) / years(1))) %>% # What does %--% do?
#   filter(years_in_project > 0) %>%
#   mutate(annual_due = start_for_annual %m+% years(years_in_project)) %>%
#   select(HouseholdID, annual_due) %>%
#   distinct()

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
  add_chronicity_data(.)

detail_columns <- c("ProjectName","HouseholdID","PersonalID","EnrollmentID","RelationshipToHoH", "EntryDate","ExitDate")

recent_program_enrollment_allDemograhics <- recent_program_enrollment_r %>%
  select(all_of(detail_columns), age_group, HoH_HMID, Destination) %>%
  left_join(Client %>%
              select(PersonalID, all_of(unname(race_columns)), RaceNone),
            by = "PersonalID") %>% 
  left_join(household_info %>% 
            select(HouseholdID,household_type),
            by = "HouseholdID")
}

#Set up race details

Race_detail <- recent_program_enrollment_allDemograhics %>% 
  select("PersonalID","EnrollmentID","RelationshipToHoH","EntryDate","ExitDate","Destination","AmIndAKNative","Asian","BlackAfAmerican","HispanicLatinaeo",
         "MidEastNAfrican", "NativeHIPacific","White","RaceNone") %>% 
  mutate(RaceCount = rowSums(across(all_of(unname(race_columns))),na.rm=TRUE)) %>%
  mutate(AmIndAKNative = case_when(AmIndAKNative == 1 ~ "AmIndAKNative", AmIndAKNative == 0 ~ NA),
         Asian = case_when(Asian == 1 ~ "Asian", Asian == 0 ~ NA),
         BlackAfAmerican = case_when(BlackAfAmerican == 1 ~ "BlackAfAmerican", BlackAfAmerican == 0 ~ NA),
         HispanicLatinaeo = case_when(HispanicLatinaeo == 1 ~ "HispanicLatinaeo", HispanicLatinaeo == 0 ~ NA),
         MidEastNAfrican = case_when(MidEastNAfrican == 1 ~ "MidEastNAfrican", MidEastNAfrican == 0 ~ NA),
         NativeHIPacific = case_when(NativeHIPacific == 1 ~ "NativeHIPacific", NativeHIPacific == 0 ~ NA),
         White = case_when(White == 1 ~ "White", White == 0 ~ NA)) %>% 
  mutate(race_list = apply(.[c(7:13)], 1, #is there a way to do this from the column select? 
                                    function(x) paste(x[!is.na(x)], collapse = "/")))

View(Race_detail)

#Set up Q12 APR totals

#standard_detail_columns2 <- standard_detail_columns[c(1:6)] #removed Household_type


Q12_detail <- recent_program_enrollment_allDemograhics %>% 
  select(all_of(standard_detail_columns)) %>%
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
      race_count > 2 ~ "Multiracial – more than 2 races, where no option is Hispanic/Latina/e/o",
      RaceNone %in% c(8, 9) ~ "Client Doesn’t Know/Prefers Not to Answer",
      TRUE ~ "Data Not Collected"
    )
  )



# Tables to compare ----
{

#APR
# APRQ12_1 <- Q12_counts %>% 
#   group_by(race_tabulation) %>%
#   summarise(APR.Total = n()) %>% 
#   filter(race_tabulation != "-",
#          is.na(race_tabulation) == FALSE)
# 
# APRQ12 <- APRQ12 %>% 
#   adorn_totals("row") %>%
#   mutate(APRQ12.per = paste0(round(100* APR.Total/sum(APRQ12$APR.Total),2),'%')) %>%
#   ungroup()

APRQ12 <- Q12_counts %>% 
  group_by(race_tabulation) %>%
  summarise(APR.Total = n()) %>% 
  filter(race_tabulation != "-",
         race_tabulation != "Client Doesn’t Know/Prefers Not to Answer",
         race_tabulation != "Data Not Collected",
         is.na(race_tabulation) == FALSE) %>% 
  adorn_totals("row") %>% 
  mutate(APRQ12.per = paste0(round(100* APR.Total/sum(APR.Total[race_tabulation != "Total"],na.rm=TRUE),2),'%')) %>%
  ungroup()



#Method 1

# M1_method <- Race_detail %>% 
#   group_by(race_list) %>% 
#   summarise(M1.Total = n()) %>%
#   filter(race_list != "") %>% #what is the random blank? Is it the raceNone/missing data elements
#   mutate(M1.per = paste0(round(100* M1.Total/sum(M1.Total),2),'%')) %>%
#   adorn_totals("row") %>%
#   ungroup()

M1_method <- Race_detail %>% 
  group_by(race_list) %>% 
  summarise(M1.Total = n()) %>%
  filter(race_list != "") %>% #what is the random blank? Is it the raceNone/missing data elements
  adorn_totals("row") %>%
  mutate(M1.per = paste0(round(100* M1.Total/sum(M1.Total[race_list != "Total"],
                                                 na.rm = TRUE), 2),'%')) %>%
  ungroup()

# M1_method_1 <- Race_detail %>% 
#   group_by(race_list) %>% 
#   summarise(M1.Total = n()) %>%
#   filter(race_list != "") 
# 
# M1_method <- M1_method_1 %>%
#   adorn_totals("row") %>%
#   mutate(M1.per = paste0(round(100* M1.Total/sum(M1_method_1$M1.Total), 2),'%')) %>%
#   ungroup()


# Method 1 Single/Combination grouping compared to APR ----
M1_APR_compare <-  APRQ12%>% 
  full_join(M1_method, by= c("race_tabulation" = "race_list"))

M1_APR_compare_ordered <- M1_APR_compare[c(1:15,17:19,16,21:23,20),]

View(M1_APR_compare_ordered)

}


# Method 2 Total Response grouping compared to APR ----

APRQ12
M2_method <- recent_program_enrollment_allDemograhics %>% 
  select(PersonalID,unname(race_columns)) %>% 
  mutate(across(
    all_of(unname(race_columns)),
    ~ as.numeric(.))) %>% 
  adorn_totals("row") %>% 
  filter(PersonalID == "Total") %>% 
  t() #transpose, but also changes class to a matrix, switch to pivot? ----

M2_Method_df <- M2_method %>% 
  as.data.frame() %>% 
  rename(Total = V1,
         " " = "race_category") %>% # how do I add a column header to blank?
  filter(Total != "Total")


M2_APR_compare <-  APRQ12%>% 
  full_join(M2_Method_df, by= c("race_tabulation" = "race_category"))





# Initial test one ----

# Logistic regression, predicting exits to Permanent housing (individual based on race/ethnicity)
# Prep the data (personalID, Race, Outcome)
# *** Not sure if this is a useful comparison. ***

Race_detail_glm <- Race_detail %>% 
  select(PersonalID,race_list,Destination) %>% 
  mutate(Binary_Destination = if_else(Destination %in% C(400:499),1,0))

#fit logistic regression model for M1
model <- glm(Binary_Destination ~ race_list, data=Race_detail_glm, family=binomial)

#view model summary
summary(model)


