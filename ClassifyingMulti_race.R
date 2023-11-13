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


source("https://raw.githubusercontent.com/HUD-Data-Lab/DataLab/main/00_read_2024_csv.R")
source("https://raw.githubusercontent.com/HUD-Data-Lab/DataLab/main/DataLab.R")
library(gt)
library(ROCit)
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
  #filter(ProjectID %in% project_list) %>% # Match project list to APR, comment out for all projects.
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

#Set up Q12 APR totals

#standard_detail_columns2 <- standard_detail_columns[c(1:6)] #removed Household_type


Q12_detail <- recent_program_enrollment_allDemograhics %>% 
  select(all_of(standard_detail_columns),Destination) %>%
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
}


# Tables to compare ----
{

# APR Pre-selected combinations defined by HUD

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

#Method 1 Single/Combination grouping - Each client represented exactly as identified in dataset

Single_Combo_method <- Race_detail %>% 
  group_by(race_list) %>% 
  summarise(Single_Combo_total = n()) %>%
  filter(race_list != "") %>% #what is the random blank? Is it the raceNone/missing data elements
  adorn_totals("row") %>%
  mutate(Single_Combo_percent = paste0(round(100* Single_Combo_total/sum(Single_Combo_total[race_list != "Total"],
                                                 na.rm = TRUE), 2),'%')) %>%
  ungroup()


# Method 1 Single/Combination grouping compared to APR ----
Single_combo_APR_compare <-  APRQ12 %>% 
  full_join(Single_Combo_method, by= c("race_tabulation" = "race_list"))

Single_combo_APR_compare_ordered <- Single_combo_APR_compare[c(1:18,20:22,19,24:32,23),]

View(Single_combo_APR_compare_ordered)



# Method 2 Total Response grouping compared to APR ---- 

Total_Response_method <- recent_program_enrollment_allDemograhics %>% 
  select(unname(race_columns)) %>% 
  mutate(across(
    all_of(unname(race_columns)),
    ~ as.numeric(.))) %>% 
  colSums() %>% 
  as.data.frame() %>%
  setNames("Total_Response_total") %>% 
  rownames_to_column("Race_Tabulation") %>% 
  adorn_totals() %>% 
  mutate(Total_Response_percent = paste0(round(100* Total_Response_total/sum(Total_Response_total[Race_Tabulation != "Total"],
                                                 na.rm = TRUE), 2),'%'))

Total_Response_APR_compare <-  APRQ12%>% 
  full_join(Total_Response_method, by= c("race_tabulation" = "Race_Tabulation"))

Total_Response_APR_compare_ordered <- Total_Response_APR_compare[c(1:22,24,23),]

# APR Joined with Single/Combination and Total Response grouping

Comparison_Table <- Single_combo_APR_compare_ordered %>% 
  full_join(Total_Response_method, by= c("race_tabulation" = "Race_Tabulation"))

Comparison_Table_ordered <- Comparison_Table[c(1:21,33,22:32),]
View(Comparison_Table_ordered)

#Make Comparison_Table_ordered a formatted table

gt(Comparison_Table_ordered) |>
  tab_header(
    title = md("**Comparing Racial Categories used in Reporting**"),
    subtitle = md("The three methods commonly used are reporting based on pre-set categories, single combination grouping, total response grouping")
  )

}

# Initial test one, using pre-determined categories (APR/CAPER) ----

# Logistic regression, predicting exits to Permanent housing (individual based on race/ethnicity)
# Prep the data (personalID, Race, Outcome)

Race_detail_APR_glm <- Q12_counts %>% 
  select(PersonalID,race_list,Destination) %>% 
  mutate(Binary_Destination = if_else(Destination %in% C(400:499),1,0))

#fit logistic regression model for pre-set categories
Race_detail_APR_model <- glm(Binary_Destination ~ race_list, data=Race_detail_APR_glm, family=binomial)

#view model summary
summary(Race_detail_APR_model)


# Initial test two, using Single/Combination grouping ----

Single_combo_glm <- Race_detail %>% 
  select(PersonalID,race_list,Destination) %>% 
  mutate(Binary_Destination = if_else(Destination %in% C(400:499),1,0))

#fit logistic regression model for M1
Single_combo_model <- glm(Binary_Destination ~ race_list, data=Single_combo_glm, family=binomial)

#view model summary
summary(Single_combo_model)

# Odds Ratios
exp(Single_combo_model$coefficients)
exp(confint(Single_combo_model))

# Get the f-statistic
modelChi <- Single_combo_model$null.deviance - Single_combo_model$deviance # Chi square value
chidf <- Single_combo_model$df.null - Single_combo_model$df.residual       # calculate degrees of freedom for chi-square test
pchisq(modelChi, chidf, lower.tail = FALSE)      # Use that info to derive p-value

# Evaluate the model

library(ROCit)

class <- Single_combo_model$y # extract the outcomes (1s and 0s) from the model
score <- qlogis(Single_combo_model$fitted.values) # extract predictions from the model

# Create the curve
roc <- rocit(score=score,   # predicted outcomes
             class=class,   # actual outcomes
             method="bin")  # binomial family

plot(roc)    # plot the curve

ciAUC(roc)   # Calculate the area under the curve w. confidence interval
# or 
round(ciAUC(roc)$AUC, 2) # Just the area under the curve

plot(Single_combo_model)


# Initial test two, using Single/Combination grouping ----

Race_detail_glm <- Race_detail %>% 
  select(PersonalID,race_list,Destination) %>% 
  mutate(Binary_Destination = if_else(Destination %in% C(400:499),1,0))

#fit logistic regression model for M1
model <- glm(Binary_Destination ~ race_list, data=Race_detail_glm, family=binomial)

#view model summary
summary(model)

plot(model)






