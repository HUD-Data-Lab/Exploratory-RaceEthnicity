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

#Double check sources to run

source("https://raw.githubusercontent.com/HUD-Data-Lab/DataLab/main/00_read_2024_csv.R")
source("https://raw.githubusercontent.com/HUD-Data-Lab/DataLab/main/DataLab.R")
source("https://raw.githubusercontent.com/HUD-Data-Lab/DataLab/main/DataLab_hc_variables.R")
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
      !is.na(ExitDate)) #%>%
  #add_chronicity_data(.) #fix later

detail_columns <- c("ProjectID","ProjectName","ProjectType","HouseholdID","PersonalID","EnrollmentID","RelationshipToHoH", "EntryDate","ExitDate")

recent_program_enrollment_allDemograhics <- recent_program_enrollment_r %>%
  select(all_of(detail_columns), age_group, HoH_HMID, Destination,MoveInDateAdj,leaver,-household_type) %>%
  left_join(Client %>%
              select(PersonalID, all_of(unname(race_columns)), RaceNone),
            by = "PersonalID") %>% 
  left_join(household_info %>% 
            select(HouseholdID,household_type),
            by = "HouseholdID")

#Set up race details

Race_detail <- recent_program_enrollment_allDemograhics %>% 
  select("ProjectID","ProjectName","ProjectType","PersonalID","EnrollmentID","HouseholdID","RelationshipToHoH","household_type","EntryDate","HoH_HMID",
         "MoveInDateAdj","ExitDate","leaver","Destination","AmIndAKNative","Asian","BlackAfAmerican","HispanicLatinaeo",
         "MidEastNAfrican", "NativeHIPacific","White","RaceNone") %>% 
  mutate(RaceCount = rowSums(across(all_of(unname(race_columns))),na.rm=TRUE)) %>%
  mutate(AmIndAKNative = case_when(AmIndAKNative == 1 ~ "AmIndAKNative", AmIndAKNative == 0 ~ NA),
         Asian = case_when(Asian == 1 ~ "Asian", Asian == 0 ~ NA),
         BlackAfAmerican = case_when(BlackAfAmerican == 1 ~ "BlackAfAmerican", BlackAfAmerican == 0 ~ NA),
         HispanicLatinaeo = case_when(HispanicLatinaeo == 1 ~ "HispanicLatinaeo", HispanicLatinaeo == 0 ~ NA),
         MidEastNAfrican = case_when(MidEastNAfrican == 1 ~ "MidEastNAfrican", MidEastNAfrican == 0 ~ NA),
         NativeHIPacific = case_when(NativeHIPacific == 1 ~ "NativeHIPacific", NativeHIPacific == 0 ~ NA),
         White = case_when(White == 1 ~ "White", White == 0 ~ NA),
         RaceNone= case_when(RaceNone == 8 & RaceCount == 0 ~ "DK/PNTA", RaceNone == 9 & RaceCount == 0 ~ "DK/PNTA", RaceNone == 99 & RaceCount == 0 ~ "Data not Collected")) %>% 
  mutate(race_list = apply(.[c(15:22)], 1, #is there a way to do this from the column select? 
                                    function(x) paste(x[!is.na(x)], collapse = "/")))



#Set up combo details for t.tests

race_columns <- c(AmIndAKNative = "American Indian, Alaska Native, or Indigenous", 
                  Asian = "Asian or Asian American", 
                  BlackAfAmerican = "Black, African American, or African", 
                  HispanicLatinaeo = "Hispanic/Latina/e/o", 
                  MidEastNAfrican = "Middle Eastern & North African", 
                  NativeHIPacific = "Native Hawaiian or Pacific Islander", 
                  White = "White")

race_columns <- setNames(names(race_columns), race_columns) #This adds the full variable names to the race_columns (you can call unname to show full variable name)


# ------------------------------------------------------------------------------
# ----------------------------- Race List --------------------------------------
# ------------------------------------------------------------------------------
# used in:
#   APR/CAPER - Q12a

race_list <- unname(race_columns)  #Saves the list with the abbreviated variable name (i.e., AmIndAKNative)



df <- recent_program_enrollment_allDemograhics


for (race_1 in race_list[1:length(race_list) - 1]) { 
  for (race_2 in race_list[(match(race_1, race_list) + 1):(length(race_list))]) {
    if (!exists("df")) {df <- recent_program_enrollment_allDemograhics}
    df <- df %>%
      mutate(
        !!paste0(race_1, ".", race_2) := get(race_1) == 1 | get(race_2) == 1
      )
    print(paste(race_1, race_2))
  }
}

}














# Time Calcs

LOT_Race_detail <- Race_detail %>%
  add_length_of_time_groups(., EntryDate, 
                            ifnull(ExitDate, ymd(report_end_date) + days(1)),
                            "APR") %>%
  add_length_of_time_groups(., EntryDate, 
                            ifnull(ExitDate, ymd(report_end_date) + days(1)),
                            "CAPER") %>%
  select(-number_of_days.y) %>%
  rename(days_enrolled = number_of_days.x,
         APR_enrollment_length_group = number_of_days_group.x,
         CAPER_enrollment_length_group = number_of_days_group.y)

LOT_Race_Aggregates <- LOT_Race_detail %>% #Program Participation
  group_by(leaver,race_list) %>% 
  summarise(Avg_Days = mean(days_enrolled),
            Median_Days = median(days_enrolled),
            Total_response_total = n())
  
LOT_Race_Exit_Permanent <- LOT_Race_detail %>% #Exits to Permanent Housing Situations
  filter(Destination >= 400 & Destination <=499,
         leaver == TRUE) %>% 
  group_by(race_list) %>% 
  summarise(Avg_Days = mean(days_enrolled),
            Median_Days = median(days_enrolled),
            Total_response_total = n())


#APR Detail Counts

LOT_Race_detail_total <- LOT_Race_detail %>%
  group_by(APR_enrollment_length_group) %>%
  summarise(Total = n_distinct(PersonalID, na.rm = TRUE))

LOT_Race_detail_leavers <- LOT_Race_detail %>%
  filter(!is.na(ExitDate)) %>%
  group_by(APR_enrollment_length_group) %>%
  summarise(Leavers = n_distinct(PersonalID, na.rm = TRUE))

LOT_Race_detail_stayers <- LOT_Race_detail %>%
  filter(is.na(ExitDate)) %>%
  group_by(APR_enrollment_length_group) %>%
  summarise(Stayers = n_distinct(PersonalID, na.rm = TRUE))

LOT_Race <- length_of_time_groups("APR", "APR_enrollment_length_group") %>%
  left_join(LOT_Race_detail_total, by = "APR_enrollment_length_group") %>%
  left_join(LOT_Race_detail_leavers, by = "APR_enrollment_length_group") %>%
  left_join(LOT_Race_detail_stayers, by = "APR_enrollment_length_group") %>%
  adorn_totals("row") %>%
  ifnull(., 0)







# Length of Stay (Pre-set categories)
# Reporting Period "2022-09-30" to "2021-10-01"




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

# APR Pre-selected combinations defined by HUD (AKA Single-comb)

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

#Method 1 Total response (all-inclusive grouping) - Each client represented exactly as identified in dataset

Total_response_method <- Race_detail %>% 
  group_by(race_list) %>% 
  summarise(Total_response_total = n()) %>%
  filter(race_list != "") %>% #what is the random blank? Is it the raceNone/missing data elements
  adorn_totals("row") %>%
  mutate(Total_response_Percent = paste0(round(100* Total_response_total/sum(Total_response_total[race_list != "Total"],
                                                 na.rm = TRUE), 2),'%')) %>%
  ungroup()


# Total Response grouping compared to APR ----
Total_response_APR_compare <-  APRQ12 %>% 
  full_join(Total_response_method, by= c("race_tabulation" = "race_list"))

Total_response_APR_compare_ordered <- Total_response_APR_compare[c(1:18,20:22,19,24:32,23),]

View(Total_response_APR_compare_ordered)


# Multiple_Categories grouping (represented once in each race/ethnicity identified (i.e., Asian/White would be counted in both Asian and white)) ---- 

Multi_cat_method <- recent_program_enrollment_allDemograhics %>% 
  select(unname(race_columns)) %>% 
  mutate(across(
    all_of(unname(race_columns)),
    ~ as.numeric(.))) %>% 
  colSums() %>% 
  as.data.frame() %>%
  setNames("Multi_cat_total") %>% 
  rownames_to_column("Race_Tabulation") %>% 
  adorn_totals() %>% 
  mutate(Multi_cat_percent = paste0(round(100* Multi_cat_total/sum(Multi_cat_total[Race_Tabulation != "Total"],
                                                 na.rm = TRUE), 2),'%'))

Multi_Cat_APR_compare <-  APRQ12%>% 
  full_join(Multi_cat_method, by= c("race_tabulation" = "Race_Tabulation"))

Multi_Cat_APR_compare_ordered <- Multi_Cat_APR_compare[c(1:22,24,23),]

# Single/Combination, Total Response grouping, Mulit_cat join

Comparison_Table <- Total_response_APR_compare_ordered %>% 
  full_join(Multi_cat_method, by= c("race_tabulation" = "Race_Tabulation"))

Comparison_Table_ordered <- Comparison_Table[c(1:21,33,22:32),]

#Make Comparison_Table_ordered a formatted table

gt(Comparison_Table_ordered) |>
  tab_header(
    title = md("**Comparing Racial Categories used in Reporting**"),
    subtitle = md("The three methods commonly used are reporting based on pre-set categories, single combination grouping, total response grouping")
  )

}

# Descriptive Visuals ----





# Bar graphs

# library
library(ggplot2)

# create a dataset
specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)

#
chart_df <- Comparison_Table_ordered %>% 
  select(race_tabulation,APR.Total, Total_response_total) %>% 
  pivot_longer(.,cols= -race_tabulation) %>% 
  filter(race_tabulation != "Total")


# Grouped
ggplot(chart_df, aes(fill=name, y=race_tabulation, x=value)) + 
  geom_col(position = "dodge")


#Statisitical Models ----

# Anova -- 
# Set up Data for import into Gwen's disparities analysis tool

Race_analysis <- Race_detail %>% 
  mutate(PresetCategories = case_when(
    RaceCount > 2 ~ "Multiracial(3 or more Races)",
    TRUE ~ race_list)
  )


#write.csv(Race_analysis, file = "Race_analysis_3orMore.csv")


# Initial test one, using pre-determined categories (APR/CAPER) ----

# Logistic regression, predicting exits to Permanent housing (individual based on race/ethnicity)
# Prep the data (personalID, Race, Outcome)

#Regression Model 1: Pre-set categories predicting exits to Permanent housing (individual based on race/ethnicity)

Race_detail_APR_glm <- Q12_counts %>% 
  select(PersonalID,race_list,Destination) %>% 
  mutate(Binary_Destination = if_else(Destination %in% C(400:499),1,0))

#fit logistic regression model for pre-set categories
Race_detail_APR_model <- glm(Binary_Destination ~ race_list, data=Race_detail_APR_glm, family=binomial)

#view model summary
summary(Race_detail_APR_model)

# Odds Ratios
exp(Race_detail_APR_model$coefficients)
exp(confint(Race_detail_APR_model))

# Get the f-statistic
modelChi_TF.model <- Total_rspnse_Race_detail_model$null.deviance - Total_rspnse_Race_detail_model$deviance # Chi square value
chidf_TF.model <- Total_rspnse_Race_detail_model$df.null - Total_rspnse_Race_detail_model$df.residual       # calculate degrees of freedom for chi-square test
pchisq(modelChi_TF.model, chidf_TF.model, lower.tail = FALSE)      # Use that info to derive p-value

# Evaluate the model

library(ROCit)

class_APR.model <- Race_detail_APR_model$y # extract the outcomes (1s and 0s) from the model
score_APR.model <- qlogis(Race_detail_APR_model$fitted.values) # extract predictions from the model

# Create the curve
roc_APR.model <- rocit(score=score_APR.model,   # predicted outcomes
             class=class_APR.model,   # actual outcomes
             method="bin")  # binomial family

plot(roc_APR.model)    # plot the curve

ciAUC(roc_APR.model)   # Calculate the area under the curve w. confidence interval
# or 
round(ciAUC(roc_APR.model)$AUC, 2) # Just the area under the curve


# Regression Model 2: Total Response (all-inclusive) grouping predicting exits to Permanent housing (individual based on race/ethnicity) ----

Total_rspnse_Race_detail_glm <- Race_detail %>% 
  select(PersonalID,race_list,Destination) %>% 
  mutate(Binary_Destination = if_else(Destination %in% C(400:499),1,0))

#fit logistic regression model for M1
Total_rspnse_Race_detail_model <- glm(Binary_Destination ~ race_list, data=Total_rspnse_Race_detail_glm, family=binomial)

#view model summary
summary(Total_rspnse_Race_detail_model)

plot(Total_rspnse_Race_detail_model)

# Odds Ratios
exp(Total_rspnse_Race_detail_model$coefficients)
exp(confint(Total_rspnse_Race_detail_model))

# Get the f-statistic
modelChi_TF.model <- Total_rspnse_Race_detail_model$null.deviance - Total_rspnse_Race_detail_model$deviance # Chi square value
chidf_TF.model <- Total_rspnse_Race_detail_model$df.null - Total_rspnse_Race_detail_model$df.residual       # calculate degrees of freedom for chi-square test
pchisq(modelChi_TF.model, chidf_TF.model, lower.tail = FALSE)      # Use that info to derive p-value

# Evalaute the model

library(ROCit)

class_TR.model <- Total_rspnse_Race_detail_model$y # extract the outcomes (1s and 0s) from the model
score_TR.model <- qlogis(Total_rspnse_Race_detail_model$fitted.values) # extract predictions from the model

# Create the curve
roc_TR.model <- rocit(score=score_TR.model,   # predicted outcomes
             class=class_TR.model,   # actual outcomes
             method="bin")  # binomial family

plot(roc_TR.model)    # plot the curve

# Area under the curve: The closer to 1 the better.
ciAUC(roc_TR.model)   # Calculate the area under the curve w. confidence interval
# or 
round(ciAUC(roc_TR.model)$AUC, 2) # Just the area under the curve

plot(roc_TR.model)

# Regression Model 3: Multiple categories predicting exits to Permanent housing (individual based on race/ethnicity)

#prep the data, Should we run it this way? Would this change how we evaluate the model?

# Step 1 list all race/ethnicity and outcomes




