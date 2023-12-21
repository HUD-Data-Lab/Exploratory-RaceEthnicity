
library(networkD3)
options(scipen=999)
set.seed(2024)

library(ggpubr)
library(broom)
library(AICcmodavg)

# Total response -- all categories are represented in all combinations

#Disparities analysis
## Compare to general population (we don't have)
## Descriptive (who is in the system) -- By project type and compare disparities
## Compare Program participation length of time to permanent exit

#Source("CLassifyingMulti_race.R)

#Overall Race and ethnicity ----
# Only using the following project types: ES (and ES NbN), RRH, TH, PH
# ES(0,1); RRH (13); TH(2); PH(3,9,10)


Race_Report_Range_Detail <- Race_detail %>% 
  filter(ProjectType %in% c(0,1,2,3,9,10,13),
         EntryDate <= report_end_date & (is.na(ExitDate) | ExitDate >= report_start_date)) %>% 
  mutate(ProjectType_name= case_when(
    ProjectType == 0 | ProjectType == 1 ~ "ES",
    ProjectType == 2 ~ "TH",
    ProjectType == 13 ~ "RRH",
    ProjectType == 3 | ProjectType == 9 | ProjectType == 10  ~ "PH"
  ))

TR_Count_Overall <- Race_Report_Range_Detail %>% 
  group_by(race_list) %>% 
  summarise(Total_response_total = n()) %>%
  mutate(Total_response_Percent = paste0(round(100* Total_response_total/sum(Total_response_total[race_list != "Total"],
                                                                             na.rm = TRUE), 2),'%')) %>%
  ungroup()

# Set up for Gwen's Simple Race Equity tool
# Race_analysis <- Race_detail %>% 
#   filter(ProjectType %in% c(0,1,2,3,9,10,13),
#          EntryDate <= report_end_date & (is.na(ExitDate) | ExitDate >= report_start_date)) %>%
#   mutate(PresetCategories = case_when(
#     RaceCount > 2 ~ "Multiracial(3 or more Races)",
#     TRUE ~ race_list)
#   )
# 
# write.csv(Race_analysis, file = "output/Race_Equity_tool/Race_analysis_3orMore_12.04.23.csv")

#Race and Ethnicity By project type ----

TR_Count_ProjectType <- Race_Report_Range_Detail %>% 
  group_by(ProjectType_name,race_list) %>% 
  summarise(Total_response_total = n()) %>%
  mutate(Total_response_Percent = paste0(round(100* Total_response_total/sum(Total_response_total[race_list != "Total"],
                                                                             na.rm = TRUE), 2),'%')) %>%
  ungroup()

# Compare Project Type to overall Population ----
# Null hypothesis there is no difference from the overall distribution of racial identity to project type X

T.Tests_Standard <- Race_Report_Range_Detail %>% 
  group_by(race_list) %>% 
  summarise(Total_response_total = n()) %>%
  mutate(Total_response_Percent = paste0(round(Total_response_total/sum(Total_response_total[race_list != "Total"],
                                                                             na.rm = TRUE), 4))) %>%
  ungroup()

T.Tests_Detail <- Race_Report_Range_Detail %>% 
  select(PersonalID,ProjectType_name,Destination,race_list) %>% 
  mutate("AmIndAKNative" =case_when(race_list == "AmIndAKNative"~1, TRUE ~0), #### This could be a loop *************
         "AmIndAKNative/Asian/HispanicLatinaeo" =case_when(race_list == "AmIndAKNative/Asian/HispanicLatinaeo"~1, TRUE ~0),
         "AmIndAKNative/BlackAfAmerican" =case_when(race_list == "AmIndAKNative/BlackAfAmerican"~1, TRUE ~0),
         "AmIndAKNative/BlackAfAmerican/HispanicLatinaeo" =case_when(race_list == "AmIndAKNative/BlackAfAmerican/HispanicLatinaeo"~1, TRUE ~0),
         "AmIndAKNative/HispanicLatinaeo" =case_when(race_list == "AmIndAKNative/HispanicLatinaeo"~1, TRUE ~0),
         "AmIndAKNative/HispanicLatinaeo/NativeHIPacific" =case_when(race_list == "AmIndAKNative/HispanicLatinaeo/NativeHIPacific"~1, TRUE ~0),
         "AmIndAKNative/HispanicLatinaeo/White" =case_when(race_list == "AmIndAKNative/HispanicLatinaeo/White"~1, TRUE ~0),
         "AmIndAKNative/NativeHIPacific" =case_when(race_list == "AmIndAKNative/NativeHIPacific"~1, TRUE ~0),
         "AmIndAKNative/White" =case_when(race_list == "AmIndAKNative/White"~1, TRUE ~0),
         "Asian" =case_when(race_list == "Asian"~1, TRUE ~0),
         "Asian/BlackAfAmerican" =case_when(race_list == "Asian/BlackAfAmerican"~1, TRUE ~0),
         "Asian/BlackAfAmerican/HispanicLatinaeo" =case_when(race_list == "Asian/BlackAfAmerican/HispanicLatinaeo"~1, TRUE ~0),
         "Asian/HispanicLatinaeo" =case_when(race_list == "Asian/HispanicLatinaeo"~1, TRUE ~0),
         "Asian/HispanicLatinaeo/NativeHIPacific" =case_when(race_list == "Asian/HispanicLatinaeo/NativeHIPacific"~1, TRUE ~0),
         "Asian/HispanicLatinaeo/White" =case_when(race_list == "Asian/HispanicLatinaeo/White"~1, TRUE ~0),
         "Asian/White" =case_when(race_list == "Asian/White"~1, TRUE ~0),
         "BlackAfAmerican" =case_when(race_list == "BlackAfAmerican"~1, TRUE ~0),
         "BlackAfAmerican/HispanicLatinaeo" =case_when(race_list == "BlackAfAmerican/HispanicLatinaeo"~1, TRUE ~0),
         "BlackAfAmerican/HispanicLatinaeo/NativeHIPacific" =case_when(race_list == "BlackAfAmerican/HispanicLatinaeo/NativeHIPacific"~1, TRUE ~0),
         "BlackAfAmerican/HispanicLatinaeo/White" =case_when(race_list == "BlackAfAmerican/HispanicLatinaeo/White"~1, TRUE ~0),
         "BlackAfAmerican/NativeHIPacific" =case_when(race_list == "BlackAfAmerican/NativeHIPacific"~1, TRUE ~0),
         "BlackAfAmerican/White" =case_when(race_list == "BlackAfAmerican/White"~1, TRUE ~0),
         "Data not Collected" =case_when(race_list == "Data not Collected"~1, TRUE ~0),
         "DK/PNTA" =case_when(race_list == "DK/PNTA"~1, TRUE ~0),
         "HispanicLatinaeo" =case_when(race_list == "HispanicLatinaeo"~1, TRUE ~0),
         "HispanicLatinaeo/NativeHIPacific" =case_when(race_list == "HispanicLatinaeo/NativeHIPacific"~1, TRUE ~0),
         "HispanicLatinaeo/White" =case_when(race_list == "HispanicLatinaeo/White"~1, TRUE ~0),
         "NativeHIPacific" =case_when(race_list == "NativeHIPacific"~1, TRUE ~0),
         "NativeHIPacific/White" =case_when(race_list == "NativeHIPacific/White"~1, TRUE ~0),
         "White" =case_when(race_list == "White"~1, TRUE ~0)
  )

{#Block for running access by Project type t-test
  
# Test for Disparities in access to ES
ES.T.Tests <- T.Tests_Detail %>% 
  filter(ProjectType_name == "ES")

ES.T.Tests.AmIndAKNative<-t.test(ES.T.Tests$`AmIndAKNative`, mu =0.0155)
ES.T.Tests.AmIndAKNative.Asian.HispanicLatinaeo<-t.test(ES.T.Tests$`AmIndAKNative/Asian/HispanicLatinaeo`, mu =0.0002)
ES.T.Tests.AmIndAKNative.BlackAfAmerican<-t.test(ES.T.Tests$`AmIndAKNative/BlackAfAmerican`, mu =0.0063)
ES.T.Tests.AmIndAKNative.BlackAfAmerican.HispanicLatinaeo<-t.test(ES.T.Tests$`AmIndAKNative/BlackAfAmerican/HispanicLatinaeo`, mu =0.0014)
ES.T.Tests.AmIndAKNative.HispanicLatinaeo<-t.test(ES.T.Tests$`AmIndAKNative/HispanicLatinaeo`, mu =0.003)
ES.T.Tests.AmIndAKNative.HispanicLatinaeo.NativeHIPacific<-t.test(ES.T.Tests$`AmIndAKNative/HispanicLatinaeo/NativeHIPacific`, mu =0.0002)
ES.T.Tests.AmIndAKNative.HispanicLatinaeo.White<-t.test(ES.T.Tests$`AmIndAKNative/HispanicLatinaeo/White`, mu =0.0006)
ES.T.Tests.AmIndAKNative.NativeHIPacific<-t.test(ES.T.Tests$`AmIndAKNative/NativeHIPacific`, mu =0.0002)
ES.T.Tests.AmIndAKNative.White<-t.test(ES.T.Tests$`AmIndAKNative/White`, mu =0.0123)
ES.T.Tests.Asian<-t.test(ES.T.Tests$`Asian`, mu =0.004)
ES.T.Tests.Asian.BlackAfAmerican<-t.test(ES.T.Tests$`Asian/BlackAfAmerican`, mu =0.0011)
ES.T.Tests.Asian.BlackAfAmerican.HispanicLatinaeo<-t.test(ES.T.Tests$`Asian/BlackAfAmerican/HispanicLatinaeo`, mu =0.0003)
ES.T.Tests.Asian.HispanicLatinaeo<-t.test(ES.T.Tests$`Asian/HispanicLatinaeo`, mu =0.0009)
ES.T.Tests.Asian.HispanicLatinaeo.NativeHIPacific<-t.test(ES.T.Tests$`Asian/HispanicLatinaeo/NativeHIPacific`, mu =0.0002)
ES.T.Tests.Asian.HispanicLatinaeo.White<-t.test(ES.T.Tests$`Asian/HispanicLatinaeo/White`, mu =0.0002)
ES.T.Tests.Asian.White<-t.test(ES.T.Tests$`Asian/White`, mu =0.0023)
ES.T.Tests.BlackAfAmerican<-t.test(ES.T.Tests$`BlackAfAmerican`, mu =0.2642)
ES.T.Tests.BlackAfAmerican.HispanicLatinaeo<-t.test(ES.T.Tests$`BlackAfAmerican/HispanicLatinaeo`, mu =0.009)
ES.T.Tests.BlackAfAmerican.HispanicLatinaeo.NativeHIPacific<-t.test(ES.T.Tests$`BlackAfAmerican/HispanicLatinaeo/NativeHIPacific`, mu =0.0002)
ES.T.Tests.BlackAfAmerican.HispanicLatinaeo.White<-t.test(ES.T.Tests$`BlackAfAmerican/HispanicLatinaeo/White`, mu =0.0027)
ES.T.Tests.BlackAfAmerican.NativeHIPacific<-t.test(ES.T.Tests$`BlackAfAmerican/NativeHIPacific`, mu =0.0008)
ES.T.Tests.BlackAfAmerican.White<-t.test(ES.T.Tests$`BlackAfAmerican/White`, mu =0.0462)
ES.T.Tests.DK.PNTA<-t.test(ES.T.Tests$`DK/PNTA`, mu =0.0044)
ES.T.Tests.Data.not.Collected<-t.test(ES.T.Tests$`Data not Collected`, mu =0.0032)
ES.T.Tests.HispanicLatinaeo<-t.test(ES.T.Tests$`HispanicLatinaeo`, mu =0.0023)
ES.T.Tests.HispanicLatinaeo.NativeHIPacific<-t.test(ES.T.Tests$`HispanicLatinaeo/NativeHIPacific`, mu =0.0006)
ES.T.Tests.HispanicLatinaeo.White<-t.test(ES.T.Tests$`HispanicLatinaeo/White`, mu =0.0575)
ES.T.Tests.NativeHIPacific<-t.test(ES.T.Tests$`NativeHIPacific`, mu =0.004)
ES.T.Tests.NativeHIPacific.White<-t.test(ES.T.Tests$`NativeHIPacific/White`, mu =0.0008)
ES.T.Tests.White<-t.test(ES.T.Tests$`White`, mu =0.5558)

# ES T-test Results

ES.T_results.AmIndAKNative<-c("AmIndAKNative",ES.T.Tests.AmIndAKNative$p.value,ES.T.Tests.AmIndAKNative$conf.int[c(1:2)])
ES.T_results.AmIndAKNative.Asian.HispanicLatinaeo<-c("AmIndAKNative/Asian/HispanicLatinaeo",ES.T.Tests.AmIndAKNative.Asian.HispanicLatinaeo$p.value,ES.T.Tests.AmIndAKNative.Asian.HispanicLatinaeo$conf.int[c(1:2)])
ES.T_results.AmIndAKNative.BlackAfAmerican<-c("AmIndAKNative/BlackAfAmerican",ES.T.Tests.AmIndAKNative.BlackAfAmerican$p.value,ES.T.Tests.AmIndAKNative.BlackAfAmerican$conf.int[c(1:2)])
ES.T_results.AmIndAKNative.BlackAfAmerican.HispanicLatinaeo<-c("AmIndAKNative/BlackAfAmerican/HispanicLatinaeo",ES.T.Tests.AmIndAKNative.BlackAfAmerican.HispanicLatinaeo$p.value,ES.T.Tests.AmIndAKNative.BlackAfAmerican.HispanicLatinaeo$conf.int[c(1:2)])
ES.T_results.AmIndAKNative.HispanicLatinaeo<-c("AmIndAKNative/HispanicLatinaeo",ES.T.Tests.AmIndAKNative.HispanicLatinaeo$p.value,ES.T.Tests.AmIndAKNative.HispanicLatinaeo$conf.int[c(1:2)])
ES.T_results.AmIndAKNative.HispanicLatinaeo.NativeHIPacific<-c("AmIndAKNative/HispanicLatinaeo/NativeHIPacific",ES.T.Tests.AmIndAKNative.HispanicLatinaeo.NativeHIPacific$p.value,ES.T.Tests.AmIndAKNative.HispanicLatinaeo.NativeHIPacific$conf.int[c(1:2)])
ES.T_results.AmIndAKNative.HispanicLatinaeo.White<-c("AmIndAKNative/HispanicLatinaeo/White",ES.T.Tests.AmIndAKNative.HispanicLatinaeo.White$p.value,ES.T.Tests.AmIndAKNative.HispanicLatinaeo.White$conf.int[c(1:2)])
ES.T_results.AmIndAKNative.NativeHIPacific<-c("AmIndAKNative/NativeHIPacific",ES.T.Tests.AmIndAKNative.NativeHIPacific$p.value,ES.T.Tests.AmIndAKNative.NativeHIPacific$conf.int[c(1:2)])
ES.T_results.AmIndAKNative.White<-c("AmIndAKNative/White",ES.T.Tests.AmIndAKNative.White$p.value,ES.T.Tests.AmIndAKNative.White$conf.int[c(1:2)])
ES.T_results.Asian<-c("Asian",ES.T.Tests.Asian$p.value,ES.T.Tests.Asian$conf.int[c(1:2)])
ES.T_results.Asian.BlackAfAmerican<-c("Asian/BlackAfAmerican",ES.T.Tests.Asian.BlackAfAmerican$p.value,ES.T.Tests.Asian.BlackAfAmerican$conf.int[c(1:2)])
ES.T_results.Asian.BlackAfAmerican.HispanicLatinaeo<-c("Asian/BlackAfAmerican/HispanicLatinaeo",ES.T.Tests.Asian.BlackAfAmerican.HispanicLatinaeo$p.value,ES.T.Tests.Asian.BlackAfAmerican.HispanicLatinaeo$conf.int[c(1:2)])
ES.T_results.Asian.HispanicLatinaeo<-c("Asian/HispanicLatinaeo",ES.T.Tests.Asian.HispanicLatinaeo$p.value,ES.T.Tests.Asian.HispanicLatinaeo$conf.int[c(1:2)])
ES.T_results.Asian.HispanicLatinaeo.NativeHIPacific<-c("Asian/HispanicLatinaeo/NativeHIPacific",ES.T.Tests.Asian.HispanicLatinaeo.NativeHIPacific$p.value,ES.T.Tests.Asian.HispanicLatinaeo.NativeHIPacific$conf.int[c(1:2)])
ES.T_results.Asian.HispanicLatinaeo.White<-c("Asian/HispanicLatinaeo/White",ES.T.Tests.Asian.HispanicLatinaeo.White$p.value,ES.T.Tests.Asian.HispanicLatinaeo.White$conf.int[c(1:2)])
ES.T_results.Asian.White<-c("Asian/White",ES.T.Tests.Asian.White$p.value,ES.T.Tests.Asian.White$conf.int[c(1:2)])
ES.T_results.BlackAfAmerican<-c("BlackAfAmerican",ES.T.Tests.BlackAfAmerican$p.value,ES.T.Tests.BlackAfAmerican$conf.int[c(1:2)])
ES.T_results.BlackAfAmerican.HispanicLatinaeo<-c("BlackAfAmerican/HispanicLatinaeo",ES.T.Tests.BlackAfAmerican.HispanicLatinaeo$p.value,ES.T.Tests.BlackAfAmerican.HispanicLatinaeo$conf.int[c(1:2)])
ES.T_results.BlackAfAmerican.HispanicLatinaeo.NativeHIPacific<-c("BlackAfAmerican/HispanicLatinaeo/NativeHIPacific",ES.T.Tests.BlackAfAmerican.HispanicLatinaeo.NativeHIPacific$p.value,ES.T.Tests.BlackAfAmerican.HispanicLatinaeo.NativeHIPacific$conf.int[c(1:2)])
ES.T_results.BlackAfAmerican.HispanicLatinaeo.White<-c("BlackAfAmerican/HispanicLatinaeo/White",ES.T.Tests.BlackAfAmerican.HispanicLatinaeo.White$p.value,ES.T.Tests.BlackAfAmerican.HispanicLatinaeo.White$conf.int[c(1:2)])
ES.T_results.BlackAfAmerican.NativeHIPacific<-c("BlackAfAmerican/NativeHIPacific",ES.T.Tests.BlackAfAmerican.NativeHIPacific$p.value,ES.T.Tests.BlackAfAmerican.NativeHIPacific$conf.int[c(1:2)])
ES.T_results.BlackAfAmerican.White<-c("BlackAfAmerican/White",ES.T.Tests.BlackAfAmerican.White$p.value,ES.T.Tests.BlackAfAmerican.White$conf.int[c(1:2)])
ES.T_results.DK.PNTA<-c("DK/PNTA",ES.T.Tests.DK.PNTA$p.value,ES.T.Tests.DK.PNTA$conf.int[c(1:2)])
ES.T_results.Data.not.Collected<-c("Data not Collected",ES.T.Tests.Data.not.Collected$p.value,ES.T.Tests.Data.not.Collected$conf.int[c(1:2)])
ES.T_results.HispanicLatinaeo<-c("HispanicLatinaeo",ES.T.Tests.HispanicLatinaeo$p.value,ES.T.Tests.HispanicLatinaeo$conf.int[c(1:2)])
ES.T_results.HispanicLatinaeo.NativeHIPacific<-c("HispanicLatinaeo/NativeHIPacific",ES.T.Tests.HispanicLatinaeo.NativeHIPacific$p.value,ES.T.Tests.HispanicLatinaeo.NativeHIPacific$conf.int[c(1:2)])
ES.T_results.HispanicLatinaeo.White<-c("HispanicLatinaeo/White",ES.T.Tests.HispanicLatinaeo.White$p.value,ES.T.Tests.HispanicLatinaeo.White$conf.int[c(1:2)])
ES.T_results.NativeHIPacific<-c("NativeHIPacific",ES.T.Tests.NativeHIPacific$p.value,ES.T.Tests.NativeHIPacific$conf.int[c(1:2)])
ES.T_results.NativeHIPacific.White<-c("NativeHIPacific/White",ES.T.Tests.NativeHIPacific.White$p.value,ES.T.Tests.NativeHIPacific.White$conf.int[c(1:2)])
ES.T_results.White<-c("White",ES.T.Tests.White$p.value,ES.T.Tests.White$conf.int[c(1:2)])

df <- data.frame("race_list" = 1,
                 "ES.P.Value" = 1 ,
                 "ES.Confidence.Interval.Low" = 1,
                 "ES.Confidence.Interval.High" = 1)

df_ES.Results <- rbind(df,ES.T_results.AmIndAKNative,ES.T_results.AmIndAKNative.Asian.HispanicLatinaeo,
      ES.T_results.AmIndAKNative.BlackAfAmerican,ES.T_results.AmIndAKNative.BlackAfAmerican.HispanicLatinaeo,
      ES.T_results.AmIndAKNative.HispanicLatinaeo,ES.T_results.AmIndAKNative.HispanicLatinaeo.NativeHIPacific,
      ES.T_results.AmIndAKNative.HispanicLatinaeo.White,ES.T_results.AmIndAKNative.NativeHIPacific,
      ES.T_results.AmIndAKNative.White,ES.T_results.Asian,ES.T_results.Asian.BlackAfAmerican,
      ES.T_results.Asian.BlackAfAmerican.HispanicLatinaeo,ES.T_results.Asian.HispanicLatinaeo,
      ES.T_results.Asian.HispanicLatinaeo.NativeHIPacific,ES.T_results.Asian.HispanicLatinaeo.White,ES.T_results.Asian.White,
      ES.T_results.BlackAfAmerican,ES.T_results.BlackAfAmerican.HispanicLatinaeo,
      ES.T_results.BlackAfAmerican.HispanicLatinaeo.NativeHIPacific,ES.T_results.BlackAfAmerican.HispanicLatinaeo.White,
      ES.T_results.BlackAfAmerican.NativeHIPacific,ES.T_results.BlackAfAmerican.White,ES.T_results.DK.PNTA,
      ES.T_results.Data.not.Collected,ES.T_results.HispanicLatinaeo,
      ES.T_results.HispanicLatinaeo.NativeHIPacific,ES.T_results.HispanicLatinaeo.White,
      ES.T_results.NativeHIPacific,ES.T_results.NativeHIPacific.White,ES.T_results.White,make.row.names = FALSE)
      
df_ES.Results<- df_ES.Results[-1,] #remove dummmy row one

#ES Join the results to the overall table

TR_Count_ES_Overall <- T.Tests_Detail %>%
  filter(ProjectType_name  == "ES") %>% 
  group_by(race_list) %>% 
  summarise(ES_total = n()) %>%
  mutate(ES_Percent = paste0(round(100* ES_total/sum(ES_total[race_list != "Total"],
                                                                             na.rm = TRUE), 2),'%')) %>%
  ungroup()


ES_Compare_Overall <-  TR_Count_Overall %>% 
  left_join(df_ES.Results,by="race_list") %>% 
  left_join(TR_Count_ES_Overall,by="race_list") %>% 
  mutate(ES.Significant = case_when(P.Value <= .05 & Confidence.Interval.Low != "NaN" ~ "Significant", TRUE ~ "Not Significant")) %>% 
  select(race_list,Total_response_total,Total_response_Percent,ES_total,ES_Percent,ES.P.Value,ES.Confidence.Interval.Low,ES.Confidence.Interval.High,ES.Significant)


# Test for Disparities in access to RRH
RRH.T.Tests <- T.Tests_Detail %>% 
  filter(ProjectType_name == "RRH")

RRH.T.Tests.AmIndAKNative<-t.test(RRH.T.Tests$`AmIndAKNative`, mu =0.0155)
RRH.T.Tests.AmIndAKNative.Asian.HispanicLatinaeo<-t.test(RRH.T.Tests$`AmIndAKNative/Asian/HispanicLatinaeo`, mu =0.0002)
RRH.T.Tests.AmIndAKNative.BlackAfAmerican<-t.test(RRH.T.Tests$`AmIndAKNative/BlackAfAmerican`, mu =0.0063)
RRH.T.Tests.AmIndAKNative.BlackAfAmerican.HispanicLatinaeo<-t.test(RRH.T.Tests$`AmIndAKNative/BlackAfAmerican/HispanicLatinaeo`, mu =0.0014)
RRH.T.Tests.AmIndAKNative.HispanicLatinaeo<-t.test(RRH.T.Tests$`AmIndAKNative/HispanicLatinaeo`, mu =0.003)
RRH.T.Tests.AmIndAKNative.HispanicLatinaeo.NativeHIPacific<-t.test(RRH.T.Tests$`AmIndAKNative/HispanicLatinaeo/NativeHIPacific`, mu =0.0002)
RRH.T.Tests.AmIndAKNative.HispanicLatinaeo.White<-t.test(RRH.T.Tests$`AmIndAKNative/HispanicLatinaeo/White`, mu =0.0006)
RRH.T.Tests.AmIndAKNative.NativeHIPacific<-t.test(RRH.T.Tests$`AmIndAKNative/NativeHIPacific`, mu =0.0002)
RRH.T.Tests.AmIndAKNative.White<-t.test(RRH.T.Tests$`AmIndAKNative/White`, mu =0.0123)
RRH.T.Tests.Asian<-t.test(RRH.T.Tests$`Asian`, mu =0.004)
RRH.T.Tests.Asian.BlackAfAmerican<-t.test(RRH.T.Tests$`Asian/BlackAfAmerican`, mu =0.0011)
RRH.T.Tests.Asian.BlackAfAmerican.HispanicLatinaeo<-t.test(RRH.T.Tests$`Asian/BlackAfAmerican/HispanicLatinaeo`, mu =0.0003)
RRH.T.Tests.Asian.HispanicLatinaeo<-t.test(RRH.T.Tests$`Asian/HispanicLatinaeo`, mu =0.0009)
RRH.T.Tests.Asian.HispanicLatinaeo.NativeHIPacific<-t.test(RRH.T.Tests$`Asian/HispanicLatinaeo/NativeHIPacific`, mu =0.0002)
RRH.T.Tests.Asian.HispanicLatinaeo.White<-t.test(RRH.T.Tests$`Asian/HispanicLatinaeo/White`, mu =0.0002)
RRH.T.Tests.Asian.White<-t.test(RRH.T.Tests$`Asian/White`, mu =0.0023)
RRH.T.Tests.BlackAfAmerican<-t.test(RRH.T.Tests$`BlackAfAmerican`, mu =0.2642)
RRH.T.Tests.BlackAfAmerican.HispanicLatinaeo<-t.test(RRH.T.Tests$`BlackAfAmerican/HispanicLatinaeo`, mu =0.009)
RRH.T.Tests.BlackAfAmerican.HispanicLatinaeo.NativeHIPacific<-t.test(RRH.T.Tests$`BlackAfAmerican/HispanicLatinaeo/NativeHIPacific`, mu =0.0002)
RRH.T.Tests.BlackAfAmerican.HispanicLatinaeo.White<-t.test(RRH.T.Tests$`BlackAfAmerican/HispanicLatinaeo/White`, mu =0.0027)
RRH.T.Tests.BlackAfAmerican.NativeHIPacific<-t.test(RRH.T.Tests$`BlackAfAmerican/NativeHIPacific`, mu =0.0008)
RRH.T.Tests.BlackAfAmerican.White<-t.test(RRH.T.Tests$`BlackAfAmerican/White`, mu =0.0462)
RRH.T.Tests.DK.PNTA<-t.test(RRH.T.Tests$`DK/PNTA`, mu =0.0044)
RRH.T.Tests.Data.not.Collected<-t.test(RRH.T.Tests$`Data not Collected`, mu =0.0032)
RRH.T.Tests.HispanicLatinaeo<-t.test(RRH.T.Tests$`HispanicLatinaeo`, mu =0.0023)
RRH.T.Tests.HispanicLatinaeo.NativeHIPacific<-t.test(RRH.T.Tests$`HispanicLatinaeo/NativeHIPacific`, mu =0.0006)
RRH.T.Tests.HispanicLatinaeo.White<-t.test(RRH.T.Tests$`HispanicLatinaeo/White`, mu =0.0575)
RRH.T.Tests.NativeHIPacific<-t.test(RRH.T.Tests$`NativeHIPacific`, mu =0.004)
RRH.T.Tests.NativeHIPacific.White<-t.test(RRH.T.Tests$`NativeHIPacific/White`, mu =0.0008)
RRH.T.Tests.White<-t.test(RRH.T.Tests$`White`, mu =0.5558)

# RRH T-test Results

RRH.T_results.AmIndAKNative<-c("AmIndAKNative",RRH.T.Tests.AmIndAKNative$p.value,RRH.T.Tests.AmIndAKNative$conf.int[c(1:2)])
RRH.T_results.AmIndAKNative.Asian.HispanicLatinaeo<-c("AmIndAKNative/Asian/HispanicLatinaeo",RRH.T.Tests.AmIndAKNative.Asian.HispanicLatinaeo$p.value,RRH.T.Tests.AmIndAKNative.Asian.HispanicLatinaeo$conf.int[c(1:2)])
RRH.T_results.AmIndAKNative.BlackAfAmerican<-c("AmIndAKNative/BlackAfAmerican",RRH.T.Tests.AmIndAKNative.BlackAfAmerican$p.value,RRH.T.Tests.AmIndAKNative.BlackAfAmerican$conf.int[c(1:2)])
RRH.T_results.AmIndAKNative.BlackAfAmerican.HispanicLatinaeo<-c("AmIndAKNative/BlackAfAmerican/HispanicLatinaeo",RRH.T.Tests.AmIndAKNative.BlackAfAmerican.HispanicLatinaeo$p.value,RRH.T.Tests.AmIndAKNative.BlackAfAmerican.HispanicLatinaeo$conf.int[c(1:2)])
RRH.T_results.AmIndAKNative.HispanicLatinaeo<-c("AmIndAKNative/HispanicLatinaeo",RRH.T.Tests.AmIndAKNative.HispanicLatinaeo$p.value,RRH.T.Tests.AmIndAKNative.HispanicLatinaeo$conf.int[c(1:2)])
RRH.T_results.AmIndAKNative.HispanicLatinaeo.NativeHIPacific<-c("AmIndAKNative/HispanicLatinaeo/NativeHIPacific",RRH.T.Tests.AmIndAKNative.HispanicLatinaeo.NativeHIPacific$p.value,RRH.T.Tests.AmIndAKNative.HispanicLatinaeo.NativeHIPacific$conf.int[c(1:2)])
RRH.T_results.AmIndAKNative.HispanicLatinaeo.White<-c("AmIndAKNative/HispanicLatinaeo/White",RRH.T.Tests.AmIndAKNative.HispanicLatinaeo.White$p.value,RRH.T.Tests.AmIndAKNative.HispanicLatinaeo.White$conf.int[c(1:2)])
RRH.T_results.AmIndAKNative.NativeHIPacific<-c("AmIndAKNative/NativeHIPacific",RRH.T.Tests.AmIndAKNative.NativeHIPacific$p.value,RRH.T.Tests.AmIndAKNative.NativeHIPacific$conf.int[c(1:2)])
RRH.T_results.AmIndAKNative.White<-c("AmIndAKNative/White",RRH.T.Tests.AmIndAKNative.White$p.value,RRH.T.Tests.AmIndAKNative.White$conf.int[c(1:2)])
RRH.T_results.Asian<-c("Asian",RRH.T.Tests.Asian$p.value,RRH.T.Tests.Asian$conf.int[c(1:2)])
RRH.T_results.Asian.BlackAfAmerican<-c("Asian/BlackAfAmerican",RRH.T.Tests.Asian.BlackAfAmerican$p.value,RRH.T.Tests.Asian.BlackAfAmerican$conf.int[c(1:2)])
RRH.T_results.Asian.BlackAfAmerican.HispanicLatinaeo<-c("Asian/BlackAfAmerican/HispanicLatinaeo",RRH.T.Tests.Asian.BlackAfAmerican.HispanicLatinaeo$p.value,RRH.T.Tests.Asian.BlackAfAmerican.HispanicLatinaeo$conf.int[c(1:2)])
RRH.T_results.Asian.HispanicLatinaeo<-c("Asian/HispanicLatinaeo",RRH.T.Tests.Asian.HispanicLatinaeo$p.value,RRH.T.Tests.Asian.HispanicLatinaeo$conf.int[c(1:2)])
RRH.T_results.Asian.HispanicLatinaeo.NativeHIPacific<-c("Asian/HispanicLatinaeo/NativeHIPacific",RRH.T.Tests.Asian.HispanicLatinaeo.NativeHIPacific$p.value,RRH.T.Tests.Asian.HispanicLatinaeo.NativeHIPacific$conf.int[c(1:2)])
RRH.T_results.Asian.HispanicLatinaeo.White<-c("Asian/HispanicLatinaeo/White",RRH.T.Tests.Asian.HispanicLatinaeo.White$p.value,RRH.T.Tests.Asian.HispanicLatinaeo.White$conf.int[c(1:2)])
RRH.T_results.Asian.White<-c("Asian/White",RRH.T.Tests.Asian.White$p.value,RRH.T.Tests.Asian.White$conf.int[c(1:2)])
RRH.T_results.BlackAfAmerican<-c("BlackAfAmerican",RRH.T.Tests.BlackAfAmerican$p.value,RRH.T.Tests.BlackAfAmerican$conf.int[c(1:2)])
RRH.T_results.BlackAfAmerican.HispanicLatinaeo<-c("BlackAfAmerican/HispanicLatinaeo",RRH.T.Tests.BlackAfAmerican.HispanicLatinaeo$p.value,RRH.T.Tests.BlackAfAmerican.HispanicLatinaeo$conf.int[c(1:2)])
RRH.T_results.BlackAfAmerican.HispanicLatinaeo.NativeHIPacific<-c("BlackAfAmerican/HispanicLatinaeo/NativeHIPacific",RRH.T.Tests.BlackAfAmerican.HispanicLatinaeo.NativeHIPacific$p.value,RRH.T.Tests.BlackAfAmerican.HispanicLatinaeo.NativeHIPacific$conf.int[c(1:2)])
RRH.T_results.BlackAfAmerican.HispanicLatinaeo.White<-c("BlackAfAmerican/HispanicLatinaeo/White",RRH.T.Tests.BlackAfAmerican.HispanicLatinaeo.White$p.value,RRH.T.Tests.BlackAfAmerican.HispanicLatinaeo.White$conf.int[c(1:2)])
RRH.T_results.BlackAfAmerican.NativeHIPacific<-c("BlackAfAmerican/NativeHIPacific",RRH.T.Tests.BlackAfAmerican.NativeHIPacific$p.value,RRH.T.Tests.BlackAfAmerican.NativeHIPacific$conf.int[c(1:2)])
RRH.T_results.BlackAfAmerican.White<-c("BlackAfAmerican/White",RRH.T.Tests.BlackAfAmerican.White$p.value,RRH.T.Tests.BlackAfAmerican.White$conf.int[c(1:2)])
RRH.T_results.DK.PNTA<-c("DK/PNTA",RRH.T.Tests.DK.PNTA$p.value,RRH.T.Tests.DK.PNTA$conf.int[c(1:2)])
RRH.T_results.Data.not.Collected<-c("Data not Collected",RRH.T.Tests.Data.not.Collected$p.value,RRH.T.Tests.Data.not.Collected$conf.int[c(1:2)])
RRH.T_results.HispanicLatinaeo<-c("HispanicLatinaeo",RRH.T.Tests.HispanicLatinaeo$p.value,RRH.T.Tests.HispanicLatinaeo$conf.int[c(1:2)])
RRH.T_results.HispanicLatinaeo.NativeHIPacific<-c("HispanicLatinaeo/NativeHIPacific",RRH.T.Tests.HispanicLatinaeo.NativeHIPacific$p.value,RRH.T.Tests.HispanicLatinaeo.NativeHIPacific$conf.int[c(1:2)])
RRH.T_results.HispanicLatinaeo.White<-c("HispanicLatinaeo/White",RRH.T.Tests.HispanicLatinaeo.White$p.value,RRH.T.Tests.HispanicLatinaeo.White$conf.int[c(1:2)])
RRH.T_results.NativeHIPacific<-c("NativeHIPacific",RRH.T.Tests.NativeHIPacific$p.value,RRH.T.Tests.NativeHIPacific$conf.int[c(1:2)])
RRH.T_results.NativeHIPacific.White<-c("NativeHIPacific/White",RRH.T.Tests.NativeHIPacific.White$p.value,RRH.T.Tests.NativeHIPacific.White$conf.int[c(1:2)])
RRH.T_results.White<-c("White",RRH.T.Tests.White$p.value,RRH.T.Tests.White$conf.int[c(1:2)])

df <- data.frame("race_list" = 1,
                 "RRH.P.Value" = 1 ,
                 "RRH.Confidence.Interval.Low" = 1,
                 "RRH.Confidence.Interval.High" = 1)

df_RRH.Results <- rbind(df,RRH.T_results.AmIndAKNative,RRH.T_results.AmIndAKNative.Asian.HispanicLatinaeo,
                        RRH.T_results.AmIndAKNative.BlackAfAmerican,RRH.T_results.AmIndAKNative.BlackAfAmerican.HispanicLatinaeo,
                        RRH.T_results.AmIndAKNative.HispanicLatinaeo,RRH.T_results.AmIndAKNative.HispanicLatinaeo.NativeHIPacific,
                        RRH.T_results.AmIndAKNative.HispanicLatinaeo.White,RRH.T_results.AmIndAKNative.NativeHIPacific,
                        RRH.T_results.AmIndAKNative.White,RRH.T_results.Asian,RRH.T_results.Asian.BlackAfAmerican,
                        RRH.T_results.Asian.BlackAfAmerican.HispanicLatinaeo,RRH.T_results.Asian.HispanicLatinaeo,
                        RRH.T_results.Asian.HispanicLatinaeo.NativeHIPacific,RRH.T_results.Asian.HispanicLatinaeo.White,RRH.T_results.Asian.White,
                        RRH.T_results.BlackAfAmerican,RRH.T_results.BlackAfAmerican.HispanicLatinaeo,
                        RRH.T_results.BlackAfAmerican.HispanicLatinaeo.NativeHIPacific,RRH.T_results.BlackAfAmerican.HispanicLatinaeo.White,
                        RRH.T_results.BlackAfAmerican.NativeHIPacific,RRH.T_results.BlackAfAmerican.White,RRH.T_results.DK.PNTA,
                        RRH.T_results.Data.not.Collected,RRH.T_results.HispanicLatinaeo,
                        RRH.T_results.HispanicLatinaeo.NativeHIPacific,RRH.T_results.HispanicLatinaeo.White,
                        RRH.T_results.NativeHIPacific,RRH.T_results.NativeHIPacific.White,RRH.T_results.White,make.row.names = FALSE)

df_RRH.Results<- df_RRH.Results[-1,] #remove dummmy row one

#RRH Join the results to the overall table

TR_Count_RRH_Overall <- T.Tests_Detail %>%
  filter(ProjectType_name  == "RRH") %>% 
  group_by(race_list) %>% 
  summarise(RRH_total = n()) %>%
  mutate(RRH_Percent = paste0(round(100* RRH_total/sum(RRH_total[race_list != "Total"],
                                                     na.rm = TRUE), 2),'%')) %>%
  ungroup()


RRH_Compare_Overall <-  TR_Count_Overall %>% 
  left_join(df_RRH.Results,by="race_list") %>% 
  left_join(TR_Count_RRH_Overall,by="race_list") %>% 
  mutate(RRH.Significant = case_when(RRH.P.Value <= .05 & RRH.Confidence.Interval.Low != "NaN" ~ "Significant", TRUE ~ "Not Significant")) %>% 
  select(race_list,Total_response_total,Total_response_Percent,RRH_total,RRH_Percent,RRH.P.Value,RRH.Confidence.Interval.Low,RRH.Confidence.Interval.High,RRH.Significant)

# Test for Disparities in access to PH
PH.T.Tests <- T.Tests_Detail %>% 
  filter(ProjectType_name == "PH")

PH.T.Tests.AmIndAKNative<-t.test(PH.T.Tests$`AmIndAKNative`, mu =0.0155)
PH.T.Tests.AmIndAKNative.Asian.HispanicLatinaeo<-t.test(PH.T.Tests$`AmIndAKNative/Asian/HispanicLatinaeo`, mu =0.0002)
PH.T.Tests.AmIndAKNative.BlackAfAmerican<-t.test(PH.T.Tests$`AmIndAKNative/BlackAfAmerican`, mu =0.0063)
PH.T.Tests.AmIndAKNative.BlackAfAmerican.HispanicLatinaeo<-t.test(PH.T.Tests$`AmIndAKNative/BlackAfAmerican/HispanicLatinaeo`, mu =0.0014)
PH.T.Tests.AmIndAKNative.HispanicLatinaeo<-t.test(PH.T.Tests$`AmIndAKNative/HispanicLatinaeo`, mu =0.003)
PH.T.Tests.AmIndAKNative.HispanicLatinaeo.NativeHIPacific<-t.test(PH.T.Tests$`AmIndAKNative/HispanicLatinaeo/NativeHIPacific`, mu =0.0002)
PH.T.Tests.AmIndAKNative.HispanicLatinaeo.White<-t.test(PH.T.Tests$`AmIndAKNative/HispanicLatinaeo/White`, mu =0.0006)
PH.T.Tests.AmIndAKNative.NativeHIPacific<-t.test(PH.T.Tests$`AmIndAKNative/NativeHIPacific`, mu =0.0002)
PH.T.Tests.AmIndAKNative.White<-t.test(PH.T.Tests$`AmIndAKNative/White`, mu =0.0123)
PH.T.Tests.Asian<-t.test(PH.T.Tests$`Asian`, mu =0.004)
PH.T.Tests.Asian.BlackAfAmerican<-t.test(PH.T.Tests$`Asian/BlackAfAmerican`, mu =0.0011)
PH.T.Tests.Asian.BlackAfAmerican.HispanicLatinaeo<-t.test(PH.T.Tests$`Asian/BlackAfAmerican/HispanicLatinaeo`, mu =0.0003)
PH.T.Tests.Asian.HispanicLatinaeo<-t.test(PH.T.Tests$`Asian/HispanicLatinaeo`, mu =0.0009)
PH.T.Tests.Asian.HispanicLatinaeo.NativeHIPacific<-t.test(PH.T.Tests$`Asian/HispanicLatinaeo/NativeHIPacific`, mu =0.0002)
PH.T.Tests.Asian.HispanicLatinaeo.White<-t.test(PH.T.Tests$`Asian/HispanicLatinaeo/White`, mu =0.0002)
PH.T.Tests.Asian.White<-t.test(PH.T.Tests$`Asian/White`, mu =0.0023)
PH.T.Tests.BlackAfAmerican<-t.test(PH.T.Tests$`BlackAfAmerican`, mu =0.2642)
PH.T.Tests.BlackAfAmerican.HispanicLatinaeo<-t.test(PH.T.Tests$`BlackAfAmerican/HispanicLatinaeo`, mu =0.009)
PH.T.Tests.BlackAfAmerican.HispanicLatinaeo.NativeHIPacific<-t.test(PH.T.Tests$`BlackAfAmerican/HispanicLatinaeo/NativeHIPacific`, mu =0.0002)
PH.T.Tests.BlackAfAmerican.HispanicLatinaeo.White<-t.test(PH.T.Tests$`BlackAfAmerican/HispanicLatinaeo/White`, mu =0.0027)
PH.T.Tests.BlackAfAmerican.NativeHIPacific<-t.test(PH.T.Tests$`BlackAfAmerican/NativeHIPacific`, mu =0.0008)
PH.T.Tests.BlackAfAmerican.White<-t.test(PH.T.Tests$`BlackAfAmerican/White`, mu =0.0462)
PH.T.Tests.DK.PNTA<-t.test(PH.T.Tests$`DK/PNTA`, mu =0.0044)
PH.T.Tests.Data.not.Collected<-t.test(PH.T.Tests$`Data not Collected`, mu =0.0032)
PH.T.Tests.HispanicLatinaeo<-t.test(PH.T.Tests$`HispanicLatinaeo`, mu =0.0023)
PH.T.Tests.HispanicLatinaeo.NativeHIPacific<-t.test(PH.T.Tests$`HispanicLatinaeo/NativeHIPacific`, mu =0.0006)
PH.T.Tests.HispanicLatinaeo.White<-t.test(PH.T.Tests$`HispanicLatinaeo/White`, mu =0.0575)
PH.T.Tests.NativeHIPacific<-t.test(PH.T.Tests$`NativeHIPacific`, mu =0.004)
PH.T.Tests.NativeHIPacific.White<-t.test(PH.T.Tests$`NativeHIPacific/White`, mu =0.0008)
PH.T.Tests.White<-t.test(PH.T.Tests$`White`, mu =0.5558)

# PH T-test Results

PH.T_results.AmIndAKNative<-c("AmIndAKNative",PH.T.Tests.AmIndAKNative$p.value,PH.T.Tests.AmIndAKNative$conf.int[c(1:2)])
PH.T_results.AmIndAKNative.Asian.HispanicLatinaeo<-c("AmIndAKNative/Asian/HispanicLatinaeo",PH.T.Tests.AmIndAKNative.Asian.HispanicLatinaeo$p.value,PH.T.Tests.AmIndAKNative.Asian.HispanicLatinaeo$conf.int[c(1:2)])
PH.T_results.AmIndAKNative.BlackAfAmerican<-c("AmIndAKNative/BlackAfAmerican",PH.T.Tests.AmIndAKNative.BlackAfAmerican$p.value,PH.T.Tests.AmIndAKNative.BlackAfAmerican$conf.int[c(1:2)])
PH.T_results.AmIndAKNative.BlackAfAmerican.HispanicLatinaeo<-c("AmIndAKNative/BlackAfAmerican/HispanicLatinaeo",PH.T.Tests.AmIndAKNative.BlackAfAmerican.HispanicLatinaeo$p.value,PH.T.Tests.AmIndAKNative.BlackAfAmerican.HispanicLatinaeo$conf.int[c(1:2)])
PH.T_results.AmIndAKNative.HispanicLatinaeo<-c("AmIndAKNative/HispanicLatinaeo",PH.T.Tests.AmIndAKNative.HispanicLatinaeo$p.value,PH.T.Tests.AmIndAKNative.HispanicLatinaeo$conf.int[c(1:2)])
PH.T_results.AmIndAKNative.HispanicLatinaeo.NativeHIPacific<-c("AmIndAKNative/HispanicLatinaeo/NativeHIPacific",PH.T.Tests.AmIndAKNative.HispanicLatinaeo.NativeHIPacific$p.value,PH.T.Tests.AmIndAKNative.HispanicLatinaeo.NativeHIPacific$conf.int[c(1:2)])
PH.T_results.AmIndAKNative.HispanicLatinaeo.White<-c("AmIndAKNative/HispanicLatinaeo/White",PH.T.Tests.AmIndAKNative.HispanicLatinaeo.White$p.value,PH.T.Tests.AmIndAKNative.HispanicLatinaeo.White$conf.int[c(1:2)])
PH.T_results.AmIndAKNative.NativeHIPacific<-c("AmIndAKNative/NativeHIPacific",PH.T.Tests.AmIndAKNative.NativeHIPacific$p.value,PH.T.Tests.AmIndAKNative.NativeHIPacific$conf.int[c(1:2)])
PH.T_results.AmIndAKNative.White<-c("AmIndAKNative/White",PH.T.Tests.AmIndAKNative.White$p.value,PH.T.Tests.AmIndAKNative.White$conf.int[c(1:2)])
PH.T_results.Asian<-c("Asian",PH.T.Tests.Asian$p.value,PH.T.Tests.Asian$conf.int[c(1:2)])
PH.T_results.Asian.BlackAfAmerican<-c("Asian/BlackAfAmerican",PH.T.Tests.Asian.BlackAfAmerican$p.value,PH.T.Tests.Asian.BlackAfAmerican$conf.int[c(1:2)])
PH.T_results.Asian.BlackAfAmerican.HispanicLatinaeo<-c("Asian/BlackAfAmerican/HispanicLatinaeo",PH.T.Tests.Asian.BlackAfAmerican.HispanicLatinaeo$p.value,PH.T.Tests.Asian.BlackAfAmerican.HispanicLatinaeo$conf.int[c(1:2)])
PH.T_results.Asian.HispanicLatinaeo<-c("Asian/HispanicLatinaeo",PH.T.Tests.Asian.HispanicLatinaeo$p.value,PH.T.Tests.Asian.HispanicLatinaeo$conf.int[c(1:2)])
PH.T_results.Asian.HispanicLatinaeo.NativeHIPacific<-c("Asian/HispanicLatinaeo/NativeHIPacific",PH.T.Tests.Asian.HispanicLatinaeo.NativeHIPacific$p.value,PH.T.Tests.Asian.HispanicLatinaeo.NativeHIPacific$conf.int[c(1:2)])
PH.T_results.Asian.HispanicLatinaeo.White<-c("Asian/HispanicLatinaeo/White",PH.T.Tests.Asian.HispanicLatinaeo.White$p.value,PH.T.Tests.Asian.HispanicLatinaeo.White$conf.int[c(1:2)])
PH.T_results.Asian.White<-c("Asian/White",PH.T.Tests.Asian.White$p.value,PH.T.Tests.Asian.White$conf.int[c(1:2)])
PH.T_results.BlackAfAmerican<-c("BlackAfAmerican",PH.T.Tests.BlackAfAmerican$p.value,PH.T.Tests.BlackAfAmerican$conf.int[c(1:2)])
PH.T_results.BlackAfAmerican.HispanicLatinaeo<-c("BlackAfAmerican/HispanicLatinaeo",PH.T.Tests.BlackAfAmerican.HispanicLatinaeo$p.value,PH.T.Tests.BlackAfAmerican.HispanicLatinaeo$conf.int[c(1:2)])
PH.T_results.BlackAfAmerican.HispanicLatinaeo.NativeHIPacific<-c("BlackAfAmerican/HispanicLatinaeo/NativeHIPacific",PH.T.Tests.BlackAfAmerican.HispanicLatinaeo.NativeHIPacific$p.value,PH.T.Tests.BlackAfAmerican.HispanicLatinaeo.NativeHIPacific$conf.int[c(1:2)])
PH.T_results.BlackAfAmerican.HispanicLatinaeo.White<-c("BlackAfAmerican/HispanicLatinaeo/White",PH.T.Tests.BlackAfAmerican.HispanicLatinaeo.White$p.value,PH.T.Tests.BlackAfAmerican.HispanicLatinaeo.White$conf.int[c(1:2)])
PH.T_results.BlackAfAmerican.NativeHIPacific<-c("BlackAfAmerican/NativeHIPacific",PH.T.Tests.BlackAfAmerican.NativeHIPacific$p.value,PH.T.Tests.BlackAfAmerican.NativeHIPacific$conf.int[c(1:2)])
PH.T_results.BlackAfAmerican.White<-c("BlackAfAmerican/White",PH.T.Tests.BlackAfAmerican.White$p.value,PH.T.Tests.BlackAfAmerican.White$conf.int[c(1:2)])
PH.T_results.DK.PNTA<-c("DK/PNTA",PH.T.Tests.DK.PNTA$p.value,PH.T.Tests.DK.PNTA$conf.int[c(1:2)])
PH.T_results.Data.not.Collected<-c("Data not Collected",PH.T.Tests.Data.not.Collected$p.value,PH.T.Tests.Data.not.Collected$conf.int[c(1:2)])
PH.T_results.HispanicLatinaeo<-c("HispanicLatinaeo",PH.T.Tests.HispanicLatinaeo$p.value,PH.T.Tests.HispanicLatinaeo$conf.int[c(1:2)])
PH.T_results.HispanicLatinaeo.NativeHIPacific<-c("HispanicLatinaeo/NativeHIPacific",PH.T.Tests.HispanicLatinaeo.NativeHIPacific$p.value,PH.T.Tests.HispanicLatinaeo.NativeHIPacific$conf.int[c(1:2)])
PH.T_results.HispanicLatinaeo.White<-c("HispanicLatinaeo/White",PH.T.Tests.HispanicLatinaeo.White$p.value,PH.T.Tests.HispanicLatinaeo.White$conf.int[c(1:2)])
PH.T_results.NativeHIPacific<-c("NativeHIPacific",PH.T.Tests.NativeHIPacific$p.value,PH.T.Tests.NativeHIPacific$conf.int[c(1:2)])
PH.T_results.NativeHIPacific.White<-c("NativeHIPacific/White",PH.T.Tests.NativeHIPacific.White$p.value,PH.T.Tests.NativeHIPacific.White$conf.int[c(1:2)])
PH.T_results.White<-c("White",PH.T.Tests.White$p.value,PH.T.Tests.White$conf.int[c(1:2)])

df <- data.frame("race_list" = 1,
                 "PH.P.Value" = 1 ,
                 "PH.Confidence.Interval.Low" = 1,
                 "PH.Confidence.Interval.High" = 1)

df_PH.Results <- rbind(df,PH.T_results.AmIndAKNative,PH.T_results.AmIndAKNative.Asian.HispanicLatinaeo,
                       PH.T_results.AmIndAKNative.BlackAfAmerican,PH.T_results.AmIndAKNative.BlackAfAmerican.HispanicLatinaeo,
                       PH.T_results.AmIndAKNative.HispanicLatinaeo,PH.T_results.AmIndAKNative.HispanicLatinaeo.NativeHIPacific,
                       PH.T_results.AmIndAKNative.HispanicLatinaeo.White,PH.T_results.AmIndAKNative.NativeHIPacific,
                       PH.T_results.AmIndAKNative.White,PH.T_results.Asian,PH.T_results.Asian.BlackAfAmerican,
                       PH.T_results.Asian.BlackAfAmerican.HispanicLatinaeo,PH.T_results.Asian.HispanicLatinaeo,
                       PH.T_results.Asian.HispanicLatinaeo.NativeHIPacific,PH.T_results.Asian.HispanicLatinaeo.White,PH.T_results.Asian.White,
                       PH.T_results.BlackAfAmerican,PH.T_results.BlackAfAmerican.HispanicLatinaeo,
                       PH.T_results.BlackAfAmerican.HispanicLatinaeo.NativeHIPacific,PH.T_results.BlackAfAmerican.HispanicLatinaeo.White,
                       PH.T_results.BlackAfAmerican.NativeHIPacific,PH.T_results.BlackAfAmerican.White,PH.T_results.DK.PNTA,
                       PH.T_results.Data.not.Collected,PH.T_results.HispanicLatinaeo,
                       PH.T_results.HispanicLatinaeo.NativeHIPacific,PH.T_results.HispanicLatinaeo.White,
                       PH.T_results.NativeHIPacific,PH.T_results.NativeHIPacific.White,PH.T_results.White,make.row.names = FALSE)

df_PH.Results<- df_PH.Results[-1,] #remove dummmy row one

#PH Join the results to the overall table

TR_Count_PH_Overall <- T.Tests_Detail %>%
  filter(ProjectType_name  == "PH") %>% 
  group_by(race_list) %>% 
  summarise(PH_total = n()) %>%
  mutate(PH_Percent = paste0(round(100* PH_total/sum(PH_total[race_list != "Total"],
                                                     na.rm = TRUE), 2),'%')) %>%
  ungroup()


PH_Compare_Overall <-  TR_Count_Overall %>% 
  left_join(df_PH.Results,by="race_list") %>% 
  left_join(TR_Count_PH_Overall,by="race_list") %>% 
  mutate(PH.Significant = case_when(PH.P.Value <= .05 & PH.Confidence.Interval.Low != "NaN" ~ "Significant", TRUE ~ "Not Significant")) %>% 
  select(race_list,Total_response_total,Total_response_Percent,PH_total,PH_Percent,PH.P.Value,PH.Confidence.Interval.Low,PH.Confidence.Interval.High,PH.Significant)
}

# Overall disparities in Exits
# T-test compare for Exits to Permanent housing
# USing single sample t-test to see if there is a difference between the proportions of those served to those exited.


Exits_Overall <- T.Tests_Detail %>% 
  mutate(Destination_name = case_when(
    Destination %in% c(400:499)~ "Permanent Housing",
    Destination %in% c(300:399)~ "Temporary Housing",
    Destination %in% c(200:299)~ "Institutional Situations",
    Destination %in% c(100:199)~ "Homeless Situations",
    Destination %in% c(1:99)~ "Other",
    is.na(Destination) ~ "Stayer",
    ),
    PH_Exits = case_when(
      Destination_name == "Permanent Housing"~ 1, TRUE ~0))

PH_Exits_Count_Overall <- Exits_Overall %>% 
  filter(Destination_name == "Permanent Housing") %>% 
  group_by(race_list) %>% 
  summarise(PH_EXITS_total = n()) %>%
  mutate(PH_EXITS_percent = paste0(round(100* PH_EXITS_total/sum(PH_EXITS_total[race_list != "Total"],
                                                                             na.rm = TRUE), 2),'%')) %>%
  ungroup()

PH_EXITS.T.Tests <- Exits_Overall %>% 
  filter(Destination_name == "Permanent Housing")

#T-tests for Exits to Permanent Housing

PH_EXITS.T.Tests.AmIndAKNative<-t.test(PH_EXITS.T.Tests$`AmIndAKNative`, mu =0.0155)
PH_EXITS.T.Tests.AmIndAKNative.Asian.HispanicLatinaeo<-t.test(PH_EXITS.T.Tests$`AmIndAKNative/Asian/HispanicLatinaeo`, mu =0.0002)
PH_EXITS.T.Tests.AmIndAKNative.BlackAfAmerican<-t.test(PH_EXITS.T.Tests$`AmIndAKNative/BlackAfAmerican`, mu =0.0063)
PH_EXITS.T.Tests.AmIndAKNative.BlackAfAmerican.HispanicLatinaeo<-t.test(PH_EXITS.T.Tests$`AmIndAKNative/BlackAfAmerican/HispanicLatinaeo`, mu =0.0014)
PH_EXITS.T.Tests.AmIndAKNative.HispanicLatinaeo<-t.test(PH_EXITS.T.Tests$`AmIndAKNative/HispanicLatinaeo`, mu =0.003)
PH_EXITS.T.Tests.AmIndAKNative.HispanicLatinaeo.NativeHIPacific<-t.test(PH_EXITS.T.Tests$`AmIndAKNative/HispanicLatinaeo/NativeHIPacific`, mu =0.0002)
PH_EXITS.T.Tests.AmIndAKNative.HispanicLatinaeo.White<-t.test(PH_EXITS.T.Tests$`AmIndAKNative/HispanicLatinaeo/White`, mu =0.0006)
PH_EXITS.T.Tests.AmIndAKNative.NativeHIPacific<-t.test(PH_EXITS.T.Tests$`AmIndAKNative/NativeHIPacific`, mu =0.0002)
PH_EXITS.T.Tests.AmIndAKNative.White<-t.test(PH_EXITS.T.Tests$`AmIndAKNative/White`, mu =0.0123)
PH_EXITS.T.Tests.Asian<-t.test(PH_EXITS.T.Tests$`Asian`, mu =0.004)
PH_EXITS.T.Tests.Asian.BlackAfAmerican<-t.test(PH_EXITS.T.Tests$`Asian/BlackAfAmerican`, mu =0.0011)
PH_EXITS.T.Tests.Asian.BlackAfAmerican.HispanicLatinaeo<-t.test(PH_EXITS.T.Tests$`Asian/BlackAfAmerican/HispanicLatinaeo`, mu =0.0003)
PH_EXITS.T.Tests.Asian.HispanicLatinaeo<-t.test(PH_EXITS.T.Tests$`Asian/HispanicLatinaeo`, mu =0.0009)
PH_EXITS.T.Tests.Asian.HispanicLatinaeo.NativeHIPacific<-t.test(PH_EXITS.T.Tests$`Asian/HispanicLatinaeo/NativeHIPacific`, mu =0.0002)
PH_EXITS.T.Tests.Asian.HispanicLatinaeo.White<-t.test(PH_EXITS.T.Tests$`Asian/HispanicLatinaeo/White`, mu =0.0002)
PH_EXITS.T.Tests.Asian.White<-t.test(PH_EXITS.T.Tests$`Asian/White`, mu =0.0023)
PH_EXITS.T.Tests.BlackAfAmerican<-t.test(PH_EXITS.T.Tests$`BlackAfAmerican`, mu =0.2642)
PH_EXITS.T.Tests.BlackAfAmerican.HispanicLatinaeo<-t.test(PH_EXITS.T.Tests$`BlackAfAmerican/HispanicLatinaeo`, mu =0.009)
PH_EXITS.T.Tests.BlackAfAmerican.HispanicLatinaeo.NativeHIPacific<-t.test(PH_EXITS.T.Tests$`BlackAfAmerican/HispanicLatinaeo/NativeHIPacific`, mu =0.0002)
PH_EXITS.T.Tests.BlackAfAmerican.HispanicLatinaeo.White<-t.test(PH_EXITS.T.Tests$`BlackAfAmerican/HispanicLatinaeo/White`, mu =0.0027)
PH_EXITS.T.Tests.BlackAfAmerican.NativeHIPacific<-t.test(PH_EXITS.T.Tests$`BlackAfAmerican/NativeHIPacific`, mu =0.0008)
PH_EXITS.T.Tests.BlackAfAmerican.White<-t.test(PH_EXITS.T.Tests$`BlackAfAmerican/White`, mu =0.0462)
PH_EXITS.T.Tests.DK.PNTA<-t.test(PH_EXITS.T.Tests$`DK/PNTA`, mu =0.0044)
PH_EXITS.T.Tests.Data.not.Collected<-t.test(PH_EXITS.T.Tests$`Data not Collected`, mu =0.0032)
PH_EXITS.T.Tests.HispanicLatinaeo<-t.test(PH_EXITS.T.Tests$`HispanicLatinaeo`, mu =0.0023)
PH_EXITS.T.Tests.HispanicLatinaeo.NativeHIPacific<-t.test(PH_EXITS.T.Tests$`HispanicLatinaeo/NativeHIPacific`, mu =0.0006)
PH_EXITS.T.Tests.HispanicLatinaeo.White<-t.test(PH_EXITS.T.Tests$`HispanicLatinaeo/White`, mu =0.0575)
PH_EXITS.T.Tests.NativeHIPacific<-t.test(PH_EXITS.T.Tests$`NativeHIPacific`, mu =0.004)
PH_EXITS.T.Tests.NativeHIPacific.White<-t.test(PH_EXITS.T.Tests$`NativeHIPacific/White`, mu =0.0008)
PH_EXITS.T.Tests.White<-t.test(PH_EXITS.T.Tests$`White`, mu =0.5558)

# PH_EXITS T-test Results

PH_EXITS.T_results.AmIndAKNative<-c("AmIndAKNative",PH_EXITS.T.Tests.AmIndAKNative$p.value,PH_EXITS.T.Tests.AmIndAKNative$conf.int[c(1:2)])
PH_EXITS.T_results.AmIndAKNative.Asian.HispanicLatinaeo<-c("AmIndAKNative/Asian/HispanicLatinaeo",PH_EXITS.T.Tests.AmIndAKNative.Asian.HispanicLatinaeo$p.value,PH_EXITS.T.Tests.AmIndAKNative.Asian.HispanicLatinaeo$conf.int[c(1:2)])
PH_EXITS.T_results.AmIndAKNative.BlackAfAmerican<-c("AmIndAKNative/BlackAfAmerican",PH_EXITS.T.Tests.AmIndAKNative.BlackAfAmerican$p.value,PH_EXITS.T.Tests.AmIndAKNative.BlackAfAmerican$conf.int[c(1:2)])
PH_EXITS.T_results.AmIndAKNative.BlackAfAmerican.HispanicLatinaeo<-c("AmIndAKNative/BlackAfAmerican/HispanicLatinaeo",PH_EXITS.T.Tests.AmIndAKNative.BlackAfAmerican.HispanicLatinaeo$p.value,PH_EXITS.T.Tests.AmIndAKNative.BlackAfAmerican.HispanicLatinaeo$conf.int[c(1:2)])
PH_EXITS.T_results.AmIndAKNative.HispanicLatinaeo<-c("AmIndAKNative/HispanicLatinaeo",PH_EXITS.T.Tests.AmIndAKNative.HispanicLatinaeo$p.value,PH_EXITS.T.Tests.AmIndAKNative.HispanicLatinaeo$conf.int[c(1:2)])
PH_EXITS.T_results.AmIndAKNative.HispanicLatinaeo.NativeHIPacific<-c("AmIndAKNative/HispanicLatinaeo/NativeHIPacific",PH_EXITS.T.Tests.AmIndAKNative.HispanicLatinaeo.NativeHIPacific$p.value,PH_EXITS.T.Tests.AmIndAKNative.HispanicLatinaeo.NativeHIPacific$conf.int[c(1:2)])
PH_EXITS.T_results.AmIndAKNative.HispanicLatinaeo.White<-c("AmIndAKNative/HispanicLatinaeo/White",PH_EXITS.T.Tests.AmIndAKNative.HispanicLatinaeo.White$p.value,PH_EXITS.T.Tests.AmIndAKNative.HispanicLatinaeo.White$conf.int[c(1:2)])
PH_EXITS.T_results.AmIndAKNative.NativeHIPacific<-c("AmIndAKNative/NativeHIPacific",PH_EXITS.T.Tests.AmIndAKNative.NativeHIPacific$p.value,PH_EXITS.T.Tests.AmIndAKNative.NativeHIPacific$conf.int[c(1:2)])
PH_EXITS.T_results.AmIndAKNative.White<-c("AmIndAKNative/White",PH_EXITS.T.Tests.AmIndAKNative.White$p.value,PH_EXITS.T.Tests.AmIndAKNative.White$conf.int[c(1:2)])
PH_EXITS.T_results.Asian<-c("Asian",PH_EXITS.T.Tests.Asian$p.value,PH_EXITS.T.Tests.Asian$conf.int[c(1:2)])
PH_EXITS.T_results.Asian.BlackAfAmerican<-c("Asian/BlackAfAmerican",PH_EXITS.T.Tests.Asian.BlackAfAmerican$p.value,PH_EXITS.T.Tests.Asian.BlackAfAmerican$conf.int[c(1:2)])
PH_EXITS.T_results.Asian.BlackAfAmerican.HispanicLatinaeo<-c("Asian/BlackAfAmerican/HispanicLatinaeo",PH_EXITS.T.Tests.Asian.BlackAfAmerican.HispanicLatinaeo$p.value,PH_EXITS.T.Tests.Asian.BlackAfAmerican.HispanicLatinaeo$conf.int[c(1:2)])
PH_EXITS.T_results.Asian.HispanicLatinaeo<-c("Asian/HispanicLatinaeo",PH_EXITS.T.Tests.Asian.HispanicLatinaeo$p.value,PH_EXITS.T.Tests.Asian.HispanicLatinaeo$conf.int[c(1:2)])
PH_EXITS.T_results.Asian.HispanicLatinaeo.NativeHIPacific<-c("Asian/HispanicLatinaeo/NativeHIPacific",PH_EXITS.T.Tests.Asian.HispanicLatinaeo.NativeHIPacific$p.value,PH_EXITS.T.Tests.Asian.HispanicLatinaeo.NativeHIPacific$conf.int[c(1:2)])
PH_EXITS.T_results.Asian.HispanicLatinaeo.White<-c("Asian/HispanicLatinaeo/White",PH_EXITS.T.Tests.Asian.HispanicLatinaeo.White$p.value,PH_EXITS.T.Tests.Asian.HispanicLatinaeo.White$conf.int[c(1:2)])
PH_EXITS.T_results.Asian.White<-c("Asian/White",PH_EXITS.T.Tests.Asian.White$p.value,PH_EXITS.T.Tests.Asian.White$conf.int[c(1:2)])
PH_EXITS.T_results.BlackAfAmerican<-c("BlackAfAmerican",PH_EXITS.T.Tests.BlackAfAmerican$p.value,PH_EXITS.T.Tests.BlackAfAmerican$conf.int[c(1:2)])
PH_EXITS.T_results.BlackAfAmerican.HispanicLatinaeo<-c("BlackAfAmerican/HispanicLatinaeo",PH_EXITS.T.Tests.BlackAfAmerican.HispanicLatinaeo$p.value,PH_EXITS.T.Tests.BlackAfAmerican.HispanicLatinaeo$conf.int[c(1:2)])
PH_EXITS.T_results.BlackAfAmerican.HispanicLatinaeo.NativeHIPacific<-c("BlackAfAmerican/HispanicLatinaeo/NativeHIPacific",PH_EXITS.T.Tests.BlackAfAmerican.HispanicLatinaeo.NativeHIPacific$p.value,PH_EXITS.T.Tests.BlackAfAmerican.HispanicLatinaeo.NativeHIPacific$conf.int[c(1:2)])
PH_EXITS.T_results.BlackAfAmerican.HispanicLatinaeo.White<-c("BlackAfAmerican/HispanicLatinaeo/White",PH_EXITS.T.Tests.BlackAfAmerican.HispanicLatinaeo.White$p.value,PH_EXITS.T.Tests.BlackAfAmerican.HispanicLatinaeo.White$conf.int[c(1:2)])
PH_EXITS.T_results.BlackAfAmerican.NativeHIPacific<-c("BlackAfAmerican/NativeHIPacific",PH_EXITS.T.Tests.BlackAfAmerican.NativeHIPacific$p.value,PH_EXITS.T.Tests.BlackAfAmerican.NativeHIPacific$conf.int[c(1:2)])
PH_EXITS.T_results.BlackAfAmerican.White<-c("BlackAfAmerican/White",PH_EXITS.T.Tests.BlackAfAmerican.White$p.value,PH_EXITS.T.Tests.BlackAfAmerican.White$conf.int[c(1:2)])
PH_EXITS.T_results.DK.PNTA<-c("DK/PNTA",PH_EXITS.T.Tests.DK.PNTA$p.value,PH_EXITS.T.Tests.DK.PNTA$conf.int[c(1:2)])
PH_EXITS.T_results.Data.not.Collected<-c("Data not Collected",PH_EXITS.T.Tests.Data.not.Collected$p.value,PH_EXITS.T.Tests.Data.not.Collected$conf.int[c(1:2)])
PH_EXITS.T_results.HispanicLatinaeo<-c("HispanicLatinaeo",PH_EXITS.T.Tests.HispanicLatinaeo$p.value,PH_EXITS.T.Tests.HispanicLatinaeo$conf.int[c(1:2)])
PH_EXITS.T_results.HispanicLatinaeo.NativeHIPacific<-c("HispanicLatinaeo/NativeHIPacific",PH_EXITS.T.Tests.HispanicLatinaeo.NativeHIPacific$p.value,PH_EXITS.T.Tests.HispanicLatinaeo.NativeHIPacific$conf.int[c(1:2)])
PH_EXITS.T_results.HispanicLatinaeo.White<-c("HispanicLatinaeo/White",PH_EXITS.T.Tests.HispanicLatinaeo.White$p.value,PH_EXITS.T.Tests.HispanicLatinaeo.White$conf.int[c(1:2)])
PH_EXITS.T_results.NativeHIPacific<-c("NativeHIPacific",PH_EXITS.T.Tests.NativeHIPacific$p.value,PH_EXITS.T.Tests.NativeHIPacific$conf.int[c(1:2)])
PH_EXITS.T_results.NativeHIPacific.White<-c("NativeHIPacific/White",PH_EXITS.T.Tests.NativeHIPacific.White$p.value,PH_EXITS.T.Tests.NativeHIPacific.White$conf.int[c(1:2)])
PH_EXITS.T_results.White<-c("White",PH_EXITS.T.Tests.White$p.value,PH_EXITS.T.Tests.White$conf.int[c(1:2)])

df <- data.frame("race_list" = 1,
                 "PH_EXITS.P.Value" = 1 ,
                 "PH_EXITS.Confidence.Interval.Low" = 1,
                 "PH_EXITS.Confidence.Interval.High" = 1)

df_PH_EXITS.Results <- rbind(df,PH_EXITS.T_results.AmIndAKNative,PH_EXITS.T_results.AmIndAKNative.Asian.HispanicLatinaeo,
                             PH_EXITS.T_results.AmIndAKNative.BlackAfAmerican,PH_EXITS.T_results.AmIndAKNative.BlackAfAmerican.HispanicLatinaeo,
                             PH_EXITS.T_results.AmIndAKNative.HispanicLatinaeo,PH_EXITS.T_results.AmIndAKNative.HispanicLatinaeo.NativeHIPacific,
                             PH_EXITS.T_results.AmIndAKNative.HispanicLatinaeo.White,PH_EXITS.T_results.AmIndAKNative.NativeHIPacific,
                             PH_EXITS.T_results.AmIndAKNative.White,PH_EXITS.T_results.Asian,PH_EXITS.T_results.Asian.BlackAfAmerican,
                             PH_EXITS.T_results.Asian.BlackAfAmerican.HispanicLatinaeo,PH_EXITS.T_results.Asian.HispanicLatinaeo,
                             PH_EXITS.T_results.Asian.HispanicLatinaeo.NativeHIPacific,PH_EXITS.T_results.Asian.HispanicLatinaeo.White,PH_EXITS.T_results.Asian.White,
                             PH_EXITS.T_results.BlackAfAmerican,PH_EXITS.T_results.BlackAfAmerican.HispanicLatinaeo,
                             PH_EXITS.T_results.BlackAfAmerican.HispanicLatinaeo.NativeHIPacific,PH_EXITS.T_results.BlackAfAmerican.HispanicLatinaeo.White,
                             PH_EXITS.T_results.BlackAfAmerican.NativeHIPacific,PH_EXITS.T_results.BlackAfAmerican.White,PH_EXITS.T_results.DK.PNTA,
                             PH_EXITS.T_results.Data.not.Collected,PH_EXITS.T_results.HispanicLatinaeo,
                             PH_EXITS.T_results.HispanicLatinaeo.NativeHIPacific,PH_EXITS.T_results.HispanicLatinaeo.White,
                             PH_EXITS.T_results.NativeHIPacific,PH_EXITS.T_results.NativeHIPacific.White,PH_EXITS.T_results.White,make.row.names = FALSE)

df_PH_EXITS.Results<- df_PH_EXITS.Results[-1,] #remove dummmy row one

#PH_EXITS Join the results to the overall table


PH_EXITS_Compare_Overall <-  TR_Count_Overall %>% 
  left_join(df_PH_EXITS.Results,by="race_list") %>% 
  left_join(PH_Exits_Count_Overall,by="race_list") %>% 
  mutate(PH_EXITS.Significant = case_when(PH_EXITS.P.Value <= .05 & PH_EXITS.Confidence.Interval.Low != "NaN" ~ "Significant", TRUE ~ "Not Significant")) %>% 
  select(race_list,Total_response_total,Total_response_Percent,PH_EXITS_total,PH_EXITS_percent,PH_EXITS.P.Value,PH_EXITS.Confidence.Interval.Low,PH_EXITS.Confidence.Interval.High,PH_EXITS.Significant)


#### Disparities comparing Overall percent enrolled to overall percent exited (not using t-test or anova)

PH_Exits_Percent_difference<- PH_EXITS_Compare_Overall %>% 
  #mutate(PH_EXITS_percent_recode = if_else(is.na(PH_EXITS_percent),0,PH_EXITS_percent)) %>% NOTE: Add zero values to Exits.
  filter(is.na(PH_EXITS_total) == FALSE) %>% #Removed all zero responses
  mutate(
    Total_response_decimal = as.numeric(sub("%", "",Total_response_Percent,fixed=TRUE))/100,
    PH_EXITS_decimal =as.numeric(sub("%", "",PH_EXITS_percent,fixed=TRUE))/100,
    Difference = (Total_response_decimal - PH_EXITS_decimal))
#%>% 
  #select(race_list,Difference,PH_EXITS.Significant)
  

Significant_Racial_categories <- c(PH_Exits_Percent_difference$race_list[PH_Exits_Percent_difference$PH_EXITS.Significant == "Significant"])



# Change baseline
Percent_difference_g <- ggplot(PH_Exits_Percent_difference, aes(x=race_list, y=Difference)) +
  geom_segment( aes(x=race_list, xend=race_list, y=0, yend=Difference), color="grey") +
  geom_point(color= ifelse(PH_Exits_Percent_difference$race_list %in% all_of(Significant_Racial_categories),"orange","grey"), size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  ) +
  xlab("") +
  ylab("Percent Difference between Enrollment and Exit to PH")




#Sanky Graph Compare by 

#Choose two to compare
Compare <- unique(TR_Count_ProjectType$race_list)
Target_group <- Compare[27]

edgelist_01 <- TR_Count_ProjectType %>% 
  select(race_list,ProjectType_name,Total_response_total) %>% 
  filter(race_list %in% all_of(Compare[c(27,25)])) %>% 
  mutate(race_list = case_when(
    race_list != all_of(Target_group) ~ "Other",
    TRUE ~ race_list)) %>% 
  rename(source = race_list,
         target = ProjectType_name,
         value = Total_response_total) 

edgelist_02 <- 

links

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE)
p

# Report tables and visuals -----

gt(TR_Count_Overall) |>
  tab_header(
    title = md("Total Response Racial Categories"),
    subtitle = md("This method reports exactly as the respondent identifies in HMIS")
  )

Percent_difference_g

