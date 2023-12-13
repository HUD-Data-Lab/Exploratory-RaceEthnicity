
library(networkD3)
options(scipen=999)
set.seed(2024)

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
  select(PersonalID,ProjectType_name,race_list) %>% 
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

#Join the results to the overall table

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
  mutate(Significant = case_when(P.Value <= .05 & Confidence.Interval.Low != "NaN" ~ "Significant", TRUE ~ "Not Significant")) %>% 
  select(race_list,Total_response_total,Total_response_Percent,ES_total,ES_Percent,P.Value,Confidence.Interval.Low,Confidence.Interval.High,Significant)


# Test for Disparities in access to RRH
RRH.T.Tests <- T.Tests_Detail %>% 
  filter(ProjectType_name == "RRH")

RRH.test.White<- t.test(RRH.T.Tests$White, mu = 0.5569) # mu generated from T.Test_Standard -> Note for Gwen Tried T.Tests_Standard$Total_response_Percent[race_list == "White"]
# Not Statistically significant we fail to reject the null.

RRH.test.BlackAfAmerican <- t.test(RRH.T.Tests$BlackAfAmerican, mu = 0.2639)
# Statistically significant we reject the null. Lower than standard

RRH.test.AmIndAKNative_BlackAfAmerican <- t.test(RRH.T.Tests$`AmIndAKNative/BlackAfAmerican`, mu = 0.0063)
#  Not Statistically significant we fail to reject the null.

# Test for Disparities in access to PH
PH.T.Tests <- T.Tests_Detail %>% 
  filter(ProjectType_name == "PH")

PH.test.White<- t.test(PH.T.Tests$White, mu = 0.5569) # mu generated from T.Test_Standard -> Note for Gwen Tried T.Tests_Standard$Total_response_Percent[race_list == "White"]
# Statistically significant we reject the null. Higher than standard

PH.test.BlackAfAmerican <- t.test(PH.T.Tests$BlackAfAmerican, mu = 0.2639)
# Not Statistically significant we fail to reject the null.

PH.test.AmIndAKNative_BlackAfAmerican <- t.test(PH.T.Tests$`AmIndAKNative/BlackAfAmerican`, mu = 0.0063)
# No enrollments into PH. Not applicable for this test. 
# PH.T.Tests %>% filter(race_list=="AmIndAKNative/BlackAfAmerican")


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





