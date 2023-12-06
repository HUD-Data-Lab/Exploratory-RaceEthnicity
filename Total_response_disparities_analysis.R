
library(networkD3)
options(scipen=999)
set.seed(2024)

# Total response -- all categories are represented in all combinations

#Disparities analysis
## Compare to general population (we don't have)
## Descriptive (who is in the system) -- By project type and compare disparities
## Compare Program participation length of time to permanent exit

#Source("CLassifyingMulti_race.R)

#Overall Race and ethnicity 
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

#Race and Ethnicity By project type

TR_Count_ProjectType <- Race_Report_Range_Detail %>% 
  group_by(ProjectType_name,race_list) %>% 
  summarise(Total_response_total = n()) %>%
  mutate(Total_response_Percent = paste0(round(100* Total_response_total/sum(Total_response_total[race_list != "Total"],
                                                                             na.rm = TRUE), 2),'%')) %>%
  ungroup()

# Compare Project Type to overall Population
# Null hypothesis there is no difference from the overall distribution of racial identity to project type X
T.Tests_Standard <- Race_Report_Range_Detail %>% 
  group_by(race_list) %>% 
  summarise(Total_response_total = n()) %>%
  mutate(Total_response_Percent = paste0(round(Total_response_total/sum(Total_response_total[race_list != "Total"],
                                                                             na.rm = TRUE), 4))) %>%
  ungroup()


T.Tests_Detail <- Race_Report_Range_Detail %>% 
  select(PersonalID,ProjectType_name,race_list) %>% 
  mutate("White" = case_when(race_list == "White" ~ 1, TRUE ~ 0),
         "BlackAfAmerican" = case_when(race_list == "BlackAfAmerican" ~ 1, TRUE ~ 0),
         "AmIndAKNative/BlackAfAmerican" = case_when(race_list == "AmIndAKNative/BlackAfAmerican" ~ 1, TRUE ~ 0))

# Test for Disparities in access to ES
ES.T.Tests <- T.Tests_Detail %>% 
  filter(ProjectType_name == "ES")

ES.test.White<- t.test(ES.T.Tests$White, mu = 0.5569) # mu generated from T.Test_Standard -> Note for Gwen Tried T.Tests_Standard$Total_response_Percent[race_list == "White"]
# Statistically significant we reject the null. Lower than standard

ES.test.BlackAfAmerican <- t.test(ES.T.Tests$BlackAfAmerican, mu = 0.2639)
# Statistically significant we reject the null. Higher than standard

ES.test.AmIndAKNative_BlackAfAmerican <- t.test(ES.T.Tests$`AmIndAKNative/BlackAfAmerican`, mu = 0.0063)
#  Not statistically significant we fail to reject the null.

# Test for Disparities in access to TH
TH.T.Tests <- T.Tests_Detail %>% 
  filter(ProjectType_name == "TH")

Th.test.White<- t.test(TH.T.Tests$White, mu = 0.5569) # mu generated from T.Test_Standard -> Note for Gwen Tried T.Tests_Standard$Total_response_Percent[race_list == "White"]
# Statistically significant we reject the null. Higher than standard

TH.test.BlackAfAmerican <- t.test(TH.T.Tests$BlackAfAmerican, mu = 0.2639)
# Statistically significant we reject the null. Lower than standard

TH.test.AmIndAKNative_BlackAfAmerican <- t.test(TH.T.Tests$`AmIndAKNative/BlackAfAmerican`, mu = 0.0063)
#  Statistically significant we reject the null. Lower than standard

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

