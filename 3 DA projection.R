


library(googlesheets4)
library(modelr)

# List of subgroups from Meet and Exceed that are relevant on Dashboard 

subgroup.list <- c(74,
      75,
      1,
      76,
      160,
      180,
      77,
      240,
      78,
      52,
      144,
      79,
      8,
      31,
      128,
      80)

### Add reference Standard points -------


reference <- read_excel(here("data","ScaleScoreREference.xlsx"))

reference2 <- pivot_longer(reference, cols = c(`1`,`2`,`3`,`4`) ) %>%
    mutate(Grade = if_else(str_length(Grade) >= 2, Grade, paste0(0,Grade))) %>%
    rename(Subject = Subject,
           GradeLevelWhenAssessed = Grade,
           ScaleScoreAchievementLevel = name,
           ScaleScoreNext = value)


reference3 <- reference2 %>%
    filter(ScaleScoreAchievementLevel == 2) %>%
    select(-ScaleScoreAchievementLevel) %>%
    rename(MeetStandard = ScaleScoreNext)


ref <- reference3 %>%
    transmute(Grade = as.numeric(GradeLevelWhenAssessed),
           Test_Id = case_match(Subject, "Math" ~ 2,
                                "ELA" ~ 1),
           MeetStandard
           ) %>%
    na.omit()

### Estimate DFS for District  -----

# Based on the grade level average scale score,  only really works for schools and groups with sufficient number 
# of students at each grade level.  

temp <- caaspp.mry %>%
    filter(           
        Grade != 13,
        Subgroup_ID %in% subgroup.list,
     #   Entity_Type == "District",
      #  str_detect(District_Name,"Soledad")
                      ) %>%
    left_join(ref) %>%
    mutate(cds = paste0(County_Code,District_Code,School_Code),
           DFS =  Mean_Scale_Score - MeetStandard) %>%
    group_by(District_Name,School_Name, cds, Test_Id, Subgroup_ID) %>%
    mutate(
           meanDFS = weighted.mean(DFS, Students_Tested),
           tot.students = sum(Students_Tested)
    ) %>%
    select(cds, District_Name, School_Name, tot.students, meanDFS )  %>%
    distinct() %>%
    left_join_codebook("CAASPP", "Subgroup_ID") %>%
    rename(Subgroup = definition)


# Pulls prior year Dashboard data 

dash.math.prior <- tbl(con, "DASH_MATH") %>% 
    filter(countyname == "Monterey",
        # DistrictCode == "10272",
        reportingyear >= yr.prior) %>%
    #    head(100) %>%
    collect() %>%
    left_join_codebook("DASH_MATH", "studentgroup") %>%
    mutate(Test_Id = 2,
           Subgroup_ID =  case_when(studentgroup ==  "AA" ~ 74,
                                    studentgroup == "AI" ~ 75,
                                    studentgroup == "ALL" ~ 1,
                                    studentgroup == "AS" ~ 76,
                                    #  CAA ~ 
                                    studentgroup == "EL" ~ 160,
                                    #   ELO ~ 160, # combine
                                    studentgroup == "EO" ~ 180,
                                    studentgroup == "FI" ~ 77,
                                    studentgroup ==  "FOS" ~ 240,
                                    studentgroup ==  "HI" ~ 78,
                                    studentgroup ==  "HOM" ~ 52,
                                    studentgroup == "MR" ~ 144,
                                    studentgroup ==  "PI" ~ 79,
                                    studentgroup ==  "RFP" ~ 8,
                                    #  SBA ~
                                    studentgroup ==  "SED" ~ 31,
                                    studentgroup ==  "SWD" ~ 128,
                                    studentgroup ==  "WH"  ~ 80)
    )  %>%
    mutate(Subgroup_ID = as.character(Subgroup_ID)) %>%
    select(cds, countyname:schoolname, studentgroup, Test_Id ,currdenom, currstatus, statuslevel, definition, Subgroup_ID)




dash.ela.prior <- tbl(con, "DASH_ELA") %>% 
    filter(countyname == "Monterey",
           # DistrictCode == "10272",
           reportingyear >= yr.prior) %>%
    #    head(100) %>%
    collect() %>%
    left_join_codebook("DASH_ELA", "studentgroup") %>%
    mutate(Test_Id = 1,
           Subgroup_ID =  case_when(studentgroup ==  "AA" ~ 74,
                                    studentgroup == "AI" ~ 75,
                                    studentgroup == "ALL" ~ 1,
                                    studentgroup == "AS" ~ 76,
                                    #  CAA ~ 
                                    studentgroup == "EL" ~ 160,
                                    #   ELO ~ 160, # combine
                                    studentgroup == "EO" ~ 180,
                                    studentgroup == "FI" ~ 77,
                                    studentgroup ==  "FOS" ~ 240,
                                    studentgroup ==  "HI" ~ 78,
                                    studentgroup ==  "HOM" ~ 52,
                                    studentgroup == "MR" ~ 144,
                                    studentgroup ==  "PI" ~ 79,
                                    studentgroup ==  "RFP" ~ 8,
                                    #  SBA ~
                                    studentgroup ==  "SED" ~ 31,
                                    studentgroup ==  "SWD" ~ 128,
                                    studentgroup ==  "WH"  ~ 80)
    )  %>%
    mutate(Subgroup_ID = as.character(Subgroup_ID)) %>%
    select(cds, countyname:schoolname, studentgroup, Test_Id ,currdenom, currstatus, statuslevel, definition, Subgroup_ID)


dash.prior <- bind_rows(dash.ela.prior,dash.math.prior)


# Pulls new estimates with prior yera dashboard to make change estimates 

joint <- left_join(temp, dash.prior) %>%
    mutate(change = meanDFS - currstatus) %>%
    filter(!is.na(meanDFS)) %>%
    select(Test_Id, District_Name, School_Name, Subgroup, meanDFS, change)








### Older for 2019 --------

# Saving for archive purposes, but don't think it is relevant anymore.



### Math -------

caaspp.2019 <- tbl(con, "CAASPP") %>% 
    filter(#County_Code == "27",
           # DistrictCode == "10272",
           Test_Year == "2019") %>%
    collect() %>%
    mutate(Subgroup_ID = as.character(Subgroup_ID)) %>%
    left_join_codebook("CAASPP", "Subgroup_ID") %>%
    rename(Subgroup = definition) %>%
    left_join(ent) %>%
    mutate(Type_ID = as.character(Type_ID)) %>%
    left_join_codebook("CAASPP", "Type_ID") %>%
    rename(Entity_Type = definition) %>%
    mutate(across(CAASPP_Reported_Enrollment:Area_4_Percentage_Near_Standard, as.numeric))



dash.2019 <- tbl(con, "DASH_MATH") %>% 
    filter(#County_Code == "27",
           # DistrictCode == "10272",
           reportingyear >= "2019") %>%
#    head(100) %>%
    collect() %>%
    left_join_codebook("DASH_MATH", "studentgroup") %>%
mutate(Subgroup_ID =  case_when(studentgroup ==  "AA" ~ 74,
          studentgroup == "AI" ~ 75,
          studentgroup == "ALL" ~ 1,
          studentgroup == "AS" ~ 76,
         #  CAA ~ 
         studentgroup == "EL" ~ 160,
        #   ELO ~ 160, # combine
        studentgroup == "EO" ~ 180,
        studentgroup == "FI" ~ 77,
        studentgroup ==  "FOS" ~ 240,
        studentgroup ==  "HI" ~ 78,
        studentgroup ==  "HOM" ~ 52,
        studentgroup == "MR" ~ 144,
        studentgroup ==  "PI" ~ 79,
        studentgroup ==  "RFP" ~ 8,
         #  SBA ~
        studentgroup ==  "SED" ~ 31,
        studentgroup ==  "SWD" ~ 128,
        studentgroup ==  "WH"  ~ 80)
)  %>%
    select(cds, countyname:schoolname, studentgroup, currdenom, currstatus, statuslevel, definition, Subgroup_ID)




caaspp.2019.rev <- caaspp.2019 %>%
    filter(Subgroup_ID %in% subgroup.list,
           Grade == 13,
           Test_Id == 2
    ) %>%
    mutate(cds = paste0(County_Code,District_Code,School_Code),
           Subgroup_ID = as.numeric(Subgroup_ID)) %>%
    select(cds, Subgroup_ID, Subgroup, Total_Tested_At_Entity_Level, Grade, Test_Id,
           Students_Tested, Percentage_Standard_Exceeded:Percentage_Standard_Not_Met,
           ) 


joint.2019.math <- left_join(dash.2019, caaspp.2019.rev)


ggplot(joint.2019, aes(x= currstatus, y = Percentage_Standard_Met_and_Above)) +
    geom_point()+
    theme_clean()

model.math <- lm(data = joint.2019.math,
   currstatus ~ Percentage_Standard_Met_and_Above + Percentage_Standard_Not_Met)


joint.2019.pred.math <- joint.2019 %>% add_predictions(model.math) %>%
    mutate(diff = pred - currstatus)


ggplot(joint.2019.pred, aes(x= pred, y = currstatus)) +
    geom_point()+
    theme_clean()

### ELA -------



dash.2019.ela <- tbl(con, "DASH_ELA") %>% 
    filter(#County_Code == "27",
        # DistrictCode == "10272",
        reportingyear >= "2019") %>%
    #    head(100) %>%
    collect() %>%
    left_join_codebook("DASH_ELA", "studentgroup") %>%
    mutate(Subgroup_ID =  case_when(studentgroup ==  "AA" ~ 74,
                                    studentgroup == "AI" ~ 75,
                                    studentgroup == "ALL" ~ 1,
                                    studentgroup == "AS" ~ 76,
                                    #  CAA ~ 
                                    studentgroup == "EL" ~ 160,
                                    #   ELO ~ 160, # combine
                                    studentgroup == "EO" ~ 180,
                                    studentgroup == "FI" ~ 77,
                                    studentgroup ==  "FOS" ~ 240,
                                    studentgroup ==  "HI" ~ 78,
                                    studentgroup ==  "HOM" ~ 52,
                                    studentgroup == "MR" ~ 144,
                                    studentgroup ==  "PI" ~ 79,
                                    studentgroup ==  "RFP" ~ 8,
                                    #  SBA ~
                                    studentgroup ==  "SED" ~ 31,
                                    studentgroup ==  "SWD" ~ 128,
                                    studentgroup ==  "WH"  ~ 80)
    )  %>%
    select(cds, countyname:schoolname, studentgroup, currdenom, currstatus, statuslevel, dass_flag, definition, Subgroup_ID)




caaspp.2019.rev.ela <- caaspp.2019 %>%
    filter(Subgroup_ID %in% subgroup.list,
           Grade == 13,
           Test_Id == 1
    ) %>%
    mutate(cds = paste0(County_Code,District_Code,School_Code),
           Subgroup_ID = as.numeric(Subgroup_ID)) %>%
    select(cds, Subgroup_ID, Subgroup, Total_Tested_At_Entity_Level, Grade, Test_Id,
           Students_Tested, Percentage_Standard_Exceeded:Percentage_Standard_Not_Met,
    ) 


joint.2019.ela <- left_join(dash.2019.ela, caaspp.2019.rev.ela)


ggplot(joint.2019.ela, aes(x= currstatus, y = Percentage_Standard_Met_and_Above)) +
    geom_point()+
    theme_clean()


#### Modeling Percent Met/Exceeded with Distance from Standard -----

model.ela <- lm(data = joint.2019.ela,
                 currstatus ~ Percentage_Standard_Met_and_Above + Percentage_Standard_Not_Met)

summary(model.ela)

# model.ela2 <- lm(data = joint.2019.ela,
#                 currstatus ~ Percentage_Standard_Exceeded +
#                     Percentage_Standard_Met +
#                     Percentage_Standard_Nearly_Met +
#                     Percentage_Standard_Not_Met)
# 
# summary(model.ela2)
# 

joint.2019.pred.ela <- joint.2019.ela %>% add_predictions(model.ela) %>%
    mutate(diff = pred - currstatus)


ggplot(joint.2019.pred.ela, aes(x= pred, y = currstatus)) +
    geom_point()+
    theme_clean()




### Predictions in fall 2022 ------


caaspp.mry.pred.math <- caaspp.mry %>%
    filter(Test_Id == 2,
           Subgroup_ID %in% subgroup.list,
           Grade == 13,
           Entity_Type == "District",
           CAASPP_Reported_Enrollment >= 30) %>%
    add_predictions(model.math) %>%
    mutate(math.status = case_when(!str_detect(District_Name, "Salinas Union|South Monterey") & pred <= -95.1 ~ "Very Low",
                                  !str_detect(District_Name, "Salinas Union|South Monterey") & pred <= -25.1 ~ "Low",
                                  !str_detect(District_Name, "Salinas Union|South Monterey") & pred <= 0 ~ "Medium",
                                  !str_detect(District_Name, "Salinas Union|South Monterey") & pred <= 34.9 ~ "High",
                                  !str_detect(District_Name, "Salinas Union|South Monterey") & pred >= 35 ~ "Very High",
                                  str_detect(District_Name, "Salinas Union|South Monterey") & pred <= -115.1 ~ "Very Low",
                                  str_detect(District_Name, "Salinas Union|South Monterey") & pred <= -60.1 ~ "Low",
                                  str_detect(District_Name, "Salinas Union|South Monterey") & pred <= -0.1 ~ "Medium",
                                  str_detect(District_Name, "Salinas Union|South Monterey") & pred <= 24.9 ~ "High",
                                  str_detect(District_Name, "Salinas Union|South Monterey") & pred >= 25 ~ "Very High",
                                  TRUE ~ "TBD"
    ))


# 
# dass <- dash.2019.ela %>%
#     select(cds, dass_flag) %>%
#     distinct()

caaspp.mry.pred.ela <- caaspp.mry %>%
    filter(Test_Id == 1,
           Subgroup_ID %in% subgroup.list,
           Grade == 13,
           Entity_Type == "District",
           CAASPP_Reported_Enrollment >= 30) %>%
    left_join(ent2) %>% 
    add_predictions(model.ela)  %>%
    mutate(ela.status = case_when(!str_detect(District_Name, "Salinas Union|South Monterey") & pred <= -70.1 ~ "Very Low",
                                  !str_detect(District_Name, "Salinas Union|South Monterey") & pred <= -5.1 ~ "Low",
                                  !str_detect(District_Name, "Salinas Union|South Monterey") & pred <= 9.9 ~ "Medium",
                                  !str_detect(District_Name, "Salinas Union|South Monterey") & pred <= 44.9 ~ "High",
                                  !str_detect(District_Name, "Salinas Union|South Monterey") & pred >= 45 ~ "Very High",
                                  str_detect(District_Name, "Salinas Union|South Monterey") & pred <= -45.1 ~ "Very Low",
                                  str_detect(District_Name, "Salinas Union|South Monterey") & pred <= -0.1 ~ "Low",
                                  str_detect(District_Name, "Salinas Union|South Monterey") & pred <= 29.9 ~ "Medium",
                                  str_detect(District_Name, "Salinas Union|South Monterey") & pred <= 74.9 ~ "High",
                                  str_detect(District_Name, "Salinas Union|South Monterey") & pred >= 75 ~ "Very High",
                                  TRUE ~ "TBD"
                                  ))


very.low.status <- caaspp.mry.pred.ela %>%
    select(District_Name, Subgroup, ela.status) %>%
    left_join(caaspp.mry.pred.math) %>%
    select(District_Name, Subgroup, ela.status, math.status) %>%
    mutate(eligible = case_when(math.status == "Very Low" & ela.status == "Very Low" ~ "Yes",
                                math.status == "Low" & ela.status == "Very Low" ~ "Yes",
                                math.status == "Very Low" & ela.status == "Low" ~ "Yes",
                                TRUE ~ "No"))

eligible.list <- very.low.status %>%
    filter(eligible == "Yes") %>%
    arrange(District_Name, Subgroup)


ss <- "https://docs.google.com/spreadsheets/d/11zcbxX8Vg_5O9n1HRDAfIcCFlhyFWW99-ucO6cC-IX8/edit#gid=0"


sheet_write(ss = ss,
            eligible.list)






#  How many in 2019 were eligible just based on CAASPP?

math.2019 <- dash.2019 %>%
    filter(statuslevel %in% c(1,2),
           countyname == "Monterey",
           Subgroup_ID %in% subgroup.list,
           rtype == "D",
           currdenom >= 30
           ) %>%
    select(districtname, definition, math.status = statuslevel)



ela.2019 <- dash.2019.ela %>%
    filter(statuslevel %in% c(1,2),
           countyname == "Monterey",
           Subgroup_ID %in% subgroup.list,
           rtype == "D",
           currdenom >= 30
    ) %>%
    select(districtname, definition, ela.status = statuslevel)


eligible.2019 <- full_join(math.2019, ela.2019) %>%
    mutate(eligible = ifelse(math.status+ela.status <=3, "Yes", "No")) %>%
    filter(eligible == "Yes") %>%
    arrange(districtname, definition)

sheet_write(ss = ss,
            eligible.2019)




#### End ------