



udp.with.perc <- udp %>%
    mutate(el.perc = english_learner_el/total_enrollment,
           frpm.perc = unduplicated_frpm_eligible_count/total_enrollment,
           )


SoMoCo <- udp.with.perc %>% 
    filter(str_detect(district_name, "Salinas Union"))


udp.comp <- udp.with.perc %>%
    filter(# district_type == "High School District",
        low_grade == "9",
           high_grade == "12",
           el.perc >= .16,
           frpm.perc >= .80, #.85
           charter_school_y_n == "No",
           str_detect(school_type,"Public"),
           total_enrollment >= 1000,
        total_enrollment <= 1500
           )

write_csv(udp.comp, "San Lucas Comparison Schools.csv")


temp <- caaspp.somoco.comp %>%
    filter(school_name %in% udp.comp$school_name ,
           district_name %in% udp.comp$district_name ,
           test_id == 2) %>%
    select(district_name, school_name, percentage_standard_met_and_above) %>%
    mutate(percentage_standard_met_and_above = as.numeric(percentage_standard_met_and_above))


caaspp.somoco.comp <- tbl(con, "CAASPP") %>% 
    filter(#County_Code == "27",
           # DistrictCode == "10272",
           test_year >= "2024",
           grade == 11,
           subgroup_id == "1",
     #      Type_ID == "7"
           ) %>%
    collect() %>%
    select(county_code = County_Code,
           district_code = District_Code,
           school_code = School_Code,
           Test_Id,
           Percentage_Standard_Met_and_Above) %>%
    mutate(Percentage_Standard_Met_and_Above = as.numeric(Percentage_Standard_Met_and_Above)) %>%
    pivot_wider(id_cols = c(county_code,district_code,school_code),
                names_from = Test_Id,
                values_from = Percentage_Standard_Met_and_Above) %>%
    rename(ELA = `1`,
           Math = `2`)

joint <- left_join(udp.comp, caaspp.somoco.comp) %>%
    # filter(ELA > 60,
    #        Math > 15) %>%
    select(county_code:school_name, total_enrollment, el.perc:Math) %>%
    mutate(el.perc = round2(el.perc*100,1),
           frpm.perc = round2(frpm.perc*100,1),
           ELA = round2(ELA,1),
           Math = round2(Math,1))



joint2 <- joint %>% filter(Math > 20,
                 ELA > 55) %>%
    mutate(cds = paste0(county_code,district_code,school_code))


### Comparison Function ------


comp.dist <- function(math.score = 20, 
                      ela.score = 55,
                      low.gr = "K",
                      high.gr = 12,
                      el.rate = .22,
                      frpm.rate = .85,
                      enr = 200,
                      yr = 2023
                      ) {
    


udp.comp <- udp.with.perc %>%
    filter(# district_type == "High School District",
        low_grade == low.gr,
        high_grade == high.gr,
        el.perc >= el.rate,
        frpm.perc >= frpm.rate,
    #    charter_school_y_n == "No",
        str_detect(school_type,"Public"),
        total_enrollment >= enr
    )

print(udp.comp)

caaspp.hold.comp <- tbl(con, "CAASPP") %>% 
    filter(#County_Code == "27",
        # DistrictCode == "10272",
        Test_Year == yr,
        Grade == 13,
        Subgroup_ID == "1",
        Type_ID == "7") %>%
    collect() %>%
    select(county_code = County_Code,
           district_code = District_Code,
           school_code = School_Code,
           Test_Id,
           Percentage_Standard_Met_and_Above) %>%
    mutate(Percentage_Standard_Met_and_Above = as.numeric(Percentage_Standard_Met_and_Above)) %>%
    pivot_wider(id_cols = c(county_code,district_code,school_code),
                names_from = Test_Id,
                values_from = Percentage_Standard_Met_and_Above) %>%
    rename(ELA = `1`,
           Math = `2`)

joint <- left_join(udp.comp, caaspp.hold.comp) 



joint2 <- joint %>%
    # filter(ELA > 60,
    #        Math > 15) %>%
    select(county_code:school_name, total_enrollment, el.perc:Math) %>%
    mutate(el.perc = round2(el.perc*100,1),
           frpm.perc = round2(frpm.perc*100,1),
           ELA = round2(ELA,1),
           Math = round2(Math,1))



joint2 %>% filter(Math > math.score,
                           ELA > ela.score) %>%
    mutate(cds = paste0(county_code,district_code,school_code))

}


comp.dist(math.score = 1, 
          ela.score = 4,
          low.gr = 6,
          high.gr = 12,
          el.rate = .1,
          frpm.rate = .4,
          enr = 200,
          yr = 2023)



### CAST example ------

caaspp.somoco.comp <- tbl(con, "CAST") %>% 
     filter(#County_Code == "27",
    #     # DistrictCode == "10272",
         Test_Year >= "2022",
         Grade == 14,
         Demographic_ID == "1",
    #     Type_ID == "7"
    ) %>%
  #  head(20) %>%
    collect() %>%
    select(county_code = County_Code,
           district_code = District_Code,
           school_code = School_Code,
#           Test_Id,
           Percentage_Standard_Met_and_Above) %>%
    mutate(Percentage_Standard_Met_and_Above = as.numeric(Percentage_Standard_Met_and_Above)) # %>%
#     pivot_wider(id_cols = c(county_code,district_code,school_code),
# #                names_from = Test_Id,
#                 values_from = Percentage_Standard_Met_and_Above) 

joint <- left_join(udp.comp, caaspp.somoco.comp) %>%
     filter(Percentage_Standard_Met_and_Above > 30) %>%
    select(county_name:school_name, total_enrollment, el.perc:Percentage_Standard_Met_and_Above) %>%
    mutate(el.perc = round2(el.perc*100,1),
           frpm.perc = round2(frpm.perc*100,1),
           # ELA = round2(ELA,1),
           # Math = round2(Math,1)
           )
