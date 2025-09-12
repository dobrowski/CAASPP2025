


udp.dist <- udp %>%
    filter(str_detect(county_name,"Monterey"),
           charter_school_y_n == "No") %>%
    group_by(district_name, district_code) %>%
    summarise(across(c(english_learner_el,unduplicated_frpm_eligible_count,total_enrollment) ,
                     sum)) %>%
    mutate(el.perc = round2(100*english_learner_el/total_enrollment,1),
           frpm.perc = round2(100*unduplicated_frpm_eligible_count/total_enrollment,1)
    )




dist.tbl <- caaspp.mry %>%
    filter(Type_ID == 6,
           Subgroup_ID == 1,
           Grade == 13,
           # County_Code == "27",
           # DistrictCode == "10272",
           Test_Year >= yr.curr) %>%
    left_join(udp.dist, by = c("District_Code" = "district_code")) %>%
    select(District_Name, Percentage_Standard_Met_and_Above, Test_Id, ends_with("perc") ) %>%
    pivot_wider(id_cols = c(District_Name,ends_with("perc") ),
                names_from = Test_Id,
                values_from = Percentage_Standard_Met_and_Above) %>%
    rename(ELA = `1`,
           Math = `2`) %>%
    arrange(desc(frpm.perc))


clipr::write_clip(dist.tbl)


write_rds( dist.tbl, here("data","dist-comp.rds"))
