

temp <- caaspp.mry %>%
    filter(entity_type == "District",
           subgroup_id == "1",
           test_id == 1,
           grade == 11) %>%
    select(district_name,grade, percentage_standard_met_and_above)


temp <- cast %>%
    filter(#str_detect(district_name, "Office"),
           district_code == "00000",
           subgroup_id==1,
           test_id == 17,
           test_year == 2024,
           grade == 13) %>%
    select(county_name, percentage_standard_met_and_above)








temp <- cast %>%
    filter(#str_detect(district_name, "Office"),
 #       district_code == "00000",
        
        subgroup_id %in% c(77,78),
        test_id == 17,
        test_year == 2024,
        grade == 13) %>%
    select(school_name, subgroup, subgroup_id, percentage_standard_met_and_above)


