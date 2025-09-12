

# exploring Home Charter



caaspp.home <- tbl(con, "CAASPP") %>% 
    filter(county_code == "27",
            district_code == "10272",
          # test_year >= yr.curr
           ) %>%
    collect() %>%
    mutate(subgroup_id = as.character(subgroup_id)) %>%
    left_join_codebook("CAASPP", "subgroup_id") %>%
    rename(subgroup = definition) %>%
    left_join(ent2) %>%
    mutate(type_id = as.character(type_id)) %>%
    left_join_codebook("CAASPP", "type_id") %>%
    rename(entity_type = definition) %>%
    mutate(across(caaspp_reported_enrollment:area_4_percentage_near_standard, as.numeric))


caaspp.home %>%
    filter( school_code == 2730232,
            subgroup_id == 1,
            grade == 13
          ) %>%
    ggplot( aes( x = factor(test_year), y = percentage_standard_met_and_above, group = test_id, colour = factor(test_id)) ) +
    geom_line() +
    mcoe_theme




schools <- tbl(con, "SCHOOLS") %>% 
    filter(#County == "Monterey",
           StatusType == "Active") %>%
    collect()


homes <- schools %>%
    filter( str_detect(str_to_lower(School), "home|virtual") ,
            !str_detect(str_to_lower(School), "hosp") ,
            SOCType != "Preschool") %>%
    select(!contains("str")) %>%
    select( -Phone,-Email,-FaxNumber)




