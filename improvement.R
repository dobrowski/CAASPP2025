

improvement <- caaspp.mry.hist %>% 
    mutate(test_year = factor(test_year)) %>%
    filter(grade == 3,
           subgroup_id == 1, #160 EL  # 128, # SWD
      #     test_id == 2, # Math 
           entity_type == "School"
    ) %>% 
    filter(!is.na(school_name),
           !str_detect(school_name, "Monterey Peninsula Unified School District Communi")) %>%
    pivot_wider(id_cols = c(school_name,district_name), names_from = c(test_id, test_year), values_from = percentage_standard_met_and_above) %>%
    mutate(math.change.2yr = `2_2024` - `2_2022`,
           ela.change.2yr = `1_2024` - `1_2022`,
           math.change.1yr = `2_2024` - `2_2023`,
           ela.change.1yr = `1_2024` - `1_2023`,
           )

improvement <- caaspp.mry.hist %>% 
    mutate(test_year = factor(test_year)) %>%
    filter(grade == 13,
           subgroup_id == 1, #160 EL  # 128, # SWD
           #     test_id == 2, # Math 
           entity_type == "District"
    ) %>% 
    pivot_wider(id_cols = c(school_name,district_name), names_from = c(test_id, test_year), values_from = percentage_standard_met_and_above) %>%
    mutate(math.change.2yr = `2_2024` - `2_2022`,
           ela.change.2yr = `1_2024` - `1_2022`,
           math.change.1yr = `2_2024` - `2_2023`,
           ela.change.1yr = `1_2024` - `1_2023`,
    )
