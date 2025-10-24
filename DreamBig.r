

## 3rd grade ELA in Dream Big districts 

dist.code.cross <- caaspp.cast.mry %>%
    select(district_code, district_name) |>
    unique() |>
    filter(!str_detect(district_name, "Charter|Academy")) |>
    na.omit()


temp <- caaspp.cast.mry %>% 
    filter( entity_type == "District",
            test_id == 1,
            test_year %in% c(2023, 2024, 2025),
            grade == 3,
            student_group_id == 1
            ) |>
    pivot_wider(id_cols = c(district_code) ,names_from = test_year, values_from = percentage_standard_met_and_above) %>%
    mutate(
        change.two.year = round2(`2025` - `2023`, digits = 1),
        change.one.year = round2(`2025` - `2024`, digits = 1)
    ) |>
    left_join(dist.code.cross)


# Alisal Union School District - No
# Greenfield Union School District - Yes
# King City Union School District - Minor
# Monterey County Office of Education - Yes
# North Monterey County Unified - No
# Pacific Grove - No
# Salinas City - No
# Santa Rita Union School District - No
# Soledad Unified School District - YES


