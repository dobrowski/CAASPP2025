
#  Comparison Counties for Bright Futures 


caaspp.bf <- tbl(con, "CAASPP") %>% 
    filter(county_code %in% c(27, 20, 16, 24, 54, 13 ),
           subgroup_id == 6,
           grade %in% c(3),
            district_code == "00000"
          # test_year >= yr.curr
          ) %>%
 #   head(100) %>%
    collect()


caaspp.bf2 <- caaspp.bf %>%
    # left_join(ent , by = join_by(county_code, district_code, type_id,
    #                              school_code )
    # ) %>%
    mutate(county_name = case_when(county_code  == "13" ~ "Imperial",
                                   county_code  == "16" ~ "Kings",
                                   county_code  == "20" ~ "Madera",
                                   county_code  == "24" ~ "Merced",
                                   county_code  == "27" ~ "Monterey",
                                   county_code  == "54" ~ "Tulare"
                                   )
    ) %>%
    filter(test_id == 1,
           grade == 3,
           test_year != 2021) %>%
    select(county_code, county_name, test_year, percentage_standard_met_and_above) %>%
    pivot_wider(names_from = test_year, values_from = percentage_standard_met_and_above )






13 = "Imperial"
16 = "Kings"
20 = "Madera"
24 = "Merced"
27 = "Monterey"
54 = "Tulare"


______



grad.bf <- tbl(con, "GRAD_FOUR") %>% 
    filter(county_code %in% c(27, 20, 16, 24, 54, 13 ),
           reporting_category == "TA",
           aggregate_level == "C",
           charter_school == "All",
           dass == "All"
           # test_year >= yr.curr
    ) %>%
       head(100) %>%
    collect()


grad.bf2 <- grad.bf |>
    select(county_name, academic_year, met_uc_csu_grad_reqs_rate) |>
    arrange(academic_year) |>
    pivot_wider(names_from = academic_year, values_from = c(met_uc_csu_grad_reqs_rate))


# clipr::write_clip(grad.bf2)

# met_uc_csu_grad_reqs_rate
# regular_hs_diploma_graduates_rate


---------
    
    

cgr.bf <- tbl(con, "CGR16") %>% 
    filter(county_code %in% c(27, 20, 16, 24, 54, 13 ),
           completer_type == "TA",
           reporting_category == "TA",
           aggregate_level == "C",
           charter_school == "All",
           alternative_school_accountability_status == "All"
           # test_year >= yr.curr
    ) %>%
    head(100) %>%
    collect()

cgr.bf2 <- cgr.bf |>
    select(county_name, academic_year, college_going_rate_total_16_months) |>
    arrange(academic_year) |>
    pivot_wider(names_from = academic_year, values_from = c(college_going_rate_total_16_months))


# clipr::write_clip(cgr.bf2)


