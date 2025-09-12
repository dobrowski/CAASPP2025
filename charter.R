
# Charter Report


temp <- udp %>%
    filter(county_code == "27",
           charter_school_y_n == "Yes") %>%
    distinct() %>%
    transmute(school_name,
              total_enrollment,
              el.perc = round2(100*english_learner_el/total_enrollment,1),
           frpm.perc = round2(100*unduplicated_frpm_eligible_count/total_enrollment,1)
    )

clipr::write_clip(temp)
