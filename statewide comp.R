

#. Just EL students 
# Just with FRPM > 80? 
# comparison districts statewide 

 caaspp.mry %>%
    filter(grade == 11,
           school_name %in% udp.comp$school_name,
           subgroup_id == "1",  # 1 All, 160 EL
           test_id == 2, # ELA 
           entity_type %in% c("School"), # ,"Direct Funded Charter School","Locally Funded Charter School"
           !is.na(percentage_standard_met_and_above)
    ) %>%
    mutate(#lea_name = case_when(entity_type == "District" ~ district_name,
           #                     str_detect(entity_type,"harter") ~ school_name),
           bar_kular = case_when(
                                 str_detect(district_name,"Pacific Grove|North Monterey|South Monterey") ~ "orange", # Pacific Grove|North Monterey|South Monterey
                                 TRUE ~ "steelblue")
           )  %>%
    
#    select(district_name,percentage_standard_met_and_above)
    
    
    ggplot2::ggplot( aes( y = percentage_standard_met_and_above/100,
                          x =forcats::fct_reorder(school_name,percentage_standard_met_and_above) ,
                          label = scales::percent(percentage_standard_met_and_above/100, accuracy = .1))) +
    ggplot2::geom_segment( ggplot2::aes(x=forcats::fct_reorder(school_name, percentage_standard_met_and_above/100),
                                        xend=forcats::fct_reorder(school_name, percentage_standard_met_and_above/100),
                                        y=0,
                                        yend=percentage_standard_met_and_above/100,
                                        color=bar_kular),
                           size =2 ) +
    ggplot2::geom_point( aes(color=bar_kular), size=5, alpha=0.6) +
    ggplot2::coord_flip() +
    ggplot2::geom_text(size = 3, color = "black") +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    #  facet_grid(facets = vars(`Student Group`), scales = "free" ) +
    ggthemes::theme_hc() +
    mcoe_theme +
    scale_color_identity() +
    
    labs(x = "",
         y = "",
         color ="",
         title = paste0("CAASPP ", yr.curr ," ", "High School Math Rates Meeting or Exceeding Standards"),
         subtitle = "Schools with 2 year requirements in Orange and 3 year requirements in Blue \nFiltered for High Schools with at least 14% English Learners and 75% Free/Reduced Price Meal Eligible ",
         caption = source.link
    )


ggsave(here("figs", paste0("High School low income", "Math Rates Meeting or Exceeding with Year Requirements ",  Sys.Date(),".png" )),
       width = 12, height = 7)
