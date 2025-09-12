

caaspp.mry %>%
    filter(county_code %in% c("27"),
           district_code == "00000",
           test_id == 1,
           subgroup_id %in% c(8,180,160, 250),
           grade == 13,
           test_year >= 2024) %>%
    ggplot(aes(x = subgroup, y = percentage_standard_met_and_above, fill = subgroup, label = percentage_standard_met_and_above )) +
    geom_col() +
    geom_label() +
    mcoe_theme +
    coord_flip() +
    theme(legend.position = "none") +
    labs(title = "Monterey County CAASPP 2024 ELA by English Acquisition Status",
         subtitle = "Percent Met and Exceeded"
         )


ggsave("Percent Met and Exceeded on CAASPP ELA by English Acquisition Status.png", width = 8, height = 6)    
    
