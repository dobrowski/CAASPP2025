



caaspp.mry.state <- tbl(con, "CAASPP") %>% 
    filter(county_code %in% c("27","00"),
            district_code == "00000",
           test_id == 2,
           subgroup_id == 128,
           grade == 13,
           test_year >= 2023) %>%
 #   head(100) %>%
    collect() %>%
    # mutate(subgroup_id = as.character(subgroup_id)) %>%
    # left_join_codebook("CAASPP", "subgroup_id") %>%
    # rename(subgroup = definition) %>%
    # left_join(ent2) %>%
    # mutate(type_id = as.character(type_id)) %>%
    # left_join_codebook("CAASPP", "type_id") %>%
    # rename(entity_type = definition) %>%
    mutate(across(caaspp_reported_enrollment:area_4_percentage_near_standard, as.numeric))


over.time <- function(df, test.id, dist.name, heading) {
    
    test.name <- if_else(test.id == 1, "ELA", "Math")
    
    
    sa.long <- df %>%
        filter(grade == 13,
               subgroup_id == "1",
               test_id == test.id, # ELA 
               school_code == "0000000"
        ) 
    
    entit <- sa.long %>%
        filter(test_year == "2022") %>%
        select(county_code,district_code,school_code, entity = entity_type)
    
    
    sa.long <- sa.long %>%
        left_join(entit) %>%
        distinct()
    
    
    ggplot(data = sa.long, aes(x = test_year, y = percentage_standard_met_and_above, group = entity)) +
        #  facet_wrap(~Entity) +
        geom_line(aes(color = entity, alpha = 1), size = 1) +
        geom_text_repel(data = sa.long %>% filter(test_year == "2019"),
                        aes(label = entity) ,
                        hjust = "left",
                        segment.size = .2,
                        segment.color = "grey",
                        size = 3,
                        nudge_x = -.4,
                        direction = "y") +
        geom_text_repel(data = sa.long %>% filter(test_year == "2022"),
                        aes(label = entity) ,
                        hjust = "right",
                        segment.size = .2,
                        segment.color = "grey",
                        fontface = "bold",
                        size = 3,
                        nudge_x = .4,
                        direction = "y") +
        geom_label(aes(label = percentage_standard_met_and_above),
                   size = 2.5,
                   label.padding = unit(0.05, "lines"),
                   label.size = 0.0) +
        theme_hc() +  # Remove the legend
        # theme(axis.text.y      = element_blank()) +
        # theme(panel.grid.major.y = element_blank()) +
        # theme(panel.grid.minor.y = element_blank()) +
        # theme(axis.ticks       = element_blank()) +
        scale_x_discrete(position = "top") +
        theme(legend.position = "none") +
        labs(#title = "San Antonio Decreased More Sharply than Monterey County or California",
            #subtitle = "CAASPP ELA",
            title = paste0(dist.name, heading, " compared to Monterey County, and California"),
            subtitle = paste0("CAASPP ",test.name),
            y = "Percent Meeting or Exceeding Standard",
            x = "")
    
    
    
    ggsave(here("figs", paste0("Over time ",dist.name," Monterey County and California - ", test.name ,  Sys.Date(),".png" )),
           width = 8, height = 6)
}



ggplot(data = caaspp.mry.state, aes(x = test_year, y = percentage_standard_met_and_above, group = county_code)) +
    #  facet_wrap(~Entity) +
    geom_line(aes(color = county_code, alpha = 1), size = 1) +
    facet_wrap(~subgroup_id) +
    # geom_text_repel(data = caaspp.mry.state %>% filter(test_year == "2023"),
    #                 aes(label = entity) ,
    #                 hjust = "left",
    #                 segment.size = .2,
    #                 segment.color = "grey",
    #                 size = 3,
    #                 nudge_x = -.4,
    #                 direction = "y") +
    # geom_text_repel(data = caaspp.mry.state %>% filter(test_year == "2024"),
    #                 aes(label = entity) ,
    #                 hjust = "right",
    #                 segment.size = .2,
    #                 segment.color = "grey",
    #                 fontface = "bold",
    #                 size = 3,
    #                 nudge_x = .4,
    #                 direction = "y") +
    geom_label(aes(label = percentage_standard_met_and_above),
               size = 2.5,
               label.padding = unit(0.05, "lines"),
               label.size = 0.0) +
    theme_hc() +  # Remove the legend
    # theme(axis.text.y      = element_blank()) +
    # theme(panel.grid.major.y = element_blank()) +
    # theme(panel.grid.minor.y = element_blank()) +
    # theme(axis.ticks       = element_blank()) +
    scale_x_discrete(position = "top") # +
 #   theme(legend.position = "none") # +
    # labs(#title = "San Antonio Decreased More Sharply than Monterey County or California",
    #     #subtitle = "CAASPP ELA",
    #     title = paste0(dist.name, heading, " compared to Monterey County, and California"),
    #     subtitle = paste0("CAASPP ",test.name),
    #     y = "Percent Meeting or Exceeding Standard",
    #     x = "")


