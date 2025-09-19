



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







#####. BAr bell ----


caaspp.cast.mry %>%
    
    
    filter(
 #       test_year >= 2023,
        entity_type == "County",
 #       district_code == "00000",
        student_group_id %in% standard.groups,
        test_id == 2,
        grade == 13
    ) |>
    pivot_wider(id_cols = c(student_group) ,names_from = test_year, values_from = percentage_standard_met_and_above) %>%
    mutate(
        change = round2(`2025` - `2024`, digits = 1),
        labl = if_else(change>0,paste0("+",change),paste0(change))
    ) %>%
    mutate(student_group = fct_reorder(student_group,`2025`) ) |>
    #  mutate(subgroup = factor(subgroup, levels = sort(unique(subgroup), decreasing = TRUE)))
    # mutate(subgroup = factor(subgroup, levels = sort((`2024`), decreasing = TRUE)))
    
    
    ggplot( aes(y = student_group)) +
    # connecting segment
    geom_segment(aes(x = `2024`, xend = `2025`,  yend = student_group), 
                 linewidth = 1.2,
                 color = "grey70",
                 arrow = arrow(#ends = "last",
                     type = "closed",
                     length = unit(0.1,"inches")
                 )
    ) +
    # endpoints
    geom_point(aes(x = `2024`), color = "#fed98e", size = 3) +
    geom_point(aes(x = `2025`), color = "#fe9929", size = 3) +
    geom_text(aes(label = labl, x = `2024`+ change/2), 
              nudge_y = 0.5,
              color = "grey70"
    ) +
    mcoe_theme +
    labs(title = "Monterey County change in Math CAASPP between 2024 and 2025",
         subtitle = "Student Groups Percentage Met or Exceeded",
         x = "Percentage Met or Exceeded"
    ) + coord_cartesian(clip = "off")


ggsave(here("figs", paste0("Change over time for Monterey County Math",  Sys.Date(),".png" )),
       width = 7, height = 5)









change.barbell.studentgroup <- function(df) {
    
    df |>
        pivot_wider(id_cols = c(student_group) ,names_from = test_year, values_from = percentage_standard_met_and_above) %>%
        mutate(
            change = round2(`2025` - `2024`, digits = 1),
            labl = if_else(change>0,paste0("+",change),paste0(change))
        ) %>%
        mutate(student_group = fct_reorder(student_group,`2025`) ) |>
        #  mutate(subgroup = factor(subgroup, levels = sort(unique(subgroup), decreasing = TRUE)))
        # mutate(subgroup = factor(subgroup, levels = sort((`2024`), decreasing = TRUE)))
        
        
        ggplot( aes(y = student_group)) +
        # connecting segment
        geom_segment(aes(x = `2024`, xend = `2025`,  yend = student_group), 
                     linewidth = 1.2,
                     color = "grey70",
                     arrow = arrow(#ends = "last",
                         type = "closed",
                         length = unit(0.1,"inches")
                     )
        ) +
        # endpoints
        geom_point(aes(x = `2024`), color = "#fed98e", size = 3) +
        geom_point(aes(x = `2025`), color = "#fe9929", size = 3) +
        geom_text(aes(label = labl, x = `2024`+ change/2), 
                  nudge_y = 0.5,
                  color = "grey70"
        ) +
        mcoe_theme 
}

# County student groups

caaspp.cast.mry %>%
    filter(
        #       test_year >= 2023,
        entity_type == "County",
        #       district_code == "00000",
        student_group_id %in% standard.groups,
        test_id == 2,
        grade == 13
    ) |>
    change.barbell.studentgroup() +
    labs(title = "Monterey County change in Math CAASPP between <span style='color:#fed98e;'>2024</span> and <span style='color:#fe9929;'>2025</span>",
         subtitle = "Student Groups Percentage Met or Exceeded",
         x = "Percentage Met or Exceeded"
    ) + 
  theme(
    plot.title = element_markdown(size = 16)
  ) +
  coord_cartesian(clip = "off")


ggsave(here("figs","county", paste0("CAASPP ", "Math"," Change between 2024 and 2025 ", Sys.Date(),".png" )),
       width = 7, height = 5)



caaspp.cast.mry %>%
    filter(
        #       test_year >= 2023,
        entity_type == "County",
        #       district_code == "00000",
        student_group_id %in% standard.groups,
        test_id == 1,
        grade == 13
    ) |>
    change.barbell.studentgroup() +
  labs(title = "Monterey County change in ELA CAASPP between <span style='color:#fed98e;'>2024</span> and <span style='color:#fe9929;'>2025</span>",
       subtitle = "Student Groups Percentage Met or Exceeded",
         x = "Percentage Met or Exceeded"
  ) + 
  theme(
    plot.title = element_markdown(size = 16)
  ) +
  coord_cartesian(clip = "off")


ggsave(here("figs","county", paste0("CAASPP ", "ELA"," Change between 2024 and 2025 ", Sys.Date(),".png" )),
       width = 7, height = 5)


# 



change.barbell.district <- function(df) {
    
    df |>
        pivot_wider(id_cols = c(district_name) ,names_from = test_year, values_from = percentage_standard_met_and_above) %>%
        mutate(
            change = round2(`2025` - `2024`, digits = 1),
            labl = if_else(change>0,paste0("+",change),paste0(change))
        ) %>%
        mutate(district_name = fct_reorder(district_name,`2025`) ) |>
        #  mutate(subgroup = factor(subgroup, levels = sort(unique(subgroup), decreasing = TRUE)))
        # mutate(subgroup = factor(subgroup, levels = sort((`2024`), decreasing = TRUE)))
        
        
        ggplot( aes(y = district_name)) +
        # connecting segment
        geom_segment(aes(x = `2024`, xend = `2025`,  yend = district_name), 
                     linewidth = 1.2,
                     color = "grey70",
                     arrow = arrow(#ends = "last",
                         type = "closed",
                         length = unit(0.1,"inches")
                     )
        ) +
        # endpoints
        geom_point(aes(x = `2024`), color = "#fed98e", size = 3) +
        geom_point(aes(x = `2025`), color = "#fe9929", size = 3) +
        geom_text(aes(label = labl, x = `2024`+ change/2), 
                  nudge_y = 0.5,
                  color = "grey70"
        ) +
        mcoe_theme 
}

# County district

caaspp.mry %>%
    bind_rows(caaspp.mry.hist) %>%
    filter(
        #       test_year >= 2023,
        entity_type == "District",
        #       district_code == "00000",
      #  student_group_id %in% standard.groups,
      student_group_id == 1,
        test_id == 2,
        grade == 13,
      !is.na(percentage_standard_met_and_above)
    ) |>
    change.barbell.district() +
    labs(title = "Monterey County Districts change in Math CAASPP between 2024 and 2025",
    #     subtitle = "Student Groups Percentage Met or Exceeded",
         x = "Percentage Met or Exceeded"
    ) + coord_cartesian(clip = "off")


ggsave(here("figs","county", paste0("CAASPP ", "Math","District Change between 2024 and 2025 ", Sys.Date(),".png" )),
       width = 8, height = 8)


caaspp.mry %>%
    bind_rows(caaspp.mry.hist) %>%
    filter(
        #       test_year >= 2023,
        entity_type == "District",
        #       district_code == "00000",
        #  student_group_id %in% standard.groups,
        student_group_id == 1,
        test_id == 1,
        grade == 13,
        !is.na(percentage_standard_met_and_above)
    ) |>
    change.barbell.district() +
    labs(title = "Monterey County Districts change in ELA CAASPP between 2024 and 2025",
         #     subtitle = "Student Groups Percentage Met or Exceeded",
         x = "Percentage Met or Exceeded"
    ) + coord_cartesian(clip = "off")


ggsave(here("figs","county", paste0("CAASPP ", "ELA","District Change between 2024 and 2025 ", Sys.Date(),".png" )),
       width = 8, height = 8)



### Barbells for countis in the state 


change.barbell <- function(df, group_col) {
    
    df |>
        pivot_wider(
            id_cols = {{ group_col }} ,
            names_from = test_year, 
            values_from = percentage_standard_met_and_above) %>%
        mutate(
            change = round2(`2025` - `2024`, digits = 1),
            labl = if_else(change>0,paste0("+",change),paste0(change))
        ) %>%
        mutate({{ group_col }} := fct_reorder({{ group_col }},`2025`) ) |>
        #  mutate(subgroup = factor(subgroup, levels = sort(unique(subgroup), decreasing = TRUE)))
        # mutate(subgroup = factor(subgroup, levels = sort((`2024`), decreasing = TRUE)))
        
        
        ggplot( aes(y = {{ group_col }})) +
        # connecting segment
        geom_segment(aes(x = `2024`, xend = `2025`,  yend = {{ group_col }}), 
                     linewidth = 1.2,
                     color = "grey70",
                     arrow = arrow(#ends = "last",
                         type = "closed",
                         length = unit(0.1,"inches")
                     )
        ) +
        # endpoints
        geom_point(aes(x = `2024`), color = "#fed98e", size = 3) +
        geom_point(aes(x = `2025`), color = "#fe9929", size = 3) +
        geom_text(aes(label = labl, x = `2024`+ change/2), 
                  nudge_y = 0.5,
                  color = "grey70"
        ) +
        mcoe_theme 
}

# County district


caaspp.ca %>% 
    filter(type_id == 5,
           student_group_id == 1,
           grade == 13,
           # County_Code == "27",
           # DistrictCode == "10272",
           test_year >= yr.curr) %>%
    clean.caaspp() %>%
    bind_rows(caaspp.county.comp) |>

    filter(
        #       test_year >= 2023,
        entity_type == "County",
        #       district_code == "00000",
        #  student_group_id %in% standard.groups,
        student_group_id == 1,
        test_id == 2,
        grade == 13,
        !is.na(percentage_standard_met_and_above)
    ) |>
    change.barbell(county_name) +
  labs(title = "Monterey County change in Math CAASPP between <span style='color:#fed98e;'>2024</span> and <span style='color:#fe9929;'>2025</span>",
       subtitle = "Student Groups Percentage Met or Exceeded",
       x = "Percentage Met or Exceeded"
  ) + 
  theme(
    plot.title = element_markdown(size = 16)
  ) +
  coord_cartesian(clip = "off")


ggsave(here("figs","county", paste0("CAASPP ", "Math","Counties Change between 2024 and 2025 ", Sys.Date(),".png" )),
       width = 5, height = 7)












