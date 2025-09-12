

# Graphing ELPAC over Times



elpac.mry <- tbl(con, "SELPAC") %>% 
    filter(CountyCode == "27",
           # DistrictCode == "10272",
           TestYear >= "2022") %>%
    collect() %>%
    left_join_codebook("SELPAC","StudentGroupID")



# 
# # %>%
#     mutate(subgroup_id = as.character(subgroup_id)) %>%
#     left_join_codebook("CAASPP", "subgroup_id") %>%
#     rename(subgroup = definition) %>%
#     left_join(ent2) %>%
#     mutate(type_id = as.character(type_id)) %>%
#     left_join_codebook("CAASPP", "type_id") %>%
#     rename(entity_type = definition) %>%
#     mutate(across(caaspp_reported_enrollment:area_4_percentage_near_standard, as.numeric))
# 
# 


elpac.somoco2 <- elpac.somoco %>%
    filter(# str_detect( DistrictName, "South Monterey" ) ,
         DistrictCode == "66068",
         SchoolCode == "0000000", 
           Grade == 13,
           StudentGroupID == "001"
           ) %>%
    select(TestYear, starts_with("Overall"))%>%
    select(TestYear, ends_with("Pcnt")) %>%
    pivot_longer(cols = starts_with("OverallPerf")) %>%
    mutate(TestYear = factor(TestYear),
           name = fct_rev(name),
            name2 = paste0("Level ", str_extract(name, "\\d+")) )

           
elpac.somoco2 %>%
    ggplot(aes(x = TestYear, y = value, group = name2, fill = name2, label = paste0( round(value, 1), "%"))) +
    geom_col(width = .6) +
    geom_label(position = position_stack(vjust = 0.5), show.legend = FALSE) +
    mcoe_theme +
    theme(axis.text.y = element_blank()) +
    labs(title = paste0( "South Monterey County - Three Year Comparison of English Learner Performance"),
         subtitle = "ELPAC Performance Levels")

ggsave(here("figs", paste0("South Monterey County - Three Year Comparison of English Learner Performance ", Sys.Date(),".png")), width = 8, height = 6)
