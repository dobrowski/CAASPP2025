


udp <- tbl(con, "upc") %>% 
    filter(# County_Code == "27",
        # DistrictCode == "10272",
        academic_year == max(academic_year)
    ) %>%
    #    head() %>%
    collect() 




udp.county <- udp %>%
    group_by(county_name) %>%
    summarise(across(c(english_learner_el,unduplicated_frpm_eligible_count,total_enrollment) ,
                     sum)) %>%
    mutate(el.perc = round2(100*english_learner_el/total_enrollment,1),
           frpm.perc = round2(100*unduplicated_frpm_eligible_count/total_enrollment,1)
    )


caaspp.county <- tbl(con, "CAASPP") %>% 
    filter(type_id == 5,
           subgroup_id == 1,
           grade == 13,
        # County_Code == "27",
           # DistrictCode == "10272",
           test_year >= yr.curr) %>%
    collect() %>%
    mutate(subgroup_id = as.character(subgroup_id)) %>%
    left_join_codebook("CAASPP", "subgroup_id") %>%
    rename(subgroup = definition) %>%
    left_join(ent , by = join_by(county_code, district_code, type_id,
                                school_code )
              ) %>%
    mutate(type_id = as.character(type_id)) %>%
    left_join_codebook("CAASPP", "type_id") %>%
    rename(entity_type = definition) %>%

    mutate(across(caaspp_reported_enrollment:area_4_percentage_near_standard, as.numeric)) %>%
    bind_rows(cast %>% filter(test_year == yr.curr, type_id == 5,
                              subgroup_id == 1,
                              grade == 13)) %>%
    select(county_name = county_name, percentage_standard_met_and_above, test_id) %>%
    pivot_wider(id_cols = county_name,
                names_from = test_id,
                values_from = percentage_standard_met_and_above) %>%
    rename(ELA = `1`,
           Math = `2`,
           Science = `17`) %>%
    left_join(udp.county) %>%
    arrange(desc(Math)) %>%
    mutate(Rank = row_number())
    
write_rds( caaspp.county, here("data","county-comp.rds"))




### 

central.coast <- caaspp.county %>%
    filter(str_detect( county_name, "Cruz|Benito|Monterey|Luis|Barb|Vent" ) )

library(googlesheets4)
write_sheet(central.coast, "https://docs.google.com/spreadsheets/d/1XVyzdcgWEm-KU-_JElUefGeCmNGqzMfw2JaQP1-IRLI/edit#gid=0" )


central.coast %>%
    lollipop(ELA,
             county_name,
             "sea green") +
    labs(x = "",
         y = "",
         color ="",
         title = ("CAASPP ELA Rates Meeting or Exceeding for Central Coast Counties"),
         caption = source.link
    ) 

ggsave(here("figs", paste0("CAASPP ELA Rates Meeting or Exceeding for Central Coast Counties",  Sys.Date(),".png" )),
       width = 8, height = 4.5)



caaspp.county %>%
    filter(el.perc > 20,
           frpm.perc > 70)


#### Top ten FRPM and EL 

central.list <- "Cruz|Benito|Monterey|Luis|Barb|Vent"
frpm.list <- "Colu|Fres|Imperi|Kern|Madera|Merced|Mont|Tular|Kings|Glen"
el.list <- "Colu|Imperi|Madera|Merced|Mono|Mont|Franc|Barb|Stan|Napa"
both.list <-  "Colu|Imperi|Madera|Merced|Mont"  # Tulare 
list.20.70 <-  "Colu|Imperi|Madera|Merced|Mont|Tular|Stan"  # Tulare 

library(googlesheets4)
write_sheet(central.coast, "https://docs.google.com/spreadsheets/d/1XVyzdcgWEm-KU-_JElUefGeCmNGqzMfw2JaQP1-IRLI/edit#gid=0" )



comp.counties.graph <- function(list, ass, kular, desc) {


caaspp.county %>%
        # Rounding to whole percentage
        mutate(
               ELA = round2(ELA,0),
               Math = round2(Math,0),
        ) %>%
        
    filter(str_detect( county_name, list ) ) %>%
        mutate(county_name = if_else(county_name == "Monterey", 
                                        paste0("<b style = 'font-size:12pt'>",county_name,"</b>"),
                                        county_name)
               ) %>%
    # lollipop({{ass}},
    #          county_name,
    #          kular) +
        
        ggplot2::ggplot( aes( y = {{ass}}/100,
                                 x =forcats::fct_reorder(county_name,{{ass}}) ,
                                 label = scales::percent({{ass}}/100, accuracy = 1))) +
        geom_segment( aes(x=forcats::fct_reorder(county_name, {{ass}}/100),
                          xend=forcats::fct_reorder(county_name, {{ass}}/100),
                          y=0,
                          yend={{ass}}/100),
                      color=kular,
                      size =2 ) +
        geom_point( color=kular, size=5, alpha=0.6) +
        coord_flip() +
        geom_text(size = 3, color = "black") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        #  facet_grid(facets = vars(`Student Group`), scales = "free" ) +
        theme_hc() +
        mcoe_theme +
        
    labs(x = "",
         y = "",
         color ="",
         title = paste0("CAASPP ", deparse(substitute(ass))," ",yr.curr ," Rates Meeting or Exceeding Standards "),
         subtitle = desc,
         caption = source.link
    ) + 
        theme(
            axis.text.y = element_markdown()
        )

  ggsave(here("figs","county", paste0("CAASPP ", deparse(substitute(ass))," Rates Meeting or Exceeding Standards ",desc,  Sys.Date(),".png" )),
         width = 8, height = 4.5)
}




chronic <- tbl(con,"CHRONIC") %>%
    filter(academic_year == max(academic_year) ,
           #      county_code == "27",
           aggregate_level %in% c("C","T"),
           reporting_category == "TA",
           charter_school == "All",
           dass == "All"
           
           #    charter_yn == "No",
           #     district_name == "Salinas City Elementary"
    ) %>%
    collect()




comp.counties.chronic <- function(list,  kular, desc) {
    
    
    chronic %>%
        filter(str_detect( county_name, list ) ) %>%
        mutate(county_name = if_else(county_name == "Monterey", 
                                     paste0("<b style = 'font-size:12pt'>",county_name,"</b>"),
                                     county_name)
        ) %>%
        lollipop(chronic_absenteeism_rate,
                 county_name,
                 kular) +
        labs(x = "",
             y = "",
             color ="",
             title = paste0("Chronic Absenteeism Rates 2023 "),
             subtitle = desc,
             caption = "Source: CDE Absenteeism Downloadable Data Files\nhttps://www.cde.ca.gov/ds/ad/filesabd.asp"
        ) + 
        theme(
            axis.text.y = element_markdown()
        )
    
    ggsave(here("figs","county", paste0("Chronic Absenteeism Rates ",desc,  Sys.Date(),".png" )),
           width = 8, height = 4.5)
}



comp.counties.graph(list = el.list,
                    ass = ELA,
                    "seagreen",
                    "Top Ten Counties with EL Students")


comp.counties.graph(list = frpm.list,
                    ass = ELA,
                    "seagreen",
                    "Top Ten Counties with FRPM Students")


comp.counties.graph(list = both.list,
                    ass = ELA,
                    "seagreen",
                    "Five Counties with most FRPM and EL Students")

comp.counties.graph(list = list.20.70,
                    ass = ELA,
                    "seagreen",
                    "Counties with >70% FRPM and >20% EL Students")



comp.counties.graph(list = el.list,
                    ass = Math,
                    "violetred",
                    "Top Ten Counties with EL Students")


comp.counties.graph(list = frpm.list,
                    ass = Math,
                    "violetred",
                    "Top Ten Counties with FRPM Students")


comp.counties.graph(list = both.list,
                    ass = Math,
                    "violetred",
                    "Five Counties with most FRPM and EL Students")


comp.counties.graph(list = list.20.70,
                    ass = Math,
                    "violetred",
                    "Counties with >70% FRPM and >20% EL Students")




comp.counties.graph(list = el.list,
                    ass = Science,
                    "goldenrod",
                    "Top Ten Counties with EL Students")


comp.counties.graph(list = frpm.list,
                    ass = Science,
                    "goldenrod",
                    "Top Ten Counties with FRPM Students")


comp.counties.graph(list = both.list,
                    ass = Science,
                    "goldenrod",
                    "Five Counties with most FRPM and EL Students")


comp.counties.graph(list = list.20.70,
                    ass = Science,
                    "goldenrod",
                    "Counties with >70% FRPM and >20% EL Students")



comp.counties.chronic(list = el.list,
                    "goldenrod",
                    "Top Ten Counties with EL Students")


comp.counties.chronic(list = frpm.list,
                    "goldenrod",
                    "Top Ten Counties with FRPM Students")


comp.counties.chronic(list = both.list,
                    "goldenrod",
                    "Five Counties with most FRPM and EL Students")


comp.counties.chronic(list = list.20.70,
                      "goldenrod",
                      "Counties with >70% FRPM and >20% EL Students")

####

udp.county %>%
    filter(el.perc >= 20,
           frpm.perc >= 70)
    
    
    
    
    caaspp.county <- tbl(con, "CAASPP") %>% 
    filter(Type_ID == 5,
           Subgroup_ID == 1,
           Grade == 13,
           # County_Code == "27",
           # DistrictCode == "10272",
           Test_Year >= yr.curr) %>%
    collect() %>%
    mutate(Subgroup_ID = as.character(Subgroup_ID)) %>%
    left_join_codebook("CAASPP", "Subgroup_ID") %>%
    rename(Subgroup = definition) %>%
    left_join(ent , by = join_by(County_Code, District_Code, Type_ID,
                                 School_Code )
    ) %>%
    mutate(Type_ID = as.character(Type_ID)) %>%
    left_join_codebook("CAASPP", "Type_ID") %>%
    rename(Entity_Type = definition) %>%
    mutate(across(CAASPP_Reported_Enrollment:Area_4_Percentage_Near_Standard, as.numeric)) %>%
    select(county_name = County_Name, Percentage_Standard_Met_and_Above, Test_Id) %>%
    pivot_wider(id_cols = county_name,
                names_from = Test_Id,
                values_from = Percentage_Standard_Met_and_Above) %>%
    rename(ELA = `1`,
           Math = `2`) %>%
    left_join(udp.county) %>%
    arrange(desc(Math)) %>%
    mutate(Rank = row_number())


    
    
df.20.70 <- caaspp.county %>%
    mutate(el.perc = round2(el.perc,0)/100,
           frpm.perc = round2(frpm.perc,0)/100,
           ELA = round2(ELA,0)/100,
           Math = round2(Math,0)/100,
           ) %>%
        # filter(el.perc >= .2,
        #        frpm.perc >= .7
        #        ) %>%
        mutate(Math.from.Monterey = Math - .22,
               ELA.from.Monterey = ELA - .35) %>%
  #  mutate_at(c(ELA,Math,ELA.from.Monterey,Math.from.Monterey,el.perc,frpm.perc), ./100 )
        select(-Rank)

clipr::write_clip(df.20.70)    
