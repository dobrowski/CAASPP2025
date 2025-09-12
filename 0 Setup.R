

# Load libraries

library(tidyverse)
library(janitor)
library(MCOE)
library(here)
library(ggthemes)
library(vroom)
library(ggrepel)
library(readxl)
library(ggtext)
library(scales)

yr.curr <- 2024

yr.prior <- 2023

con <- mcoe_sql_con()

# ent <- read_delim(here("data","sb_ca2022entities_csv.txt"), delim = "^")
ent <- read_delim(here("data","sb_ca2024entities_csv.txt"), delim = "^")


ent <- vroom(here("data","sb_ca2024entities_csv.txt"),
             .name_repair = ~ janitor::make_clean_names(., case = "snake"),
             delim = "^"
)


ent2 <- ent %>%
    select(-test_year, -type_id)


districts <- c("Carmel",
               "Pacific Grove",
               "Graves",
               "Washington",
               "Lagunita",
               "Spreckels",
               "South Monterey",
               "Chualar",
               "Mission",
               "San Antonio",
               "Santa Rita",
               "Salinas Union",
               "Alisal",
               "Monterey Peninsula",
               "North Monterey",
               "San Lucas",
               "Salinas City",
               "Soledad",
               "King City",
               "Monterey County Office of Ed",
               "Gonzales",
               "Greenfield",
               "Bradley",
               "San Ardo"
               
)


caaspp.mry <- tbl(con, "CAASPP") %>% 
    filter(county_code == "27",
           # DistrictCode == "10272",
           test_year >= yr.curr) %>%
    collect() %>%
    mutate(subgroup_id = as.character(subgroup_id)) %>%
    left_join_codebook("CAASPP", "subgroup_id") %>%
    rename(subgroup = definition) %>%
    left_join(ent2) %>%
    mutate(type_id = as.character(type_id)) %>%
    left_join_codebook("CAASPP", "type_id") %>%
    rename(entity_type = definition) %>%
    mutate(across(caaspp_reported_enrollment:area_4_percentage_near_standard, as.numeric))



clean.caaspp <- function(df) {
    df %>%
        mutate(subgroup_id = as.character(subgroup_id)) %>%
        left_join_codebook("CAASPP", "subgroup_id") %>%
        rename(subgroup = definition) %>%
        left_join(ent2) %>%
        mutate(type_id = as.character(type_id)) %>%
        left_join_codebook("CAASPP", "type_id") %>%
        rename(entity_type = definition) %>%
        mutate(across(caaspp_reported_enrollment:area_4_percentage_near_standard, as.numeric))
    
}

caaspp.suhsd <- tbl(con, "CAASPP") %>% 
    filter(County_Code == "27",
            District_Code == "66159",
           Test_Year %in% c("2019", "2022")
           )%>%
    collect() %>%
    clean.caaspp()

yr.acad <- paste0(yr.prior,"-",yr.curr)

udp <- tbl(con, "upc") %>% 
    filter(# County_Code == "27",
        # DistrictCode == "10272",
        academic_year ==   yr.acad
    ) %>%
    #    head() %>%
    collect() 




caaspp.wide <- function(df, namer) {
    df %>%
        pivot_wider(id_cols = {{namer}},
                    names_from = Test_Id,
                    values_from = Percentage_Standard_Met_and_Above) %>%
        rename(ELA = `1`,
               Math = `2`)
    
}




cast <- tbl(con, "CAST") %>% 
    filter(# county_code == "27",
           # DistrictCode == "10272",
           test_year >= "2022") %>%
    collect()  %>%
    mutate(demographic_id = as.character(demographic_id),
           subgroup_id = demographic_id,
           students_tested = total_students_tested) %>%
    left_join_codebook("CAST", "demographic_id") %>%
    rename(subgroup = definition) %>%
    select(-district_name, -school_name) %>%
    left_join(ent2, by = join_by(filler, county_code,
                                 district_code, school_code) ) %>%
    mutate(type_id = as.character(type_id)) %>%
    left_join_codebook("CAST", "type_id") %>%
    rename(entity_type = definition)


cast.mry <- cast %>% 
    filter( county_code == "27",
        # DistrictCode == "10272",
        test_year >= "2022") 

caaspp.cast.mry <- caaspp.mry %>%
    bind_rows(cast.mry) %>%
    filter(test_year == yr.curr) 

