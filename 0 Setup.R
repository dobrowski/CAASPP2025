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
library(googlesheets4)

yr.curr <- 2025

yr.prior <- 2024

con <- mcoe_sql_con()

# ent <- read_delim(here("data","sb_ca2022entities_csv.txt"), delim = "^")
# ent <- read_delim(here("data", "sb_ca2024entities_csv.txt"), delim = "^")

ent <- vroom(
  here("data", "sb_ca2025entities_csv.txt"),
  .name_repair = ~ janitor::make_clean_names(., case = "snake"),
  delim = "^"
)


ent2 <- ent %>%
  select(-test_year, -type_id)


clean.caaspp <- function(df) {
  df %>%
    mutate(student_group_id = as.character(student_group_id)) %>%
    left_join_codebook("CAASPP", "student_group_id") %>%
    rename(student_group = definition) %>%
    left_join(ent2) %>%
    mutate(type_id = as.character(type_id)) %>%
    left_join_codebook("CAASPP", "type_id") %>%
    rename(entity_type = definition) %>%
    #      mutate(across(total_students_enrolled:composite_area_2_total, as.numeric))
    mutate(across(
      any_of(c(
        "total_students_enrolled",
        "total_students_tested",
        "total_students_tested_with_scores",
        "total_caaspp_enrollment",
        "total_tested_at_entity_level",
        "total_tested_at_subgroup_level",
        "caaspp_reported_enrollment",
        "students_tested",
        "mean_scale_score",
        "percentage_standard_exceeded",
        "percentage_standard_met",
        "percentage_standard_met_and_above",
        "percentage_standard_nearly_met",
        "percentage_standard_not_met",
        "students_with_scores",
        "area_1_percentage_above_standard",
        "area_1_percentage_at_or_near_standard",
        "area_1_percentage_below_standard",
        "area_2_percentage_above_standard",
        "area_2_percentage_at_or_near_standard",
        "area_2_percentage_below_standard",
        "area_3_percentage_above_standard",
        "area_3_percentage_at_or_near_standard",
        "area_3_percentage_below_standard",
        "area_4_percentage_above_standard",
        "area_4_percentage_at_or_near_standard",
        "area_4_percentage_below_standard",
        "total_tested_with_scores",
        "area_1_percentage_near_standard",
        "area_2_percentage_near_standard",
        "area_3_percentage_near_standard",
        "area_4_percentage_near_standard",
        "count_standard_exceeded",
        "count_standard_met",
        "count_standard_met_and_above",
        "count_standard_nearly_met",
        "count_standard_not_met",
        "overall_total",
        "area_1_count_above_standard",
        "area_1_count_near_standard",
        "area_1_count_below_standard",
        "area_1_total",
        "area_2_count_above_standard",
        "area_2_count_near_standard",
        "area_2_count_below_standard",
        "area_2_total",
        "area_3_count_above_standard",
        "area_3_count_near_standard",
        "area_3_count_below_standard",
        "area_3_total",
        "area_4_count_above_standard",
        "area_4_count_near_standard",
        "area_4_count_below_standard",
        "area_4_total",
        "composite_area_1_percentage_above_standard",
        "composite_area_1_count_above_standard",
        "composite_area_1_percentage_near_standard",
        "composite_area_1_count_near_standard",
        "composite_area_1_percentage_below_standard",
        "composite_area_1_count_below_standard",
        "composite_area_1_total",
        "composite_area_2_percentage_above_standard",
        "composite_area_2_count_above_standard",
        "composite_area_2_percentage_near_standard",
        "composite_area_2_count_near_standard",
        "composite_area_2_percentage_below_standard",
        "composite_area_2_count_below_standard",
        "composite_area_2_total",
        "life_sciences_domain_percent_below_standard",
        "life_sciences_domain_percent_near_standard",
        "life_sciences_domain_count_above_standard",
        "life_sciences_domain_total",
        "physical_sciences_domain_count_below_standard",
        "physical_sciences_domain_count_near_standard",
        "physical_sciences_domain_count_above_standard",
        "physical_sciences_domain_total",
        "earth_and_space_sciences_domain_count_below_standard",
        "earth_and_space_sciences_domain_count_near_standard",
        "earth_and_space_sciences_domain_count_above_standard",
        "earth_and_space_sciences_domain_total",
        "life_sciences_domain_percent_below_standard",
        "life_sciences_domain_count_below_standard",
        "life_sciences_domain_percent_near_standard",
        "life_sciences_domain_count_near_standard",
        "life_sciences_domain_percent_above_standard",
        "life_sciences_domain_count_above_standard",
        "life_sciences_domain_total",
        "physical_sciences_domain_percent_below_standard",
        "physical_sciences_domain_count_below_standard",
        "physical_sciences_domain_percent_near_standard",
        "physical_sciences_domain_count_near_standard",
        "physical_sciences_domain_percent_above_standard",
        "physical_sciences_domain_count_above_standard",
        "physical_sciences_domain_total",
        "earth_and_space_sciences_domain_percent_below_standard",
        "earth_and_space_sciences_domain_count_below_standard",
        "earth_and_space_sciences_domain_percent_near_standard",
        "earth_and_space_sciences_domain_count_near_standard",
        "earth_and_space_sciences_domain_percent_above_standard",
        "earth_and_space_sciences_domain_count_above_standard",
        "earth_and_space_sciences_domain_total"
      )),
      as.numeric
    ))
}


caaspp.ca <- vroom(
  here("data", "sb_ca2025_all_csv_v1.txt"),
  .name_repair = ~ janitor::make_clean_names(., case = "snake"),
  delim = "^"
) %>%
  clean.caaspp()


cast.ca <- vroom(
  here("data", "cast_ca2025_all_csv_v1.txt"),
  .name_repair = ~ janitor::make_clean_names(., case = "snake"),
  delim = "^"
) %>%
  clean.caaspp()


caaspp.mry <- caaspp.ca |>
  bind_rows(cast.ca) %>%
  filter(
    county_code == "27",
    # DistrictCode == "10272",
    # test_year >= yr.curr
  ) # %>%
#  clean.caaspp()

#### ELPAC --------

elpac.ca <- vroom(
  here("data", "sa_elpac2025_all_csv_v1.txt"),
  .name_repair = ~ janitor::make_clean_names(., case = "snake"),
  delim = "^"
)


### District List ----

districts <- c(
  "Carmel",
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


## standards group list ----

standard.groups <- c(
  1, #	All Students	,	All Students	,
  128, #	Disability Status	,	Reported disabilities	,
  # 99	,	#	Disability Status	,	No reported disabilities	,
  31, #	Economic Status	,	Socioeconomically disadvantaged	,
  # 111	,	#	Economic Status	,	Not socioeconomically disadvantaged	,
  # 6	,	#	English-Language Fluency	,	IFEP, RFEP, and EO (Fluent English proficient and English only)	,
  # 7	,	#	English-Language Fluency	,	IFEP (Initial fluent English proficient)	,
  8, #	English-Language Fluency	,	RFEP (Reclassified fluent English proficient)	,
  # 120	,	#	English-Language Fluency	,	ELs enrolled less than 12 months	,
  # 142	,	#	English-Language Fluency	,	ELs enrolled 12 months or more	,
  160, #	English-Language Fluency	,	EL (English learner)	,
  # 243	,	#	English-Language Fluency	,	ADEL (Adult English learner)	,
  180, #	English-Language Fluency	,	EO (English only)	,
  # 170	,	#	English-Language Fluency	,	Ever–EL	,
  250, #	English-Language Fluency	,	LTEL (Long-Term English learner)	,
  # 251	,	#	English-Language Fluency	,	AR–LTEL (At-Risk of becoming LTEL)	,
  # 252	,	#	English-Language Fluency	,	Never–EL	,
  # 190	,	#	English-Language Fluency	,	TBD (To be determined)	,
  75, #	Race and Ethnicity	,	American Indian or Alaska Native	,
  76, #	Race and Ethnicity	,	Asian	,
  74, #	Race and Ethnicity	,	Black or African American	,
  77, #	Race and Ethnicity	,	Filipino	,
  78, #	Race and Ethnicity	,	Hispanic or Latino	,
  79, #	Race and Ethnicity	,	Native Hawaiian or Pacific Islander	,
  80, #	Race and Ethnicity	,	White	,
  144, #	Race and Ethnicity	,	Two or more races	,
  # 201	,	#	Ethnicity for Economically Disadvantaged	,	American Indian or Alaska Native	,
  # 202	,	#	Ethnicity for Economically Disadvantaged	,	Asian	,
  # 200	,	#	Ethnicity for Economically Disadvantaged	,	Black or African American	,
  # 203	,	#	Ethnicity for Economically Disadvantaged	,	Filipino	,
  # 204	,	#	Ethnicity for Economically Disadvantaged	,	Hispanic or Latino	,
  # 205	,	#	Ethnicity for Economically Disadvantaged	,	Native Hawaiian or Pacific Islander	,
  # 206	,	#	Ethnicity for Economically Disadvantaged	,	White	,
  # 207	,	#	Ethnicity for Economically Disadvantaged	,	Two or more races	,
  # 221	,	#	Ethnicity for Not Economically Disadvantaged	,	American Indian or Alaska Native	,
  # 222	,	#	Ethnicity for Not Economically Disadvantaged	,	Asian	,
  # 220	,	#	Ethnicity for Not Economically Disadvantaged	,	Black or African American	,
  # 223	,	#	Ethnicity for Not Economically Disadvantaged	,	Filipino	,
  # 224	,	#	Ethnicity for Not Economically Disadvantaged	,	Hispanic or Latino	,
  # 225	,	#	Ethnicity for Not Economically Disadvantaged	,	Native Hawaiian or Pacific Islander	,
  # 226	,	#	Ethnicity for Not Economically Disadvantaged	,	White	,
  # 227	,	#	Ethnicity for Not Economically Disadvantaged	,	Two or more races	,
  # 4	,	#	Gender	,	Female	,
  # 3	,	#	Gender	,	Male	,
  # 28	,	#	Migrant	,	Migrant education	,
  # 29	,	#	Migrant	,	Not migrant education	,
  # 90	,	#	Parent Education	,	Not a high school graduate	,
  # 91	,	#	Parent Education	,	High school graduate	,
  # 92	,	#	Parent Education	,	Some college (includes AA degree)	,
  # 93	,	#	Parent Education	,	College graduate	,
  # 94	,	#	Parent Education	,	Graduate school/Postgraduate	,
  # 121	,	#	Parent Education	,	Declined to state	,
  # 50	,	#	Military Status	,	Armed forces family member	,
  # 51	,	#	Military Status	,	Not armed forces family member	,
  52, #	Homeless Status	,	Homeless	,
  # 53	,	#	Homeless Status	,	Not homeless	,
  240 #	Foster Status	,	Foster youth	,
  # 241		#	Foster Status	,	Not foster youth	,
)


#### Historic data ----

caaspp.county.comp <- tbl(con, "CAASPP") %>%
  filter(
    #county_code == "27",
    type_id %in% c(5, 4),
    subgroup_id == 1,
    grade == 13,
    # DistrictCode == "10272",
    #     test_year >= yr.prior
  ) %>%
  collect() %>%
  rename(student_group_id = subgroup_id) %>%
  clean.caaspp()


caaspp.mry.hist <- tbl(con, "CAASPP") %>%
  filter(
    county_code == "27",
    # DistrictCode == "10272",
    test_year >= yr.prior
  ) %>%
  collect() %>%
  # rename(student_group_id = subgroup_id) %>%
  clean.caaspp()


yr.acad <- paste0(yr.prior, "-", yr.curr)

udp <- tbl(con, "upc") %>%
  filter(
    # County_Code == "27",
    # DistrictCode == "10272",
    academic_year == yr.acad
  ) %>%
  #    head() %>%
  collect()


caaspp.wide <- function(df, namer) {
  df %>%
    pivot_wider(
      id_cols = {{ namer }},
      names_from = Test_Id,
      values_from = Percentage_Standard_Met_and_Above
    ) %>%
    rename(ELA = `1`, Math = `2`)
}


# Historic CAST data ----

cast <- tbl(con, "CAST") %>%
  filter(
    # county_code == "27",
    # DistrictCode == "10272",
    test_year >= "2022"
  ) %>%
  collect() %>%
  mutate(
    demographic_id = as.character(demographic_id),
    subgroup_id = demographic_id,
    students_tested = total_students_tested
  ) %>%
  left_join_codebook("CAST", "demographic_id") %>%
  rename(subgroup = definition) %>%
  select(-district_name, -school_name) %>%
  left_join(
    ent2,
    by = join_by(filler, county_code, district_code, school_code)
  ) %>%
  mutate(type_id = as.character(type_id)) %>%
  left_join_codebook("CAST", "type_id") %>%
  rename(entity_type = definition)


cast.mry <- cast %>%
  filter(
    county_code == "27",
    # DistrictCode == "10272",
    test_year >= "2022"
  ) |>
  mutate(
    demographic_id = as.character(demographic_id),
    student_group_id = demographic_id,
    students_tested = total_students_tested
  ) |>
  select(-entity_type) |>
  clean.caaspp()

caaspp.cast.mry <- caaspp.mry |>
  bind_rows(caaspp.mry.hist) %>%
  bind_rows(cast.mry)


cast.counties <- cast %>%
  filter(
    entity_type == "County",
    #   county_code == "27",
    # DistrictCode == "10272",
    test_year >= "2022"
  ) |>
  mutate(
    demographic_id = as.character(demographic_id),
    student_group_id = demographic_id,
    students_tested = total_students_tested
  ) |>
  select(-entity_type) |>
  clean.caaspp()

caaspp.cast.ca <- caaspp.ca |>
  bind_rows(caaspp.county.comp) |> ## CAASPP state history
  bind_rows(cast.counties) |> # CAST historic
  bind_rows(cast.ca) |> # CAST current year
  distinct()
