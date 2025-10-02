# Pulls data about unduplicated pupils to support comparison

udp.county <- udp %>%
  group_by(county_name) %>%
  summarise(across(
    c(english_learner_el, unduplicated_frpm_eligible_count, total_enrollment),
    sum
  )) %>%
  mutate(
    el.perc = round2(100 * english_learner_el / total_enrollment, 1),
    frpm.perc = round2(
      100 * unduplicated_frpm_eligible_count / total_enrollment,
      1
    )
  )


# Creates dataset for comparison across counties

temp <- caaspp.cast.ca %>%
  filter(
    type_id == 5,
    student_group_id == 1,
    grade == 13,
    # County_Code == "27",
    # DistrictCode == "10272",
    test_year >= yr.curr,
    entity_type == "County",
    #       district_code == "00000",
    #  student_group_id %in% standard.groups,
    # test_id == 1,
    !is.na(percentage_standard_met_and_above)
  )


caaspp.county <- caaspp.cast.ca %>%
  # bind_rows(cast.ca) |>
  filter(
    type_id == 5,
    student_group_id == 1,
    grade == 13,
    # County_Code == "27",
    # DistrictCode == "10272",
    test_year >= yr.curr
  ) %>%
  #  clean.caaspp() %>%
  select(
    county_name = county_name,
    percentage_standard_met_and_above,
    test_id
  ) %>%
  pivot_wider(
    id_cols = county_name,
    names_from = test_id,
    values_from = percentage_standard_met_and_above
  ) %>%
  rename(ELA = `1`, Math = `2`, Science = `17`) %>%
  left_join(udp.county) %>%
  arrange(desc(Math)) %>%
  mutate(Rank = row_number())

# write_rds( caaspp.county, here("data","county-comp.rds"))

###

central.coast <- caaspp.county %>%
  filter(str_detect(county_name, "Cruz|Benito|Monterey|Luis|Barb|Vent"))

write_sheet(
  central.coast,
  "https://docs.google.com/spreadsheets/d/1XVyzdcgWEm-KU-_JElUefGeCmNGqzMfw2JaQP1-IRLI/edit#gid=0"
)


central.coast %>%
  lollipop(ELA, county_name, "sea green") +
  labs(
    x = "",
    y = "",
    color = "",
    title = ("CAASPP ELA Rates Meeting or Exceeding for Central Coast Counties"),
    caption = source.link
  )

ggsave(
  here(
    "figs",
    paste0(
      "CAASPP ELA Rates Meeting or Exceeding for Central Coast Counties",
      Sys.Date(),
      ".png"
    )
  ),
  width = 8,
  height = 4.5
)


caaspp.county %>%
  filter(el.perc > 20, frpm.perc > 75) #70


#### Top ten FRPM and EL

all.list <- "a|e|i|o"
central.list <- "Cruz|Benito|Monterey|Luis|Barb|Vent"
frpm.list <- "Colu|Fres|Imperi|Kern|Madera|Merced|Mont|Tular|Mendo"
el.list <- "Colu|Imperi|Mono|Mont|Franc|Barb|Stan|Sacra"
both.list <- "Colu|Imperi|Mont" # Tulare
list.20.70 <- "Colu|Imperi|Madera|Merced|Mont|Tular|Stan" # Tulare
list.20.75 <- "Colu|Imperi|Madera|Merced|Mont|Tular" # Tulare

class.3.above <- c(
  "Alameda|Contra|Fresno|Kern|Angeles|Monterey|Orange|Placer|Riverside|Sacramento|Bernardino|Diego|Joaquin|Mateo|Barbara|Clara|Stanislaus|Tulare|Ventura"
)


write_sheet(
  central.coast,
  "https://docs.google.com/spreadsheets/d/1XVyzdcgWEm-KU-_JElUefGeCmNGqzMfw2JaQP1-IRLI/edit#gid=0"
)


comp.counties.graph <- function(list, ass, kular, desc) {
  caaspp.county %>%
    # Rounding to whole percentage
    mutate(
      ELA = round2(ELA, 1),
      Math = round2(Math, 1),
    ) %>%

    filter(str_detect(county_name, list)) %>%
    mutate(
      county_name = if_else(
        county_name == "Monterey",
        paste0("<b style = 'font-size:12pt'>", county_name, "</b>"),
        county_name
      )
    ) %>%
    # lollipop({{ass}},
    #          county_name,
    #          kular) +

    ggplot2::ggplot(aes(
      y = {{ ass }} / 100,
      x = forcats::fct_reorder(county_name, {{ ass }}),
      label = scales::percent({{ ass }} / 100, accuracy = .1)
    )) +
    geom_segment(
      aes(
        x = forcats::fct_reorder(county_name, {{ ass }} / 100),
        xend = forcats::fct_reorder(county_name, {{ ass }} / 100),
        y = 0,
        yend = {{ ass }} / 100
      ),
      color = kular,
      size = 2
    ) +
    geom_point(color = kular, size = 5, alpha = 0.6) +
    coord_flip() +
    geom_text(size = 3, color = "black") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    #  facet_grid(facets = vars(`Student Group`), scales = "free" ) +
    theme_hc() +
    mcoe_theme +

    labs(
      x = "",
      y = "",
      color = "",
      title = paste0(
        "CAASPP ",
        deparse(substitute(ass)),
        " ",
        yr.curr,
        " Rates Meeting or Exceeding Standards "
      ),
      subtitle = desc,
      caption = source.link
    ) +
    theme(
      axis.text.y = element_markdown()
    )

  ggsave(
    here(
      "figs",
      "county",
      paste0(
        "CAASPP ",
        deparse(substitute(ass)),
        " Rates Meeting or Exceeding Standards ",
        desc,
        Sys.Date(),
        ".png"
      )
    ),
    width = 8,
    height = 4.5
  )
}


### Chronic Section ------

chronic <- tbl(con, "CHRONIC") %>%
  filter(
    academic_year == max(academic_year),
    #      county_code == "27",
    aggregate_level %in% c("C", "T"),
    reporting_category == "TA",
    charter_school == "All",
    dass == "All"

    #    charter_yn == "No",
    #     district_name == "Salinas City Elementary"
  ) %>%
  collect()


comp.counties.chronic <- function(list, kular, desc) {
  chronic %>%
    filter(str_detect(county_name, list)) %>%
    mutate(
      county_name = if_else(
        county_name == "Monterey",
        paste0("<b style = 'font-size:12pt'>", county_name, "</b>"),
        county_name
      )
    ) %>%
    lollipop(chronic_absenteeism_rate, county_name, kular) +
    labs(
      x = "",
      y = "",
      color = "",
      title = paste0("Chronic Absenteeism Rates 2023 "),
      subtitle = desc,
      caption = "Source: CDE Absenteeism Downloadable Data Files\nhttps://www.cde.ca.gov/ds/ad/filesabd.asp"
    ) +
    theme(
      axis.text.y = element_markdown()
    )

  ggsave(
    here(
      "figs",
      "county",
      paste0("Chronic Absenteeism Rates ", desc, Sys.Date(), ".png")
    ),
    width = 8,
    height = 4.5
  )
}

#### Run graphs -------

comp.counties.graph(list = all.list, ass = ELA, "pink", "All Counties")

ggsave(
  here(
    "figs",
    "county",
    paste0(
      "CAASPP ",
      "ELA",
      " Rates Meeting or Exceeding Standards ",
      "All Counties",
      Sys.Date(),
      ".png"
    )
  ),
  width = 12,
  height = 8
)


comp.counties.graph(
  list = el.list,
  ass = ELA,
  "seagreen",
  "Top Eight Counties with EL Students"
)


comp.counties.graph(
  list = frpm.list,
  ass = ELA,
  "seagreen",
  "Top Nine Counties with FRPM Students"
)


comp.counties.graph(
  list = both.list,
  ass = ELA,
  "seagreen",
  "Three Counties with most FRPM and EL Students"
)

comp.counties.graph(
  list = list.20.70,
  ass = ELA,
  "darkseagreen2",
  "Counties with >70 percent FRPM and >20 percent EL Students"
)


comp.counties.graph(
  list = el.list,
  ass = Math,
  "violetred",
  "Top Eight Counties with EL Students"
)


comp.counties.graph(
  list = frpm.list,
  ass = Math,
  "violetred",
  "Top Nine Counties with FRPM Students"
)


comp.counties.graph(
  list = both.list,
  ass = Math,
  "violetred",
  "Three Counties with most FRPM and EL Students"
)


comp.counties.graph(
  list = list.20.70,
  ass = Math,
  "darkseagreen2",
  "Counties with >70 percent FRPM and >20 percent EL Students"
)


comp.counties.graph(
  list = el.list,
  ass = Science,
  "goldenrod",
  "Top Eight Counties with EL Students"
)


comp.counties.graph(
  list = frpm.list,
  ass = Science,
  "goldenrod",
  "Top Nine Counties with FRPM Students"
)


comp.counties.graph(
  list = both.list,
  ass = Science,
  "goldenrod",
  "Three Counties with most FRPM and EL Students"
)


comp.counties.graph(
  list = list.20.70,
  ass = Science,
  "goldenrod",
  "Counties with >70 percent FRPM and >20 percent EL Students"
)


comp.counties.chronic(
  list = el.list,
  "goldenrod",
  "Top Eight Counties with EL Students"
)


comp.counties.chronic(
  list = frpm.list,
  "goldenrod",
  "Top Nine Counties with FRPM Students"
)


comp.counties.chronic(
  list = both.list,
  "goldenrod",
  "Three Counties with most FRPM and EL Students"
)


comp.counties.chronic(
  list = list.20.70,
  "goldenrod",
  "Counties with >70 percent FRPM and >20 percent EL Students"
)

####

caaspp.county <- tbl(con, "CAASPP") %>%
  filter(
    Type_ID == 5,
    student_group_id == 1,
    Grade == 13,
    # County_Code == "27",
    # DistrictCode == "10272",
    Test_Year >= yr.curr
  ) %>%
  collect() %>%
  mutate(student_group_id = as.character(student_group_id)) %>%
  left_join_codebook("CAASPP", "student_group_id") %>%
  rename(Subgroup = definition) %>%
  left_join(
    ent,
    by = join_by(County_Code, District_Code, Type_ID, School_Code)
  ) %>%
  mutate(Type_ID = as.character(Type_ID)) %>%
  left_join_codebook("CAASPP", "Type_ID") %>%
  rename(Entity_Type = definition) %>%
  mutate(across(
    CAASPP_Reported_Enrollment:Area_4_Percentage_Near_Standard,
    as.numeric
  )) %>%
  select(
    county_name = County_Name,
    Percentage_Standard_Met_and_Above,
    Test_Id
  ) %>%
  pivot_wider(
    id_cols = county_name,
    names_from = Test_Id,
    values_from = Percentage_Standard_Met_and_Above
  ) %>%
  rename(ELA = `1`, Math = `2`) %>%
  left_join(udp.county) %>%
  arrange(desc(Math)) %>%
  mutate(Rank = row_number())


df.20.70 <- caaspp.county %>%
  mutate(
    el.perc = round2(el.perc, 0) / 100,
    frpm.perc = round2(frpm.perc, 0) / 100,
    ELA = round2(ELA, 0) / 100,
    Math = round2(Math, 0) / 100,
  ) %>%
  # filter(el.perc >= .2,
  #        frpm.perc >= .7
  #        ) %>%
  mutate(Math.from.Monterey = Math - .22, ELA.from.Monterey = ELA - .35) %>%
  #  mutate_at(c(ELA,Math,ELA.from.Monterey,Math.from.Monterey,el.perc,frpm.perc), ./100 )
  select(-Rank)

clipr::write_clip(df.20.70)


### Determine how many counties improved and average size of improvement ----

state.wide <- caaspp.ca %>%
  filter(type_id == 5, student_group_id == 1, grade == 13) |>
  # clean.caaspp() |>
  bind_rows(caaspp.county.comp) |>
  filter(test_id == 1, str_detect(county_name, class.3.above)) |>
  pivot_wider(
    id_cols = county_name,
    names_from = test_year,
    values_from = percentage_standard_met_and_above
  ) |>
  mutate(
    change.one.year = round2(`2025` - `2024`, digits = 1),
    change.two.year = round2(`2025` - `2023`, digits = 1),
    change.one.pos = if_else(change.one.year > 0, TRUE, FALSE),
    change.two.pos = if_else(change.two.year > 0, TRUE, FALSE)
  )


# Estimating California -----

caaspp.ca %>%
  filter(type_id == 5, student_group_id == 1, grade == 13) |>
  clean.caaspp() |>
  group_by(test_id) |>
  summarise(weighted.mean(
    percentage_standard_met_and_above,
    total_students_tested_with_scores
  ))
