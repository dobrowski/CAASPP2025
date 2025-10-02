### Graph Functions ------

# All Districts All Students

logo <- mcoe_logo()

source.link <- "Source: Smarter Balance Summative Assessment Research Files  \n https://caaspp-elpac.ets.org/"


county.graph <- function(
  df = caaspp.mry,
  test.id,
  kular = "steel blue",
  grd = 13
) {
  test.name <- case_match(test.id, 1 ~ "ELA", 2 ~ "Math", 17 ~ "Science", )

  grd.name <- if_else(grd == 13, "", paste0(" - Grade ", grd))

  df %>%
    filter(
      grade == grd,
      student_group_id == "1",
      test_id == test.id, # ELA
      entity_type == "District",
      !is.na(percentage_standard_met_and_above)
    ) %>%
    lollipop(percentage_standard_met_and_above, district_name, kular) +
    labs(
      x = "",
      y = "",
      color = "",
      title = paste0("CAASPP ", yr.curr, " ", test.name, grd.name),
      subtitle = "Rates Meeting or Exceeding Standards by District",
      caption = source.link
    )
  #    grid::grid.raster(logo, x = 0.03, y = 0.03, just = c('left', 'bottom'), width = unit(.75, 'inches'))
  # dev.off()

  ggsave(
    here(
      "figs",
      paste0(
        "All Districts ",
        test.name,
        " - ",
        grd,
        " Rates Meeting or Exceeding ",
        Sys.Date(),
        ".png"
      )
    ),
    width = 7,
    height = 5
  )
}


county.graph(caaspp.mry, 1, "lightskyblue1")

county.graph(caaspp.mry, 2, "lightskyblue1")


# loop for middle grades
for (j in 4:6) {
  for (i in 1:2) {
    county.graph(caaspp.mry, i, "lightskyblue1", grd = j)
  }
}
county.graph(caaspp.mry, 1, "plum1", grd = 4)

county.graph(caaspp.mry, 2, "steel blue", )


cast.mry %>%
  filter(test_year == 2024) %>%
  county.graph(17, "lightgreen")


county.graph(caaspp.imp, 1, "limegreen")

county.graph(caaspp.imp, 2, "limegreen")

cast.imp %>%
  filter(test_year == yr.curr) %>%
  county.graph(17, "limegreen")


# Sorts in Alphabetical Order

county.alpha <- function(df = caaspp.mry, test.id, colorme) {
  test.name <- case_match(test.id, 1 ~ "ELA", 2 ~ "Math", 17 ~ "Science", )

  df %>%
    filter(
      grade == 13,
      student_group_id == "1",
      test_id == test.id, # ELA
      entity_type == "District",
      !is.na(percentage_standard_met_and_above)
    ) %>%
    ggplot2::ggplot(aes(
      y = percentage_standard_met_and_above / 100,
      x = reorder(district_name, desc(district_name)), #forcats::fct_reorder(District_Name,Percentage_Standard_Met_and_Above) ,
      label = scales::percent(
        percentage_standard_met_and_above / 100,
        accuracy = .1
      )
    )) +
    geom_segment(
      aes(
        x = reorder(district_name, desc(district_name)), #forcats::fct_reorder(District_Name, Percentage_Standard_Met_and_Above/100),
        xend = reorder(district_name, desc(district_name)), #forcats::fct_reorder(District_Name, Percentage_Standard_Met_and_Above/100),
        y = 0,
        yend = percentage_standard_met_and_above / 100
      ),
      color = colorme,
      size = 2
    ) +
    geom_point(color = colorme, size = 5, alpha = 0.6) +
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
        yr.curr,
        " ",
        test.name,
        " Rates Meeting or Exceeding Standards by District"
      ),
      caption = source.link
    )
  # grid::grid.raster(logo, x = 0.03, y = 0.03, just = c('left', 'bottom'), width = unit(.75, 'inches'))
  # # dev.off()

  ggsave(
    here(
      "figs",
      paste0(
        "All Districts ",
        test.name,
        " Rates Meeting or Exceeding - Alpha ",
        Sys.Date(),
        ".png"
      )
    ),
    width = 8,
    height = 6
  )
}


county.alpha(caaspp.mry, 1, colorme = "steel blue")
county.alpha(caaspp.mry, 2, colorme = "steel blue")

cast.mry %>%
  filter(test_year == 2024) %>%
  county.alpha(17, colorme = "lightgreen")

#
#
# #  11th Grade ELA
#
# caaspp.mry %>%
#     filter(Grade == 11,
#            student_group_id == "1",
#            Test_Id == 1, # ELA
#            Entity_Type == "School",
#            !is.na(Percentage_Standard_Met_and_Above)
#            )%>%
#     lollipop(Percentage_Standard_Met_and_Above,
#              School_Name,
#              "sea green") +
#     labs(x = "",
#          y = "",
#          color ="",
#          title = ("CAASPP ELA Rates Meeting or Exceeding by 11th grade"),
#          caption = source.link
#     )
#
# ggsave(here("figs", paste0("All Districts 11th grade ELA Rates Meeting or Exceeding",  Sys.Date(),".png" )),
#        width = 8, height = 6)
#
# # Student Groups at Salinas Union
#
# caaspp.mry %>%
#     filter(Grade == 13,
#            str_detect(District_Name,"Salinas Union"),
#           # student_group_id == "1",
#            Test_Id == 1, # ELA
#            Entity_Type == "District",
#            !is.na(Percentage_Standard_Met_and_Above),
#           !str_detect(Subgroup, " - ")
#     ) %>%
#     lollipop(Percentage_Standard_Met_and_Above,
#              Subgroup,
#              "sea green") +
#     labs(x = "",
#          y = "",
#          color ="",
#          title = ("CAASPP ELA Rates Meeting or Exceeding at Salinas Union by Student Group"),
#          caption = source.link
#     )
#
# ggsave(here("figs", paste0("CAASPP ELA Rates Meeting or Exceeding at Salinas Union by Student Group",  Sys.Date(),".png" )),
#        width = 8, height = 6)
#
#
#

caaspp.mry %>%
  filter(
    Grade == 13,
    is.na(District_Name),
    # student_group_id == "1",
    Test_Id == 1, # ELA
    #      Entity_Type == "District",
    !is.na(Percentage_Standard_Met_and_Above),
    !str_detect(Subgroup, " - ")
  ) %>%
  lollipop(Percentage_Standard_Met_and_Above, Subgroup, "steel blue") +
  labs(
    x = "",
    y = "",
    color = "",
    title = paste0(
      "Monterey County ",
      "ELA",
      " Rates Meeting or Exceeding Standards by Student Group"
    ),
    caption = source.link
  )

ggsave(
  here(
    "figs",
    paste0(
      "Monterey County ",
      "ELA",
      " Rates Meeting or Exceeding by Student Group",
      Sys.Date(),
      ".png"
    )
  ),
  width = 8,
  height = 6
)


### Subgroups by District ------

lolli.subgroups <- function(df = caaspp.mry, dist = "", test.id = 1) {
  test.name <- case_match(test.id, 1 ~ "ELA", 2 ~ "Math", 17 ~ "Science", )

  df %>%
    filter(
      grade == 13,
      str_detect(district_name, dist),
      student_group_id %in% standard.groups,

      # student_group_id == "1",
      test_id == test.id, # ELA
      entity_type == "District",
      !is.na(percentage_standard_met_and_above),
      !str_detect(student_group, " - ")
    ) %>%
    mutate(
      subgroup.n = paste0(
        student_group,
        " (",
        total_students_tested_with_scores,
        ")"
      )
    ) %>%
    lollipop(percentage_standard_met_and_above, subgroup.n, "sea green") +
    labs(
      x = "",
      y = "",
      color = "",
      title = paste0(
        "CAASPP ",
        yr.curr,
        " ",
        test.name,
        " Rates Meeting or Exceeding Standards at \n",
        dist,
        " by Student Group"
      ),
      caption = source.link
    )

  ggsave(
    here(
      "figs",
      paste0(
        dist,
        " ",
        test.name,
        " Rates by Student Group ",
        Sys.Date(),
        ".png"
      )
    ),
    width = 8,
    height = 4.5
  )
}

lolli.subgroups(caaspp.mry, "South Monterey County", 2)

lolli.subgroups(caaspp.mry, "South Monterey County", 1)

lolli.subgroups(caaspp.mry, "Soledad", 2)

cast.mry %>%
  filter(test_year == 2024) %>%
  lolli.subgroups("South Monterey County", 17)


districts <- c("Greenfield Union", "South Monterey County")

districts <- caaspp.mry$district_name %>% unique()

test.list <- c(1, 2, 17)

for (i in test.list) {
  for (j in districts) {
    lolli.subgroups(caaspp.mry, j, i)
  }
}


### Subgroups countywide ------

# caaspp.mry %>%
#     filter(grade == 13,
#            is.na(district_name),
#
#            student_group_id %in% standard.groups,
#            test_id == 2, # ELA
#  #          Entity_Type == "District",
#            !is.na(percentage_standard_met_and_above),
#            !str_detect(student_group, " - ")
#     ) %>%
#     lollipop(percentage_standard_met_and_above,
#              student_group,
#              "orange") +
#     labs(x = "",
#          y = "",
#          color ="",
#          title = paste0("CAASPP ", "Math ", yr.curr , " Rates Meeting or Exceeding Standards \n Monterey County by Student Group "),
#          caption = source.link
#     )
#
# ggsave(here("figs", paste0("Monterey County ", "Math" ,  " Rates by Student Group ",  Sys.Date(),".png" )),
#        width = 8, height = 4.5)

lolli.subgroups.county <- function(df, testy, kular, tit) {
  test.name <- case_match(testy, 1 ~ "ELA", 2 ~ "Math", 17 ~ "Science", )

  df %>%
    filter(
      #grade == 13,
      #is.na(district_name),

      student_group_id %in% standard.groups,
      test_id == testy, # ELA
      #          Entity_Type == "District",
      !is.na(percentage_standard_met_and_above),
      #    !str_detect(subgroup, " - ")
    ) %>%
    lollipop(percentage_standard_met_and_above, student_group, kular) +
    labs(
      x = "",
      y = "",
      color = "",
      title = paste0(
        "CAASPP ",
        test.name,
        " ",
        yr.curr,
        " Rates Meeting or Exceeding Standards \n",
        tit,
        " by Student Group"
      ),
      caption = source.link
    )

  ggsave(
    here(
      "figs",
      paste0(
        tit,
        " ",
        test.name,
        " Rates by Student Group ",
        Sys.Date(),
        ".png"
      )
    ),
    width = 8,
    height = 4.5
  )
}


caaspp.mry %>%
  filter(grade == 13, is.na(district_name)) %>%
  lolli.subgroups.county(2, "orange", "Monterey County")


caaspp.cast.mry %>%
  filter(grade == 13, is.na(district_name)) %>%
  lolli.subgroups.county(17, "orange", "Monterey County")


#### Subgroups in a School --------

lolli.subgroups.school <- function(
  df = caaspp.mry,
  dist = "",
  schoo = "",
  test.id = 1,
  kular = "sea green"
) {
  test.name <- case_match(test.id, 1 ~ "ELA", 2 ~ "Math", 17 ~ "Science", )

  df %>%
    filter(
      grade == 13,
      str_detect(district_name, dist),
      str_detect(school_name, schoo),
      student_group_id %in% standard.groups,
      # student_group_id == "1",
      test_id == test.id, # ELA
      entity_type == "School",
      !is.na(percentage_standard_met_and_above),
      # !str_detect(subgroup, " - "),
      # !str_detect(subgroup, "Declined")
    ) %>%
    mutate(
      subgroup.n = paste0(
        student_group,
        " (",
        total_students_tested_with_scores,
        ")"
      )
    ) %>%
    lollipop(percentage_standard_met_and_above, subgroup.n, kular) +
    # theme(panel.grid.major.x = element_line(color = "dark grey",
    #                                       size = 0.5,
    #                                       linetype = 1)) +
    labs(
      x = "",
      y = "",
      color = "",
      title = paste0(
        "CAASPP ",
        yr.curr,
        " ",
        test.name,
        " Rates Meeting or Exceeding Standards"
      ),
      subtitle = paste0(dist, " - ", schoo, " by Student Group"),
      #            caption = source.link
    )

  ggsave(
    here(
      "figs",
      dist,
      paste0(
        dist,
        "-",
        schoo,
        " ",
        test.name,
        " Rates by Student Group ",
        Sys.Date(),
        ".png"
      )
    ),
    width = 8,
    height = 4.5
  )
}


lolli.subgroups.school(caaspp.cast.mry, "Soledad", "Main St", 17)


lolli.subgroups.school("Salinas Union", "^Salinas High", 1)

lolli.subgroups.school("Salinas Union", "^Salinas High", 2)

caaspp.mry %>%
  filter(
    Grade == 13,
    str_detect(District_Name, "South Monterey County"),
    str_detect(School_Name, "King City"),

    student_group_id %in% c("1", "128", "160"),
    Test_Id == 2, # ELA
    Entity_Type == "School",
    !is.na(Percentage_Standard_Met_and_Above),
    !str_detect(Subgroup, " - "),
    !str_detect(Subgroup, "Declined")
  ) %>%
  mutate(
    subgroup.n = paste0(Subgroup, " (", Total_Tested_At_Entity_Level, ")")
  ) %>%
  lollipop(Percentage_Standard_Met_and_Above, subgroup.n, "sea green") +
  labs(
    x = "",
    y = "",
    color = "",
    title = paste0("CAASPP ", "Math", " Rates Meeting or Exceeding Standards"),
    subtitle = paste0(
      "South Monterey County",
      "-",
      "King City",
      " by Student Group"
    ),
    caption = source.link
  )

ggsave(
  here(
    "figs",
    paste0(
      "South Monterey County",
      "-",
      "King City ",
      " ",
      "Math",
      " Rates by Student Group ",
      Sys.Date(),
      ".png"
    )
  ),
  width = 4,
  height = 3
)


caaspp.mry %>%
  filter(str_detect(district_name, "Greenfield")) %>%
  select(school_name) %>%
  distinct()


schools <- c(
  "Mary Chapa Academy",
  "Arroyo Seco Academy",
  "Oak Avenue Elementary",
  "Cesar Chavez Elementary",
  "Vista Verde Middle"
)

for (i in schools) {
  for (j in test.list) {
    lolli.subgroups.school(caaspp.cast.mry, "Greenfield", i, 17)
  }
}


lolli.subgroups.school(
  caaspp.cast.mry.curr,
  "Salinas Union High",
  "^Salinas High",
  17
)
lolli.subgroups.school(caaspp.cast.mry.curr, "Greenfield", "Mary Chapa", 1)

dist.list <- caaspp.cast.mry.curr |>
  select(district_name) |>
  na.omit() |>
  distinct() |>
  unlist()


for (dist in dist.list) {
  print(dist)

  schools <- caaspp.cast.mry.curr |>
    filter(str_detect(dist, district_name)) |>
    select(school_name) |>
    na.omit() |>
    distinct() |>
    unlist()

  for (i in schools) {
    print(i)

    for (j in test.list) {
      lolli.subgroups.school(caaspp.cast.mry.curr, dist, i, j)
    }
  }
}


### Charter Schools -------

lolli.subgroups.charter <- function(df, schoo = "", test.id = 1) {
  test.name <- case_match(test.id, 1 ~ "ELA", 2 ~ "Math", 17 ~ "Science", )

  df %>%
    filter(
      grade == 13,
      str_detect(school_name, schoo),
      student_group_id %in% standard.groups,

      # student_group_id == "1",
      test_id == test.id, # ELA
      type_id %in% c(9, 10),
      !is.na(percentage_standard_met_and_above),
      !str_detect(student_group, " - "),
      !str_detect(student_group, "Declined")
    ) %>%
    mutate(
      student_group.n = paste0(
        student_group,
        " (",
        total_students_tested_with_scores,
        ")"
      )
    ) %>%
    lollipop(percentage_standard_met_and_above, student_group.n, "sea green") +
    # theme(panel.grid.major.x = element_line(color = "dark grey",
    #                                         size = 0.5,
    #                                         linetype = 1)) +
    labs(
      x = "",
      y = "",
      color = "",
      title = paste0(
        "CAASPP ",
        yr.curr,
        " ",
        test.name,
        " Rates Meeting or Exceeding Standards"
      ),
      subtitle = paste0(schoo, " by Student Group"),
      caption = source.link
    )

  ggsave(
    here(
      "figs",
      paste0(
        schoo,
        " ",
        test.name,
        " Rates by Student Group ",
        Sys.Date(),
        ".png"
      )
    ),
    width = 8,
    height = 4.5
  )
}

caaspp.cast.mry |>
  filter(test_year == yr.curr) |>
  lolli.subgroups.charter("Monterey Bay", 1)


schools <- caaspp.mry %>%
  filter(type_id %in% c(9, 10)) %>%
  select(school_name) %>%
  distinct() %>%
  unlist()

for (i in schools) {
  for (j in test.list) {
    lolli.subgroups.charter(caaspp.mry, i, j)
  }
}


caaspp.mry %>%
  filter(
    grade == 13,
    student_group_id == "1",
    test_id == 2, # ELA
    entity_type %in%
      c("Direct Funded Charter School", "Locally Funded Charter School"),
    !is.na(percentage_standard_met_and_above)
  ) %>%
  lollipop(percentage_standard_met_and_above, school_name, "violet") +
  labs(
    x = "",
    y = "",
    color = "",
    title = paste0("Charter CAASPP ", yr.curr, " ", "Math"),
    subtitle = "Rates Meeting or Exceeding Standards by District",
    caption = source.link
  )


ggsave(
  here(
    "figs",
    paste0(
      "Charters ",
      "Math",
      " Rates Meeting or Exceeding ",
      Sys.Date(),
      ".png"
    )
  ),
  width = 10,
  height = 6
)


county.graph.w.charter <- function(df = caaspp.mry, test.id) {
  test.name <- case_match(test.id, 1 ~ "ELA", 2 ~ "Math", 17 ~ "Science", )

  df %>%
    filter(
      grade == 13,
      student_group_id == "1",
      test_id == test.id, # ELA
      entity_type %in%
        c(
          "District",
          "Direct Funded Charter School",
          "Locally Funded Charter School"
        ),
      !is.na(percentage_standard_met_and_above)
    ) %>%
    mutate(
      lea_name = case_when(
        entity_type == "District" ~ district_name,
        str_detect(entity_type, "harter") ~ school_name
      ),
      bar_kular = case_when(
        entity_type == "District" ~ "steelblue",
        str_detect(entity_type, "harter") ~ "orange"
      )
    ) %>%
    mutate(
      lea_name.n = paste0(
        lea_name,
        " (",
        total_students_tested_with_scores,
        ")"
      )
    ) %>%

    ggplot2::ggplot(aes(
      y = percentage_standard_met_and_above / 100,
      x = forcats::fct_reorder(lea_name.n, percentage_standard_met_and_above),
      label = scales::percent(
        percentage_standard_met_and_above / 100,
        accuracy = .1
      )
    )) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = forcats::fct_reorder(
          lea_name.n,
          percentage_standard_met_and_above / 100
        ),
        xend = forcats::fct_reorder(
          lea_name.n,
          percentage_standard_met_and_above / 100
        ),
        y = 0,
        yend = percentage_standard_met_and_above / 100,
        color = bar_kular
      ),
      size = 2
    ) +
    ggplot2::geom_point(aes(color = bar_kular), size = 5, alpha = 0.6) +
    ggplot2::coord_flip() +
    ggplot2::geom_text(size = 3, color = "black") +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    #  facet_grid(facets = vars(`Student Group`), scales = "free" ) +
    ggthemes::theme_hc() +
    mcoe_theme +
    scale_color_identity() +

    labs(
      x = "",
      y = "",
      color = "",
      title = paste0(
        "CAASPP ",
        yr.curr,
        " ",
        test.name,
        " Rates Meeting or Exceeding Standards by District"
      ),
      caption = source.link
    )

  ggsave(
    here(
      "figs",
      paste0(
        "Charters with Districts ",
        test.name,
        " Rates Meeting or Exceeding ",
        Sys.Date(),
        ".png"
      )
    ),
    width = 12,
    height = 7
  )
}


county.graph.w.charter(caaspp.mry, 1)

caaspp.mry %>%
  filter(!(district_code == "10272" & school_code == "0000000")) %>%
  county.graph.w.charter(2)


#### Subgroups by Feeder schools ----

lolli.subgroups.school.feeder8 <- function(dist = "", schoo = "", test.id = 1) {
  test.name <- if_else(test.id == 1, "ELA", "Math")

  caaspp.mry2019 %>%
    filter(
      Grade == 8,
      str_detect(District_Name, dist),
      str_detect(School_Name, schoo),

      # student_group_id == "1",
      Test_Id == test.id, # ELA
      # Entity_Type == "School",
      !is.na(Percentage_Standard_Met_and_Above),
      !str_detect(Subgroup, " - "),
      !str_detect(Subgroup, "Declined")
    ) %>%
    mutate(
      subgroup.n = paste0(Subgroup, " (", Total_Tested_At_Entity_Level, ")")
    ) %>%
    lollipop(Percentage_Standard_Met_and_Above, subgroup.n, "sea green") +
    labs(
      x = "",
      y = "",
      color = "",
      title = paste0(
        "CAASPP ",
        test.name,
        " Rates Meeting or Exceeding in 8th Grade 2019"
      ),
      subtitle = paste0(dist, "-", schoo, " by Student Group"),
      caption = source.link
    )

  ggsave(
    here(
      "figs",
      paste0(
        dist,
        "-",
        schoo,
        " ",
        test.name,
        " Rates by Student Group ",
        Sys.Date(),
        ".png"
      )
    ),
    width = 8,
    height = 6
  )
}


lolli.subgroups.school.feeder8("King City", "Chalone Peaks", 1)

lolli.subgroups.school.feeder8("Greenfield", "Vista Verde", 1)


lolli.subgroups.school.feeder8("King City", "Chalone Peaks", 2)

lolli.subgroups.school.feeder8("Greenfield", "Vista Verde", 2)


### Schools in a District Comparison -------

lolli.schools <- function(df, dist, test.id = 1, kular = "seagreen") {
  test.name <- case_match(test.id, 1 ~ "ELA", 2 ~ "Math", 17 ~ "Science", )

  df %>%
    filter(
      grade == 13,
      str_detect(district_name, dist),
      student_group_id == "1",
      test_id == test.id, # ELA
      #           entity_type == "School",
      !is.na(percentage_standard_met_and_above)
    ) %>%
    mutate(
      school_name = if_else(is.na(school_name), district_name, school_name)
    ) %>%
    mutate(
      name.n = paste0(school_name, " (", total_students_tested_with_scores, ")")
    ) %>%
    lollipop(percentage_standard_met_and_above, name.n, kular) +
    labs(
      x = "",
      y = "",
      color = "",
      title = paste0(
        "CAASPP ",
        yr.curr,
        " ",
        test.name,
        " Rates Meeting or Exceeding Standards \n",
        dist,
        " by School"
      ),
      caption = source.link
    )

  ggsave(
    here(
      "figs",
      paste0(dist, " ", test.name, " Rates by School ", Sys.Date(), ".png")
    ),
    width = 8,
    height = 4.5
  )
}

caaspp.cast.mry.curr <- caaspp.cast.mry |>
  filter(test_year == max(test_year))

caaspp.cast.mry.curr |>
  lolli.schools("Gonzales", 17)


lolli.schools("Greenfield", 1)


caaspp.mry %>%
  mutate(
    school_name = if_else(
      (district_code == "10272" & school_code == "0000000"),
      "Monterey County Office of Ed \n including Salinas Community, Wellington Smith, \nOpen Door, Home Charter, and Special Ed",
      school_name
    )
  ) %>%
  lolli.schools("Monterey County Office of Ed", 2)


lolli.schools(caaspp.cast.mry, "Monterey County Office of Ed", 17)


for (i in test.list) {
  for (j in districts) {
    lolli.schools(caaspp.mry, j, i)
  }
}


lolli.schools.charters <- function(df, test.id = 1, kular = "seagreen") {
  test.name <- case_match(test.id, 1 ~ "ELA", 2 ~ "Math", 17 ~ "Science", )

  df %>%
    filter(
      grade == 13,
      type_id %in% c(9, 10),
      student_group_id == "1",
      test_id == test.id, # ELA
      #           entity_type == "School",
      !is.na(percentage_standard_met_and_above)
    ) %>%
    mutate(
      school_name = if_else(is.na(school_name), district_name, school_name)
    ) %>%
    mutate(
      school_name.n = paste0(
        school_name,
        " (",
        total_students_tested_with_scores,
        ")"
      )
    ) %>%

    lollipop(percentage_standard_met_and_above, school_name.n, kular) +
    labs(
      x = "",
      y = "",
      color = "",
      title = paste0(
        "Monterey County Charters ",
        test.name,
        " \nRates Meeting or Exceeding Standards by School"
      ),
      caption = paste0(
        source.link,
        "\nNumbers in parentheses refer to number of students tested"
      )
    )

  ggsave(
    here(
      "figs",
      paste0(
        "Monterey County Charters ",
        test.name,
        " Rates by School ",
        Sys.Date(),
        ".png"
      )
    ),
    width = 6,
    height = 4
  )
}


lolli.schools.charters(caaspp.cast.mry, test.id = 1, kular = "seagreen")

lolli.schools.charters(caaspp.cast.mry, test.id = 2, kular = "seagreen")

lolli.schools.charters(caaspp.cast.mry, test.id = 17, kular = "seagreen")


###  Slopegraph ------

caaspp.mry.prior <- tbl(con, "CAASPP") %>%
  filter(
    county_code == "27",
    # DistrictCode == "10272",
    test_year == yr.prior
  ) %>%
  collect() %>%
  mutate(student_group_id = as.character(student_group_id)) %>%
  left_join_codebook("CAASPP", "student_group_id") %>%
  rename(subgroup = definition) %>%
  left_join(ent2, by = join_by(county_code, district_code, school_code)) %>%
  mutate(type_id = as.character(type_id)) %>%
  left_join_codebook("CAASPP", "type_id") %>%
  rename(entity_type = definition) %>%
  mutate(across(
    caaspp_reported_enrollment:area_4_percentage_near_standard,
    as.numeric
  )) %>%
  mutate(
    district_name = coalesce(district_name.x, district_name.y),
    school_name = coalesce(school_name.x, school_name.y)
  )


caaspp.long <- caaspp.mry %>%
  bind_rows(caaspp.mry.prior) %>%
  filter(
    Grade == 13,
    student_group_id == "1",
    Test_Id == 2, # ELA
    School_Code == "0000000",
    # !is.na(Percentage_Standard_Met_and_Above)
  )


ggplot(
  data = caaspp.long,
  aes(
    x = Test_Year,
    y = Percentage_Standard_Met_and_Above,
    group = District_Name
  )
) +
  geom_line(aes(color = District_Name, alpha = 1), size = 1) +
  geom_text_repel(
    data = caaspp.long %>% filter(Test_Year == "2022"),
    aes(label = District_Name),
    hjust = "left",
    segment.size = .2,
    segment.color = "grey",
    size = 3,
    nudge_x = -.4,
    direction = "y"
  ) +
  geom_text_repel(
    data = caaspp.long %>% filter(Test_Year == yr.curr),
    aes(label = District_Name),
    hjust = "right",
    segment.size = .2,
    segment.color = "grey",
    fontface = "bold",
    size = 3,
    nudge_x = .4,
    direction = "y"
  ) +
  geom_label(
    aes(label = Percentage_Standard_Met_and_Above),
    size = 2.5,
    label.padding = unit(0.05, "lines"),
    label.size = 0.0
  ) +
  theme_hc() + # Remove the legend
  theme(axis.text.y = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(axis.ticks = element_blank()) +
  scale_x_discrete(position = "top") +
  theme(legend.position = "none") +
  labs(
    title = "Most districts were flat\n from 21-22 to 22-23 on Math",
    y = "Percent Meeting or Exceeding Standard by Grade",
    x = ""
  )

### Overlapping bars ----

caaspp.long <- caaspp.mry %>%
  bind_rows(caaspp.mry.prior, cast.mry) %>%
  filter(
    grade == 13,
    student_group_id == "1",
    entity_type == "District",
    test_id == 2, # Math
    #     school_code == "0000000",
    # !is.na(Percentage_Standard_Met_and_Above)
  )


caaspp.long2 <- caaspp.long %>%
  mutate(
    ranker = ifelse(test_year == yr.curr, percentage_standard_met_and_above, NA)
  ) %>%
  filter(district_name != "NA", district_name != "Big Sur Unified")

#
#     ggplot(mapping = aes(x = reorder(district_name, percentage_standard_met_and_above),
#                          y = percentage_standard_met_and_above/100)) +
#         geom_col(data =  caaspp.long2[caaspp.long2$test_year == yr.prior,],
#                  fill = "light grey",
#                  width = 0.75) +
#         geom_col(data = caaspp.long2[caaspp.long2$test_year == yr.curr,],
#         #         position = "dodge" ,
#                  width = 0.5,
#                  fill = "sea green") +
#         coord_flip() +
#         mcoe_theme +
#         scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#
#         labs(title = "CAASPP Math Percent Meet and Exceed by District",
#              subtitle = paste0("Grey is ",yr.prior," and Green is ", yr.curr),
#              y = "",
#              x = "",
#              caption = source.link
#         )
#
#     ggsave(here("figs", paste0("All Districts Math compared to prior year ",  Sys.Date(),".png" )),
#            width = 8, height = 6)
#
#
#     # ELA version
#
#
#     caaspp.long <- caaspp.mry %>%
#         bind_rows(caaspp.mry.prior) %>%
#         filter(grade == 13,
#                student_group_id == "1",
#                entity_type == "District",
#      #          test_id == 1, # ELA
#                #     school_code == "0000000",
#                # !is.na(Percentage_Standard_Met_and_Above)
#         )
#
#
#
#
#     caaspp.long2 <- caaspp.long %>%
#         mutate(ranker = ifelse(test_year == yr.curr, percentage_standard_met_and_above, NA)) %>%
#         filter(district_name != "NA",
#                district_name != "Big Sur Unified")
#
#
#     ggplot(mapping = aes(x = reorder(District_Name, Percentage_Standard_Met_and_Above),
#                          y = Percentage_Standard_Met_and_Above/100)) +
#         geom_col(data =  caaspp.long2[caaspp.long2$Test_Year == yr.prior,],
#                  fill = "light grey",
#                  width = 0.75) +
#         geom_col(data = caaspp.long2[caaspp.long2$Test_Year == yr.curr,],
#                  #         position = "dodge" ,
#                  width = 0.5,
#                  fill = "steel blue") +
#         coord_flip() +
#         mcoe_theme +
#         scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#         labs(title = "CAASPP ELA Percent Meet and Exceed by District",
#              subtitle = paste0("Grey is ",yr.prior," and Blue is ",yr.curr),
#              y = "",
#              x = "",
#              caption = source.link
#         )
#
#     ggsave(here("figs", paste0("All Districts ELA compared 2022 ",  Sys.Date(),".png" )),
#            width = 8, height = 6)

compare.years <- function(
  df,
  collie,
  test.id = 1,
  title.name,
  kular = "Steel Blue"
) {
  test.name <- case_match(test.id, 1 ~ "ELA", 2 ~ "Math", 17 ~ "Science", )

  df <- df %>%
    filter(test_id == test.id) #%>%
  #   mutate(coll = fct_reorder({{collie}}, desc(Percentage_Standard_Met_and_Above))) %>%

  ggplot(
    mapping = aes(
      x = reorder({{ collie }}, percentage_standard_met_and_above),
      y = percentage_standard_met_and_above / 100
    )
  ) +
    geom_col(
      data = df[df$test_year == yr.curr, ],
      #         position = "dodge" ,
      width = 0.5,
      fill = kular
    ) +
    geom_col(
      data = df[df$test_year == yr.prior, ],
      fill = "light grey",
      width = 0.75
    ) +
    geom_col(
      data = df[df$test_year == yr.curr, ],
      #         position = "dodge" ,
      width = 0.5,
      fill = kular
    ) +
    coord_flip() +
    mcoe_theme +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = paste0(
        title.name,
        " CAASPP ",
        test.name,
        " Rates Meeting or Exceeding"
      ),
      subtitle = paste0("Grey is ", yr.prior, " and ", kular, " is ", yr.curr),
      y = "",
      x = "",
      caption = source.link
    )

  ggsave(
    here(
      "figs",
      paste0(
        title.name,
        " ",
        test.name,
        " Change in Rates by Student Group ",
        Sys.Date(),
        ".png"
      )
    ),
    width = 8,
    height = 4.5
  )
}

caaspp.county.comp <- tbl(con, "CAASPP") %>%
  filter(
    # Type_ID == 5,
    subgroup_id == 1,
    grade == 13,
    # County_Code == "27",
    district_code == "00000",
    test_year %in% c(yr.prior, yr.curr)
  ) %>%
  collect() %>%
  left_join(ent2) %>%
  rename(student_group_id = subgroup_id) %>%

  clean.caaspp()


caaspp.county.comp %>%
  mutate(
    percentage_standard_met_and_above = as.numeric(
      percentage_standard_met_and_above
    )
  ) %>%
  filter(county_code != "00") %>%
  compare.years(county_name, 1, "Counties")


cast %>%
  filter(
    # Type_ID == 5,
    student_group_id == 1,
    grade == 13,
    # County_Code == "27",
    district_code == "00000",
    test_year %in% c(yr.prior, yr.curr),
    county_name != "Alpine"
  ) %>%
  mutate(
    percentage_standard_met_and_above = as.numeric(
      percentage_standard_met_and_above
    )
  ) %>%
  filter(county_code != "00") %>%
  compare.years(county_name, 17, "Counties")


caaspp.long2 %>%
  #   filter(!str_detect(district_name,"Lagunita|Graves|Bradley")) %>%
  compare.years(district_name, 2, "Districts")


district.change <- caaspp.long2 %>%
  select(
    district_name,
    test_year,
    test_id,
    percentage_standard_met_and_above
  ) %>%
  pivot_wider(
    names_from = c(test_year, test_id),
    values_from = percentage_standard_met_and_above,
    names_prefix = "yr"
  ) %>%
  mutate(
    #ela.change = yr2024_1 - yr2023_1,
    #math.change = yr2024_2 - yr2023_2,
    science.change = yr2024_17 - yr2023_17
  )

# Compare Years for Charters

caaspp.mry %>%
  bind_rows(caaspp.mry.prior, cast.mry) %>%
  filter(type_id %in% c(9, 10)) %>%
  mutate(
    percentage_standard_met_and_above = as.numeric(
      percentage_standard_met_and_above / 100
    )
  ) %>%
  compare.years(school_name, 17, "Charter Schools")


###

caaspp.mry %>%
  bind_rows(caaspp.mry.prior) %>%
  #    cast.mry %>%
  filter(
    grade == 13,
    # student_group_id == "1",
    student_group_id %in% standard.groups,
    #        student_group_id %notin% c(190, 200, 201, 202, 203, 204, 205, 206, 207, 220, 221, 222, 223, 224, 225, 226, 227, 250, 251, 252),  # Removing the 250-252 because did not exist in 2022
    entity_type == "County",
    !is.na(percentage_standard_met_and_above)
  ) %>%
  compare.years(subgroup, 2, "County Student Groups", kular = "orange")


# Want to see which studen groups increased and decreasedthe most

biggest.change <- # caaspp.mry %>%
  #  bind_rows(caaspp.mry.prior) %>%
  cast.mry %>%
  filter(
    grade == 13,
    # student_group_id == "1",
    student_group_id %in% standard.groups,
    #        student_group_id %notin% c(190, 200, 201, 202, 203, 204, 205, 206, 207, 220, 221, 222, 223, 224, 225, 226, 227, 250, 251, 252),  # Removing the 250-252 because did not exist in 2022
    entity_type == "County",
    !is.na(percentage_standard_met_and_above)
  ) %>%
  select(subgroup, test_year, test_id, percentage_standard_met_and_above) %>%
  pivot_wider(
    names_from = c(test_year, test_id),
    values_from = percentage_standard_met_and_above,
    names_prefix = "yr"
  ) %>%
  mutate(
    # ela.change = yr2024_1 - yr2023_1,
    # math.change = yr2024_2 - yr2023_2
    science.change = yr2024_17 - yr2023_17
  )


imp.comp <- caaspp.mry %>%
  bind_rows(caaspp.imp, cast.mry, cast.imp) %>%
  filter(
    grade == 13,
    test_year == yr.curr,
    # student_group_id == "1",
    student_group_id %in% standard.groups,
    #        student_group_id %notin% c(190, 200, 201, 202, 203, 204, 205, 206, 207, 220, 221, 222, 223, 224, 225, 226, 227, 250, 251, 252),  # Removing the 250-252 because did not exist in 2022
    entity_type == "County",
    !is.na(percentage_standard_met_and_above)
  ) %>%
  select(subgroup, county_name, test_id, percentage_standard_met_and_above) %>%
  pivot_wider(
    names_from = c(county_name, test_id),
    values_from = percentage_standard_met_and_above
  ) %>%
  mutate(
    ela.change = Monterey_1 - Imperial_1,
    math.change = Monterey_2 - Imperial_2,
    science.change = Monterey_17 - Imperial_17
  )


###

# Simple County Growth

caaspp.cast.mry %>%

  filter(
    grade == 13,
    student_group_id == "1",
    test_year >= "2023",
    #     test_year %in% c(yr.curr,yr.prior),
    # student_group_id %in%  standard.groups,
    #        student_group_id %notin% c(190, 200, 201, 202, 203, 204, 205, 206, 207, 220, 221, 222, 223, 224, 225, 226, 227, 250, 251, 252),  # Removing the 250-252 because did not exist in 2022
    entity_type == "County",
    !is.na(percentage_standard_met_and_above)
  ) %>%
  mutate(
    test_year = factor(test_year),
    test = case_match(test_id, 1 ~ "ELA", 2 ~ "Math", 17 ~ "Science", ),
    label = paste0(round(percentage_standard_met_and_above, 1), "%")
  ) %>%
  ggplot(aes(
    x = test,
    y = percentage_standard_met_and_above / 100,
    group = test_year,
    fill = test_year,
    label = label
  )) +
  geom_col(position = "dodge") +
  geom_label(position = position_dodge(width = 1), show.legend = FALSE) +
  mcoe_theme +
  scale_y_continuous(labels = scales::percent) +
  #    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = paste0(
      "Monterey County Rates Meeting or Exceeding Improvements in 2025"
    ),
    #   subtitle = paste0("Grey is ",yr.prior," and ",kular," is ",yr.curr),
    y = "Percentage Met or Exceeded",
    x = "",
    caption = source.link
  )


ggsave(
  here(
    "figs",
    paste0(
      "Monterey County Rates Meeting or Exceeding Improvements in 2025 ",
      Sys.Date(),
      ".png"
    )
  ),
  width = 8,
  height = 6
)


simple.change <- function(df, tit) {
  df %>%
    mutate(
      test_year = factor(test_year),
      test = case_match(test_id, 1 ~ "ELA", 2 ~ "Math", 17 ~ "Science", ),
      label = paste0(round(percentage_standard_met_and_above, 1), "%")
    ) %>%
    ggplot(aes(
      x = test,
      y = percentage_standard_met_and_above / 100,
      group = test_year,
      fill = test_year,
      label = label
    )) +
    geom_col(position = "dodge") +
    geom_label(position = position_dodge(width = 1), show.legend = FALSE) +
    mcoe_theme +
    scale_y_continuous(labels = scales::percent) +
    #    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = paste0(tit, " Rates Meeting or Exceeding Improvements in 2024"),
      #   subtitle = paste0("Grey is ",yr.prior," and ",kular," is ",yr.curr),
      y = "Percentage Met or Exceeded",
      x = "",
      caption = source.link
    )

  ggsave(
    here(
      "figs",
      paste0(
        tit,
        " Rates Meeting or Exceeding Improvements in 2024 ",
        Sys.Date(),
        ".png"
      )
    ),
    width = 8,
    height = 6
  )
}


caaspp.mry %>%
  bind_rows(caaspp.mry.prior, cast.mry) %>%

  filter(
    grade == 13,
    student_group_id == "1",
    test_year %in% c(yr.curr, yr.prior),
    entity_type == "District",
    str_detect(district_name, "North Monterey County"),
    !is.na(percentage_standard_met_and_above)
  ) %>%
  simple.change("North Monterey County")


###

compare.years.dist.group <- function(dist, testy, kular) {
  dist.hold <- caaspp.mry %>%
    bind_rows(caaspp.mry.prior) %>%
    filter(
      grade == 13,
      entity_type == "District",
      str_detect(district_name, dist),
      #   student_group_id == "1",
      test_id == testy, # Math
      #    School_Code == "0000000",
      !is.na(percentage_standard_met_and_above),
      student_group_id %notin%
        c(
          121,
          190,
          200,
          201,
          202,
          203,
          204,
          205,
          206,
          207,
          220,
          221,
          222,
          223,
          224,
          225,
          226,
          227,
          250,
          251,
          252
        ),
      student_group_id %in%
        c(
          1,
          7,
          8,
          180,
          160,
          250,
          31,
          128,
          74,
          75,
          77,
          78,
          80,
          76,
          79,
          144,
          28,
          240,
          52
        )
    )

  tabyl.set <- dist.hold %>%
    tabyl(student_group_id) %>%
    filter(n != 2)

  tabyl.set

  purge.list <- tabyl.set %>%
    select(student_group_id) %>%
    unlist()

  purge.list

  dist.hold %>%
    filter(student_group_id %notin% tabyl.set$student_group_id) %>%
    mutate(
      ranker = ifelse(
        test_year == yr.curr,
        percentage_standard_met_and_above,
        NA
      )
    ) %>%
    compare.years(subgroup, testy, dist, kular)
}


compare.years.dist.group(dist = "Chualar", testy = 1, kular = "Pink")


compare.years.dist.group(
  dist = "South Monterey County",
  testy = 2,
  kular = "DarkMagenta"
)


compare.years.dist.group(dist = "Greenfield", testy = 2, kular = "Limegreen") # #007937ff


#
compare.years.dist.group("Bradley", 1, kular = "Pink")


# Not ordered for some, like Chualar and SoMoCo
for (i in 1:2) {
  for (j in districts) {
    compare.years.dist.group(j, i, kular = "Pink")
  }
}


# TRoubleshooting

caaspp.mry %>%
  bind_rows(caaspp.mry.prior) %>%
  filter(
    Grade == 13,
    # student_group_id == "1",
    student_group_id %notin%
      c(
        121,
        190,
        200,
        201,
        202,
        203,
        204,
        205,
        206,
        207,
        220,
        221,
        222,
        223,
        224,
        225,
        226,
        227,
        250,
        251,
        252
      ), # Removing the 250-252 because did not exist in 2022
    Entity_Type == "District",
    !is.na(Percentage_Standard_Met_and_Above),
    str_detect(District_Name, "Chualar")
  ) %>%
  tabyl(student_group_id)

### Three Year Facet -----

caaspp.three.year <- tbl(con, "CAASPP") %>%
  filter(
    county_code %in% c("27", "00"),
    # DistrictCode == "10272",
    test_year %in% c("2022", yr.prior, yr.curr),
    type_id %in% c(3, 4, 5)
  ) %>%
  collect() %>%
  mutate(student_group_id = as.character(student_group_id)) %>%
  left_join_codebook("CAASPP", "student_group_id") %>%
  rename(subgroup = definition) %>%
  left_join(ent2) %>%
  mutate(type_id = as.character(type_id)) %>%
  left_join_codebook("CAASPP", "type_id") %>%
  rename(entity_type = definition) %>%
  mutate(across(
    caaspp_reported_enrollment:area_4_percentage_near_standard,
    as.numeric
  ))


temp <- caaspp.three.year %>%
  filter(
    student_group_id %in% c(74, 75, 76, 77, 78, 79, 80),
    grade == 13,
    test_id == 1
  ) %>%
  #  mutate(definition = fct_relevel(definition, "Kindergarten", after = 0 )) %>%
  #  filter(str_starts(reporting_category, subby)) %>%
  ggplot(aes(
    x = test_year,
    y = percentage_standard_met_and_above,
    group = county_name,
    color = county_name
  )) +
  geom_line() +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  facet_wrap(~subgroup) +
  mcoe_theme +
  scale_color_few() +
  ylim(0, 30) +
  guides(color = guide_legend(""))


### Grades by EL ----

caaspp.mry %>%
  filter(
    student_group_id %in% c(1, 160),
    Entity_Type == "County",
    Test_Id == 1
  ) %>%
  ggplot(aes(
    y = Percentage_Standard_Met_and_Above,
    x = Grade,
    group = Subgroup,
    fill = Subgroup
  )) +
  geom_col(position = "dodge") +
  mcoe_theme +
  labs(
    title = "The percent of EL students meeting or exceeding standards decreases the higher the grade level. \nFor students overall it increases by grade level."
  )


### End ish ------

caaspp.suhsd %>%
  filter(
    Grade == 13,
    str_detect(District_Name, "Salinas Union"),
    School_Code == "0000000",
    !str_detect(Subgroup, "Not migrant"), # missing in 2019 and so messes up order if not excluded
    !is.na(Percentage_Standard_Met_and_Above),
    !str_detect(Subgroup, " - ") # to remove all the race by socio-econ status categories
  ) %>%
  compare.years(Subgroup, 1)


temp <- tbl(con, "CAASPP") %>%
  filter(
    County_Code == "27",
    District_Code == "66191",
    Test_Year %in% c("2019", "2022")
  ) %>%
  collect() %>%
  clean.caaspp() %>%
  filter(
    Grade == 13,
    #          str_detect(District_Name,"Salinas Union"),
    School_Code == "0000000",
    #       !str_detect(Subgroup, "Not migrant"),  # missing in 2019 and so messes up order if not excluded
    #      !is.na(Percentage_Standard_Met_and_Above),
    !str_detect(Subgroup, " - ") # to remove all the race by socio-econ status categories
  )

temp %>%
  group_by(Subgroup) %>%
  filter(!is.na(Percentage_Standard_Met_and_Above)) %>%
  add_count() %>%
  filter(n >= 4) %>%
  compare.years(Subgroup, 1)


# ELA by Grade ---
caaspp.ela.3.5 <- tbl(con, "CAASPP") %>%
  filter(
    # Type_ID == 5,
    student_group_id == 1,
    Grade %in% c(3, 4, 5),
    County_Code == "27",
    #   District_Code == "00000",
    School_Code == "0000000",
    Test_Year %in% c("2019", "2022")
  ) %>%
  collect() %>%
  clean.caaspp() %>%
  left_join(ent2) %>%
  mutate(
    Percentage_Standard_Met_and_Above = as.numeric(
      Percentage_Standard_Met_and_Above
    )
  )


for (g in 3:5) {
  caaspp.ela.3.5 %>%
    filter(Grade == g, District_Code != "00000") %>%
    compare.years(District_Name, 1, paste0("ELA ", g, " Grade"))
}


caaspp.ela.3.5.wide <- caaspp.ela.3.5 %>%
  filter(Test_Id == 1) %>%
  select(
    District_Code,
    District_Name,
    Grade,
    Test_Id,
    Test_Year,
    Percentage_Standard_Met_and_Above,
    Students_with_Scores
  ) %>%
  mutate(
    Count_Met_And_Above = round2(
      Students_with_Scores * Percentage_Standard_Met_and_Above / 100,
      0
    )
  ) %>%
  group_by(District_Code, District_Name, Test_Id, Test_Year) %>%
  mutate(
    denom = sum(Students_with_Scores),
    numer = sum(Count_Met_And_Above),
    Percentage_Standard_Met_and_Above = 100 * numer / denom
  ) %>%
  select(
    District_Code,
    District_Name,
    Test_Id,
    Test_Year,
    Percentage_Standard_Met_and_Above
  ) %>%
  distinct() %>%
  na.omit() %>%
  filter(!str_detect(District_Name, "Lagunita|Mission|Office|Antonio")) %>%
  compare.years(District_Name, 1, paste0("Combined 3-5 Grade"))


### Single School ----

caaspp.soledad <- tbl(con, "CAASPP") %>%
  filter(
    County_Code == "27",
    District_Code == "75440",
    Test_Year %in% c("2019", "2022")
  ) %>%
  collect() %>%
  clean.caaspp()

caaspp.soledad %>%
  filter(
    Grade == 13,
    #        str_detect(District_Name,"Salinas Union"),
    str_detect(School_Name, "Franscioni"),
    !str_detect(Subgroup, "Not migrant"), # missing in 2019 and so messes up order if not excluded
    !str_detect(Subgroup, "Declined"), # missing in 2019 and so messes up order if not excluded
    #  !str_detect(Subgroup, "IFEP"),  # missing in 2019 and so messes up order if not excluded
    #  !str_detect(Subgroup, "Homeless"),  # missing in 2019 and so messes up order if not excluded
    !is.na(Percentage_Standard_Met_and_Above),
    !str_detect(Subgroup, " - ") # to remove all the race by socio-econ status categories
  ) %>%
  compare.years(Subgroup, 2, "Franscioni")

### San Antonio -----

dist.name <- "South Monterey County"
dist.code <- 66068

caaspp.san.antonio <- tbl(con, "CAASPP") %>%
  filter(
    county_code %in% c("00", "27"),
    district_code %in% c("00000", dist.code),
    test_year %in% c("2022", "2023")
  ) %>%
  collect() %>%
  clean.caaspp()


san.antonio.groups <- caaspp.san.antonio %>%
  filter(
    test_year == "2023",
    district_code == dist.code,
    !is.na(percentage_standard_met_and_above),
    grade == "13"
  ) %>%
  select(subgroup, percentage_standard_met_and_above) %>%
  mutate(
    subgroup = factor(subgroup),
    subgroup = fct_reorder(subgroup, percentage_standard_met_and_above)
  )


# Compare Years

caaspp.san.antonio %>%
  filter(
    Grade == 13,
    District_Code == dist.code,
    School_Code == "0000000",
    Subgroup %in% san.antonio.groups$Subgroup,
    !str_detect(Subgroup, "Not migrant"), # missing in 2019 and so messes up order if not excluded
    #        !str_detect(Subgroup, "Graduate school"),    # missing in 2019 and so messes up order if not excluded
    #   !str_detect(Subgroup, "with disability"),  # missing in 2019 and so messes up order if not excluded
    #   !str_detect(Subgroup, "English learner"),  # missing in 2019 and so messes up order if not excluded
    #   !str_detect(Subgroup, "Homeless"),  # missing in 2019 and so messes up order if not excluded
    !str_detect(Subgroup, "Not Foster"), # missing in 2019 and so messes up order if not excluded
    !str_detect(Subgroup, "White - Not"), # missing in 2019 and so messes up order if not excluded
    !str_detect(Subgroup, "Declined"), # missing in 2019 and so messes up order if not excluded
    !is.na(Percentage_Standard_Met_and_Above),
    #        !str_detect(Subgroup, " - ") # to remove all the race by socio-econ status categories
  ) %>%
  compare.years(Subgroup, 1, dist.name)

#ggsave(here("figs", paste0("Compared years ",dist.name," ELA.png")))

three.levels <- function(df, test.id, dist.name) {
  test.name <- if_else(test.id == 1, "ELA", "Math")

  san.antonio.graph <- df %>%
    filter(
      grade == 13,
      test_year == yr.curr,
      subgroup %in% san.antonio.groups$subgroup,
      test_id == test.id,
      entity_type != "School",
      subgroup != "NA"
    )

  ggplot(
    mapping = aes(
      x = reorder(subgroup, percentage_standard_met_and_above),
      y = percentage_standard_met_and_above / 100,
      fill = entity_type
    )
  ) +
    geom_col(
      data = san.antonio.graph[san.antonio.graph$entity_type == "State", ],
      fill = "light grey",
      width = 0.8
    ) +
    geom_col(
      data = san.antonio.graph[san.antonio.graph$entity_type == "County", ],
      fill = "light blue",
      width = 0.65
    ) +
    geom_col(
      data = san.antonio.graph[san.antonio.graph$entity_type == "District", ],
      fill = "orange",
      width = 0.50
    ) +
    coord_flip() +
    mcoe_theme +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = paste0(
        yr.curr,
        " CAASPP ",
        test.name,
        " Percent Meet and Exceed by Student Group"
      ),
      subtitle = paste0(
        "Grey is California, Blue is Monterey County and Orange is ",
        dist.name
      ),
      y = "",
      x = "",
      caption = source.link
    )

  ggsave(
    here(
      "figs",
      paste0(
        dist.name,
        ", Monterey County and California - ",
        test.name,
        Sys.Date(),
        ".png"
      )
    ),
    width = 8,
    height = 6
  )
}

three.levels(caaspp.san.antonio, 2, dist.name)

#  Over time

over.time <- function(df, test.id, dist.name, heading) {
  test.name <- if_else(test.id == 1, "ELA", "Math")

  sa.long <- df %>%
    filter(
      grade == 13,
      student_group_id == "1",
      test_id == test.id, # ELA
      school_code == "0000000"
    )

  entit <- sa.long %>%
    filter(test_year == "2022") %>%
    select(county_code, district_code, school_code, entity = entity_type)

  sa.long <- sa.long %>%
    left_join(entit) %>%
    distinct()

  ggplot(
    data = sa.long,
    aes(x = test_year, y = percentage_standard_met_and_above, group = entity)
  ) +
    #  facet_wrap(~Entity) +
    geom_line(aes(color = entity, alpha = 1), size = 1) +
    geom_text_repel(
      data = sa.long %>% filter(test_year == "2019"),
      aes(label = entity),
      hjust = "left",
      segment.size = .2,
      segment.color = "grey",
      size = 3,
      nudge_x = -.4,
      direction = "y"
    ) +
    geom_text_repel(
      data = sa.long %>% filter(test_year == "2022"),
      aes(label = entity),
      hjust = "right",
      segment.size = .2,
      segment.color = "grey",
      fontface = "bold",
      size = 3,
      nudge_x = .4,
      direction = "y"
    ) +
    geom_label(
      aes(label = percentage_standard_met_and_above),
      size = 2.5,
      label.padding = unit(0.05, "lines"),
      label.size = 0.0
    ) +
    theme_hc() + # Remove the legend
    # theme(axis.text.y      = element_blank()) +
    # theme(panel.grid.major.y = element_blank()) +
    # theme(panel.grid.minor.y = element_blank()) +
    # theme(axis.ticks       = element_blank()) +
    scale_x_discrete(position = "top") +
    theme(legend.position = "none") +
    labs(
      #title = "San Antonio Decreased More Sharply than Monterey County or California",
      #subtitle = "CAASPP ELA",
      title = paste0(
        dist.name,
        heading,
        " compared to Monterey County, and California"
      ),
      subtitle = paste0("CAASPP ", test.name),
      y = "Percent Meeting or Exceeding Standard",
      x = ""
    )

  ggsave(
    here(
      "figs",
      paste0(
        "Over time ",
        dist.name,
        " Monterey County and California - ",
        test.name,
        Sys.Date(),
        ".png"
      )
    ),
    width = 8,
    height = 6
  )
}

over.time(caaspp.san.antonio, 1, dist.name, " decreased less sharply")


##### Numbers of Participants in MCOE numbers    ------

mcoe <- caaspp.mry %>%
  filter(
    str_detect(district_name, "Office"),
    grade == 13,
    student_group_id == 1,
    test_year == 2024
  ) %>%
  select(
    ends_with("code"),
    ends_with("name"),
    test_id,
    students_with_scores,
    student_group_id,
    subgroup,
    total_students_tested_with_scores,
    caaspp_reported_enrollment,
    percentage_standard_met_and_above
  )


#### Barbell Graphs for two year comparison ------

caaspp.mry.hist %>%
  filter(
    test_year >= 2023,
    district_code == "00000",
    student_group_id %in% standard.groups,
    test_id == 2,
    grade == 13
  ) |>
  pivot_wider(
    id_cols = c(subgroup),
    names_from = test_year,
    values_from = percentage_standard_met_and_above
  ) %>%
  mutate(
    change = round2(`2024` - `2023`, digits = 1),
    labl = if_else(change > 0, paste0("+", change), paste0(change))
  ) %>%
  mutate(subgroup = fct_reorder(subgroup, `2024`)) |>
  #  mutate(subgroup = factor(subgroup, levels = sort(unique(subgroup), decreasing = TRUE)))
  # mutate(subgroup = factor(subgroup, levels = sort((`2024`), decreasing = TRUE)))

  ggplot(aes(y = subgroup)) +
  # connecting segment
  geom_segment(
    aes(x = `2023`, xend = `2024`, yend = subgroup),
    linewidth = 1.2,
    color = "grey70",
    arrow = arrow(
      #ends = "last",
      type = "closed",
      length = unit(0.1, "inches")
    )
  ) +
  # endpoints
  geom_point(aes(x = `2023`), color = "#fed98e", size = 3) +
  geom_point(aes(x = `2024`), color = "#fe9929", size = 3) +
  geom_text(
    aes(label = labl, x = `2023` + change / 2),
    nudge_y = 0.5,
    color = "grey70"
  ) +
  mcoe_theme +
  labs(
    title = "Monterey County change in Math CAASPP between 2023 and 2024",
    subtitle = "Student Groups Percentage Met or Exceeded",
    x = "Percentage Met or Exceeded"
  ) +
  coord_cartesian(clip = "off")


ggsave(
  here(
    "figs",
    paste0("Change over time for Monterey County Math", Sys.Date(), ".png")
  ),
  width = 8,
  height = 6
)

### End -----
