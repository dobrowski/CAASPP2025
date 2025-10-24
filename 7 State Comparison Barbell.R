caaspp.mry.state <- tbl(con, "CAASPP") %>%
  filter(
    county_code %in% c("27", "00"),
    district_code == "00000",
    test_id == 2,
    subgroup_id == 128,
    grade == 13,
    test_year >= 2023
  ) %>%
  #   head(100) %>%
  collect() %>%
  # mutate(subgroup_id = as.character(subgroup_id)) %>%
  # left_join_codebook("CAASPP", "subgroup_id") %>%
  # rename(subgroup = definition) %>%
  # left_join(ent2) %>%
  # mutate(type_id = as.character(type_id)) %>%
  # left_join_codebook("CAASPP", "type_id") %>%
  # rename(entity_type = definition) %>%
  mutate(across(
    caaspp_reported_enrollment:area_4_percentage_near_standard,
    as.numeric
  ))


over.time <- function(df, test.id, dist.name, heading) {
  test.name <- if_else(test.id == 1, "ELA", "Math")

  sa.long <- df %>%
    filter(
      grade == 13,
      subgroup_id == "1",
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


ggplot(
  data = caaspp.mry.state,
  aes(x = test_year, y = percentage_standard_met_and_above, group = county_code)
) +
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
  scale_x_discrete(position = "top") # +
#   theme(legend.position = "none") # +
# labs(#title = "San Antonio Decreased More Sharply than Monterey County or California",
#     #subtitle = "CAASPP ELA",
#     title = paste0(dist.name, heading, " compared to Monterey County, and California"),
#     subtitle = paste0("CAASPP ",test.name),
#     y = "Percent Meeting or Exceeding Standard",
#     x = "")

#####. BAr bell ----

# Helper plotting function: draws a “barbell” change chart between two year columns (2024 vs 2025)

change.barbell <- function(df, group_col, ordered.sort = TRUE) {
  df |>
    pivot_wider(
      id_cols = {{ group_col }}, # grouping variable passed tidily
      names_from = test_year, # becomes columns like `2024`, `2025`
      values_from = percentage_standard_met_and_above
    ) %>%
    # Compute change and a label with +/− sign
    mutate(
      change = round2(`2025` - `2024`, digits = 1),
      labl = if_else(change > 0, paste0("+", change), paste0(change))
    ) %>%
    # Order groups by 2025 value for cleaner sorting on the y-axis
    {
      if (ordered.sort) {
        mutate(., {{ group_col }} := fct_reorder({{ group_col }}, `2025`))
      } else {
        .
      }
    } |>
    #  mutate(subgroup = factor(subgroup, levels = sort(unique(subgroup), decreasing = TRUE)))
    # mutate(subgroup = factor(subgroup, levels = sort((`2024`), decreasing = TRUE)))

    ggplot(aes(y = {{ group_col }})) +
    # Draw connecting segment from 2024 to 2025 with an arrow showing direction
    geom_segment(
      aes(x = `2024`, xend = `2025`, yend = {{ group_col }}),
      linewidth = 1.2,
      color = "grey70",
      arrow = arrow(
        #ends = "last",
        type = "closed",
        length = unit(0.1, "inches")
      )
    ) +
    # Endpoints for each year, colored to match legend/title styling
    geom_point(aes(x = `2024`), color = "#fed98e", size = 3) +
    geom_point(aes(x = `2025`), color = "#fe9929", size = 3) +
    geom_text(
      aes(label = labl, x = `2024` + change / 2),
      nudge_y = 0.5,
      color = "grey70"
    ) +
    lims(x = c(0, NA)) +
    mcoe_theme() +
    labs(y = "")
}


# County student groups

caaspp.mry.hist |>
  # caaspp.cast.mry %>%
  filter(
    #       test_year >= 2023,
    entity_type == "County",
    #       district_code == "00000",
    student_group_id %in% standard.groups,
    test_id == 2,
    grade == 13
  ) |>
  change.barbell(student_group, ordered.sort = TRUE) +
  # Title uses HTML to color year tokens
  labs(
    title = "Monterey County change in Math CAASPP between <span style='color:#fed98e;'>2024</span> and <span style='color:#fe9929;'>2025</span>",
    subtitle = "Student Groups Percentage Met or Exceeded",
    x = "Percentage Met or Exceeded"
  ) +
  theme(
    plot.title = element_markdown(size = 16)
  ) +
  # Allow geoms/labels to extend past the plotting panel (e.g., right-edge labels)
  coord_cartesian(clip = "off")


ggsave(
  here(
    "figs",
    "county",
    paste0(
      "CAASPP ",
      "Math",
      " Change between 2024 and 2025 ",
      Sys.Date(),
      ".png"
    )
  ),
  width = 7,
  height = 5
)


caaspp.cast.mry %>%
  filter(
    #       test_year >= 2023,
    entity_type == "County",
    #       district_code == "00000",
    student_group_id %in% standard.groups,
    test_id == 1,
    grade == 13
  ) |>
  change.barbell(student_group) +
  labs(
    title = "Monterey County change in ELA CAASPP between <span style='color:#fed98e;'>2024</span> and <span style='color:#fe9929;'>2025</span>",
    subtitle = "Student Groups Percentage Met or Exceeded",
    x = "Percentage Met or Exceeded"
  ) +
  theme(
    plot.title = element_markdown(size = 16)
  ) +
  coord_cartesian(clip = "off")


ggsave(
  here(
    "figs",
    "county",
    paste0(
      "CAASPP ",
      "ELA",
      " Change between 2024 and 2025 ",
      Sys.Date(),
      ".png"
    )
  ),
  width = 7,
  height = 5
)


# County district

smalls <- udp |>
  filter(county_name == "Monterey", school_name == "N/A") |>
  transmute(
    district_name_smalls = if_else(
      total_enrollment < 300,
      paste0(district_name, "*"),
      district_name
    ),
    district_name,
    total_enrollment
  )


caaspp.cast.mry |>
  filter(
    test_year >= 2024,
    entity_type == "District",
    #       district_code == "00000",
    #  student_group_id %in% standard.groups,
    student_group_id == 1,
    test_id == 2,
    grade == 13,
    !is.na(percentage_standard_met_and_above)
  ) |>
  left_join(smalls) |>
  change.barbell(district_name_smalls) +
  labs(
    title = "Monterey County Districts change in Math CAASPP between <span style='color:#fed98e;'>2024</span> and <span style='color:#fe9929;'>2025</span>",
    #     subtitle = "Student Groups Percentage Met or Exceeded",
    x = "Percentage Met or Exceeded",
    caption = "* denotes small districts with enrollment less than 300 students"
  ) +
  theme(
    plot.title = element_markdown(size = 16)
  ) +
  coord_cartesian(clip = "off")

ggsave(
  here(
    "figs",
    "county",
    paste0(
      "CAASPP ",
      "Math",
      "District Change between 2024 and 2025 ",
      Sys.Date(),
      ".png"
    )
  ),
  width = 12,
  height = 8
)


caaspp.cast.mry |>
  filter(
    test_year >= 2024,
    entity_type == "District",
    #       district_code == "00000",
    #  student_group_id %in% standard.groups,
    student_group_id == 1,
    test_id == 1,
    grade == 13,
    !is.na(percentage_standard_met_and_above)
  ) |>
  left_join(smalls) |>
  change.barbell(district_name_smalls) +
  labs(
    title = "Monterey County Districts change in ELA CAASPP between <span style='color:#fed98e;'>2024</span> and <span style='color:#fe9929;'>2025</span>",
    #     subtitle = "Student Groups Percentage Met or Exceeded",
    x = "Percentage Met or Exceeded",
    caption = "* denotes small districts with enrollment less than 300 students"
  ) +
  theme(
    plot.title = element_markdown(size = 16)
  ) +
  coord_cartesian(clip = "off")

#fmt: skip
ggsave(
  here("figs","county",
    paste0("CAASPP ", "ELA",  "District Change between 2024 and 2025 ",  Sys.Date(),".png")
  ),
  width = 12,
  height = 8
)


caaspp.cast.mry |>
  filter(
    test_year >= 2024,
    entity_type == "District",
    #       district_code == "00000",
    #  student_group_id %in% standard.groups,
    student_group_id == 1,
    test_id == 17,
    grade == 13,
    !is.na(percentage_standard_met_and_above)
  ) |>
  left_join(smalls) |>
  change.barbell(district_name_smalls) +
  labs(
    title = "Monterey County Districts change in Science CAASPP between <span style='color:#fed98e;'>2024</span> and <span style='color:#fe9929;'>2025</span>",
    #     subtitle = "Student Groups Percentage Met or Exceeded",
    x = "Percentage Met or Exceeded",
    caption = "* denotes small districts with enrollment less than 300 students"
  ) +
  theme(
    plot.title = element_markdown(size = 16)
  ) +
  coord_cartesian(clip = "off")

ggsave(
  here(
    "figs",
    "county",
    paste0(
      "CAASPP ",
      "Science",
      "District Change between 2024 and 2025 ",
      Sys.Date(),
      ".png"
    )
  ),
  width = 12,
  height = 8
)


# County district

caaspp.cast.ca %>%
  filter(
    type_id == 5,
    student_group_id == 1,
    grade == 13,
    # County_Code == "27",
    # DistrictCode == "10272",
    test_year >= yr.prior,
    entity_type == "County",
    #       district_code == "00000",
    #  student_group_id %in% standard.groups,
    test_id == 1,
    !is.na(percentage_standard_met_and_above)
  ) |>
  mutate(
    county_name = if_else(
      county_name == "Monterey",
      paste0("<b style = 'font-size:12pt'>", county_name, "</b>"),
      county_name
    )
  ) %>%
  mutate(
    county_name = if_else(
      str_detect(county_name, class.3.above),
      county_name,
      paste0(county_name, "*"),
    )
  ) %>%

  change.barbell(county_name) +
  labs(
    title = "Counties change in ELA CAASPP between <span style='color:#fed98e;'>2024</span> and <span style='color:#fe9929;'>2025</span>",
    subtitle = "Student Groups Percentage Met or Exceeded",
    x = "Percentage Met or Exceeded",
    caption = "* denotes counties with enrollment less than 60,000 students (Class 3)"
  ) +
  theme(
    plot.title = element_markdown(size = 16),
    axis.text.y = element_markdown()
  ) +
  coord_cartesian(clip = "off")


#fmt: skip
ggsave(
  here(   "figs",  "county",paste0("CAASPP ",  "ELA", "Counties Change between 2024 and 2025 ", Sys.Date(),".png" )  ),
  width = 16,
  height = 12
)


################### Charters ---------

caaspp.cast.mry %>%
  filter(
    # type_id == 5,
    student_group_id == 1,
    grade == 13,
    # County_Code == "27",
    # DistrictCode == "10272",
    test_year >= yr.prior,
    entity_type %in%
      c(
        "District",
        "Direct Funded Charter School",
        "Locally Funded Charter School"
      ),
    #       district_code == "00000",
    #  student_group_id %in% standard.groups,
    test_id == 1,
    !is.na(percentage_standard_met_and_above)
  ) |>
  mutate(
    district_name = if_else(
      entity_type == "District",
      district_name,
      paste0("<b style = 'font-size:12pt'>", school_name, "</b>")
    )
  ) %>%

  change.barbell(district_name) +
  labs(
    title = "Charter and Comparison LEA Change in ELA CAASPP between <span style='color:#fed98e;'>2024</span> and <span style='color:#fe9929;'>2025</span>",
    subtitle = "Student Groups Percentage Met or Exceeded",
    x = "Percentage Met or Exceeded",
    #    caption = "* denotes counties with enrollment less than 60,000 students (Class 3)"
  ) +
  theme(
    plot.title = element_markdown(size = 16),
    axis.text.y = element_markdown()
  ) +
  coord_cartesian(clip = "off")


#fmt: skip
ggsave(
  here(   "figs",  paste0("CAASPP ",  "ELA", "LEAs and Charters Change between 2024 and 2025 ", Sys.Date(),".png" )  ),
  width = 14,
  height = 8
)


caaspp.cast.mry %>%
  filter(
    # type_id == 5,
    student_group_id == 1,
    grade == 13,
    # County_Code == "27",
    # DistrictCode == "10272",
    test_year >= yr.prior,
    entity_type %in%
      c(
        #"District",
        "Direct Funded Charter School",
        "Locally Funded Charter School"
      ),
    #       district_code == "00000",
    #  student_group_id %in% standard.groups,
    test_id == 1,
    !is.na(percentage_standard_met_and_above)
  ) |>
  # mutate(
  #   district_name = if_else(
  #     entity_type == "District",
  #     district_name,
  #     paste0("<b style = 'font-size:12pt'>", school_name, "</b>")
  #   )
  # ) %>%

  change.barbell(district_name) +
  labs(
    title = "Charter Change in ELA CAASPP between <span style='color:#fed98e;'>2024</span> and <span style='color:#fe9929;'>2025</span>",
    subtitle = "Student Groups Percentage Met or Exceeded",
    x = "Percentage Met or Exceeded",
    #    caption = "* denotes counties with enrollment less than 60,000 students (Class 3)"
  ) +
  theme(
    plot.title = element_markdown(size = 16),
    axis.text.y = element_markdown()
  ) +
  coord_cartesian(clip = "off")


#fmt: skip
ggsave(
  here(   "figs",  paste0("CAASPP ",  "ELA", " Charters Change between 2024 and 2025 ", Sys.Date(),".png" )  ),
  width = 10,
  height = 5
)


##### District Barbells -----

# Build a district-level working dataset
district.change <- caaspp.mry.hist %>%
  filter(
    # type_id == 5,
    # student_group_id == 1,
    grade == 13,
    # County_Code == "27",
    # DistrictCode == "10272",
    test_year >= yr.prior,
    entity_type %in%
      c(
        "District"
        #"Direct Funded Charter School",
        #"Locally Funded Charter School"
      ),
    #       district_code == "00000",
    student_group_id %in% standard.groups,
    #  test_id %in% c(1,2),
    !is.na(percentage_standard_met_and_above)
  )


# For each district/test/student_group, count how many yearly rows exist
# (used to drop groups that only have a single year and thus cannot show change).
change.count <- district.change |>
  group_by(district_name, test_id, student_group) |>
  count()

# Join counts and keep only groups with at least two years (n != 1)
district.change <- district.change |>
  left_join(change.count) |>
  filter(n != 1)

# Create a simple vector of distinct district names to iterate over
dist.list <- district.change |>
  select(district_name) |>
  na.omit() |>
  distinct() |>
  unlist()


# Loop over districts, then over assessments (1 = ELA, 2 = Math; 17 handled in case_match)
for (dist in dist.list) {
  print(dist) # progress log to console

  for (test in c(1, 2)) {
    print(test)

    test.name <- case_match(test, 1 ~ "ELA", 2 ~ "Math", 17 ~ "Science")

    district.change |>
      filter(test_id == test, district_name == dist) |>
      change.barbell(student_group) +
      labs(
        title = paste0(
          #        dist,
          "Change in ",
          test.name,
          " CAASPP between <span style='color:#fed98e;'>2024</span> and <span style='color:#fe9929;'>2025</span>"
        ),
        subtitle = paste0(dist, " - Percentage Met or Exceeded"),
        x = "Percentage Met or Exceeded",
        #    caption = "* denotes counties with enrollment less than 60,000 students (Class 3)"
      ) +
      theme(
        plot.title = element_markdown(size = 16),
        axis.text.y = element_markdown()
      ) +
      coord_cartesian(clip = "off")

    #fmt: skip
    ggsave(
      here(   "figs", dist,  paste0(dist, "CAASPP ",  test.name, " Change between 2024 and 2025 ", Sys.Date(),".png" )  ),
      width = 8,
      height = 4.5
    )
  }
}


#### Change Schools in a District ----

district.schools.change <- caaspp.mry.hist %>%
  filter(
    # type_id == 5,
    student_group_id == 1,
    grade == 13,
    # County_Code == "27",
    # DistrictCode == "10272",
    test_year >= yr.prior,
    entity_type %in%
      c(
        "School",
        # "District"
        "Direct Funded Charter School",
        "Locally Funded Charter School"
      ),
    #       district_code == "00000",
    # student_group_id %in% standard.groups,
    #  test_id %in% c(1,2),
    !is.na(percentage_standard_met_and_above)
  )


school.count <- district.schools.change |>
  group_by(district_name, test_id, school_name) |>
  count()

district.schools.change <- district.schools.change |>
  left_join(school.count) |>
  filter(n != 1)


dist.list <- district.schools.change |>
  select(district_name) |>
  na.omit() |>
  distinct() |>
  unlist()


for (dist in dist.list) {
  print(dist)

  for (test in c(1, 2)) {
    print(test)

    test.name <- case_match(test, 1 ~ "ELA", 2 ~ "Math", 17 ~ "Science")

    district.schools.change |>
      filter(test_id == test, district_name == dist) |>
      change.barbell(school_name) +
      labs(
        title = paste0(
          #        dist,
          " Change in ",
          test.name,
          " CAASPP between <span style='color:#fed98e;'>2024</span> and <span style='color:#fe9929;'>2025</span>"
        ),
        subtitle = paste0(dist, " - School Percentage Met or Exceeded"),
        x = "Percentage Met or Exceeded",
        #    caption = "* denotes counties with enrollment less than 60,000 students (Class 3)"
      ) +
      theme(
        plot.title = element_markdown(size = 16),
        axis.text.y = element_markdown()
      ) +
      coord_cartesian(clip = "off")

    #fmt: skip
    ggsave(
      here(   "figs", dist,  paste0(dist, " School CAASPP ",  test.name, " Change between 2024 and 2025 ", Sys.Date(),".png" )  ),
      width = 8,
      height = 4.5
    )
  }
}


### Student groups in a school -------

schools.studentgroup.change <- caaspp.mry.hist %>%
  filter(
    # type_id == 5,
    # student_group_id == 1,
    grade == 13,
    # County_Code == "27",
    # DistrictCode == "10272",
    test_year >= yr.prior,
    entity_type %in%
      c(
        "School",
        # "District"
        "Direct Funded Charter School",
        "Locally Funded Charter School"
      ),
    #       district_code == "00000",
    # student_group_id %in% standard.groups,
    #  test_id %in% c(1,2),
    !is.na(percentage_standard_met_and_above),
    !str_detect(school_name, "Alisal Virtual")
  )


school.studentgroup.count <- schools.studentgroup.change |>
  group_by(district_name, test_id, school_name, student_group) |>
  count()

schools.studentgroup.change <- schools.studentgroup.change |>
  left_join(school.studentgroup.count) |>
  filter(n != 1)


dist.list <- schools.studentgroup.change |>
  select(district_name) |>
  na.omit() |>
  distinct() |>
  unlist()

# dist.list <- dist.list[29]

for (dist in dist.list) {
  print(dist)

  schools <- schools.studentgroup.change |>
    filter(str_detect(dist, district_name)) |>
    select(school_name) |>
    na.omit() |>
    distinct() |>
    unlist()

  for (i in schools) {
    print(i)

    for (test in c(1, 2)) {
      print(test)

      test.name <- case_match(test, 1 ~ "ELA", 2 ~ "Math", 17 ~ "Science")

      schools.studentgroup.change |>
        filter(
          test_id == test,
          district_name == dist,
          school_name == i,
          student_group_id %in% standard.groups
        ) |>
        change.barbell(student_group) +
        labs(
          title = paste0(
            #       dist,
            " Change in ",
            test.name,
            " CAASPP between <span style='color:#fed98e;'>2024</span> and <span style='color:#fe9929;'>2025</span>"
          ),
          subtitle = paste0(dist, " - ", i, " - Percentage Met or Exceeded"),
          x = "Percentage Met or Exceeded",
          #    caption = "* denotes counties with enrollment less than 60,000 students (Class 3)"
        ) +
        theme(
          plot.title = element_markdown(size = 16),
          axis.text.y = element_markdown()
        ) +
        coord_cartesian(clip = "off")

      #fmt: skip
      ggsave(
      here(   "figs", dist,  paste0(dist, " - " , i, " CAASPP ",  test.name, " Change between 2024 and 2025 ", Sys.Date(),".png" )  ),
      width = 8,
      height = 4.5
    )
    }
  }
}


# Barbells for change by grade level by school

schools.grade.change <- caaspp.mry.hist %>%
  filter(
    # type_id == 5,
    student_group_id == 1,
    grade != 13,
    # County_Code == "27",
    # DistrictCode == "10272",
    test_year >= yr.prior,
    entity_type %in%
      c(
        "School",
        # "District"
        "Direct Funded Charter School",
        "Locally Funded Charter School"
      ),
    #       district_code == "00000",
    # student_group_id %in% standard.groups,
    #  test_id %in% c(1,2),
    !is.na(percentage_standard_met_and_above)
  ) |>
  mutate(grade = factor(grade))


schools.grade.count <- schools.grade.change |>
  group_by(district_name, test_id, school_name, grade) |>
  count()

schools.grade.change <- schools.grade.change |>
  left_join(schools.grade.count) |>
  filter(n != 1)


dist.list <- schools.grade.change |>
  select(district_name) |>
  na.omit() |>
  distinct() |>
  unlist()

dist.list <- dist.list[23:28]


for (dist in dist.list) {
  print(dist)

  schools <- schools.grade.change |>
    filter(str_detect(dist, district_name)) |>
    select(school_name) |>
    na.omit() |>
    distinct() |>
    unlist()

  for (i in schools) {
    print(i)

    for (test in c(1, 2)) {
      print(test)

      test.name <- case_match(test, 1 ~ "ELA", 2 ~ "Math", 17 ~ "Science")

      schools.grade.change |>
        filter(
          test_id == test,
          district_name == dist,
          school_name == i,
          # student_group_id %in% standard.groups
        ) |>
        change.barbell(grade, ordered.sort = FALSE) +
        labs(
          title = paste0(
            #       dist,
            " Change in ",
            test.name,
            " CAASPP between <span style='color:#fed98e;'>2024</span> and <span style='color:#fe9929;'>2025</span>"
          ),
          subtitle = paste0(dist, " - ", i, " - Percentage Met or Exceeded"),
          x = "Percentage Met or Exceeded",
          #    caption = "* denotes counties with enrollment less than 60,000 students (Class 3)"
        ) +
        theme(
          plot.title = element_markdown(size = 16),
          axis.text.y = element_markdown()
        ) +
        coord_cartesian(clip = "off")

      #fmt: skip
      ggsave(
        here(   "figs", dist,  paste0(dist, " - " , i, " CAASPP by Grade ",  test.name, " Change between 2024 and 2025 ", Sys.Date(),".png" )  ),
        width = 8,
        height = 4.5
      )
    }
  }
}


#### Summary tables ------

district.schools.change2 <- district.schools.change |>
  pivot_wider(
    id_cols = c(district_name, school_name, test_id), # grouping variable passed tidily
    names_from = test_year, # becomes columns like `2024`, `2025`
    values_from = percentage_standard_met_and_above
  ) %>%
  # Compute change and a label with +/− sign
  mutate(
    change = round2(`2025` - `2024`, digits = 1),
    labl = if_else(change > 0, paste0("+", change), paste0(change))
  ) |>
  filter(test_id == 2) |>
  arrange(desc(change))


district.change2 <- district.change |>
  filter(student_group_id == 1) |> #128, 160
  pivot_wider(
    id_cols = c(district_name, test_id), # grouping variable passed tidily
    names_from = test_year, # becomes columns like `2024`, `2025`
    values_from = percentage_standard_met_and_above
  ) %>%
  # Compute change and a label with +/− sign
  mutate(
    change = round2(`2025` - `2024`, digits = 1),
    labl = if_else(change > 0, paste0("+", change), paste0(change))
  ) |>
  #  filter(test_id == 2) |>
  filter(str_detect(
    district_name,
    "Greenfield|King|South|Peninsul|Salinas Union|Ardo|North|Soledad|Gonzales"
  ))
arrange(test_id, desc(change))


### Function for top change ----

district.change <- caaspp.cast.mry %>%
  filter(
    # type_id == 5,
    # student_group_id == 1,
    grade == 13,
    # County_Code == "27",
    # DistrictCode == "10272",
    test_year >= yr.prior,
    entity_type %in%
      c(
        "County"
        # "District"
        #"Direct Funded Charter School",
        #"Locally Funded Charter School"
      ),
    #       district_code == "00000",
    student_group_id %in% standard.groups,
    #  test_id %in% c(1,2),
    !is.na(percentage_standard_met_and_above)
  )


# For each district/test/student_group, count how many yearly rows exist
# (used to drop groups that only have a single year and thus cannot show change).
change.count <- district.change |>
  group_by(test_id, student_group) |>
  count()

# Join counts and keep only groups with at least two years (n != 1)
district.change <- district.change |>
  left_join(change.count) |>
  filter(n != 1)


district.change2 <- district.change |>
  # filter(student_group_id == 31) |> #128, 160
  pivot_wider(
    id_cols = c(student_group, test_id), # grouping variable passed tidily
    names_from = test_year, # becomes columns like `2024`, `2025`
    values_from = percentage_standard_met_and_above
  ) %>%
  # Compute change and a label with +/− sign
  mutate(
    change = round2(`2025` - `2024`, digits = 1),
    labl = if_else(change > 0, paste0("+", change), paste0(change))
  ) |>
  #  filter(test_id == 2) |>
  arrange(desc(change))


district.change <- caaspp.cast.mry %>%
  filter(
    # type_id == 5,
    student_group_id == 1,
    # grade == 13,
    # County_Code == "27",
    # DistrictCode == "10272",
    test_year >= yr.prior,
    entity_type %in%
      c(
        "County"
        # "District"
        #"Direct Funded Charter School",
        #"Locally Funded Charter School"
      ),
    #       district_code == "00000",
    student_group_id %in% standard.groups,
    #  test_id %in% c(1,2),
    !is.na(percentage_standard_met_and_above)
  )


# For each district/test/student_group, count how many yearly rows exist
# (used to drop groups that only have a single year and thus cannot show change).
change.count <- district.change |>
  group_by(test_id, grade) |>
  count()

# Join counts and keep only groups with at least two years (n != 1)
district.change <- district.change |>
  left_join(change.count) |>
  filter(n != 1)


district.change2 <- district.change |>
  # filter(student_group_id == 31) |> #128, 160
  pivot_wider(
    id_cols = c(grade, test_id), # grouping variable passed tidily
    names_from = test_year, # becomes columns like `2024`, `2025`
    values_from = percentage_standard_met_and_above
  ) %>%
  # Compute change and a label with +/− sign
  mutate(
    change = round2(`2025` - `2024`, digits = 1),
    labl = if_else(change > 0, paste0("+", change), paste0(change))
  ) |>
  #  filter(test_id == 2) |>
  arrange(desc(change))
