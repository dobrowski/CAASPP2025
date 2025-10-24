greenfield.elpac <- elpac.ca |>
  filter(
    county_code == "27",
    str_detect(district_name, "Greenfield Union"),
    #    grade == 13,
    student_group_id == "001",
  ) |>
  mutate(
    grade = if_else(grade == "13", "All", grade),
    grade = str_remove(grade, "0"),
    grade = factor(grade),
    grade = fct_relevel(grade, "All", "KN")
  )

greenfield.elpac <- greenfield.elpac |>
  filter(grade == 6 & school_code == "0000000") |>
  mutate(school_name = "Vista Verde Middle") |>
  bind_rows(greenfield.elpac)


elpac.by.grade <- function(df, skul) {
  greenfield.elpac |>
    filter(
      str_detect(school_name, skul)
    ) |>
    pivot_longer(cols = (ends_with("pcnt") & contains("domain"))) |>
    mutate(
      domain = str_split(name, "_", simplify = TRUE)[, 1],
      rank = str_split(name, "_", simplify = TRUE)[, 3],
      value = as.numeric(value),
      domain = factor(
        domain,
        levels = c("speaking", "listening", "reading", "writing")
      ),
      rank = factor(rank, levels = c("begin", "moderate", "developed"))
    ) |>
    ggplot(aes(
      x = domain,
      group = rank,
      y = value,
      fill = rank,
      label = value
    )) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5)) +
    facet_wrap(~grade) +
    mcoe_theme +
    labs(
      title = paste0(skul, " - Student Achievement by ELAPC Domain 2025"),
      subtitle = "Percentage at each developmental level"
    )
}


elpac.by.grade(greenfield.elpac, "Arroyo")


school.list <- greenfield.elpac |>
  select(school_name) |>
  na.exclude() |>
  filter(!str_detect(school_name, "District")) |>
  unique() |>
  pull()


for (schol in school.list) {
  elpac.by.grade(greenfield.elpac, schol)
}


###

elpac.ca |>
  filter(
    county_code == "27",
    str_detect(district_name, "Greenfield Union"),
    str_detect(school_name, "Vista"),
    # student_group_id %in% c("001", "120", "160", "250", "251"),
    student_group_id %in% c("248", "242", "243", "244", "245", "246", "247"),
    grade == 13,
    #  student_group_id == "001",
  ) |>
  select(student_group_id, total_students_tested_with_scores)
