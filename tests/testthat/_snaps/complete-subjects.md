# complete_subjects keeps only subjects observed in every condition

    Code
      complete_subjects(df, group, value, subject.id = id)
    Output
      # A tibble: 6 x 3
        .rowid group value
         <int> <fct> <dbl>
      1      1 a         1
      2      1 b         2
      3      3 a         5
      4      3 b         6
      5      4 a         7
      6      4 b         8

# complete_subjects needs at most one row per subject and condition

    Each `id` value must appear at most once per level of `group`.
    i Aggregate repeated observations within each condition first.

