
wakefield::r_data_frame(
  n = 100,
  cat1 = r_sample_factor(x = LETTERS[1:3]),
  cat2 = r_sample_factor(x = LETTERS[1:3]),
  cat3 = r_sample_factor(x = LETTERS[1:3]),
  bin1 = r_sample_logical()
) %>%
  dplyr::filter(bin1) %>%
  dplyr::select(-bin1) %>%
  xtabs( ~ ., data = .)