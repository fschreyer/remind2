library(dplyr)

test_that("testRanges works for magpie object", {

  data <- bind_rows(
    expand_grid(variable = 'Foo Share (%)',
                value = c(-1, 0, 42, 100, 101)),
    expand_grid(variable = 'bar share (percent)',
                value = c(-1, 0, 42, 100, 101))) %>%
      group_by(variable) %>%
      mutate(year = 2000 + (1:n())) %>%
      ungroup() %>%
      select(year, variable, value) %>%
      as.magpie(spatial = 0, temporal = 1, datacol = 3)

  tests <- list(list("Share.*\\((%|Percent)\\)$", low = 0, up = 100))

  expect_error(testRanges(data, tests, reaction = "stop"))
})

