test_that("time_cumulate works as expected", {
  test <- new.magpie(years = c(2005, 2010, 2015), fill = c(5, 10, 15))

  result <- timeCumulate(test, includeEndYear = FALSE)
  expect_equal(as.numeric(result[, 2005, ]), 0)
  expect_equal(as.numeric(result[, 2010, ]), 35)
  expect_equal(as.numeric(result[, 2015, ]), 95)

  result <- timeCumulate(test, includeEndYear = TRUE)
  expect_equal(as.numeric(result[, 2005, ]), 5)
  expect_equal(as.numeric(result[, 2010, ]), 45)
  expect_equal(as.numeric(result[, 2015, ]), 110)

  test <- new.magpie(years = c(2005, 2010, 2015), fill = c(5, NA, 15))
  expect_warning(timeCumulate(test))
})
