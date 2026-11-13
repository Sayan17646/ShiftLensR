test_that("validation accepts valid frames and rejects malformed input", {
  expect_invisible(sl_validate(data.frame(x = 1:3)))
  expect_error(sl_validate(list(x = 1:3)), "data frame")
  expect_error(sl_validate(data.frame()), "at least one row and one column")
  duplicate_names <- data.frame(a = 1:2, b = 3:4)
  names(duplicate_names) <- c("x", "x")
  expect_error(sl_validate(duplicate_names), "unique")
})

test_that("profile and summary report missingness and numeric statistics", {
  result <- sl_profile(data.frame(x = c(1, 2, NA), group = c("a", "a", "b")))
  expect_s3_class(result, "shiftlens_profile")
  expect_equal(result$missing, c(1, 0))
  expect_equal(result$mean[1], 1.5)
  expect_equal(summary(result)$rows, 3L)
  expect_equal(summary(result)$total_missing, 1)
})

test_that("quality reports missing values, constant features, and duplicates", {
  result <- sl_quality(data.frame(x = c(1, 1, NA), y = c("a", "a", "a")))
  expect_true(all(c("missing_values", "constant_feature", "duplicate_rows") %in% result$issue))
})

test_that("comparison distinguishes numeric and categorical changes", {
  reference <- data.frame(x = c(1, 2, 3, 4), group = c("a", "a", "b", "b"))
  current <- data.frame(x = c(4, 5, 6, 7), group = c("a", "b", "b", "c"))
  result <- sl_compare(reference, current)
  expect_equal(result$type, c("numeric", "categorical"))
  expect_gt(result$standardized_mean_delta[1], 0)
  expect_gt(result$categorical_tv[2], 0)
})

test_that("comparison handles no shared columns", {
  result <- sl_compare(data.frame(x = 1:2), data.frame(y = 3:4))
  expect_equal(nrow(result), 0L)
})
