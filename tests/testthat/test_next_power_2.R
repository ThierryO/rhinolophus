context("next_power_2")
test_that("next_power_2 handles exception", {
  expect_error(next_power_2("a"), "x is not numeric")
  expect_error(next_power_2(0.5), "x must be > 1")
  expect_error(next_power_2(c(0.5, 10)), "x must be > 1")
})
test_that("next_power_2 works correctly", {
  expect_identical(next_power_2(c(1.5, 2)), c(2, 2))
  expect_identical(next_power_2(c(3, 4)), c(4, 4))
  expect_identical(next_power_2(3L), 4)
})
