library(testthat)

test_that("Bissection function finds root correctly", {
  f <- function(x) x^2 - 4  # The root should be x = ±2

  # Call bissection to find the root in the interval [1, 3]
  result <- bissection(f, 1, 3)

  # Check that the root is approximately 2
  expect_equal(result$Root, 2, tolerance = 1e-6)
  expect_true(abs(f(result$Root)) <= 1e-6)
})

test_that("Bisection handles case with no root in interval", {
  f <- function(x) x + 1
  expect_warning(bissection(f, 2, 3), "No root exists in this interval!")
})

