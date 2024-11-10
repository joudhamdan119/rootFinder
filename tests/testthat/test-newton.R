test_that("Newton-Raphson finds root for simple quadratic function", {
  f <- function(x) x^2 - 4
  result <- newton(f, x0 = 3)
  expect_true(abs(result$Root - 2) < 1e-6)
})

test_that("The method did not converge within the specified maximum iterations.", {
  f <- function(x) x^2 - 4
  expect_message(newton(f, x0 = 1000, MAXITERATIONS = 10),
                 "The method did not converge within the specified maximum iterations.")

  result <- newton(f, x0 = 1000, MAXITERATIONS = 10)
  expect_true(is.na(result))
})

