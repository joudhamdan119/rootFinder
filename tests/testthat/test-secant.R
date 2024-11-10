test_that("Secant method finds root for simple quadratic function", {
  f <- function(x) x^2 - 4
  result <- secant(f, x0 = 1, x1 = 3)
  expect_true(abs(result$Root - 2) < 1e-6)
  expect_true(result$Iteration <= 10000)
})

test_that("Secant method does not converge within maximum iterations", {
  f <- function(x) x^2 - 4
  expect_message(secant(f, x0 = 1000, x1 = 1001, MAXITERATIONS = 10),
                 "ERROR: The method did not converge within the specified maximum iterations.")

  result <- secant(f, x0 = 1000, x1 = 1001, MAXITERATIONS = 10)
  expect_true(is.na(result))
})
