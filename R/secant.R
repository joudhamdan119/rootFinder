#' Secant Method
#'
#' A function to find the root of a given function using the secant method.
#' This method approximates the root by iteratively drawing secant lines.
#'
#' @param f A function for which the root is sought.
#' @param x0 The first initial guess for the root.
#' @param x1 The second initial guess for the root.
#' @param MAXITERATIONS Maximum number of iterations before stopping.
#' @param tol The tolerance for stopping criteria. The method stops when the function value is smaller than this tolerance.
#' @return A list with the root of the function, the value of the function at that root, and the number of iterations it took.
#' @export
secant <- function(f, x0, x1, MAXITERATIONS = 10000, tol = 1e-6)
{
  x = x1 - ((x1 - x0) / (f(x1) - f(x0))) * f(x1)
  i = 1
  while (i <= MAXITERATIONS)
  {
    if (abs(f(x)) > tol)
      i = i + 1
    else
      break

    x0 = x1
    x1 = x
    x = x1 - ((x1 - x0) / (f(x1) - f(x0))) * f(x1)
  }

  if (i == MAXITERATIONS + 1)
  {
    message("ERROR: The method did not converge within the specified maximum iterations.")
    return(NA)
  }

  out=list(Root=x,Value=f(x),Iteration=i)
  return(out)
}
