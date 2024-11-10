#' Newton-Raphson Method
#'
#' A function to find the root of a given function using the Newton-Raphson method.
#' This method uses the function values and their derivatives to approximate the root.
#' @param f A function for which the root is sought.
#' @param x0 The initial guess for the root.
#' @param MAXITERATIONS Maximum number of iterations before stopping.
#' @param tol The tolerance for stopping criteria. The method stops when the difference between consecutive estimates is smaller than this tolerance.
#' @return A list with the root of the function, the value of the function at that root, and the number of iterations it took.
#' @export
newton <- function(f, x0, MAXITERATIONS = 10000, tol = 1e-6)
{
  i = 1
  if (my_derivative(f, x0) == 0)
  {
    message("Newton's Method fails!")
    return(NA)
  }
  x = x0 - (f(x0) / my_derivative(f, x0))
  while(i <= MAXITERATIONS)
  {
    if (abs(x - x0) > tol)
      i = i + 1
    else
      break

    x0 = x
    if (my_derivative(f, x0) == 0)
    {
      message("Newton's Method fails!")
      return(NA)
    }
    x = x0 - (f(x0) / my_derivative(f, x0))
  }

  if (i == MAXITERATIONS + 1)
  {
    message("The method did not converge within the specified maximum iterations.")
    return(NA)
  }

  out=list(Root=x,Value=f(x),Iteration=i)
  return(out)
}
