#' Bisection Method
#' A function to find the root of a given function within an interval using the bisection method.
#' @param f A function for which the root is sought.
#' @param a The starting point of the interval.
#' @param b The ending point of the interval.
#' @param MAXITERATIONS  Maximum number of iterations.
#' @param  tol Tolerance for stopping criteria.
#' @return The estimated root of the function.
#' @export
bissection <- function(f, a, b, MAXITERATIONS = 10000, tol = 1e-6)
{
  if (f(a)*f(b) > 0)
    warning("No root exists in this interval!")
  else if (f(a) == 0)
    c = a
  else if (f(b) == 0)
    c = b
  else
  {
    c = (a+b) / 2
    i = 1
    while (i <= MAXITERATIONS)
    {
      if (abs(f(c)) > tol)
        i = i + 1
      else
        break

      c = (a+b) / 2
      if (f(a)*f(c)<0)
        b = c
      if (f(b)*f(c)<0)
        a = c
    }

    if (i == MAXITERATIONS + 1)
    {
      message("ERROR: The method did not converge within the specified maximum iterations.")
      return(NA)
    }

    out=list(Root=c,Value=f(c),Iteration=i)
    return(out)
  }
}
