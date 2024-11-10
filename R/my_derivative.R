my_derivative <- function(f, a)
{
  h = 1e-10 # choose h as very small
  derivative = (f(a + h) - f(a)) / h

  return (derivative)
}
