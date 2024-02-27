cumsum_reset <- function(x, reset = NA)
{
  count <- 0
  ret <- rep(NA_integer_, length(x))
  for (i in seq_along(x))  {
    count <- count + x[i]
    if (x[i] %in% reset) count <- 0
    ret[i] <- count
  }
  return(ret)
}
