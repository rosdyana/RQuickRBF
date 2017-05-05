#' Generate center select
#' @param data training datasets 
#' @param number number of center
#' @export
#' @examples
#' \dontrun{
#' csv1 = read.csv("data.csv", sep = ",", header = TRUE)
#' cs = center_select(csv1, 666)
#' }
center_select <- function(data, number)
{
  chunk <- number
  df <- data.frame(data)
  n <- nrow(df)
  if (n < chunk) return(print("Please choose another number, data lenght less than your request center number."))
  r  <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
  d <- split(df,r)
  x <- sample(1:ceiling(n/chunk),1,replace=T)
  return(data.frame(d[x]))
}
