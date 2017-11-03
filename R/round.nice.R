#'Nice Round
#'
#'Rounds numbers to a "pretty" number
#'@param x Most numbers. I'm not sure how robust this is
#'@export
#'@examples
#'round.nice()

round.nice <- function(x, nice=c(1:10)){
  if(length(x)!=1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

