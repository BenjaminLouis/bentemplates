#' Concatenate strings and ignore missing values
#'
#' @param ... several string values
#'
#' @return Concatenated strings given as input with space delimiter. A missing
#' value (\code{NA}) is simply ignored. If all niputs are \code{NA}'s, \code{NA} is
#' returned
#' @export
#'
#' @examples
#' concatenate("Hello", NA, "world", NA)
#' concatenate(NA, NA)
concatenate <- function(...) {
  st <- glue(..., .na = "", .sep = " ")
  st <- gsub("\\s{2,}", " ", st)
  st <- gsub("(^\\s+)|(\\s+$)", "", st)
  st <- gsub("^$", NA, st)
  return(st)
}


#' Transform a number into a string representing the number with two decimals,
#' a comma delimiter and the euro sign
#'
#' @param x a numeric value
#'
#' @return the numeric value with two decimals, a comma delimiter, and the euro sign
#' @export
#'
#' @examples
#' parse_amount(pi)
parse_amount <- function(x) {
  x <- format(x, nsmall = 2, digits = 2)
  x <- gsub("\\.", ",", x)
  x <- paste(x, "\u20AC")
  return(x)
}
