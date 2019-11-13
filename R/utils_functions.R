#' Make first letter uppersase
#'
#' @param st a string value
#'
#' @return The same string with the first letter as uppercase
#' @export
#'
#'
#' @examples
#' up_first("hello")
up_first <- function(st) {
  vapply(st, function(x) gsub("^.{1}", toupper(substr(x, 1, 1)), x), FUN.VALUE = NA_character_, USE.NAMES = FALSE)
}



#' Remove header of a HTML table
#'
#' @param x a HTML table with <thead> header tags
#'
#' @return the same HTML table but without the header part
#' @export
#'
#' @examples
#' library(knitr)
#' tab <- kable(iris, format = "html")
#' tab
#'
#' #Removing header
#' remove_header(tab)
#'
remove_header <- function(x) { gsub("<thead>.*</thead>", "", x) }



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
