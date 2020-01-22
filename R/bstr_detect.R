
#' Detect the presence or absence of a pattern in a bstr object
#' @inheritParams class_bstr
#' @param negate If TRUE, return non-matching elements.
#' @export
#' @examples
#' bstr_detect(c("apple", "banana"), "n")
#' bstr_detect(c("apple", "banana"), c("a", "n"))
#'
bstr_detect <- function(bstrobj, pattern, negate = FALSE) {
  bstrobj <- as_bstr(bstrobj)
  stringr::str_detect(bstrobj, pattern, negate)
}

