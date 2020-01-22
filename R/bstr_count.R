
#' Count the number of matches in a bstr object
#' @inheritParams class_bstr
#' @export
#' @examples
#' bstr_count(c("apple", "banana"))
#' bstr_count(c("apple", "banana"), "a")
#' bstr_count(c("apple", "banana"), c("a", "n"))
#'
bstr_count <- function(bstrobj, pattern = "") {
  bstrobj <- as_bstr(bstrobj)
  stringr::str_count(bstrobj, pattern)
}

