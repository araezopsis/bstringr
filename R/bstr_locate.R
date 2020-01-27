
#' Locate all positions of patterns in a bstr object
#' @inheritParams class_bstr
#' @export
#' @examples
#' bstr_locate(c("ABCDE", "BCDE"), "A")
#' bstr_locate(c("ABCDE"), c("A", "C"))
#' bstr_locate(c("ABCDE", "BC"), c("A", "D", "F"))
#'
bstr_locate <- function(bstrobj, pattern) {
  bstrobj <- as_bstr(bstrobj)
  n <- names(bstrobj)
  li <- stringi::stri_locate_all_regex(bstrobj, pattern)
  names(li) <- n
  li
}

