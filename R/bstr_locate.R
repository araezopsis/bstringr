
#' Locate all positions of patterns in a bstr object
#' @inheritParams class_bstr
#' @export
#' @examples
#' bstr_locate(c("ABCDE"), c("A", "C"))
#' bstr_locate(c("ABCDE", "BC"), c("A", "C"))
#'
bstr_locate <- function(bstrobj, pattern) {
  bstrobj <- as_bstr(bstrobj)
  n <- names(bstrobj)
  li <- stringr::str_locate_all(bstrobj, pattern)
  names(li) <- n
  li
}

