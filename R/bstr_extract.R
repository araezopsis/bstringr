
#' Extract matching patterns from a bstr object
#' @inheritParams class_bstr
#' @export
#' @examples
#' bstr_locate(c("ABCDE"), c("A", "C"))
#' bstr_extract(c("ABCDE"), c("A", "C"))
#'
#' bstr_locate(c("ABCDE", "BC"), c("A", "C"))
#' bstr_extract(c("ABCDE", "BC"), c("A", "C"))
#'
bstr_extract <- function(bstrobj, pattern) {
  . <- NULL
  bstr_locate(bstrobj, pattern) %>%
    bstr_sub_all(bstrobj, .)
}

