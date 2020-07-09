
# #' Set names of a bstrobj
# #' @inheritParams class_bstr
# #' @param nm Vector of names, the same length as bstrobj
# #'
# bstr_rename <- function(bstrobj, nm) bstr(bstrobj, n = nm)

#' Replace names of the bstring object
#' @inheritParams bstr_replace
#' @export
#' @examples
#' temp <- bstr(c("aaa", "bbb"))
#' c(temp, bstr_replace_name(temp, "\\d", c("one", "two")))
#'
bstr_replace_name <- function(bstrobj, pattern, replacement) {
  nm <-
    names(bstrobj) %>%
    stringr::str_replace(pattern = pattern, replacement = replacement)
  names(bstrobj) <- nm
  bstrobj
}

