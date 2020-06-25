#' Remove all matched patterns in bstr sequences
#'
#' @inheritParams class_bstr
#' @return A bstr object.
#' @examples
#' temp <- bstr(c("ATGCCCTAG", "aTGCcCtAg"))
#' c(temp, remove = bstr_remove(temp, "[aT]"))
#' c(temp, remove = bstr_remove(temp, "aT"))
#' c(temp, remove = bstr_remove(temp, "aT", TRUE))
#'
#' bstr_remove_num(bstr("ac12 -xe gg. "))
#' bstr_remove_notalpha(bstr("ac12 -xe gg. "))
#' bstr_remove_gap(bstr("ac12 -xe gg. "))
#'
#' @name remove

#' @rdname remove
#' @export
bstr_remove <- function(bstrobj, pattern, case_sensitive = FALSE) {
  bstrobj <- as_bstr(bstrobj)
  at <- attributes(bstrobj)

  if(!case_sensitive) pattern <- paste0("(?i)", pattern)
  bstrobj <- stringr::str_remove_all(string = bstrobj, pattern = pattern)

  attributes(bstrobj) <- at
  bstrobj
}

#' @rdname remove
#' @export
bstr_remove_num <- function(bstrobj) {
  bstr_remove(bstrobj = bstrobj, pattern = "[[:digit:]]")
}

#' @rdname remove
#' @export
bstr_remove_notalpha <- function(bstrobj) {
  bstr_remove(bstrobj = bstrobj, pattern = "[^[:alpha:]]")
}

#' @rdname remove
#' @param gap_chr a gap character
#' @export
bstr_remove_gap <- function(bstrobj, gap_chr = "-") {
  bstr_remove(bstrobj = bstrobj, pattern = gap_chr)
}

