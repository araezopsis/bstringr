#' Remove all matched patterns in bstr sequences
#'
#' @inheritParams class_bstr_arg
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
bstr_remove <-
  function(bstrobj, pattern, case_sensitive = F){
    bstrobj <- as_bstr(bstrobj)
    at <- attributes(bstrobj)
    if(!case_sensitive) pattern <- paste0("(?i)", pattern)

    bstrobj <- stringr::str_remove_all(string = bstrobj, pattern = pattern)

    attributes(bstrobj) <- at
    bstrobj
  }

#' @rdname remove
#' @export
bstr_remove_num <-
  function(bstrobj){
    bstr_remove(bstrobj = bstrobj, pattern = "[[:digit:]]")
  }

#' @rdname remove
#' @export
bstr_remove_notalpha <-
  function(bstrobj){
    bstr_remove(bstrobj = bstrobj, pattern = "[^[:alpha:]]")
  }

#' @rdname remove
#' @param gap_chr a gap character
#' @export
bstr_remove_gap <-
  function(bstrobj, gap_chr = "-"){
    bstr_remove(bstrobj = bstrobj, pattern = gap_chr)
  }

#' Replace matched patterns in bstr sequences by replacements
#' @inheritParams class_bstr_arg
#' @param pattern Pattern to look for.
#' @param replacement A character vector of replacements or a function.
#' @export
#' @examples
#' bstr_replace("AtGcTaat", "at", "cc")
#' bstr_replace("AtGcTaat", "at", c("cc", "xx"))
#' bstr_replace("AtGcTaat", c("at", "gc"), c("cc", "xx"))
#' bstr_replace("AtGcTaat", c("at", "gc"), c("cc", "xx"), TRUE)
#' bstr_replace("AtGcTaat", c("at", "gc"), bstr_switch_case)
#'
bstr_replace <-
  function(bstrobj, pattern, replacement, case_sensitive = FALSE) {
    bstrobj <- as_bstr(bstrobj)
    at <- attributes(bstrobj)
    if(!case_sensitive) pattern <- paste0("(?i)", pattern)

    bstrobj <- stringr::str_replace_all(
      string = bstrobj,
      pattern = pattern,
      replacement = replacement
    )

    attributes(bstrobj) <- at
    bstrobj
  }
