#' bstr_remove
#' @importFrom stringr str_remove_all
#' @inheritParams class_bstr_arg
#' @export
bstr_remove <-
  function(bstrobj, pattern, case_sensitive = F){
    bstrobj <- as_bstr(bstrobj)
    at <- attributes(bstrobj)
    if(!case_sensitive) pattern <- paste0("(?i)", pattern)

    bstrobj <- str_remove_all(string = bstrobj, pattern = pattern)

    attributes(bstrobj) <- at
    bstrobj
  }

#' bstr_remove_num
#' @inheritParams class_bstr_arg
#' @export
bstr_remove_num <-
  function(bstrobj){
    bstr_remove(bstrobj = bstrobj, pattern = "[[:digit:]]")
  }

#' bstr_remove_notalpha
#' @inheritParams class_bstr_arg
#' @export
bstr_remove_notalpha <-
  function(bstrobj){
    bstr_remove(bstrobj = bstrobj, pattern = "[^[:alpha:]]")
  }

#' Remove all gap character
#' @inheritParams class_bstr_arg
#' @param gap_chr a gap character
#' @export
bstr_remove_gap <-
  function(bstrobj, gap_chr = "-"){
    bstr_remove(bstrobj = bstrobj, pattern = gap_chr)
  }

#' bstr_replace
#' @importFrom stringr str_replace_all
#' @inheritParams class_bstr_arg
#' @param replacement a character vector
#' @export
bstr_replace <-
  function(bstrobj, pattern, replacement, case_sensitive = F){
    bstrobj <- as_bstr(bstrobj)
    at <- attributes(bstrobj)
    if(!case_sensitive) pattern <- paste0("(?i)", pattern)

    bstrobj <- str_replace_all(
      string = bstrobj,
      pattern = pattern,
      replacement = replacement
    )

    attributes(bstrobj) <- at
    bstrobj
  }
