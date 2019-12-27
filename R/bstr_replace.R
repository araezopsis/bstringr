
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
