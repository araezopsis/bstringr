
#' Sequence length in the bstr sequences.
#' @inheritParams class_bstr
#' @export
#' @examples
#' bstr_length(c("ATG", "cc tgAGT2--"))
#' bstr_length(NA_character_)
#'
bstr_length <- function(bstrobj) {
  as_bstr(bstrobj) %>% stringr::str_length()
}
