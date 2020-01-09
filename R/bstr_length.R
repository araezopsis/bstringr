
#' Sequence length in the bstr sequences.
#' @inheritParams class_bstr
#' @export
#' @name length
#' @examples
#' test <- c("ATG", "cc tgAGT2--")
#' bstr_length(test)
#' bstr_length(NA_character_)
#'
bstr_length <-
  function(bstrobj) {
    as_bstr(bstrobj) %>% stringr::str_length()
  }

#' @rdname length
#' @export
dstr_length <- bstr_length

#' @rdname length
#' @export
astr_length <- bstr_length

