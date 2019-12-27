
#' Detect invalid IUPAC DNA characters
#' @inheritParams class_bstr
#' @param negate If TRUE, return non-matching elements.
#' @export
#' @examples
#' test <- c("a", "B", "c", "D", "e")
#' is_valid_dna_character(test)
#' is_valid_dna_character(test, negate = TRUE) %>%
#'   test[.]
#'
is_valid_dna_character <-
  function(x, negate = FALSE) {
    if(!is.character(x)) stop("x must be a character vector")
    stringr::str_to_upper(x) %>%
      stringr::str_detect(REGEX_NOT_DNA_IUPAC_ALPHABET, negate = !negate)
  }

