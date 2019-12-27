
#' Detect invalid IUPAC Amino Acid characters
#' @inheritParams class_bstr
#' @param negate If TRUE, return non-matching elements.
#' @export
#' @examples
#' test <- c(letters[1:10])
#' test2 <-
#'   c(paste(test[1:5], collapse = ""), paste(test[6:10], collapse = ""))
#' is_valid_aa_character(test) %>% table
#' is_valid_aa_character(test, negate = TRUE) %>% test[.]
#' is_valid_aa_character(test2)
#'
is_valid_aa_character <-
  function(x, negate = FALSE) {
    if(!is.character(x)) stop("x must be a character vector")
    stringr::str_to_upper(x) %>%
      stringr::str_detect(REGEX_NOT_AA_ALPHABET, negate = !negate)
  }

