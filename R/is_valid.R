
#' Detect invalid IUPAC DNA characters
#' @inheritParams class_bstr
#' @param negate If TRUE, return non-matching elements.
#' @name check_validity
#' @export
#' @examples
#' test <- c("a", "B", "c", "D", "e")
#' is_valid_dna_character(test)
#' is_valid_dna_character(test, negate = TRUE) %>%
#'   test[.]
#'
#' test <- c(letters[1:10])
#' test2 <-
#'   c(paste(test[1:5], collapse = ""), paste(test[6:10], collapse = ""))
#' is_valid_aa_character(test) %>% table
#' is_valid_aa_character(test, negate = TRUE) %>% test[.]
#' is_valid_aa_character(test2)
#'
is_valid_dna_character <-
  function(x, negate = FALSE) {
    if(!is.character(x)) stop("x must be a character vector")
    stringr::str_to_upper(x) %>%
      stringr::str_detect(REGEX_NOT_DNA_IUPAC_ALPHABET, negate = !negate)
  }

#' Detect invalid IUPAC Amino Acid characters
#' @rdname check_validity
#' @export
is_valid_aa_character <-
  function(x, negate = FALSE) {
    if(!is.character(x)) stop("x must be a character vector")
    stringr::str_to_upper(x) %>%
      stringr::str_detect(REGEX_NOT_AA_ALPHABET, negate = !negate)
  }

