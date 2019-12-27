
#' Constructing astr class object
#' @inheritParams class_bstr_arg
#' @export
#' @examples
#' astr("Wqrld", "HELLQ", ucase = TRUE)
#'
astr <-
  function(x, n, ucase = F){
    a <- bstr(x, n, ucase)
    if(any(is_valid_aa_character(a, negate = TRUE)))
      stop("input contains invalid Amino Acid character")
    class(a) <- c("astr", class(a))
    a
  }

#' Detect invalid IUPAC Amino Acid characters
#' @inheritParams class_bstr_arg
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

#' check class
#' @inheritParams class_bstr_arg
#' @export
#' @examples
#' is_astr(astr("I.am.a.geek"))
#'
is_astr <- function(x) inherits(x, "astr")

#' Convert character vector to astr class
#' @inheritParams class_bstr_arg
#' @export
#' @examples
#' as_astr("Wqrld", "HELLQ", ucase = TRUE)
#'
as_astr <-
  function(x, n, ucase = FALSE){
    if(!is_astr(x)){
      return(astr(x, n, ucase))
    }else{
      x
    }
  }
