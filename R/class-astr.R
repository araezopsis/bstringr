
#' Constructing astr class object
#' @inheritParams class_bstr_arg
#' @export
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
is_valid_aa_character <-
  function(x, negate = FALSE) {
    if(!is.character(x)) stop("x must be a character vector")
    stringr::str_to_upper(x) %>%
      stringr::str_detect(REGEX_NOT_AA_ALPHABET, negate = !negate)
  }


#' check class
#' @inheritParams class_bstr_arg
#' @export
is_astr <- function(x) inherits(x, "astr")

#' Convert character vector to dstr class
#' @inheritParams class_bstr_arg
#' @export
as_astr <-
  function(x, n, ucase = F){
    if(!is_astr(x)){
      return(astr(x, n))
    }else{
      x
    }
  }
