
#' Constructer of the dstr class object
#' @inheritParams class_bstr_arg
#' @export
dstr <-
  function(x, n, ucase = F){
    d <- bstr(x, n, ucase)
    if(any(is_valid_dna_character(d, negate = TRUE)))
      stop("input contains invalid DNA character")
    class(d) <- c("dstr", class(d))
    d
  }

#' Detect invalid IUPAC DNA characters
#' @inheritParams class_bstr_arg
#' @param negate If TRUE, return non-matching elements.
#' @export
is_valid_dna_character <-
  function(x, negate = FALSE) {
    if(!is.character(x)) stop("x must be a character vector")
    stringr::str_to_upper(x) %>%
      stringr::str_detect(REGEX_NOT_DNA_IUPAC_ALPHABET, negate = !negate)
  }

#' check dstr class
#' @inheritParams class_bstr_arg
#' @export
is_dstr <- function(x) inherits(x, "dstr")

#' Convert character vector to dstr class
#' @inheritParams class_bstr_arg
#' @export
as_dstr <-
  function(x, n, ucase = F){
    if(!is_dstr(x)){
      return(dstr(x, n, ucase))
    }else{
      x
    }
  }
