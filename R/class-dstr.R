
#' Constructer of the dstr class object
#' @inheritParams class_bstr_arg
#' @export
#' @examples
#' dstr("A.Bad.bat", "It is")
#' dstr(c("A", "bad", "Bat"), ucase = TRUE)
#'
dstr <-
  function(x, n, ucase = FALSE){
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

#' check dstr class
#' @inheritParams class_bstr_arg
#' @export
#' @examples
#' is_dstr(dstr("bad"))
#'
is_dstr <- function(x) inherits(x, "dstr")

#' Convert character vector to dstr class
#' @inheritParams class_bstr_arg
#' @export
#' @examples
#' as_dstr("bad", "good", ucase = TRUE)
#' # as_dstr(c("good", "bad")) # Error
#'
as_dstr <-
  function(x, n, ucase = FALSE){
    if(!is_dstr(x)){
      return(dstr(x, n, ucase))
    }else{
      x
    }
  }
