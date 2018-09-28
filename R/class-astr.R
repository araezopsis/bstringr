
AA_ALPHABET <-
  c("A", "R", "N", "D", "C", "Q", "E", "G", "H", "I", "L", "K",
    "M", "F", "P", "S", "T", "W", "Y", "V", "X")

# AA_ALPHABET %>% stringr::str_c(collapse = "") %>% paste0("[", ., "]")
AA_ALPHABET_REGEX <-
  "[ARNDCQEGHILKMFPSTWYVX\\-\\+\\.\\*]"
AA_ALPHABET_REGEX_not <-
  "[^ARNDCQEGHILKMFPSTWYVX\\-\\+\\.\\*]"

#' Constructing astr class object
#' @importFrom stringr str_detect
#' @importFrom stringr str_to_upper
#' @param x Character vector
#' @param n Character vector
#' @param ucase Character vector
#' @export
astr <-
  function(x, n, ucase = T){
    a <- bstr(x, n, ucase)
    if(any(str_detect(str_to_upper(a), AA_ALPHABET_REGEX_not)))
      stop("input contains NOT Amino Acid character")
    class(a) <- c("astr", class(a))
    a
  }

#' check class
#' @param x x
#' @export
is_astr <- function(x) inherits(x, "astr")

#' Convert character vector to dstr class
#' @inheritParams bstr
#' @export
as_astr <-
  function(x, n){
    if(!is_astr(x)){
      return(astr(x, n))
    }else{
      x
    }
  }
