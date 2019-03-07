
AA_ALPHABET <-
  c("A", "R", "N", "D", "C", "Q", "E", "G", "H", "I", "L", "K",
    "M", "F", "P", "S", "T", "W", "Y", "V", "X")

# AA_ALPHABET %>% stringr::str_c(collapse = "") %>% paste0("[", ., "]")
AA_ALPHABET_REGEX <-
  "(?i)[ARNDCQEGHILKMFPSTWYVX\\-\\+\\.\\*]"
AA_ALPHABET_REGEX_not <-
  "(?i)[^ARNDCQEGHILKMFPSTWYVX\\-\\+\\.\\*]"

#' Constructing astr class object
#' @importFrom stringr str_detect
#' @inheritParams class_bstr_arg
#' @export
astr <-
  function(x, n, ucase = F){
    a <- bstr(x, n, ucase)
    if(any(str_detect(x, AA_ALPHABET_REGEX_not)))
      stop("input contains invalid Amino Acid character")
    class(a) <- c("astr", class(a))
    a
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
