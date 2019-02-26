
DNA_ALPHABET <-
  c("A", "C", "G", "T", "M", "R", "W", "S", "Y", "K", "V", "H",
    "D", "B", "N", "-", "+", ".")

# DNA_ALPHABET %>% stringr::str_c(collapse = "") %>% paste0("[", ., "]")
DNA_ALPHABET_REGEX <-
  "(?i)[ACGTMRWSYKVHDBN\\-\\+\\.]"
DNA_ALPHABET_REGEX_not <-
  "(?i)[^ACGTMRWSYKVHDBN\\-\\+\\.]"

#' Constructer of the dstr class object
#' @importFrom stringr str_detect
#' @importFrom stringr str_to_upper
#' @inheritParams class_bstr_arg
#' @export
dstr <-
  function(x, n, ucase = F){
    d <- bstr(x, n, ucase)
    if(any(str_detect(str_to_upper(d), DNA_ALPHABET_REGEX_not)))
      stop("input contains invalid DNA character")
    class(d) <- c("dstr", class(d))
    d
  }

#' check class
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
