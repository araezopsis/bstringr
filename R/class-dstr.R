
DNA_ALPHABET <-
  c("A", "C", "G", "T", "M", "R", "W", "S", "Y", "K", "V", "H",
    "D", "B", "N", "-", "+", ".")

# DNA_ALPHABET %>% stringr::str_c(collapse = "") %>% paste0("[", ., "]")
DNA_ALPHABET_REGEX <-
  "[ACGTMRWSYKVHDBN\\-\\+\\.]"
DNA_ALPHABET_REGEX_not <-
  "[^ACGTMRWSYKVHDBN\\-\\+\\.]"

#' Constructer of the dstr class object
#' @importFrom stringr str_detect
#' @importFrom stringr str_to_upper
#' @param x Character vector
#' @param n Character vector
#' @param ucase Character vector
#' @export
dstr <-
  function(x, n, ucase = T){
    d <- bstr(x, n, ucase)
    if(any(str_detect(str_to_upper(d), DNA_ALPHABET_REGEX_not)))
      stop("input contains NOT DNA character")
    class(d) <- c("dstr", class(d))
    d
  }

#' check class
#' @param x x
#' @export
is_dstr <- function(x) inherits(x, "dstr")

#' Convert character vector to dstr class
#' @inheritParams bstr
#' @export
as_dstr <-
  function(x, n){
    if(!is_dstr(x)){
      return(dstr(x, n))
    }else{
      x
    }
  }
