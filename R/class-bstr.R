
#' Constructing bstr class object
#' @importFrom stringr str_to_upper
#' @param x Character vector
#' @param n Character vector
#' @param ucase Character vector
#' @export
bstr <-
  function(x, n, ucase = T){
    if(!is.character(x)) stop("input x is not character vector")

    if(missing(n)){
      n <- names(x)
      if(is.null(n)){
        n <- rep("No name sequence", length(x))
      }
    }

    if(ucase) x <- str_to_upper(x)

    names(x) <- n
    class(x) <- c("bstr", "character")
    x
  }

#' check class
#' @param x x
#' @export
is_bstr <- function(x) inherits(x, "bstr")

#' Convert character vector to biostring class
#' @inheritParams bstr
#' @export
as_bstr <-
  function(x, n){
    if(!is_bstr(x)){
      bstr(x, n)
    }else{
      x
    }
  }
