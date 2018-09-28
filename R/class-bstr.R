
#' Constructer of the bstr class object
#' @importFrom stringr str_to_upper
#' @param x A character vector which convert to a bstr object.
#' @param n A character vector which is name of x.
#' @param ucase A logical. If TRUE the x is converted to upper case. (default: TRUE)
#' @export
bstr <-
  function(x, n, ucase = T){
    if(!is.character(x)) stop("x must be a character vector.")

    if(missing(n)){
      n <- names(x)
      if(is.null(n)){
        n <- paste0(rep("no name ", length(x)), seq_along(x))
      }
    }else{
      if(length(x) != length(n)) stop("The length of x and n are different.")
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
