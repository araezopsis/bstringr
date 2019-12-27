
#' Common bstr class augments
#' @param x A character vector which convert to a bstr object.
#' @param n A character vector which is name of x.
#' @param ucase A logical. If TRUE the x is converted to upper case. (default: FALSE)
#'
#' @param bstrobj bstr class object or character vector
#' @param dstrobj dstr class object or character vector
#' @param astrobj astr class object or character vector
#'
#' @param pattern regex pattern
#' @param case_sensitive sensitive to case in pattern (default:FALSE)
class_bstr_arg <- function(x, n, ucase, bstrobj, dstrobj, astrobj, pattern, case_sensitive){}

#' Constructer of the bstr class object
#' @inheritParams class_bstr_arg
#' @export
#' @examples
#' bstr("Apple", "apple")
#' bstr(c("Apple", "potato"), c("apple", "imo"), ucase = TRUE)
#'
bstr <-
  function(x, n, ucase = FALSE){
    if(!is.character(x)) stop("x must be a character vector.")

    if(missing(n)){
      n <- names(x)
      if(is.null(n)){
        n <- paste0(rep("no name ", length(x)), seq_along(x))
      }
    }else{
      if(length(x) != length(n)) stop("The length of x and n are different.")
    }

    if(ucase) x <- stringr::str_to_upper(x)

    names(x) <- n
    class(x) <- c("bstr", "character")
    x
  }

#' check class
#' @inheritParams class_bstr_arg
#' @export
#' @examples
#' is_bstr(bstr("apple"))
#' is_bstr(c("apple", "orange"))
#'
is_bstr <- function(x) inherits(x, "bstr")

#' Convert character vector to bstr class
#' @inheritParams class_bstr_arg
#' @export
#' @examples
#' as_bstr(c("apple", "egg"))
#' as_bstr("apple", "egg", TRUE)
#'
as_bstr <-
  function(x, n, ucase = FALSE){
    if(!is_bstr(x)){
      bstr(x, n, ucase)
    }else{
      x
    }
  }

#' Add object attribute
#' @param bstrobj bstring object
#' @param attr_name A character. attribute names
#' @param attrs attrs
#' @export
bstr_add_attr_for_object <-
  function(bstrobj, attr_name, attrs){
    bstrobj <- as_bstr(bstrobj)
    at <- attributes(bstrobj)

    if(!any(names(at) %in% "attr_obj")){
      at[["attr_obj"]] <- list()
    }

    at[["attr_obj"]][[attr_name]] <- attrs

    attributes(bstrobj) <- at
    return(bstrobj)
  }

#' Add sequence attribute
#' @param bstrobj bstring object
#' @param attr_name A character. attribute names
#' @param attrs attrs
#' @export
bstr_add_attr_for_seq <-
  function(bstrobj, attr_name, attrs){
    bstrobj <- as_bstr(bstrobj)
    at <- attributes(bstrobj)

    if(check_attr_seq(bstrobj, attrs)) stop()
    if(!any(names(at) %in% "attr_seq")) at[["attr_seq"]] <- list()

    at[["attr_seq"]][[attr_name]] <- attrs
    attributes(bstrobj) <- at
    return(bstrobj)
  }

check_attr_seq <-
  function(bstrobj, attrs){
    return(!any(
      (length(bstrobj) != length(attrs))
    ))
  }
