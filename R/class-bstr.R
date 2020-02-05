
#' Common bstr class augments
#' @param x A character vector which convert to a bstr object.
#' @param n A character vector which is name of x.
#' @param ucase A logical. If TRUE the x is converted to upper case. (default: FALSE)
#'
#' @param bstrobj bstr class object or character vector
#' @param dstrobj dstr class object or character vector
#' @param pstrobj pstr class object or character vector
#'
#' @param pattern regex pattern
#' @param case_sensitive sensitive to case in pattern (default:FALSE)
#' @name class_bstr
NULL


### bstr(), dstr(), pstr() ------------------------------------------------

#' Constructer of the bstr class object
#' @inheritParams class_bstr
#' @name construct_bstr
#' @export
#' @examples
#' bstr("Apple", "apple")
#' bstr(c("Apple", "potato"), c("apple", "imo"), ucase = TRUE)
#'
#' dstr("A.Bad.bat", "It is")
#' dstr(c("A", "bad", "Bat"), ucase = TRUE)
#'
#' pstr("Wqrld", "HELLQ", ucase = TRUE)
#'
#' ### Check class
#' is_bstr(bstr("apple"))
#' is_bstr(c("apple", "orange"))
#' is_dstr(dstr("bad"))
#' is_pstr(pstr("I.am.a.geek"))
#'
#' ### Convert character to bstr object
#' as_dstr("bad", "good", ucase = TRUE)
#' # as_dstr(c("good", "bad")) # Error
#' as_pstr("Wqrld", "HELLQ", ucase = TRUE)
#'
bstr <- function(x, n, ucase = FALSE) {
  if(!is.character(x)) stop("x must be a character vector.")

  if(missing(n)) {
    n <- names(x)
    if(is.null(n)) n <- paste0(rep("no name ", length(x)), seq_along(x))
  } else {
    if(length(x) != length(n)) stop("The length of x and n are different.")
  }

  if(ucase) x <- stringr::str_to_upper(x)

  names(x) <- n
  class(x) <- c("bstr", "character")
  x
}

#' @rdname construct_bstr
#' @export
dstr <-
  function(x, n, ucase = FALSE) {
    d <- bstr(x, n, ucase)
    if(any(is_valid_dna_character(d, negate = TRUE)))
      stop("input contains invalid DNA character")
    class(d) <- c("dstr", class(d))
    d
  }

#' @rdname construct_bstr
#' @export
pstr <-
  function(x, n, ucase = F) {
    a <- bstr(x, n, ucase)
    if(any(is_valid_aa_character(a, negate = TRUE)))
      stop("input contains invalid Amino Acid character")
    class(a) <- c("pstr", class(a))
    a
  }

### is_bstr(), is_dstr(), is_pstr() -----------------------------------------

#' @rdname construct_bstr
#' @export
is_bstr <- function(x) inherits(x, "bstr")

#' @rdname construct_bstr
#' @export
is_dstr <- function(x) inherits(x, "dstr")

#' @rdname construct_bstr
#' @export
is_pstr <- function(x) inherits(x, "pstr")

### as_bstr(), as_dstr(), as_pstr() -----------------------------------------

#' @rdname construct_bstr
#' @export
as_bstr <- function(x, n, ucase = FALSE) {
  if(!is_bstr(x)) {
    bstr(x, n, ucase)
  } else {
    x
  }
}

#' @rdname construct_bstr
#' @export
as_dstr <- function(x, n, ucase = FALSE) {
  if(!is_dstr(x)) {
    dstr(x, n, ucase)
  } else {
    x
  }
}

#' @rdname construct_bstr
#' @export
as_pstr <- function(x, n, ucase = FALSE) {
  if(!is_pstr(x)) {
    pstr(x, n, ucase)
  } else {
    x
  }
}

### Other ----------------------------------------------------------------

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
