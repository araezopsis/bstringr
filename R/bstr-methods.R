
#' bstr_sub
#' @importFrom stringr str_sub
#' @param bstrobj bstr class object or character vector
#' @param start start
#' @param end end
#' @export
bstr_sub <-
  function(bstrobj, start = 1L, end = -1L){
    bstrobj <- as_bstr(bstrobj)
    at <- attributes(bstrobj)

    bstrobj <- str_sub(bstrobj, start = start, end = end)

    bstrobj
  }

#' bstr_remove_all
#' @importFrom stringr str_remove_all
#' @param bstrobj bstr class object or character vector
#' @param pattern regex pattern
#' @export
bstr_remove_all <-
  function(bstrobj, pattern){
    bstrobj <- as_bstr(bstrobj)
    n <- names(bstrobj)
    stringr::str_remove_all(string = bstrobj, pattern = pattern) %>%
      bstr(n)
  }


#' Remove gap character
#' @importFrom stringr str_remove_all
#' @param bstrobj bstr class object or character vector
#' @param gap_chr a gap character
#' @export
bstr_degap <-
  function(bstrobj, gap_chr = "-"){
    bstr_remove_all(bstrobj = bstrobj, pattern = gap_chr)
  }


#' bstr_reverse
#' @importFrom stringi stri_reverse
#' @param bstrobj bstr class object or character vector
#' @export
bstr_reverse <-
  function(bstrobj){
    bstrobj <- as_bstr(bstrobj)
    at <- attributes(bstrobj)

    bstrobj <- stri_reverse(bstrobj)

    attributes(bstrobj) <- at
    bstrobj
  }
