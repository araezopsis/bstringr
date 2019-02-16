#' bstr_length
#' @param bstrobj bstr class object or character vector
#' @export
bstr_length <-
  function(bstrobj){
    nchar(x = bstrobj)
  }

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

    attributes(bstrobj) <- at
    bstrobj
  }

#' bstr_sub_true
#' @importFrom stringr str_sub
#' @inheritParams bstr_sub
#' @param i index
#' @param gap_chr gap character
#' @export
bstr_sub_true <-
  function(bstrobj, i, start, end, gap_chr = "-"){
    bstrobj <- as_bstr(bstrobj)
    at <- attributes(bstrobj)

    tosub <- bstrobj[i]
    if(length(tosub) != 1) stop("index i is wrong.")

    tosub <-
      str_extract_all(tosub, ".")[[1]] %>%
      str_detect(paste0("[^", gap_chr, "]")) %>%
      cumsum

    if(missing(start)) start <- 1
    if(missing(end)) end <- max(tosub)

    bstrobj <-
      str_sub(
        bstrobj,
        start = which(tosub == start)[1],
        end = which(tosub == end)[1]
      )

    attributes(bstrobj) <- at
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
    at <- attributes(bstrobj)

    bstrobj <- str_remove_all(string = bstrobj, pattern = pattern)

    attributes(bstrobj) <- at
    bstrobj
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
