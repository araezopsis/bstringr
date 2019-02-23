
#' bstr_length
#' @inheritParams class_bstr_arg
#' @export
bstr_length <- function(bstrobj) nchar(x = bstrobj)

#' bstr_sub
#' @importFrom stringr str_sub
#' @inheritParams class_bstr_arg
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
#' @param i sequence index
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



