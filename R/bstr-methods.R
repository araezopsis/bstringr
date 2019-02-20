
#' Common bstr class augments
#' @param bstrobj bstr class object or character vector
#' @param pattern regex pattern
class_bstr_arg <- function(bstrobj, pattern){}

#' bstr_length
#' @inheritParams class_bstr_arg
#' @export
bstr_length <- function(bstrobj) nchar(x = bstrobj)

#' bstr_to_lower
#' @importFrom stringr str_to_lower
#' @inheritParams class_bstr_arg
#' @export
bstr_to_lower <-
  function(bstrobj){
    bstrobj <- as_bstr(bstrobj)
    at <- attributes(bstrobj)

    bstrobj <- str_to_lower(bstrobj)

    attributes(bstrobj) <- at
    bstrobj
  }

#' bstr_to_upper
#' @importFrom stringr str_to_upper
#' @inheritParams class_bstr_arg
#' @export
bstr_to_upper <-
  function(bstrobj){
    bstrobj <- as_bstr(bstrobj)
    at <- attributes(bstrobj)

    bstrobj <- str_to_upper(bstrobj)

    attributes(bstrobj) <- at
    bstrobj
  }

#' bstr_reverse
#' @importFrom stringi stri_reverse
#' @inheritParams class_bstr_arg
#' @export
bstr_reverse <-
  function(bstrobj){
    bstrobj <- as_bstr(bstrobj)
    at <- attributes(bstrobj)

    bstrobj <- stri_reverse(bstrobj)

    attributes(bstrobj) <- at
    bstrobj
  }

#' bstr_remove
#' @importFrom stringr str_remove_all
#' @inheritParams class_bstr_arg
#' @export
bstr_remove <-
  function(bstrobj, pattern){
    bstrobj <- as_bstr(bstrobj)
    at <- attributes(bstrobj)

    bstrobj <- str_remove_all(string = bstrobj, pattern = pattern)

    attributes(bstrobj) <- at
    bstrobj
  }

#' bstr_remove_num
#' @inheritParams class_bstr_arg
#' @export
bstr_remove_num <-
  function(bstrobj){
    bstr_remove(bstrobj = bstrobj, pattern = "[[:digit:]]")
  }

#' bstr_remove_notalpha
#' @inheritParams class_bstr_arg
#' @export
bstr_remove_notalpha <-
  function(bstrobj){
    bstr_remove(bstrobj = bstrobj, pattern = "[^[:alpha:]]")
  }

#' Remove all gap character
#' @inheritParams class_bstr_arg
#' @param gap_chr a gap character
#' @export
bstr_degap <-
  function(bstrobj, gap_chr = "-"){
    bstr_remove(bstrobj = bstrobj, pattern = gap_chr)
  }

#' bstr_replace
#' @importFrom stringr str_replace_all
#' @inheritParams class_bstr_arg
#' @param replacement a character vector
#' @export
bstr_replace <-
  function(bstrobj, pattern, replacement){
    bstrobj <- as_bstr(bstrobj)
    at <- attributes(bstrobj)

    bstrobj <- str_replace_all(
      string = bstrobj,
      pattern = pattern,
      replacement = replacement
    )

    attributes(bstrobj) <- at
    bstrobj
  }


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



