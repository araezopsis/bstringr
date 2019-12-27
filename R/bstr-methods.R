
#' bstr_sub_true
#' @inheritParams sub
#' @param i sequence index
#' @param gap_chr gap character
#' @export
bstr_sub_true <-
  function(bstrobj, i, start, end, gap_chr = "-") {
    bstrobj <- as_bstr(bstrobj)
    at <- attributes(bstrobj)

    tosub <- bstrobj[i]
    if(length(tosub) != 1) stop("index i is wrong.")

    tosub <-
      stringr::str_extract_all(tosub, ".")[[1]] %>%
      stringr::str_detect(paste0("[^", gap_chr, "]")) %>%
      cumsum

    if(missing(start)) start <- 1
    if(missing(end)) end <- max(tosub)

    bstrobj <-
      stringr::str_sub(
        bstrobj,
        start = which(tosub == start)[1],
        end = which(tosub == end)[1]
      )

    attributes(bstrobj) <- at
    bstrobj
  }

