
subset_at <- function(at, ...) {
  n <- at[["names"]]
  if(any(names(at) %in% "attr_seq")) {
    seq_at <- at[["attr_seq"]]
    for(i in seq_along(seq_at)) {
      names(seq_at[[i]]) <- n
    }
    at[["attr_seq"]] <- lapply(seq_at, function(x) x[...])
  }
  names(n) <- n
  at[["names"]] <- n[...]
  names(at[["names"]]) <- NULL
  at
}

#' subsetting bstr class object
#' @param x x
#' @param ... ...
#' @param drop drop
#' @export
#' @examples
#' (test <- dstr_rand_seq(3, 10, seed = 1))
#' test[1]
#' test[2:1]
#'
#' test[2] <- "mutated"
#' test
#'
"[.bstr" <- function(x, ..., drop = F) {
  y <- NextMethod("[", "character")
  at <- attributes(x)
  attributes(y) <- subset_at(at, ...)
  y
}

#' Combine Values into a bstr class object
#' @param ... ...
#' @export
#' @examples
#' c(dstr_rand_seq(2, 5, seed = 1), dstr_rand_seq(3, 5, seed = 2))
#' c(dstr_rand_seq(3, 5, seed = 2), dstr_rand_seq(2, 5, seed = 1))
#'
#' c("hoge", dstr_rand_seq(2, 5, seed = 1))
#' # c(dstr_rand_seq(3, 5, seed = 2), "hoge") # Error
#' c(dstr_rand_seq(3, 5, seed = 2), as_bstr("hoge"))
#'
"c.bstr" <- function(...) {
  if(all(unlist(lapply(list(...), is_bstr)))) {
    y <- c(unlist(lapply(list(...), unclass)))
    y <- as_bstr(y)
  } else {
    stop("input contains not bstr class object")
  }
}

#' sort bstr
#' @param x x
#' @param decreasing logical. FALSE
#' @param ... ...
#' @param by sort bstr object by c("names", "length")
#' @export
#' @examples
#' test <- bstr_rand_seq(6, c(10, 5, 10, 12, 13, 3), seed = 1)
#' test
#' sort(test, decreasing = TRUE)
#' sort(test, by = "length")
#'
#' names(test) <- c("test1", "test10", "test10", "test20", "test3", "test100")
#' sort(test)
#'
#' bstr_sort(test)
#' bstr_sort_subname(test, "test.")
#' bstr_sort_subname(test, start = 1, end = 4)
#'
sort.bstr <- function(x, decreasing = FALSE, ..., by = c("names", "length")) {
  by <- match.arg(by)
  xc <- class(x)
  class(x) <- "character"

  if(by == "names") {
    o <- order(names(x), decreasing = decreasing)
  } else {
    o <- order(nchar(x), decreasing = decreasing)
  }
  x <- x[o]
  class(x) <- xc
  x
}

#' @inheritParams class_bstr
#' @name sort.bstr
#' @param decreasing A boolean. If FALSE, the default,
#' sorts from lowest to highest; if TRUE sorts from highest to lowest.
#' @param na_last Where should NA go? TRUE at the end,
#' FALSE at the beginning, NA dropped.
#' @param numeric If TRUE, will sort digits numerically, instead of as strings.
#' @export
bstr_sort <- function(bstrobj, by = c("names", "length"),
                      decreasing = FALSE, na_last = TRUE, numeric = TRUE) {
  by <- match.arg(by)
  bstrobj <- as_bstr(bstrobj)
  if(by == "names") {
    o <- stringr::str_order(x = names(bstrobj), decreasing = decreasing,
                            na_last = na_last, numeric = numeric)
  } else {
    o <- nchar(bstrobj) %>% order
  }
  bstrobj[o]
}

#' @inheritParams class_bstr
#' @param pattern regular expression to extract substrings of the name to sort
#' @param start start position to extract substrings of the name
#' @param end end position to extract substrings of the name
#' @name sort.bstr
#' @export
bstr_sort_subname <- function(bstrobj, pattern, start, end, decreasing = FALSE,
                              na_last = TRUE, numeric = TRUE) {
  bstrobj <- as_bstr(bstrobj)
  n <- names(bstrobj)
  if(!missing(pattern)) {
    sub_n <- stringr::str_extract(n, pattern)
  } else {
    sub_n <- stringr::str_sub(n, start, end)
  }
  bstrobj[stringr::str_order(x = sub_n, decreasing = decreasing,
                             na_last = na_last, numeric = numeric)]
}

#' Convert sequence to Biostrings::*StringSet object
#' @importFrom Biostrings BStringSet
#' @importFrom Biostrings DNAStringSet
#' @importFrom Biostrings AAStringSet
#' @param x sequence
#' @export
bstr2BioString <- function(x) {
  x <- as_bstr(x)
  x_class <- class(x)
  class(x) <- "character"

  x <-
    switch(x_class[1],
           "bstr" = BStringSet(x, use.names = T),
           "dstr" = DNAStringSet(x, use.names = T),
           "astr" = AAStringSet(x, use.names = T)
    )
  x
}

#' Convert sequence to Biostrings::*StringSet object
#' @param x sequence
#' @export
Bio2bstr <- function(x) {
  bstr(as.character(x), names(x), ucase = F)
}
# Biostrings::DNAStringSet(c(hoge = "AGAG", hige = "atg")) %>% Bio2bstr()

