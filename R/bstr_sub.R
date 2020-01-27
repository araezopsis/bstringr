
#' Extract and replace subsequences from a bstr sequences
#' @inheritParams class_bstr
#' @param start start
#' @param end end
#' @param omit_na Single logical value. If `TRUE`, missing values in any of the
#'   arguments provided will result in an unchanged input.
#' @param value replacement string
#' @param replacement alias of \code{value} [wherever applicable]
#' @param ... arguments to be passed to \code{bstr_sub<-}
#' @export
#' @examples
#' temp <- bstr_rand_seq(1, 10, seed = 1)
#' c(
#'   temp,
#'   bstr_sub(temp, 1, 5),
#'   bstr_sub(temp, 5),
#'   bstr_sub(temp, end = 5),
#'   bstr_sub(temp, 1:2, c(6,2))
#' )
#'
#' # Negative indices
#' c(
#'   temp,
#'   bstr_sub(temp, -1),
#'   bstr_sub(temp, -7),
#'   bstr_sub(temp, end = -7)
#' )
#'
#' # Alternatively, you can pass in a two colum matrix, as in the
#' # output from bstr_locate
#' temp <- bstr("abcde fghij")
#' pos <- bstr_locate(temp, "[aefi]")[[1]]
#' bstr_sub(temp, pos)
#'
#' # Vectorisation
#' bstr_sub(temp, seq_len(bstr_length(temp)))
#' bstr_sub(temp, end = seq_len(bstr_length(temp)))
#'
#' # Replacement form
#' x <- "BBCDEF"
#' bstr_sub(x, 1, 1) <- "A"; x
#' bstr_sub(x, -1, -1) <- "K"; x
#' bstr_sub(x, -2, -2) <- "GHIJ"; x
#' bstr_sub(x, 2, -2) <- ""; x
#'
#' # If you want to keep the original if some argument is NA,
#' # use omit_na = TRUE
#' x1 <- x2 <- x3 <- x4 <- bstr("AAA")
#' bstr_sub(x1, 1, NA) <- "B"
#' bstr_sub(x2, 1, 2) <- NA
#' bstr_sub(x3, 1, NA, omit_na = TRUE) <- "B"
#' bstr_sub(x4, 1, 2, omit_na = TRUE) <- NA
#' c(x1, x2, x3, x4)
#'
#' @name sub
NULL

#' @rdname sub
#' @export
bstr_sub <-
  function(bstrobj, start = 1L, end = -1L) {
    bstrobj <- as_bstr(bstrobj)
    at <- attributes(bstrobj)

    bstrobj <- stringr::str_sub(bstrobj, start = start, end = end)

    attributes(bstrobj) <- at
    bstrobj
  }

#' @rdname sub
#' @export
"bstr_sub<-" <-
  function(bstrobj, start = 1L, end = -1L, omit_na = FALSE, value) {
    bstrobj <- as_bstr(bstrobj)
    at <- attributes(bstrobj)

    if (is.matrix(start)) {
      stringi::stri_sub(bstrobj, from = start, omit_na = omit_na) <- value
    } else {
      stringi::stri_sub(bstrobj, from = start, to = end, omit_na = omit_na) <- value
    }
    attributes(bstrobj) <- at
    bstrobj
  }

#' @rdname sub
#' @export
bstr_sub_replace <- function(..., replacement, value = replacement)
  `bstr_sub<-`(..., value=value)

#' Extract and replace all subsequences from a bstr sequences
#' @inheritParams class_bstr
#' @param from a list of integer vectors giving the start indexes or a
#'   list of two-column matrices, each of type \code{cbind(from, to)}
#' @param to a list of integer vectors giving the end indexes
#' @param length a list of integer vectors giving the substring lengths
#' @param omit_na a single logical value; indicates whether missing values
#'   in any of the indexes or in \code{value} leave the part of the
#'   corresponding input string
#'   unchanged [replacement function only]
#' @param replacement alias of \code{value} [wherever applicable]
#' @param value a list of character vectors defining the replacement strings
#'   [replacement function only]
#' @param ... arguments to be passed to \code{bstr_sub_all<-}
#' @export
#' @name sub_all
#' @rdname sub_all
#' @examples
#' (temp <- dstr_rand_seq(3, 20, "[AT]", seed = 1))
#'
#' (pos_A_trails <- stringr::str_locate_all(temp, "A{2,}"))
#'
#' # Extract all A trails
#' bstr_sub_all(temp, pos_A_trails)
#'
#' # Substitute all A trails by x
#' bstr_sub_all(temp, pos_A_trails) <- "x"
#' temp
#'
#' # Substitute all A trails by case switched ones
#' original <- change <- dstr_rand_seq(2, 10, "[AaT]", seed = 1)
#' pos_A_trails <- stringr::str_locate_all(change, "[Aa]{2,}")
#' switched_A_trails <-
#'   bstr_sub_all(change, pos_A_trails) %>%
#'   lapply(bstr_switch_case)
#' bstr_sub_all(change, pos_A_trails) <- switched_A_trails
#' c(original[1], change[1], original[2], change[2])
#'
bstr_sub_all <-
  function(bstrobj, from = list(1L), to = list(-1L), length) {
    bstrobj <- as_bstr(bstrobj)

    nm <- names(bstrobj)
    cls <- class(bstrobj)
    bstrobj <-
      stringi::stri_sub_all(str = bstrobj, from = from, to = to, length = length) %>%
      purrr::map2(nm, ~ bstr(.x, paste(.y, seq_along(.x))))
    for(i in bstrobj) class(i) <- cls

    names(bstrobj) <- nm
    bstrobj
  }

#' @rdname sub_all
#' @export
`bstr_sub_all<-` <-
  function(bstrobj, from=list(1L), to=list(-1L), length, omit_na=FALSE, value) {
    bstrobj <- as_bstr(bstrobj)
    at <- attributes(bstrobj)

    bstrobj <-
      stringi::`stri_sub_all<-`(str = bstrobj, from = from, to = to, length = length, omit_na = omit_na, value = value)

    attributes(bstrobj) <- at
    bstrobj
  }

#' @rdname sub_all
#' @export
bstr_sub_replace_all <- function(..., replacement, value = replacement)
  `bstr_sub_all<-`(..., value = value)

#' @rdname sub_all
#' @export
bstr_sub_all_replace <- bstr_sub_replace_all

