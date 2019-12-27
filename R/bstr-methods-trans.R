#' Convert case of a bstring sequence.
#'
#' @inheritParams class_bstr_arg
#' @examples
#' temp <- dstr_rand_seq(1, 10, "[atgcATGC]")
#' c(
#'   temp,
#'   upper = bstr_to_upper(temp),
#'   lower = bstr_to_lower(temp),
#'   switch = bstr_switch_case(temp)
#' )
#'
#' @name case
NULL

#' @rdname case
#' @export
bstr_to_upper <-
  function(bstrobj) {
    bstrobj <- as_bstr(bstrobj)
    at <- attributes(bstrobj)

    bstrobj <- stringr::str_to_upper(bstrobj)

    attributes(bstrobj) <- at
    bstrobj
  }

#' @rdname case
#' @export
bstr_to_lower <-
  function(bstrobj) {
    bstrobj <- as_bstr(bstrobj)
    at <- attributes(bstrobj)

    bstrobj <- stringr::str_to_lower(bstrobj)

    attributes(bstrobj) <- at
    bstrobj
  }

#' @rdname case
#' @export
bstr_switch_case <-
  function(bstrobj) {
    bstrobj <- as_bstr(bstrobj)
    at <- attributes(bstrobj)

    bstrobj <- bstrobj %>% stringr::str_extract_all(".")
    lower_c <- bstrobj %>% purrr::map(~ stringr::str_detect(.x, "[[:lower:]]"))
    upper_c <- bstrobj %>% purrr::map(~ stringr::str_detect(.x, "[[:upper:]]"))

    bstrobj <-
      bstrobj %>%
      purrr::map2(lower_c, ~ ifelse(.y, stringr::str_to_upper(.x), .x)) %>%
      purrr::map2(upper_c, ~ ifelse(.y, stringr::str_to_lower(.x), .x)) %>%
      purrr::map_chr(~ paste0(.x, collapse = ""))

    attributes(bstrobj) <- at
    bstrobj
  }

#' Reverse bstr sequence
#' @inheritParams class_bstr_arg
#' @export
#' @examples
#' temp <- bstr_rand_seq(2, 6)
#' c(temp, reverse = bstr_reverse(temp))
#'
bstr_reverse <-
  function(bstrobj){
    bstrobj <- as_bstr(bstrobj)
    at <- attributes(bstrobj)

    bstrobj <- stringi::stri_reverse(bstrobj)

    attributes(bstrobj) <- at
    bstrobj
  }
