
#' Apply function to both forward and reverse-complement sequences.
#' @inheritParams class_bstr
#' @param negate If TRUE, return non-matching elements.
#' @name dstr_both
#' @examples
#' temp <- c("aaagggcccc", "aaagggtttcccctttccc", "gggtttccc")
#' dstr_detect_both(temp, "aaa")
#' dstr_count_both(temp, "aaa")
#' dstr_locate_both(temp, "aaa")
#' dstr_locate_both(temp, "aaa") %>%
#'   lapply(. %>% bstr_sub_all(temp, .))
#'
NULL

#' @rdname dstr_both
#' @export
dstr_detect_both <- function(dstrobj, pattern, negate = FALSE) {
  dstrobj <- as_dstr(dstrobj)
  list(
    fwd = stringr::str_detect(dstrobj, pattern, negate),
    rc = stringr::str_detect(dstr_rev_comp(dstrobj), pattern, negate)
  )
}

#' @rdname dstr_both
#' @export
dstr_count_both <- function(dstrobj, pattern) {
  dstrobj <- as_dstr(dstrobj)
  list(
    fwd = stringr::str_count(dstrobj, pattern),
    rc = stringr::str_count(dstr_rev_comp(dstrobj), pattern)
  )
}

#' @rdname dstr_both
#' @export
dstr_locate_both <- function(dstrobj, pattern) {
  dstrobj <- as_dstr(dstrobj)
  rc_dstrobj <- dstr_rev_comp(dstrobj)
  n <- names(dstrobj)
  li <-
    list(
      # fwd = stringi::stri_locate_all_regex(dstrobj, pattern),
      # rc = stringi::stri_locate_all_regex(rc_dstrobj, pattern),
      fwd = bstr_locate(dstrobj, pattern),
      rc = bstr_locate(rc_dstrobj, pattern),
      len = bstr_length(dstrobj)
    )

  empty_mat <- as.matrix(data.frame(start = NA_integer_, end = NA_integer_))
  li$comp <-
    li$rc %>%
    purrr::map(~ data.frame(start = .x[,2], end = .x[,1])) %>%
    purrr::map(as.matrix, rownames.force = FALSE) %>%
    purrr::map2(li$len, ~ .y - .x + 1L)
  li$rc <- li$len <- NULL
  li
}


