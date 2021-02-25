
#' Return complement sequences
#' @inheritParams class_bstr
#' @rdname dstr_complement
#' @export
#' @examples
#' temp <- dstr_rand_seq(3, 10, "[atgcATGC]", seed = 1)
#' c(temp, compl = dstr_complement(temp))
#'
dstr_complement <- function(dstrobj) {
  chartr(old = "atgcATGC", "tacgTACG", as_dstr(dstrobj))
}

#' Return reverse complement sequences
#' @inheritParams class_bstr
#' @rdname dstr_rev_comp
#' @export
#' @examples
#' temp <- dstr_rand_seq(3, 10, "[atgcATGC]", seed = 1)
#' c(temp, compl = dstr_rev_comp(temp))
#'
dstr_rev_comp <- function(dstrobj) {
  dstr_complement(dstrobj) %>% bstr_reverse()
}
