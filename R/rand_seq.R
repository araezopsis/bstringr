
#' Generate random bstr sequences of desired lengths.
#' @param n single integer, nubmer of sequences
#' @param length integer vector, desired sequence lengths
#' @param pattern character vector specifying character classes
#' @param seed a single integer. specify seeds.
#' @export
#' @examples
#' bstr_rand_seq(3, 20, seed = 1)
#' bstr_rand_seq(3, 5:7, c("[a-c]", "[A-C]", "[1-3]"), seed = 1)
#'
bstr_rand_seq <- function(n, length, pattern = "[A-Za-z]", seed) {
  if(!missing(seed)) {
    set.seed(seed)
  }
  stringi::stri_rand_strings(n = n, length = length, pattern = pattern) %>%
    as_bstr()
}

#' Generate random dstr sequences of desired lengths.
#' @inheritParams bstr_rand_seq
#' @export
#' @examples
#' dstr_rand_seq(3, 20, seed = 1)
#' dstr_rand_seq(3, 5:7, c("[atcg]", "[ATCG]", "[N.]"), seed = 1)
#'
dstr_rand_seq <-
  function(n, length, pattern = "[ATGC]", seed) {
    if(!missing(seed)) {
      set.seed(seed)
    }
    stringi::stri_rand_strings(n = n, length = length, pattern = pattern) %>%
      as_dstr()
  }

#' Generate random astr sequences of desired lengths.
#' @inheritParams bstr_rand_seq
#' @export
#' @examples
#' astr_rand_seq(3, 20, seed = 1)
#' astr_rand_seq(3, 5:7, c("[arnd]", "[ARND]", "[X.]"), seed = 1)
#'
astr_rand_seq <-
  function(n, length, pattern = "[ARNDCQEGHILKMFPSTWYVX]", seed) {
    if(!missing(seed)) {
      set.seed(seed)
    }
    stringi::stri_rand_strings(n = n, length = length, pattern = pattern) %>%
      as_astr()
  }

