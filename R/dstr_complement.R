
#' Return complement sequences
#' @inheritParams class_bstr
#' @rdname dstr_complement
#' @export
#' @examples
#' temp <- dstr_rand_seq(3, 10, "[atgcATGC]", seed = 1)
#' c(temp, compl = dstr_complement(temp))
#'
#' # Fast and memory efficient verstion of dstr_complement with
#' # case conversion
#' c(temp, compl = dstr_complement_fast(temp))
#'
dstr_complement <- function(dstrobj) {
  dstrobj <- as_dstr(dstrobj)
  at <- attributes(dstrobj)

  pos_a <- stringr::str_locate_all(dstrobj, "a")
  pos_t <- stringr::str_locate_all(dstrobj, "t")
  pos_g <- stringr::str_locate_all(dstrobj, "g")
  pos_c <- stringr::str_locate_all(dstrobj, "c")
  pos_A <- stringr::str_locate_all(dstrobj, "A")
  pos_T <- stringr::str_locate_all(dstrobj, "T")
  pos_G <- stringr::str_locate_all(dstrobj, "G")
  pos_C <- stringr::str_locate_all(dstrobj, "C")

  bstr_sub_all(dstrobj, pos_a) <- "t"
  bstr_sub_all(dstrobj, pos_t) <- "a"
  bstr_sub_all(dstrobj, pos_g) <- "c"
  bstr_sub_all(dstrobj, pos_c) <- "g"
  bstr_sub_all(dstrobj, pos_A) <- "T"
  bstr_sub_all(dstrobj, pos_T) <- "A"
  bstr_sub_all(dstrobj, pos_G) <- "C"
  bstr_sub_all(dstrobj, pos_C) <- "G"

  attributes(dstrobj) <- at
  dstrobj
}

#' @rdname dstr_complement
#' @export
dstr_complement_fast <- function(dstrobj) {
  dstrobj <- as_dstr(dstrobj)
  at <- attributes(dstrobj)

  comp_map <- c("a" = "T", "t" = "A", "g" = "C", "c" = "G")
  dstrobj <-
    stringr::str_to_lower(dstrobj) %>%
    stringr::str_replace_all(comp_map) %>%
    stringr::str_to_upper()

  attributes(dstrobj) <- at
  dstrobj
}


# old_dstr_complement <-
#   function(dstrobj){
#     dstrobj <- as_dstr(dstrobj)
#     at <- attributes(dstrobj)
#
#     dstrobj_l <- dstrobj %>% stringr::str_extract_all(".")
#     lower_c <- dstrobj_l %>% purrr::map(~ stringr::str_detect(.x, "[[:lower:]]"))
#     upper_c <- dstrobj_l %>% purrr::map(~ stringr::str_detect(.x, "[[:upper:]]"))
#
#     comp_map <- c("a" = "T", "t" = "A", "g" = "C", "c" = "G")
#     dstrobj <-
#       stringr::str_to_lower(dstrobj) %>%
#       stringr::str_replace_all(comp_map)
#
#     dstrobj <-
#       dstrobj %>%
#       stringr::str_extract_all(".") %>%
#       purrr::map2(lower_c, ~ ifelse(.y, stringr::str_to_lower(.x), .x)) %>%
#       purrr::map2(upper_c, ~ ifelse(.y, stringr::str_to_upper(.x), .x)) %>%
#       purrr::map_chr(~ paste0(.x, collapse = ""))
#
#     attributes(dstrobj) <- at
#     dstrobj
#   }

# test <- dstr_rand_seq(100, 1000, "[atgcATGC]")
# bench::mark(
#   dstr_complement(test) %>% bstr_to_upper(),
#   old_dstr_complement(test) %>% bstr_to_upper(),
#   dstr_complement_fast(test) %>% bstr_to_upper()
# ) %>% dplyr::glimpse()

# test <- dstr_rand_seq(10, 10^7, "[atgcATGC]")
# pryr::object_size(test)
# bench::mark(
#   dstr_complement(test) %>% bstr_to_upper(),
#   dstr_complement_fast(test) %>% bstr_to_upper()
# ) %>% dplyr::glimpse()

# test2 <- bstr2BioString(test)
# dplyr::bind_rows(
#   bench::mark(dstr_complement_fast(test)),
#   bench::mark(Biostrings::complement(test2))
# ) %>% dplyr::glimpse()

#' Return reverse complement sequences
#' @inheritParams class_bstr
#' @rdname dstr_rev_comp
#' @export
#' @examples
#' temp <- dstr_rand_seq(3, 10, "[atgcATGC]", seed = 1)
#' c(temp, compl = dstr_rev_comp(temp))
#'
#' # Fast and memory efficient verstion of dstr_rev_comp with
#' # case conversion
#' c(temp, compl = dstr_rev_comp_fast(temp))
#'
dstr_rev_comp <- function(dstrobj) {
  dstr_complement(dstrobj) %>% bstr_reverse()
}

#' @rdname dstr_rev_comp
#' @export
dstr_rev_comp_fast <- function(dstrobj) {
  dstr_complement_fast(dstrobj) %>% bstr_reverse()
}


