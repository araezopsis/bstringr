
#' calculate molecular weight
#' @inheritParams class_bstr
#' @name calc_mw
#' @examples
#'
#' (test <- dstr_rand_seq(3, 20, seed = 1))
#' calc_mw_dsDNA(test)
#' calc_mw_ssRNA(test)
#'
#' (res <- pstr_rand_seq(3, 100, seed = 1)%>% calc_mw_peptide())
#' barplot(aa_percent ~ name + aa, legend.text = unique(res$name),
#'   beside = TRUE, data = res)
#'
NULL

#' Calculate double strand DNA molecular weight
#' @rdname calc_mw
#' @export
calc_mw_dsDNA <- function(dstrobj) {
  .data <- NULL
  dstrobj <- as_dstr(dstrobj)
  tibble::tibble(
    seq_name = names(dstrobj),
    seq = as.character(dstrobj),
    num_bases = nchar(dstrobj),
    applox_MW = (.data$num_bases * 607.4) + 157.9
  )
}

#' Calculate single strand RNA molecular weight
#' @rdname calc_mw
#' @export
calc_mw_ssRNA <- function(dstrobj) {
  .data <- NULL
  dstrobj <- as_dstr(dstrobj)
  tibble::tibble(
    seq_name = names(dstrobj),
    seq = as.character(dstrobj),
    num_bases = nchar(dstrobj),
    applox_MW = (.data$num_bases * 320.5) + 159.0
  )
}

#' Calculate peptide molecular weight
#' @rdname calc_mw
#' @export
calc_mw_peptide <- function(pstrobj) {
  residue <- aa_count <- pep_len <- pos <- h_index <- three_letter <-
    name <- aa_mw <- aa <- NULL
  pstrobj <- as_pstr(pstrobj)

  pstrobj %>%
    bstr2longdf() %>%
    dplyr::rename(aa = residue) %>%
    dplyr::left_join(AA_DATA, by = "aa") %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(
      pep_len = dplyr::n(),
      pep_mw = sum(aa_mw, na.rm = TRUE)) %>%
    dplyr::group_by(name, aa) %>%
    dplyr::mutate(
      aa_count = dplyr::n(),
      aa_percent = aa_count / pep_len * 100,
      aa_mw_sum = sum(aa_mw)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-pos, -h_index, -three_letter) %>%
    dplyr::distinct()
}
