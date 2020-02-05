
#' calculate molecular weight
#' @inheritParams class_bstr
#' @name calc_mw
#' @examples
#'
#' (test <- dstr_rand_seq(3, 20, seed = 1))
#' calc_mw_dsDNA(test)
#' calc_mw_ssRNA(test)
#'
#' (res <- pstr_rand_seq(3, 100, seed = 1) %>% calc_mw_peptide())
#' barplot(percent ~ name + aa, legend.text = unique(res$name),
#'   beside = TRUE, data = res)
#'
NULL

#' Calculate double strand DNA molecular weight
#' @rdname calc_mw
#' @export
calc_mw_dsDNA <-
  function(dstrobj){
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
calc_mw_ssRNA <-
  function(dstrobj){
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
calc_mw_peptide <-
  function(pstrobj){
    . <- len <- name <- count <- aa <- aa_mw <- aa_mw_sum <- NULL
    pstrobj <- as_bstr(pstrobj)
    purrr::map(names(AMINO_ACID), ~ stringr::str_count(pstrobj, .x)) %>%
      purrr::set_names(names(AMINO_ACID)) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        len = rowSums(.),
        name = names(pstrobj)
      ) %>%
      tidyr::pivot_longer(
        cols = -c(len:name),
        names_to = "aa", values_to = "count"
      ) %>%
      dplyr::mutate(
        percent = count / len * 100,
        aa_mw = AA_WEIGHT[aa],
        aa_mw_sum = count * aa_mw
      ) %>%
      dplyr::group_by(name) %>%
      dplyr::mutate(pep_mw = sum(aa_mw_sum)) %>%
      dplyr::ungroup()
  }
