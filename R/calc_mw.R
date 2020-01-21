
#' Calculate protein molecular weight
#' @param astrobj astr object.
#' @export
#' @examples
#' astr_rand_seq(3, 100, seed = 1) %>% calc_MW_peptide()
#'
calc_MW_peptide <-
  function(astrobj){
    . <- len <- name <- count <- aa <- aa_mw <- aa_mw_sum <- NULL
    astrobj <- as_bstr(astrobj)
    purrr::map(names(AMINO_ACID), ~ stringr::str_count(astrobj, .x)) %>%
      purrr::set_names(names(AMINO_ACID)) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        len = rowSums(.),
        name = names(astrobj)
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

#' Calculate double strand DNA molecular weight
#' @param dna dna sequence
#' @export
calc_MW_dsDNA <-
  function(dna){
    .data <- NULL
    dna <- as_dstr(dna)
    tibble::tibble(
      seq_name = names(dna),
      seq = as.character(dna),
      num_bases = nchar(dna),
      applox_MW = (.data$num_bases * 607.4) + 157.9
    )
  }

#' Calculate single strand RNA molecular weight
#' @param dna dna sequence
#' @export
calc_MW_ssRNA <-
  function(dna){
    .data <- NULL
    dna <- as_dstr(dna)
    tibble::tibble(
      seq_name = names(dna),
      seq = as.character(dna),
      num_bases = nchar(dna),
      applox_MW = (.data$num_bases * 320.5) + 159.0
    )
  }
