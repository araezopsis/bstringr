
#' Translate dna -> protein
#' @param dstrobj sequence you want to translate
#' @export
#' @examples
#' "atgtga" %>% dstr_translate()
#'
dstr_translate <-
  function(dstrobj) {
    dstrobj <- as_dstr(dstrobj) %>% bstr_to_upper()
    at <- attributes(dstrobj)

    pep <-
      stringr::str_extract_all(dstrobj, "...") %>%
      purrr::map(~ CODONS[.x]) %>%
      purrr::map_chr(~ paste0(.x, collapse = ""))
    if(any(is.na(pep))) warning("The sequence contained codon miss match.")

    attributes(pep) <- at
    as_pstr(pep)
  }

# test_orf <-
#   Biostrings::GENETIC_CODE %>%
#   {tibble::tibble(aa = ., codon = names(.))} %>%
#   dplyr::group_by(aa) %>%
#   dplyr::sample_n(1) %>%
#   dplyr::pull(codon) %>%
#   paste(collapse = "")
# test_orf %>% dstr_translate()
# test_orf %>% bstr_sub(end = -2) %>% dstr_translate()

