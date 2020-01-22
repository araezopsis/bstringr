
#' Translate dna -> protein
#' @param dstrobj sequence you want to translate
#' @export
dstr_translate <-
  function(dstrobj){
    dstrobj <- as_dstr(dstrobj) %>% bstr_to_upper()
    at <- attributes(dstrobj)

    pep <-
      stringr::str_extract_all(dstrobj, "...") %>%
      purrr::map(~ CODONS[.x]) %>%
      purrr::map_chr(~ paste0(.x, collapse = ""))
    if(any(is.na(pep))) warning("The sequence contained codon miss match.")

    attributes(pep) <- at
    as_astr(pep)
  }

