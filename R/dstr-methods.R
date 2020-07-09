
# MW = (numA * 313.2) + (numC * 289.2 ) + (numG * 329.2 )+ (numT * 304.2) - 61.9

#' Remove stop codon from DNA sequence
#' @inheritParams class_bstr
#' @param stop_codon regular expression pattern of stop codon
#' @export
#' @examples
#' dstr("ATGgtatAG") %>% {c(., dstr_remove_stop(.))}
#'
dstr_remove_stop <- function(dstrobj, stop_codon = "(TAA|TGA|TAG)$") {
  dstrobj <- as_dstr(dstrobj)
  at <- attributes(dstrobj)

  loc <- bstr_to_upper(dstrobj) %>% bstr_locate(stop_codon)
  bstr_sub_all(dstrobj, loc) <- ""

  attributes(dstrobj) <- at
  dstrobj
}


#' Convert IUPAC CODE to regular expression
#' @inheritParams class_bstr
#' @export
#' @examples
#' dstr_iupac2regex("MRWSYKVHDBNNN")
#'
dstr_iupac2regex <- function(dstrobj) {
  dstrobj <- as_dstr(dstrobj) %>% bstr_to_upper()

  dstrobj %>%
    stringr::str_replace_all("M", "[AC]") %>%
    stringr::str_replace_all("R", "[AG]") %>%
    stringr::str_replace_all("W", "[AT]") %>%
    stringr::str_replace_all("S", "[CG]") %>%
    stringr::str_replace_all("Y", "[CT]") %>%
    stringr::str_replace_all("K", "[GT]") %>%
    stringr::str_replace_all("V", "[ACG]") %>%
    stringr::str_replace_all("H", "[ACT]") %>%
    stringr::str_replace_all("D", "[AGT]") %>%
    stringr::str_replace_all("B", "[CGT]") %>%
    stringr::str_replace_all("N", "[ACGT]") %>%
    as.character()
}


#' Primer check
#' @inheritParams class_bstr
#' @export
dstr_primer_check <-
  function(dstrobj){
    dstrobj <- as_dstr(dstrobj)

    result_list <- list()
    result_list$primer_seq <- dstrobj
    result_list$primer_length <- stringr::str_count(dstrobj)

    # GC%
    result_list$GC_total <- calc_GCper(dstrobj)
    half_pos <- (stringr::str_count(dstrobj) %/% 2)
    result_list$GC_firsthalf <-
      stringr::str_sub(dstrobj, 1, half_pos) %>% calc_GCper
    result_list$GC_secondhalf <-
      stringr::str_sub(dstrobj, half_pos + 1) %>% calc_GCper

    result_list$is_last_GorC <-
      stringr::str_extract(dstrobj, ".$") %>% stringr::str_detect("[GC]")

    result_list$Tm <- calc_oligoDNATm(dstrobj)
    data.frame(result_list, stringsAsFactors = F)
  }



