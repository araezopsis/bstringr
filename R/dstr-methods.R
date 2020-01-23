
# MW = (numA * 313.2) + (numC * 289.2 ) + (numG * 329.2 )+ (numT * 304.2) - 61.9

#' Remove stop codon from DNA sequence
#' @param dstrobj dstr class object or character vector
#' @param stop_codon regular expression pattern of stop codon
#' @export
#' @examples
#' "ATGgtatAG" %>% {c(., dstr_remove_stop(.))} %>% bstr
#'
dstr_remove_stop <-
  function(dstrobj, stop_codon = "(TAA|TGA|TAG)$"){
    dstrobj <- as_dstr(dstrobj)
    at <- attributes(dstrobj)

    loc <- bstr_to_upper(dstrobj) %>% bstr_locate(stop_codon)
    bstr_sub_all(dstrobj, loc) <- ""

    attributes(dstrobj) <- at
    dstrobj
  }



#' Find open reading frames from DNA
#' @param dstrobj a sequence you want to find orfs
#' @param search_none_stop A logical. If TRUE, only search orfs with
#'   a stop codon. defalut is FALSE
#' @export
dstr_find_orf <-
  function(dstrobj, search_none_stop = T){
    . <- NULL
    dstrobj <- as_dstr(dstrobj)
    n <- names(dstrobj)

    search_pattern <-
      ifelse(
        search_none_stop,
        "ATG(.{3})*?($|TAG|TGA|TAA)",
        "ATG(.{3})*?(TAG|TGA|TAA)"
      )

    start_pos_li <-
      stringr::str_locate_all(dstrobj, "ATG(.{3})*?((.{1,2})$|TAG|TGA|TAA)") %>%
      purrr::map(~ .x[,1])

    orf_li <- list()
    for(i in seq_along(start_pos_li)){
      if(length(start_pos_li[[i]]) > 0){
        orf_end <-
          purrr::map_int(.x = start_pos_li[[i]],
                         .f = function(pos){
                           stringr::str_sub(dstrobj[i], pos) %>%
                             stringr::str_extract("ATG(.{3})*") %>%
                             stringr::str_locate(search_pattern) %>%
                             .[1L, 2L]
                         })

        # Exclude NA character from orf
        not_na_orf_end <- !is.na(orf_end)

        # Naming orf
        lorf <- length(orf_end)
        if(lorf > 1L){
          names(orf_end) <- stringr::str_c("ORF", seq_len(lorf))
        } else if(lorf == 1L){
          names(orf_end) <- "ORF"
        }

        orf_li[[i]] <-
          list(
            "seq_name" = n[i],
            "orf_name" = names(orf_end)[not_na_orf_end],
            "orf_start" = start_pos_li[[i]][not_na_orf_end],
            "orf_end" = start_pos_li[[i]][not_na_orf_end] +
              orf_end[not_na_orf_end] - 1L
          )
      }
    }
    return(orf_li)
  }


#############################################


#' Convert IUPAC CODE to regular expression
#' @param dstrobj A character
#' @export
#' @examples
#' dstr_iupac2regex("MRWSYKVHDBNNN")
#'
dstr_iupac2regex <-
  function(dstrobj){
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
#' @param dstrobj A primer sequence
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


#' Calculate PCR product length
#' @param template template DNA sequence
#' @param primerF left primer sequence
#' @param primerR right primer sequence
#' @param FRC logical. TRUE when reverse complemented primer F. default is FALSE.
#' @param RRC logical. TRUE when reverse complemented primer R. default is TRUE.
#' @export
#' @examples
#' dstr_pcr("ACAATGTGTGTATGATGGTAGTAGAC", "ATGTG", "TACTA")
#'
dstr_pcr <-
  function(template, primerF, primerR, FRC = FALSE, RRC = TRUE){
    . <- NULL
    template <- as_dstr(template, "template")
    primerF <- as_dstr(primerF, "primerF")
    primerR <- as_dstr(primerR, "primerR")

    alignF <- dstr_align(template, primerF, rc = FRC)
    alignR <- dstr_align(template, primerR, rc = RRC)

    sub_start <- . %>% Biostrings::subject() %>% Biostrings::start()
    sub_end <- . %>% Biostrings::subject() %>% Biostrings::end()

    primerF_match_start <- sub_start(alignF)
    primerR_match_end <- sub_end(alignR)
    product <- bstr_sub(template, primerF_match_start, primerR_match_end)

    if(FRC) primerF <- dstr_rev_comp(primerF) %>% dstr("primerF RC")
    if(RRC) primerR <- dstr_rev_comp(primerR) %>% dstr("primerR RC")

    c(template, primerF, primerR, dstr(product, "product"))
  }


