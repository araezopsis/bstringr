
# MW = (numA * 313.2) + (numC * 289.2 ) + (numG * 329.2 )+ (numT * 304.2) - 61.9

#' dstr_remove_all
#' @importFrom stringr str_remove_all
#' @param dstrobj dstr class object or character vector
#' @param pattern regex pattern
#' @export
dstr_remove_all <-
  function(dstrobj, pattern){
    dstrobj <- as_dstr(dstrobj)
    n <- names(dstrobj)
    stringr::str_remove_all(string = dstrobj, pattern = pattern) %>%
      dstr(n)
  }

#' Remove stop codon from DNA sequence
#' @importFrom stringr str_remove_all
#' @param dstrobj dstr class object or character vector
#' @param stop_codon stop codon pattern
#' @export
dstr_trim_stop <-
  function(dstrobj, stop_codon = "(TAA|TGA|TAG)$"){
    dstr_remove_all(dstrobj = dstrobj, pattern = stop_codon)
  }

#' Return the reverse complement sequence
#' @importFrom stringr str_to_lower
#' @importFrom stringr str_replace_all
#' @importFrom stringi stri_reverse
#' @param x x
#' @export
dstr_rc <-
  function(x){
    dstrobj <- as_dstr(x)
    n <- names(dstrobj)
    dstrobj %>%
      str_to_lower() %>%
      str_replace_all("t", "A") %>%
      str_replace_all("a", "T") %>%
      str_replace_all("c", "G") %>%
      str_replace_all("g", "C") %>%
      stri_reverse() %>%
      dstr(n)
  }

#' Translate dna -> protein
#' @importFrom stringr str_to_upper
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_c
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @param x sequence you want to translate
#' @export
dstr_translate <-
  function(x){
    dstrobj <- as_dstr(x)
    n <- names(dstrobj)
    pro <-
      str_extract_all(dstrobj, "...") %>%
      map(~ CODONS[.x]) %>%
      map_chr(~ str_c(.x, collapse = ""))

    if(any(is.na(pro))) warning("The sequence contained codon miss match.")
    bstr(pro, n)
  }


#' Find open reading frames from DNA
#' @importFrom stringr str_locate_all
#' @importFrom stringr str_sub
#' @importFrom stringr str_extract
#' @importFrom purrr map_chr
#' @importFrom purrr imap
#' @param x a sequence you want to find orfs
#' @param search_none_stop A logical. If TRUE, only search orfs with a stop codon. defalut is FALSE
#' @export
dstr_find_orf <-
  function(x, search_none_stop = F){
    . <- NULL
    dstrobj <- as_dstr(x)
    n <- names(dstrobj)

    search_pattern <-
      ifelse(
        search_none_stop,
        "ATG(.{3})*?($|TAG|TGA|TAA)",
        "ATG(.{3})*?(TAG|TGA|TAA)"
      )

    start_pos_li <-
      str_locate_all(dstrobj, "ATG") %>%
      map(~ .x[,1])

    orf_li <- list()
    for(i in seq_along(start_pos_li)){
      if(length(start_pos_li[[i]]) > 0){
        orf <-
          map_chr(.x = start_pos_li[[i]],
                  .f = function(pos){
                    str_sub(dstrobj[i], pos) %>%
                      str_extract("ATG(.{3})*") %>%
                      str_extract(search_pattern)
                  })

        # Exclude NA character from orf
        orf <- orf[!is.na(orf)]

        # Naming orf
        lorf <- length(orf)
        if(lorf > 1L){
          names(orf) <- str_c(" ORF", seq_len(lorf))
        } else if(lorf == 1L){
          names(orf) <- "ORF"
        }

        orf_li[[i]] <-
          list(
            "seq_name" = n[i],
            "orf" = dstr(orf)
          )
      }
    }
    return(orf_li)
  }


#############################################


#' convert IUPAC CODE to regular expression
#' @importFrom stringr str_replace_all
#' @param dna A character
dstr_convert_iupac2regex <-
  function(dna){
    dna <- as_dstr(dna)

    dna %>%
      str_replace_all("M", "[AC]") %>%
      str_replace_all("R", "[AG]") %>%
      str_replace_all("W", "[AT]") %>%
      str_replace_all("S", "[CG]") %>%
      str_replace_all("Y", "[CT]") %>%
      str_replace_all("K", "[GT]") %>%
      str_replace_all("V", "[ACG]") %>%
      str_replace_all("H", "[ACT]") %>%
      str_replace_all("D", "[AGT]") %>%
      str_replace_all("B", "[CGT]") %>%
      str_replace_all("N", "[ACGT]")
  }


#' Primer check
#' @importFrom stringr str_count
#' @importFrom stringr str_sub
#' @importFrom stringr str_extract
#' @importFrom stringr str_detect
#' @param dna A primer sequence
#' @export
dstr_primer_check <-
  function(dna){
    dna <- as_dstr(dna)

    result_list <- list()
    result_list$primer_seq <- dna
    result_list$primer_length <- str_count(dna)

    # GC%
    result_list$GC_total <- calc_GCper(dna)
    half_pos <- (str_count(dna) %/% 2)
    result_list$GC_firsthalf <-
      str_sub(dna, 1, half_pos) %>% calc_GCper
    result_list$GC_secondhalf <-
      str_sub(dna, half_pos + 1) %>% calc_GCper

    result_list$is_last_GorC <-
      str_extract(dna, ".$") %>% str_detect("[GC]")

    result_list$Tm <- calc_oligoDNATm(dna)

    data.frame(result_list, stringsAsFactors = F)
  }


#' Calculate PCR product length
#' @importFrom dplyr data_frame
#' @importFrom glue glue
#' @importFrom Biostrings nchar
#' @importFrom Biostrings pattern
#' @importFrom Biostrings subject
#' @importFrom Biostrings start
#' @importFrom Biostrings end
#' @param template template DNA sequence
#' @param primerF left primer sequence
#' @param primerR right primer sequence
#' @param FRC logical. TRUE when reverse complemented primer F. default is FALSE.
#' @param RRC logical. TRUE when reverse complemented primer R. default is TRUE.
#' @export
dstr_pcr <-
  function(template, primerF, primerR, FRC = F, RRC = T){
    template <- as_dstr(template)
    primerF <- as_dstr(primerF)
    primerR <- as_dstr(primerR)

    alignF <- dstr_align(template, primerF, rc = FRC)
    alignR <- dstr_align(template, primerR, rc = RRC)

    primerF_len <- nchar(primerF)
    primerF_match_start <- subject(alignF) %>% start
    primerF_match_end <- subject(alignF) %>% end

    primerR_match_start <- subject(alignR) %>% start
    primerR_match_end <- subject(alignR) %>% end
    product_size <- primerR_match_end - primerF_match_start + 1

    pcr_result <-
      data_frame(
      primerF_seq = primerF,
      primerF_Tm = calc_oligoDNATm(primerF),
      match_templateF = subject(alignF) %>% as.character(),
      match_primerF = Biostrings::pattern(alignF) %>% as.character(),

      primerR_seq = primerR,
      primerR_Tm = calc_oligoDNATm(primerR),
      match_templateR = subject(alignR) %>% as.character(),
      match_primerR = Biostrings::pattern(alignR) %>% as.character(),

      size = product_size
    )

    with(pcr_result,
         glue("
template: '{format_seq(template, 40)}'
template length: {nchar(template)} bp

primer F: '{primerF}', Tm = {primerF_Tm}
template length: {nchar(primerF)} bp
aligned template: '{match_templateF}'
aligned primer F: '{match_primerF}'

primer R: '{primerR}', Tm = {primerR_Tm}
template length: {nchar(primerR)} bp
aligned template: '{match_templateR}'
aligned primer R: '{match_primerR}'

product size: {product_size} bp
              ") %>% cat
    )

    invisible(pcr_result)
  }

# dstr_pcr("ATGTGTGTATGATGGTAGTA", "ATGTG", "TAGTA", RRC = F)

