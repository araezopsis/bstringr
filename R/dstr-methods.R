
# MW = (numA * 313.2) + (numC * 289.2 ) + (numG * 329.2 )+ (numT * 304.2) - 61.9

#' Remove stop codon from DNA sequence
#' @importFrom stringr str_remove_all
#' @param dstrobj dstr class object or character vector
#' @param stop_codon stop codon pattern
#' @export
dstr_remove_stop <-
  function(dstrobj, stop_codon = "(?i)(TAA|TGA|TAG)$"){
    dstrobj <- as_dstr(dstrobj)
    at <- attributes(dstrobj)

    dstrobj <- bstr_remove(dstrobj, pattern = stop_codon)

    attributes(dstrobj) <- at
    dstrobj
  }

#' Return complement sequences
#' @param dstrobj dstr object
#' @rdname dstr_complement
#' @export
#' @examples
#' temp <- dstr_rand_seq(3, 10, "[atgcATGC]")
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
dstr_complement_fast <-
  function(dstrobj){
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
#' @param dstrobj dstr object
#' @rdname dstr_rev_comp
#' @export
#' @examples
#' temp <- dstr_rand_seq(3, 10, "[atgcATGC]")
#' c(temp, compl = dstr_rev_comp(temp))
#'
#' # Fast and memory efficient verstion of dstr_rev_comp with
#' # case conversion
#' c(temp, compl = dstr_rev_comp_fast(temp))
#'
dstr_rev_comp <-
  function(dstrobj){
    dstr_complement(dstrobj) %>% bstr_reverse()
  }

#' @rdname dstr_rev_comp
#' @export
dstr_rev_comp_fast <-
  function(dstrobj){
    dstr_complement_fast(dstrobj) %>% bstr_reverse()
  }


#' Translate dna -> protein
#' @importFrom stringr str_to_upper
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_c
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @param dstrobj sequence you want to translate
#' @export
dstr_translate <-
  function(dstrobj){
    dstrobj <- as_dstr(dstrobj) %>% bstr_to_upper()
    at <- attributes(dstrobj)

    pep <-
      str_extract_all(dstrobj, "...") %>%
      map(~ CODONS[.x]) %>%
      map_chr(~ str_c(.x, collapse = ""))
    if(any(is.na(pep))) warning("The sequence contained codon miss match.")

    attributes(pep) <- at
    as_astr(pep)
  }


#' Find open reading frames from DNA
#' @importFrom stringr str_locate_all
#' @importFrom stringr str_locate
#' @importFrom stringr str_sub
#' @importFrom stringr str_extract
#' @importFrom purrr map_chr
#' @importFrom purrr imap
#' @param dstrobj a sequence you want to find orfs
#' @param search_none_stop A logical. If TRUE, only search orfs with a stop codon. defalut is FALSE
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
      str_locate_all(dstrobj, "ATG(.{3})*?((.{1,2})$|TAG|TGA|TAA)") %>%
      map(~ .x[,1])

    orf_li <- list()
    for(i in seq_along(start_pos_li)){
      if(length(start_pos_li[[i]]) > 0){
        orf_end <-
          map_int(.x = start_pos_li[[i]],
                  .f = function(pos){
                    str_sub(dstrobj[i], pos) %>%
                      str_extract("ATG(.{3})*") %>%
                      str_locate(search_pattern) %>%
                      .[1L, 2L]
                  })

        # Exclude NA character from orf
        not_na_orf_end <- !is.na(orf_end)

        # Naming orf
        lorf <- length(orf_end)
        if(lorf > 1L){
          names(orf_end) <- str_c("ORF", seq_len(lorf))
        } else if(lorf == 1L){
          names(orf_end) <- "ORF"
        }

        orf_li[[i]] <-
          list(
            "seq_name" = n[i],
            "orf_name" = names(orf_end)[not_na_orf_end],
            "orf_start" = start_pos_li[[i]][not_na_orf_end],
            "orf_end" = start_pos_li[[i]][not_na_orf_end] + orf_end[not_na_orf_end] - 1L
          )
      }
    }
    return(orf_li)
  }


#############################################


#' convert IUPAC CODE to regular expression
#' @importFrom stringr str_replace_all
#' @param dstrobj A character
dstr_convert_iupac2regex <-
  function(dstrobj){
    dstrobj <- as_dstr(dstrobj)

    dstrobj %>%
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
#' @param dstrobj A primer sequence
#' @export
dstr_primer_check <-
  function(dstrobj){
    dstrobj <- as_dstr(dstrobj)

    result_list <- list()
    result_list$primer_seq <- dstrobj
    result_list$primer_length <- str_count(dstrobj)

    # GC%
    result_list$GC_total <- calc_GCper(dstrobj)
    half_pos <- (str_count(dstrobj) %/% 2)
    result_list$GC_firsthalf <- str_sub(dstrobj, 1, half_pos) %>% calc_GCper
    result_list$GC_secondhalf <- str_sub(dstrobj, half_pos + 1) %>% calc_GCper

    result_list$is_last_GorC <- str_extract(dstrobj, ".$") %>% str_detect("[GC]")

    result_list$Tm <- calc_oligoDNATm(dstrobj)

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

