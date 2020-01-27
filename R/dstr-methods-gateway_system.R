

#' Return the product sequence of after TOPO reaction
#' @param insert insert sequence. without 5'CACC
#' @param pentr optional. pENTR D-TOPO sequence or the fasta file path
#' @export
#' @examples
#' dstr_gw_topo("atgatgatgtga")
#'
dstr_gw_topo <- function(insert, pentr) {
  . <- NULL
  topo_3term <- "TCCGCGGCCGCCCCCTTCACC"
  topo_5term <- "AAGGGTGGGCGCGCCGAC"

  # Read pENTR D-TOPO sequence
  if(missing(pentr)) {
    pentr_url <-
      "http://tools.thermofisher.com/content/sfs/vectors/pentr_dtopo_seq.txt"
    pentr <-
      readr::read_lines(pentr_url) %>%
      paste(collapse = "") %>%
      as_dstr()
  } else {
    if(file.exists(pentr)) pentr <- read_fasta(pentr) %>% as_dstr()
    else pentr <- as_dstr(pentr)
  }
  if(!all(bstr_detect(pentr, topo_3term) & bstr_detect(pentr, topo_5term)))
    stop("pentr does not have valid pENTR D-TOPO sequence.")

  insert <- as_dstr(insert)
  n <- names(insert)
  has_3_cacc <- dstr_rev_comp_fast(insert) %>% bstr_detect("^CACC")
  if(has_3_cacc) stop("3' terminal of insert sequence has 'CACC'.")

  loc_topo <- paste0(topo_3term, ".*?", topo_5term) %>% bstr_locate(pentr, .)
  pentr <- bstr_to_lower(pentr)
  bstr_sub_all(pentr, loc_topo) <-
    paste0(bstr_to_lower(topo_3term),
           bstr_to_upper(insert),
           bstr_to_lower(topo_5term))
  names(pentr) <- n
  pentr
}


#' Return product sequence after LR reaction
#' @param entry Entry clone
#' @param distination Distination vector
#' @export
dstr_gw_lr <-
  function(entry, distination){
    . <- NULL
    entry <- as_dstr(entry)
    ne <- names(entry)
    distination <- as_dstr(distination)
    nd <- names(distination)

    attR1 <- "(?i)ACAAGTTTGTACAAAAAAGCTGAAC" #attR1 for distination
    attR2 <- "(?i)GTTCAGCTTTCTTGTACAAAGTGGT" #attR2 for distination
    attL1  <- "(?i)CCAACTTTGTACAAAAAAGCAGGCT" #attL1 for entry
    attL2  <- "(?i)ACCCAGCTTTCTTGTACAAAGTTGG" #attL2 for entry

    if(any(!bstr_detect(distination, attR1)))
      stop("distination have no attR1 sequence.")
    if(any(!bstr_detect(distination, attR2)))
      stop("distination have no attR2 sequence.")
    if(any(!bstr_detect(entry, attL1)))
      stop("entry have no attL1 sequence.")
    if(any(!bstr_detect(entry, attL2)))
      stop("entry have no attL2 sequence.")

    recombinate <-
      paste0(
        #outer_left
        stringr::str_to_lower(
          stringr::str_extract(distination, paste0("^.*", attR1)) %>%
            {stringr::str_sub(., end = stringr::str_count(.) - 17)}
        ),
        #insert
        stringr::str_to_upper(
          stringr::str_extract(entry, paste0(attL1, ".*", attL2)) %>%
            {stringr::str_sub(., start = 9, end = stringr::str_count(.) - 15)}
        ),

        #outer_right
        stringr::str_to_lower(
          stringr::str_extract(distination, paste0(attR2, ".*$")) %>%
            stringr::str_sub(start = 11)
        )
      )
    recombinate %>% dstr(ne)
  }


#' TOPO and LR reaction
#' @inheritParams dstr_gw_topo
#' @inheritParams dstr_gw_lr
#' @export
dstr_gw_topo_lr <-
  function(insert, distination, pentr){
    dstr_gw_lr(dstr_gw_topo(insert, pentr), distination)
  }


#' Find a main open reading frame from Gataway system vector
#' @param x a vector sequence you want to find orfs
#' @export
dstr_find_orf_from_vector <-
  function(x){
    . <- NULL
    dstrobj <- as_dstr(x)
    n <- names(dstrobj)

    attB1 <- "ACAAGTTTGTACAAAAAAGCAGGCT"
    attB2 <- "ACCCAGCTTTCTTGTACAAAGTGGT"

    pos_start <-
      stringr::str_locate_all(dstrobj, attB1) %>%
      purrr::map(~ .x[,2])
    pos_end <-
      stringr::str_locate_all(dstrobj, attB2) %>%
      purrr::map(~ .x[,1])

    if(any(purrr::map_int(pos_start, length) != 1L))
      stop("Number of attB1 is wrong.")
    if(any(purrr::map_int(pos_end, length) != 1L))
      stop("Number of attB2 is wrong.")

    B1toB2 <-
      purrr::pmap_chr(
        list(dstrobj, pos_start, pos_end),
        ~ stringr::str_sub(..1, ..2 + 21, ..3 - 17)
      )
    if(any(purrr::map_int(B1toB2, nchar) == 0))
      stop("Inserted sequence of pENTR D-TOPO is missing.")

    orf_li <- dstr_find_orf(dstrobj)
    main_orf <-
      purrr::map2(orf_li, B1toB2,
                  ~ list(
                    "seq_name" = .x$seq_name,
                    "orf" = .x$orf[stringr::str_detect(.x$orf, .y)])) %>%
      purrr::map(~ dstr(sort(.x$orf, T, by = "l")[1], paste(.x$seq_name, "ORF")))
    main_orf %>% unlist %>% dstr
  }


#' Find and translate ORF from expression vector
#' @param dna sequence you want to translate
#' @export
dstr_translate_from_vector <-
  function(dna){
    dstr_translate(dstr_find_orf_from_vector(dna))
  }
