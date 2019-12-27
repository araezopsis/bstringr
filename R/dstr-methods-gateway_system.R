
#' Return the product sequence of after TOPO reaction
#' @param cds coding sequence which you want to clone. without 5'CACC
#' @export
dstr_TOPO <-
  function(cds){
    entry <- "CTTTCCTGCGTTATCCCCTGATTCTGTGGATAACCGTATTACCGCCTTTGAGTGAGCTGATACCGCTCGCCGCAGCCGAACGACCGAGCGCAGCGAGTCAGTGAGCGAGGAAGCGGAAGAGCGCCCAATACGCAAACCGCCTCTCCCCGCGCGTTGGCCGATTCATTAATGCAGCTGGCACGACAGGTTTCCCGACTGGAAAGCGGGCAGTGAGCGCAACGCAATTAATACGCGTACCGCTAGCCAGGAAGAGTTTGTAGAAACGCAAAAAGGCCATCCGTCAGGATGGCCTTCTGCTTAGTTTGATGCCTGGCAGTTTATGGCGGGCGTCCTGCCCGCCACCCTCCGGGCCGTTGCTTCACAACGTTCAAATCCGCTCCCGGCGGATTTGTCCTACTCAGGAGAGCGTTCACCGACAAACAACAGATAAAACGAAAGGCCCAGTCTTCCGACTGAGCCTTTCGTTTTATTTGATGCCTGGCAGTTCCCTACTCTCGCGTTAACGCTAGCATGGATGTTTTCCCAGTCACGACGTTGTAAAACGACGGCCAGTCTTAAGCTCGGGCCCCAAATAATGATTTTATTTTGACTGATAGTGACCTGTTCGTTGCAACAAATTGATGAGCAATGCTTTTTTATAATGCCAACTTTGTACAAAAAAGCAGGCTCCGCGGCCGCCCCCTTCACCAAGGGTGGGCGCGCCGACCCAGCTTTCTTGTACAAAGTTGGCATTATAAGAAAGCATTGCTTATCAATTTGTTGCAACGAACAGGTCACTATCAGTCAAAATAAAATCATTATTTGCCATCCAGCTGATATCCCCTATAGTGAGTCGTATTACATGGTCATAGCTGTTTCCTGGCAGCTCTGGCCCGTGTCTCAAAATCTCTGATGTTACATTGCACAAGATAAAAATATATCATCATGAACAATAAAACTGTCTGCTTACATAAACAGTAATACAAGGGGTGTTATGAGCCATATTCAACGGGAAACGTCGAGGCCGCGATTAAATTCCAACATGGATGCTGATTTATATGGGTATAAATGGGCTCGCGATAATGTCGGGCAATCAGGTGCGACAATCTATCGCTTGTATGGGAAGCCCGATGCGCCAGAGTTGTTTCTGAAACATGGCAAAGGTAGCGTTGCCAATGATGTTACAGATGAGATGGTCAGACTAAACTGGCTGACGGAATTTATGCCTCTTCCGACCATCAAGCATTTTATCCGTACTCCTGATGATGCATGGTTACTCACCACTGCGATCCCCGGAAAAACAGCATTCCAGGTATTAGAAGAATATCCTGATTCAGGTGAAAATATTGTTGATGCGCTGGCAGTGTTCCTGCGCCGGTTGCATTCGATTCCTGTTTGTAATTGTCCTTTTAACAGCGATCGCGTATTTCGTCTCGCTCAGGCGCAATCACGAATGAATAACGGTTTGGTTGATGCGAGTGATTTTGATGACGAGCGTAATGGCTGGCCTGTTGAACAAGTCTGGAAAGAAATGCATAAACTTTTGCCATTCTCACCGGATTCAGTCGTCACTCATGGTGATTTCTCACTTGATAACCTTATTTTTGACGAGGGGAAATTAATAGGTTGTATTGATGTTGGACGAGTCGGAATCGCAGACCGATACCAGGATCTTGCCATCCTATGGAACTGCCTCGGTGAGTTTTCTCCTTCATTACAGAAACGGCTTTTTCAAAAATATGGTATTGATAATCCTGATATGAATAAATTGCAGTTTCATTTGATGCTCGATGAGTTTTTCTAATCAGAATTGGTTAATTGGTTGTAACACTGGCAGAGCATTACGCTGACTTGACGGGACGGCGCAAGCTCATGACCAAAATCCCTTAACGTGAGTTACGCGTCGTTCCACTGAGCGTCAGACCCCGTAGAAAAGATCAAAGGATCTTCTTGAGATCCTTTTTTTCTGCGCGTAATCTGCTGCTTGCAAACAAAAAAACCACCGCTACCAGCGGTGGTTTGTTTGCCGGATCAAGAGCTACCAACTCTTTTTCCGAAGGTAACTGGCTTCAGCAGAGCGCAGATACCAAATACTGTCCTTCTAGTGTAGCCGTAGTTAGGCCACCACTTCAAGAACTCTGTAGCACCGCCTACATACCTCGCTCTGCTAATCCTGTTACCAGTGGCTGCTGCCAGTGGCGATAAGTCGTGTCTTACCGGGTTGGACTCAAGACGATAGTTACCGGATAAGGCGCAGCGGTCGGGCTGAACGGGGGGTTCGTGCACACAGCCCAGCTTGGAGCGAACGACCTACACCGAACTGAGATACCTACAGCGTGAGCATTGAGAAAGCGCCACGCTTCCCGAAGGGAGAAAGGCGGACAGGTATCCGGTAAGCGGCAGGGTCGGAACAGGAGAGCGCACGAGGGAGCTTCCAGGGGGAAACGCCTGGTATCTTTATAGTCCTGTCGGGTTTCGCCACCTCTGACTTGAGCGTCGATTTTTGTGATGCTCGTCAGGGGGGCGGAGCCTATGGAAAAACGCCAGCAACGCGGCCTTTTTACGGTTCCTGGCCTTTTGCTGGCCTTTTGCTCACATGTT"
    entry <- as_dstr(entry, "pENTR D-TOPO")
    cds <- as_dstr(cds)
    n <- names(cds)

    lrec_out <- "TCCGCGGCCGCCCCCTTCACC"
    rrec_out <- "AAGGGTGGGCGCGCCGAC"

    result_seq <-
      purrr::pmap(
        list(
          stringr::str_extract(entry, paste0("^.*", lrec_out)) %>%
            stringr::str_to_lower(),
          cds %>% stringr::str_to_upper(),
          stringr::str_extract(entry, paste0(rrec_out, ".*$")) %>%
            stringr::str_to_lower()
        ),
        ~ stringr::str_c(..1, ..2, ..3)
      )

    result_seq <- dstr(unlist(result_seq), n, ucase = F)
    class(result_seq) <- c("dstr", "bstr", "character")
    result_seq
  }


#' Return product sequence after LR reaction
#' @param entry Entry clone
#' @param distination Distination vector
#' @export
dstr_LR <-
  function(entry, distination){
    . <- NULL
    entry <- as_dstr(entry)
    ne <- names(entry)
    distination <- as_dstr(distination)
    nd <- names(distination)

    lrec_out <- "(?i)ACAAGTTTGTACAAAAAAGCTGAAC" #attR1
    rrec_out <- "(?i)GTTCAGCTTTCTTGTACAAAGTGGT" #attR2
    lrec_in  <- "(?i)CCAACTTTGTACAAAAAAGCAGGCT" #attL1
    rrec_in  <- "(?i)ACCCAGCTTTCTTGTACAAAGTTGG" #attL2

    if(any(!stringr::str_detect(distination, lrec_out))) stop("outer have no lrec_out sequence.")
    if(any(!stringr::str_detect(distination, rrec_out))) stop("outer have no rrec_out sequence.")
    if(any(!stringr::str_detect(entry, lrec_in))) stop("insert have no lrec_in sequence.")
    if(any(!stringr::str_detect(entry, rrec_in))) stop("insert have no rrec_in sequence.")

    recombinate <-
      paste0(
        #outer_left
        stringr::str_to_lower(
          stringr::str_extract(distination, paste0("^.*", lrec_out)) %>%
          {stringr::str_sub(., end = stringr::str_count(.) - 17)}
        ),
        #insert
        stringr::str_to_upper(
          stringr::str_extract(entry, paste0(lrec_in, ".*", rrec_in)) %>%
          {stringr::str_sub(., start = 9, end = stringr::str_count(.) - 15)}
        ),

        #outer_right
        stringr::str_to_lower(
          stringr::str_extract(distination, paste0(rrec_out, ".*$")) %>%
            stringr::str_sub(start = 11)
        )
      )
    recombinate %>% dstr(ne)
  }


#' TOPO and LR reaction
#' @param cds coding sequence which you want to clone. without 5'CACC
#' @param distination Distination vector
#' @export
dstr_TOPO_LR <-
  function(cds, distination){
    dstr_LR(dstr_TOPO(cds), distination)
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
