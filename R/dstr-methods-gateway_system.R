
#' Return the product sequence of after TOPO reaction
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_to_lower
#' @importFrom stringr str_to_upper
#' @importFrom stringr str_c
#' @importFrom purrr pmap
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
      pmap(
        list(
          str_extract(entry, paste0("^.*", lrec_out)) %>% str_to_lower(),
          cds %>% str_to_upper(),
          str_extract(entry, paste0(rrec_out, ".*$")) %>% str_to_lower()
        ),
        ~ str_c(..1, ..2, ..3)
      )

    result_seq <- dstr(unlist(result_seq), n, ucase = F)
    class(result_seq) <- c("dstr", "bstr", "character")
    result_seq
  }


#' Return product sequence after LR reaction
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_replace
#' @importFrom stringr str_to_upper
#' @importFrom stringr str_to_lower
#' @importFrom stringr str_sub
#' @importFrom stringr str_count
#' @importFrom magrittr %>%
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

    if(any(!str_detect(distination, lrec_out))) stop("outer have no lrec_out sequence.")
    if(any(!str_detect(distination, rrec_out))) stop("outer have no rrec_out sequence.")
    if(any(!str_detect(entry, lrec_in))) stop("insert have no lrec_in sequence.")
    if(any(!str_detect(entry, rrec_in))) stop("insert have no rrec_in sequence.")

    recombinate <-
      paste0(
        #outer_left
        str_to_lower(
          str_extract(distination, paste0("^.*", lrec_out)) %>%
          {str_sub(., end = str_count(.) - 17)}
        ),
        #insert
        str_to_upper(
          str_extract(entry, paste0(lrec_in, ".*", rrec_in)) %>%
          {str_sub(., start = 9, end = str_count(.) - 15)}
        ),

        #outer_right
        str_to_lower(
          str_extract(distination, paste0(rrec_out, ".*$")) %>%
            str_sub(start = 11)
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
#' @importFrom stringr str_locate_all
#' @importFrom stringr str_sub
#' @importFrom stringr str_detect
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom purrr map_int
#' @importFrom purrr pmap_chr
#' @param x a vector sequence you want to find orfs
#' @export
dstr_find_orf_from_vector <-
  function(x){
    . <- NULL
    dstrobj <- as_dstr(x)
    n <- names(dstrobj)

    attB1 <- "ACAAGTTTGTACAAAAAAGCAGGCT"
    attB2 <- "ACCCAGCTTTCTTGTACAAAGTGGT"

    pos_start <- str_locate_all(dstrobj, attB1) %>% map(~ .x[,2])
    pos_end <- str_locate_all(dstrobj, attB2) %>% map(~ .x[,1])

    if(any(map_int(pos_start, length) != 1L)) stop("Number of attB1 is wrong.")
    if(any(map_int(pos_end, length) != 1L)) stop("Number of attB2 is wrong.")

    B1toB2 <-
      pmap_chr(
        list(dstrobj, pos_start, pos_end),
        ~ str_sub(..1, ..2 + 21, ..3 - 17)
      )
    if(any(map_int(B1toB2, nchar) == 0)) stop("Inserted sequence of pENTR D-TOPO is missing.")

    orf_li <- dstr_find_orf(dstrobj)
    main_orf <-
      map2(orf_li, B1toB2,
           ~ list(
             "seq_name" = .x$seq_name,
             "orf" = .x$orf[str_detect(.x$orf, .y)])) %>%
      map(~ dstr(sort(.x$orf, T, by = "l")[1], paste(.x$seq_name, "ORF")))
    main_orf %>% unlist %>% dstr
  }


#' Find and translate ORF from expression vector
#' @param dna sequence you want to translate
#' @export
dstr_translate_from_vector <-
  function(dna){
    dstr_translate(dstr_find_orf_from_vector(dna))
  }
