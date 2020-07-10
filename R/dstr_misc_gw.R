
add_i_mod <- function(x) paste0("(?i)", x)

#' TOPO reaction and LR reaction
#' @param insert insert sequence. without 5'CACC
#' @param pentr optional. pENTR D-TOPO sequence or the fasta file path
#' @param entry Entry clone
#' @param distination Distination vector
#' @param distination_to_lower change distination vector to lower case
#' @name gw
#' @examples
#' dstr_gw_topo("atgatgatgtga")
#'
#' attR1 <- "ACAAGTTTGTACAAAAAAGCTGAAC"
#' attR2 <- "GTTCAGCTTTCTTGTACAAAGTGGT"
#' A9 <- "AAAAAAAAA"
#' dummy <- paste0(A9, attR1, A9, attR2, A9, collapse = "")
#' dummy2 <- dstr_rev_comp(dummy)
#' dummy3 <- paste0(dummy, dummy2, collapse = "")
#'
#' test_distination <- dstr(c(dummy, dummy2, dummy3), paste0("dummy", 1:3))
#' test_distination
#'
#' dstr_gw_topo("atgtga") %>%
#'   dstr_gw_lr(test_distination) %>%
#'   write_fasta()
#'
#' dstr_gw_topo("atgtga") %>%
#'   dstr_gw_lr_both(test_distination) %>%
#'   write_fasta()
#'
NULL

#' @rdname gw
#' @export
get_pentr <- function(pentr) {
  if(missing(pentr)) {
    pentr_url <-
      "http://tools.thermofisher.com/content/sfs/vectors/pentr_dtopo_seq.txt"
    pentr <-
      readr::read_lines(pentr_url) %>%
      paste(collapse = "") %>%
      as_dstr("pENTR D-TOPO")
  } else {
    if(file.exists(pentr)) pentr <- read_fasta(pentr) %>% as_dstr()
    else pentr <- as_dstr(pentr)
  }
  pentr
}

#' @rdname gw
#' @export
dstr_gw_topo <- function(insert, pentr) {
  . <- NULL
  topo_3term <- "TCCGCGGCCGCCCCCTTCACC"
  topo_5term <- "AAGGGTGGGCGCGCCGAC"

  # Read pENTR D-TOPO sequence
  pentr <- get_pentr(pentr)
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

#' @rdname gw
#' @export
dstr_gw_lr <-
  function(entry, distination, distination_to_lower = TRUE){
    . <- NULL
    entry <- as_dstr(entry)
    ne <- names(entry)
    distination <- as_dstr(distination)
    nd <- names(distination)

    if(distination_to_lower) distination <- bstr_to_lower(distination)

    attR1 <- "ACAAGTTTGTACAAAAAAGCTGAAC" #attR1 for distination
    attR2 <- "GTTCAGCTTTCTTGTACAAAGTGGT" #attR2 for distination
    pattern_attR12 <- paste0(attR1, ".*?", attR2) %>% add_i_mod
    attL1 <- "CCAACTTTGTACAAAAAAGCAGGCT" #attL1 for entry
    attL2 <- "ACCCAGCTTTCTTGTACAAAGTTGG" #attL2 for entry
    pattern_attL12 <- paste0(attL1, ".*?", attL2) %>% add_i_mod
    attB1 <- "ACAAGTTTGTACAAAAAAGCAGGCT" #attB1 for product
    attB2 <- "ACCCAGCTTTCTTGTACAAAGTGGT" #attB2 for product

    loc_attR12 <- bstr_locate(distination, pattern_attR12)

    f <- function(x) {
      bstr_replace(
        bstrobj = x,
        pattern = pattern_attL12,
        replacement = . %>%
          bstr_replace(add_i_mod(attL1), attB1) %>%
          bstr_replace(add_i_mod(attL2), attB2)
      )
    }
    modified_entry <- entry %>% f()
    subseq_attB12 <-
      bstr_extract(modified_entry, add_i_mod(paste0(attB1, ".*?", attB2)))[[1]] %>%
      bstr_to_upper()

    bstr_sub_all(distination, loc_attR12, omit_na = TRUE) <- subseq_attB12
    distination
  }

#' @rdname gw
#' @export
dstr_gw_lr_both <- function(entry, distination){
  product <- dstr_gw_lr(entry, distination) %>% dstr_rev_comp()
  dstr_gw_lr(entry, product, distination_to_lower = FALSE) %>% dstr_rev_comp()
}

#' @rdname gw
#' @export
dstr_gw_topo_lr <-
  function(insert, distination, pentr){
    dstr_gw_lr(dstr_gw_topo(insert, pentr), distination)
  }

#' Find open reading frames from Gateway system compatible vector
#' @inheritParams class_bstr
#' @export
#' @examples
#' attR1 <- "ACAAGTTTGTACAAAAAAGCTGAAC"
#' attR2 <- "GTTCAGCTTTCTTGTACAAAGTGGT"
#' A9 <- "AAAAAAAAA"
#' dummy <- paste0(A9, attR1, A9, attR2, A9, collapse = "")
#' dummy2 <- dstr_rev_comp(dummy)
#' dummy3 <- paste0(dummy, dummy2, collapse = "")
#' test_distination <- dstr(c(dummy, dummy2, dummy3), paste0("dummy", 1:3))
#'
#' vectors <-
#'   dstr_gw_topo("atgtga") %>%
#'   dstr_gw_lr_both(test_distination)
#' vectors
#'
#' dstr_extract_orfs_from_vector(vectors)
#' dstr_translate_from_vector(vectors)
#' dstr_translate_from_vector(dstr_rev_comp_fast(vectors))
#'
dstr_extract_orfs_from_vector <- function(dstrobj) {
    . <- NULL
    dstrobj <- as_dstr(dstrobj)
    n <- names(dstrobj)

    attB1 <- "ACAAGTTTGTACAAAAAAGCAGGCT"
    attB2 <- "ACCCAGCTTTCTTGTACAAAGTGGT"

    subseq_attB12 <-
      bstr_extract(dstrobj, add_i_mod(paste0(attB1, ".*?", attB2))) %>%
      bstr_unlist(omit_na = FALSE) %>%
      bstr_sub(25L + 21L, -(25L + 17L))

    no_attB12 <- is.na(subseq_attB12)
    orf_li <- dstr_extract_orfs(dstrobj)
    main_orf <-
      purrr::map2(
        .x = orf_li[!no_attB12],
        .y = subseq_attB12[!no_attB12],
        .f = ~ .x[bstr_detect(.x, .y)]
      )
    main_orf %>% bstr_unlist()
  }

#' @rdname dstr_extract_orfs_from_vector
#' @export
dstr_find_orfs_from_vector <- dstr_extract_orfs_from_vector

#' @rdname dstr_extract_orfs_from_vector
#' @export
dstr_translate_from_vector <- function(dstrobj) {
    dstr_translate(dstr_find_orfs_from_vector(dstrobj))
  }


