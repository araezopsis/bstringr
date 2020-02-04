
pairwise_alignment <- function(sub, pat, ...) {
  Biostrings::pairwiseAlignment(subject = sub, pattern = pat, ...)
}

extract_aligned_sub <- function(align) {
  x <- Biostrings::alignedSubject(align) %>% as.character()
  bstr(x, n = paste0(seq_along(x), " subject"))
}

extract_aligned_pat <- function(align) {
  x <- Biostrings::alignedPattern(align) %>% as.character()
  bstr(x, n = paste0(seq_along(x), " pattern"))
}

#' Align bstr sequence
#' @param sub a subject sequence
#' @param pat pattern sequence
#' @param type type of alignment. One of "global", "local", "overlap",
#' "global-local", and "local-global".
#' @param gapO gap opening penalty
#' @param gapE gap extension penalty
#' @name align_pairwise
#' @export
#' @examples
#' bstr_align_pairwise("abcdefgh", "cde")
#' bstr_align_pairwise("abcdefgh", "cde", type = "local")
#' bstr_align_pairwise("abcdefgh", c("cde", "befg"))
#' bstr_align_pairwise("abcdefgh", c("cde", "befg"), gapO = 0, gapE = 1)
#'
bstr_align_pairwise <- function(sub, pat, type = "global", gapO = 10, gapE = 4) {
  if(length(sub) != 1L) stop("length sub must be 1")
  sub <- as_bstr(sub); n_sub <- names(sub)
  pat <- as_bstr(pat); n_pat <- names(pat)
  n_pair <- paste0("pair", seq_along(pat), ": ")

  al <- pairwise_alignment(sub, pat, type = type,
                           gapOpening = gapO, gapExtension = gapE)
  c(
    bstr(extract_aligned_sub(al), paste0(n_pair, n_sub)) %>% sort,
    bstr(extract_aligned_pat(al), paste0(n_pair, n_pat)) %>% sort
  ) %>%
    bstr_sort_subname(pattern = "^[^:]+")
}

#' @rdname align_pairwise
#' @param rc logical value. if TRUE, the seq2 is aligned after reverse complemented. default is FALSE.
#' @export
#' @examples
#' dstr_align_pairwise("aaaccctttggg", "gggaaa")
#' dstr_align_pairwise("aaaccctttggg", "gggaaa", TRUE)
#' dstr_align_pairwise("aaaccctttggg", c("gggaaa", "cccttt"), 1)
#'
dstr_align_pairwise <- function(sub, pat, rc, type = "global", gapO = 10, gapE = 4) {
  if(length(sub) != 1L) stop("length sub must be 1")
  sub <- as_dstr(sub); n_sub <- names(sub)
  pat <- as_dstr(pat); n_pat <- names(pat)
  n_pair <- paste0("pair", seq_along(pat), ": ")

  if(missing(rc)) rc <- rep(FALSE, length(pat))
  pat[rc] <- dstr_rev_comp(pat[rc])
  n_pat[rc] <- paste0(n_pat[rc], " RC")

  al <- pairwise_alignment(sub, pat, type = type,
                           gapOpening = gapO, gapExtension = gapE)
  c(
    dstr(extract_aligned_sub(al), paste0(n_pair, n_sub)) %>% sort,
    dstr(extract_aligned_pat(al), paste0(n_pair, n_pat)) %>% sort
  ) %>%
    bstr_sort_subname(pattern = "^[^:]+")
}

#' Align multiple sequences to reference
#' @name align_multi
#' @inheritParams align_pairwise
#' @export
#' @examples
#' ref <- c(reference = "abcdefghijk")
#' patterns <- c(one = "abc", two = "d", three = "efgjk")
#' bstr_align_multi(ref, patterns)
#'
bstr_align_multi <- function(sub, pat, type = "global", gapO = 10, gapE = 4) {
  sub <- as_bstr(sub); n_sub <- names(sub)
  pat <- as_bstr(pat); n_pat <- names(pat)
  al <- pairwise_alignment(sub, pat, type = type,
                           gapOpening = gapO, gapExtension = gapE)
  c(
    bstr(sub, n_sub),
    bstr(extract_aligned_pat(al), n_pat)
  )
}

#' @rdname align_multi
#' @export
#' @examples
#' ref <- c(ref = "AAACCCTTTGGG")
#' patterns <- c(A = "AAA", C = "CCC", T = "TTT", G = "GGG")
#' dstr_align_multi(ref, patterns)
#' dstr_align_multi(ref, patterns, 1:2)
#' dstr_align_multi(ref, patterns, c("T", "G"))
#'
dstr_align_multi <- function(sub, pat, rc, type = "global", gapO = 10, gapE = 4) {
  sub <- as_dstr(sub); n_sub <- names(sub)
  pat <- as_dstr(pat); n_pat <- names(pat); names(n_pat) <- n_pat
  if(missing(rc)) rc <- rep(FALSE, length(pat))
  pat[rc] <- dstr_rev_comp_fast(pat[rc])
  n_pat[rc] <- paste0(n_pat[rc], " RC")
  al <- pairwise_alignment(sub, pat, type = type,
                           gapOpening = gapO, gapExtension = gapE)
  c(
    dstr(sub, n_sub),
    dstr(extract_aligned_pat(al), n_pat)
  )
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
#' dstr_pcr("ACAATGTGTGTATGATGGTAGTAGAC", "ATGTG", "TACTA") %>%
#'   {dstr_align_multi(.[1], .[-1])}
#'
dstr_pcr <- function(template, primerF, primerR, FRC = FALSE, RRC = TRUE) {
  . <- NULL
  len <-
    c(template, primerF, primerR) %>%
    lapply(function(x) length(x) != 1L) %>%
    unlist %>%
    any
  if(len) stop("template, primerF, primerR must be length 1.")

  template <- as_dstr(template, "template")
  primerF <- as_dstr(primerF, "primerF")
  primerR <- as_dstr(primerR, "primerR")
  if(FRC) primerF <- dstr_rev_comp(primerF) %>% dstr("primerF RC")
  if(RRC) primerR <- dstr_rev_comp(primerR) %>% dstr("primerR RC")

  primerF_match_start <-
    pairwise_alignment(template, primerF, type = "local") %>%
    Biostrings::subject() %>%
    Biostrings::start()
  primerR_match_end <-
    pairwise_alignment(template, primerR, type = "local") %>%
    Biostrings::subject() %>%
    Biostrings::end()

  product <- bstr_sub(template, primerF_match_start, primerR_match_end)
  c(template, primerF, primerR, dstr(product, "product"))
}

