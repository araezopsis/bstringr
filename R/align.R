
extract_aligned_sub <- function(align) {
  x <- Biostrings::alignedSubject(align) %>% as.character()
  bstr(x, n = paste0(seq_along(x), " subject"))
}

extract_aligned_pat <- function(align) {
  x <- Biostrings::alignedPattern(align) %>% as.character()
  bstr(x, n = paste0(seq_along(x), " pattern"))
}

pairwisealignment <- function(sub, pat, ...) {
  Biostrings::pairwiseAlignment(subject = sub, pattern = pat, ...)
}

#' Align bstr sequence
#' @param sub subject sequence
#' @param pat pattern sequence
#' @param type a character string
#' @name align
#' @export
#' @examples
#' bstr_align_pairwise("abcdefgh", "cde", type = "local")
#' bstr_align_pairwise("abcdefgh", "cde", type = "global")
#' bstr_align_pairwise("abcdefgh", c("cde", "befg"), type = "global")
#'
bstr_align_pairwise <-
  function(sub, pat, type = "local") {
    sub <- as_bstr(sub)
    pat <- as_bstr(pat)
    al <- pairwisealignment(sub, pat, type = type)
    c(extract_aligned_sub(al), extract_aligned_pat(al)) %>% sort
  }

#' Align dna sequence
#' @inheritParams align
#' @param rc logical value. if TRUE, the seq2 is aligned after reverse complemented. default is FALSE.
#' @export
dstr_align_pairwise <-
  function(sub, pat, rc = F, type = "local") {
    sub <- as_dstr(sub)
    pat <- ifelse(rc, dstr_rev_comp(pat), as_dstr(pat))
    al <- pairwisealignment(sub, pat, type = type)
    c(extract_aligned_sub(al), extract_aligned_pat(al)) %>% sort
  }

# dstr_align_multi_pats("AAACCCTTTGGG", c("AAA", "CCC", "TTT", "GGG"), rep(FALSE, 4))
dstr_align_multi_pats <- function(ref, pats, rcs, type = "local-global") {
  ref <- as_dstr(ref)
  pats <- as_dstr(pats)
  pats[rcs] <- dstr_rev_comp_fast(pats[rcs])
  al <- pairwisealignment(ref, pats, type = type)
  names(ref) <- "reference"
  c(ref, extract_aligned_pat(al))
}



align_2seq <- function(ref, sub, rc = F) {
  bstringr::dstr_align(seq1  = ref, seq2 = sub, rc = rc) %>%
    Biostrings::aligned() %>%
    as.character() %>%
    return
  # bstringr::as_dstr(n = names(sub))
}


#' Align multiple bstr sequence
#' @inheritParams align
#' @param pats pattern sequences
#' @export
bstr_multi_align <-
  function(sub, pats){
    sub <- as_bstr(sub)
    pats <- as_bstr(pats)

    li <- list()
    for(i in names(pats)){
      li[[i]] <-
        list(
          align = bstr_align(sub, pats[i])
        )
    }
    li
  }

#' Align multiple bstr sequence
#' @param subject subject sequence
#' @param queries query sequences
#' @export
dstr_multi_align <-
  function(subject, queries){
    subject <- as_dstr(subject)
    queries <- as_dstr(queries)

    li <- list()
    for(i in names(queries)){
      align_f <- bstr_align(subject, queries[i])
      align_r <- bstr_align(subject, dstr_rev_comp(queries[i]))
      score_f <- Biostrings::score(align_f) %>% as.integer()
      score_r <- Biostrings::score(align_r) %>% as.integer()

      if(score_f >= score_r){
        align <- align_f
      }else{
        align <- align_r
      }

      li[[i]] <-
        list(
          align, # ifelse()で処理できず
          orientation = dplyr::if_else(score_f >= score_r, "forward", "reverse")
        )
    }
    li
  }
