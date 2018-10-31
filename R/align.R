
#' Align bstr sequence
#' @param seq1 sequence
#' @param seq2 sequence
#' @export
bstr_align <-
  function(seq1, seq2){
    seq1 <- as_bstr(seq1) %>% bstr2BioString()
    seq2 <- as_bstr(seq2) %>% bstr2BioString()
    Biostrings::pairwiseAlignment(subject = seq1, pattern = seq2, type = "local")
  }

#' Align dna sequence
#' @param seq1 sequence
#' @param seq2 sequence
#' @param rc logical value. if TRUE, the seq2 is aligned after reverse complemented. default is FALSE.
#' @export
dstr_align <-
  function(seq1, seq2, rc = F){
    seq1 <- as_dstr(seq1) %>% bstr2BioString
    seq2 <- ifelse(rc, dstr_rc(seq2), as_dstr(seq2)) %>% bstr2BioString
    Biostrings::pairwiseAlignment(subject = seq1, pattern = seq2, type = "local")
  }

#' Align multiple bstr sequence
#' @param subject subject sequence
#' @param queries query sequences
#' @export
bstr_multi_align <-
  function(subject, queries){
    subject <- as_bstr(subject)
    queries <- as_bstr(queries)

    li <- list()
    for(i in names(queries)){
      li[[i]] <-
        list(
          align = bstr_align(subject, queries[i])
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
      align_r <- bstr_align(subject, dstr_rc(queries[i]))
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
          orientation = if_else(score_f >= score_r, "forward", "reverse")
        )
    }
    li
  }
