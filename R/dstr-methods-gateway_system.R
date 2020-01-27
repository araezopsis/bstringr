
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
