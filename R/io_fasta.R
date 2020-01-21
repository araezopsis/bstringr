#' Read fasta file
#' @param fpath A fasta file path.
#' @param ucase logical
#' @export
#' @examples
#' inf <- system.file("extdata", "test.fas", package = "bstringr")
#' read_fasta(inf)
#'
read_fasta <-
  function(fpath, ucase = F){
    . <- is_name <- NULL
    fas <- readr::read_lines(fpath)

    temp <-
      tibble::tibble(fas) %>%
      dplyr::mutate(
        is_name = stringr::str_detect(.$fas, "^>"),
        grp = cumsum(is_name),
        seq_name = dplyr::if_else(condition = is_name == T,
                                  true = stringr::str_remove(fas, "^>"),
                                  false = NA_character_)
      ) %>%
      split(.$grp)

    s <- purrr::map_chr(temp, ~ stringr::str_c(.x$fas[-1], collapse = ""))
    names(s) <- NULL
    n <- purrr::map_chr(temp, ~ .x$seq_name[1])
    names(n) <- NULL

    bstr(s, n, ucase = ucase)
  }

#' write fasta file
#' @param x x
#' @param fpath A fasta file path.
#' @param width w
#' @export
#' @examples
#' (temp <- dstr_rand_seq(n = 3, length = c(60, 90, 200), seed = 1))
#' write_fasta(temp)
#'
write_fasta <-
  function(x, fpath, width = 60){
    x <- as_bstr(x)
    n <- names(x)

    temp <-
      purrr::map2(
        n,
        stringr::str_extract_all(x, stringr::str_glue(".{{1,{width}}}")),
        ~ c(stringr::str_c(">", .x), .y)
      ) %>%
      unlist
    if(missing(fpath)){
      return(temp)
    }else{
      readr::write_lines(temp, fpath)
    }
  }

