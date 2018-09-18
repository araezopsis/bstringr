#' Read fasta file
#' @importFrom readr read_lines
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove
#' @importFrom stringr str_c
#' @importFrom purrr map_chr
#' @importFrom dplyr data_frame
#' @importFrom dplyr mutate
#' @importFrom dplyr if_else
#' @param fpath A fasta file path.
#' @param ucase logical
#' @export
read_fasta <-
  function(fpath, ucase = F){
    . <- NULL
    .data <- NULL
    fas <- read_lines(fpath)

    temp <-
      data_frame(
        fas,
        is_name = str_detect(.data$fas, "^>"),
        grp = cumsum(.data$is_name),
        seq_name = if_else(.data$is_name == T, str_remove(.data$fas, "^>"), NA_character_)
      ) %>%
      split(.$grp)

    s <- map_chr(temp, ~ str_c(.x$fas[-1], collapse = ""))
    names(s) <- NULL
    n <- map_chr(temp, ~ .x$seq_name[1])
    names(n) <- NULL

    bstr(s, n, ucase = ucase)
  }

#' write fasta file
#' @importFrom stringr str_c
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_glue
#' @importFrom readr write_lines
#' @importFrom purrr map2
#' @param x x
#' @param fpath A fasta file path.
#' @param width w
#' @export
write_fasta <-
  function(x, fpath, width = 60){
    x <- as_bstr(x)
    n <- names(x)

    temp <-
      map2(
        n,
        str_extract_all(x, str_glue(".{{1,{width}}}")),
        ~ c(str_c(">", .x), .y)
      ) %>%
      unlist
    if(missing(fpath)){
      return(temp)
    }else{
      write_lines(temp, fpath)
    }
  }

