
# library(crayon)
# temp <- c("abc", "ABC", "AbCd") %>% bstr(ucase = F)
# temp %>% stringr::str_replace_all("[[:upper:]]+", crayon::bgRed)


#' print biostrings class object
#' @importFrom stringr str_replace_all
#' @importFrom crayon bgRed
#' @param x x
#' @export
cat_string <-
  function(x){
    cat(str_replace_all(x, "[[:upper:]]", crayon::bgRed))
  }


#' print biostrings class object
#' @importFrom purrr map
#' @param x x
#' @param ... ...
#' @param n number of printing elements
#' @param l_name max length of name
#' @param l_seq max length of sequence
#' @export
print.bstr <-
  function(x, ..., n = 6, l_name = 20, l_seq = 60){
    lx <- length(x)
    n <- ifelse(n > lx, lx, n)

    cat("class:", paste0(class(x), collapse = ","), "\n")
    cat("number of sequences:", lx, "\n")

    purrr::walk(
      format_row(x = x, n = n, l_name = l_name, l_seq = l_seq),
      ~ cat(., "\n")
    )

    invisible(x)
  }


# min(l_name) == 20
#' format bstr name
#' @importFrom stringr str_count
#' @importFrom stringr str_sub
#' @importFrom stringr str_pad
#' @inheritParams print.bstr
format_name <-
  function(x, l_name){
    x <- as.character(x)
    x[is.na(x)] <- "<NA>"
    x <- ifelse(
      test = str_count(x) > l_name,
      yes = paste0(str_sub(x, end = l_name - 3), "..."),
      no = x
    )
    str_pad(x, l_name, side = "both")
  }

# min(l_seq) == 6, (5 %/% 2 - 2) == 0
#' format bstr sequence
#' @importFrom stringr str_count
#' @importFrom stringr str_sub
#' @importFrom stringr str_pad
#' @inheritParams print.bstr
format_seq <-
  function(x, l_seq){
    l_seq_half <- (l_seq %/% 2) - 2

    class(x) <- "character"
    x[is.na(x)] <- "<NA>"
    x <-
      ifelse(
        test = str_count(x) <= l_seq,
        yes = x,
        no = paste0(
          str_sub(x, end = l_seq_half),
          "....",
          str_sub(x, start = -l_seq_half)
        )
      )
    str_pad(x, l_seq, side = "right")
  }

#' format bstr
#' @inheritParams print.bstr
format_row <-
  function(x, n, l_name, l_seq){
    x <- x[seq_len(n)]
    paste0(
      "[",
      seq_along(x) %>% {str_pad(., max(nchar(.)), "right")},
      "] ",
      format_name(names(x), l_name),
      ": ",
      format_seq(x, l_seq)
    )
  }
