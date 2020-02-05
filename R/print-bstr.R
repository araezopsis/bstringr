#
# # library(crayon)
# # temp <- c("abc", "ABC", "AbCd") %>% bstr(ucase = F)
# # temp %>% stringr::str_replace_all("[[:upper:]]+", crayon::bgRed)
#
#
# #' print biostrings class object
# #' @importFrom stringr str_replace_all
# #' @importFrom crayon bgRed
# #' @param x x
# #' @export
# cat_string <-
#   function(x){
#     cat(str_replace_all(x, "[[:upper:]]", crayon::bgRed))
#   }


#' print bstr class object
#' @param x x
#' @param ... ...
#' @param n number of printing elements
#' @param l_name max length of name
#' @param l_seq max length of sequence
#' @export
#' @examples
#' dstr_rand_seq(10, 20, seed = 1)
#' pstr_rand_seq(10, 20, seed = 1)
#'
#' test1 <- dstr_rand_seq(1, 20, seed = 1)
#' test2 <- dstr_rand_seq(1, 21, seed = 1)
#' print(test1, l_seq = 20L)
#' print(test2, l_seq = 20L)
#'
#' print(test1, l_name = 9L)
#' print(test1, l_name = 8L)
#'
#' dstr_rand_seq(10, 20, seed = 1) %>% print(n = 20L)
#'
print.bstr <-
  function(x, ..., n = 6L, l_name = 20L, l_seq = 40L){
    lx <- length(x)
    n <- ifelse(n > lx, lx, n)

    cat("class:", paste0(class(x), collapse = ","), "\n")
    cat("number of sequences:", lx, "\n")

    purrr::walk(
      format_row(x = x, n = n, l_name = l_name, l_seq = l_seq),
      ~ cat(.x, "\n")
    )

    invisible(x)
  }


# min(l_name) == 20
#' format bstr name
#' @inheritParams print.bstr
format_parts_name <-
  function(x, l_name){
    class(x) <- "character"
    x[is.na(x)] <- "<NA>"
    x <- ifelse(
      test = stringr::str_count(x) > l_name,
      yes = paste0(stringr::str_sub(x, end = l_name - 3), "..."),
      no = x
    )
    stringr::str_pad(x, l_name, side = "both")
  }

# min(l_seq) == 6, (5 %/% 2 - 2) == 0
#' format bstr sequence
#' @inheritParams print.bstr
format_parts_seq <-
  function(x, l_seq){
    l_seq_half <- (l_seq %/% 2) - 2

    class(x) <- "character"
    x[is.na(x)] <- "<NA>"
    x <-
      ifelse(
        test = stringr::str_count(x) <= l_seq,
        yes = x,
        no = paste0(
          stringr::str_sub(x, end = l_seq_half),
          "....",
          stringr::str_sub(x, start = -l_seq_half)
        )
      )
    stringr::str_pad(x, l_seq, side = "right")
  }

#' format bstr sequence length
#' @inheritParams print.bstr
format_parts_length <-
  function(x){
    class(x) <- "character"
    x <- stringr::str_count(x)
    x_maxn <- max(stringr::str_count(x))
    stringr::str_pad(x, width = x_maxn)
  }

#' format bstr
#' @inheritParams print.bstr
format_row <-
  function(x, n, l_name, l_seq){
    . <- NULL
    x <- x[seq_len(n)]
    paste0(
      "[",
      seq_along(x) %>% {stringr::str_pad(., max(nchar(.)), "right")},
      "] ",
      format_parts_name(names(x), l_name),
      ": ",
      format_parts_seq(x, l_seq),
      " ",
      format_parts_length(x)
    )
  }


