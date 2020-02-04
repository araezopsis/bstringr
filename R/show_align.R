
get_console_width <- function() options()$width

#' show alignment
#' @inheritParams class_bstr
#' @inheritParams bstr_remove_gap
#' @param length column length
#' @export
#' @examples
#' dstr_rand_seq(5, seq(40, 110, l = 5), seed = 1) %>%
#'   show_align(length = 40)
#'
#' bstr_rand_seq(2, c(15, 30), "[AT]", seed = 2) %>%
#'   {bstr_align_pairwise(.[1], .[2])} %>%
#'   show_align(length = 40)
#'
show_align <- function(bstrobj, length, gap_chr = "-") {
    . <- NULL
    ruler <- "_________|"
    l_name <- 20L
    bstrobj <- as_bstr(bstrobj)
    n <- names(bstrobj) %>% c("", .) %>% format_parts_name(l_name)

    if(missing(length)) {
      max_bstr_len <- max(bstr_length(bstrobj))
      length <-
        get_console_width() %>%
        {. - l_name} %>%
        {. - nchar(max_bstr_len)} %>%
        {. %/% 10L * 10L}
      if(length > ((max_bstr_len + 10L) %/% 10L * 10L))
        length <- (max_bstr_len + 10L) %/% 10L * 10L
    }
    if((length %% 10L) != 0L) stop()

    parts <- bstrobj %>% stringr::str_extract_all(paste0(".{1,", length, "}"))
    parts_len <- parts %>% purrr::map(~ stringr::str_remove_all(.x, gap_chr)) %>% purrr::map(nchar) %>% purrr::map(cumsum)
    parts <- purrr::map(parts, ~ stringr::str_pad(.x, width = length + 1L, side = "right"))
    parts <- purrr::map2(parts, parts_len, ~ paste0(.x, .y))

    max_len <- purrr::map_int(parts, ~ length(.x)) %>% max
    rulers <- stringr::str_dup(ruler, length %/% 10)
    li <- list()
    for(i in seq_len(max_len)) {
      temp <- rulers
      for(j in parts) {
        temp_str <- j[i]
        temp <- c(temp, ifelse(is.na(temp_str), "", temp_str))
      }
      li[[i]] <-temp
    }

    lines <-
      purrr::map(li, ~ paste(n, .x, sep = ": ")) %>%
      purrr::map(~ c(.x, "")) %>%
      unlist
    purrr::walk(lines, ~ cat(.x, "\n"))
    invisible(lines)
  }

