
#' bstrobj <-> data.frame
#' @inheritParams class_bstr
#' @name bstr2df
#' @export
#' @examples
#' test <- bstr_rand_seq(3, 10, seed = 1)
#'
#' test %>% bstr2df
#' test %>% bstr2longdf
#'
#' test %>% bstr2df %>% df2bstr
#' test %>% bstr2longdf %>% longdf2bstr
#'
bstr2df <- function(bstrobj) {
  bstrobj <- as_bstr(bstrobj)
  tibble::tibble(
    name = names(bstrobj),
    seq = bstrobj
  )
}

#' @rdname bstr2df
#' @export
bstr2longdf <- function(bstrobj) {
  name <- pos <- residue <- NULL
  bstrobj <- as_bstr(bstrobj)
  suppressWarnings(
    bstrobj %>%
      bstr_extract(".") %>%
      purrr::imap(~ tibble::tibble(name = .y, residue = unlist(.x))) %>%
      purrr::map(~ dplyr::mutate(.x, pos = dplyr::row_number())) %>%
      dplyr::bind_rows() %>%
      dplyr::select(name, pos, residue)
  )
}

#' @param df data.frame
#' @rdname bstr2df
#' @export
df2bstr <- function(df) {
  if(!all(c("name", "seq") %in% colnames(df)))
    stop("df must has 'name' and 'seq' columns.")
  bstr(x = df[["seq"]], n = df[["name"]])
}

#' @param longdf long data.frame
#' @rdname bstr2df
#' @export
longdf2bstr <- function(longdf) {
  name <- pos <- NULL
  if(!all(c("name", "pos", "residue") %in% colnames(longdf)))
    stop("longdf must has 'name', 'pos' and 'residue' columns.")

  dplyr::arrange(longdf, pos) %>%
    dplyr::group_split(name) %>%
    purrr::map(~ bstr(x = paste0(.x[["residue"]], collapse = ""),
                      n = .x[["name"]][1])) %>%
    purrr::reduce(c)
}

