
#' unlist list of bstr objects to a bstr object
#' @param list_bstr list of bstr objects
#' @param omit_na logical. default is TRUE
#' @export
#' @examples
#' (li_bstr <- bstr_rand_seq(3, 10, seed = 1) %>% lapply(as_bstr))
#' unlist_bstr(li_bstr)
#'
unlist_bstr <- function(list_bstr, omit_na = TRUE) {
  list_bstr <- purrr::map(list_bstr, as_bstr)
  if(omit_na) list_bstr <- purrr::map(list_bstr, ~ .x[!is.na(.x)])
  purrr::reduce(list_bstr, c)
}

#' @rdname unlist_bstr
#' @export
bstr_unlist <- unlist_bstr

