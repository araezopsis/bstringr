#'
#' #' Split sequence by residues and return data.frame
#' #' @importFrom dplyr data_frame
#' #' @importFrom dplyr mutate
#' #' @importFrom dplyr row_number
#' #' @importFrom stringr str_extract_all
#' #' @param seq A character vector.
#' #' @param seq_name_fct A factor.
#' split_sequence <-
#'   function(seq, seq_name_fct){
#'     data_frame(
#'       residue = str_extract_all(seq, ".")[[1]]
#'     ) %>%
#'       mutate(seq_name = seq_name_fct) %>%
#'       mutate(pos = row_number())
#'   }
#'
#'
#' #' Split sequence by residues and return data.frame
#' #' @importFrom purrr map2
#' #' @importFrom dplyr bind_rows
#' #' @param bstrobj A bstr object or A character vector
#' #' @examples
#' #' bstr2df(c("ATGC", "AGCT--GT"))
#' #' @export
#' bstr2df <-
#'   function(bstrobj){
#'     bstrobj <- as_bstr(bstrobj)
#'     at <- attributes(bstrobj)
#'
#'     map2(
#'       bstrobj,
#'       factor(names(bstrobj)),
#'       ~ split_sequence(.x, .y)
#'     ) %>%
#'       bind_rows()
#'   }
