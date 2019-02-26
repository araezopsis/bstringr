#' bstr_to_lower
#' @importFrom stringr str_to_lower
#' @inheritParams class_bstr_arg
#' @export
bstr_to_lower <-
  function(bstrobj){
    bstrobj <- as_bstr(bstrobj)
    at <- attributes(bstrobj)

    bstrobj <- str_to_lower(bstrobj)

    attributes(bstrobj) <- at
    bstrobj
  }

#' bstr_to_upper
#' @importFrom stringr str_to_upper
#' @inheritParams class_bstr_arg
#' @export
bstr_to_upper <-
  function(bstrobj){
    bstrobj <- as_bstr(bstrobj)
    at <- attributes(bstrobj)

    bstrobj <- str_to_upper(bstrobj)

    attributes(bstrobj) <- at
    bstrobj
  }

#' bstr_switch_case
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_detect
#' @importFrom stringr str_to_upper
#' @importFrom stringr str_to_lower
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom purrr map_chr
#' @inheritParams class_bstr_arg
#' @export
bstr_switch_case <-
  function(bstrobj){
    bstrobj <- as_bstr(bstrobj)
    at <- attributes(bstrobj)

    bstrobj <- bstrobj %>% str_extract_all(".")
    lower_c <- bstrobj %>% map(~ str_detect(.x, "[[:lower:]]"))
    upper_c <- bstrobj %>% map(~ str_detect(.x, "[[:upper:]]"))

    bstrobj <-
      bstrobj %>%
      map2(lower_c, ~ ifelse(.y, str_to_upper(.x), .x)) %>%
      map2(upper_c, ~ ifelse(.y, str_to_lower(.x), .x)) %>%
      map_chr(~ paste0(.x, collapse = ""))

    attributes(bstrobj) <- at
    bstrobj
  }

#' bstr_reverse
#' @importFrom stringi stri_reverse
#' @inheritParams class_bstr_arg
#' @export
bstr_reverse <-
  function(bstrobj){
    bstrobj <- as_bstr(bstrobj)
    at <- attributes(bstrobj)

    bstrobj <- stri_reverse(bstrobj)

    attributes(bstrobj) <- at
    bstrobj
  }
