
REGEX_ORF <-
  c(
    include_none_stop = "ATG([ATGC]{3})*?($|TAG|TGA|TAA)",
    exclude_none_stop = "ATG([ATGC]{3})*?(TAG|TGA|TAA)"
  )

#' Find and extract all open reading frames in a dstr object
#' @inheritParams class_bstr
#' @param orf_pattern regular expression pattern of open reading frame
#' @name orfs
#' @export
#' @examples
#' (temp <- dstr_rand_seq(2, 200, seed = 1))
#' temp %>% dstr_locate_orfs() %>% {bstr_sub_all(temp, .)}
#' dstr_locate_orfs("atgatgtga")
#'
#' (temp <- dstr_rand_seq(2, 200, seed = 1))
#' temp %>% dstr_extract_orfs()
#' # temp %>% dstr_extract_orfs(REGEX_ORF)
#'
dstr_locate_orfs <- function(dstrobj, orf_pattern = REGEX_ORF[2]) {
  dstrobj <- as_dstr(dstrobj) %>% bstr_to_upper()
  bstr_locate(dstrobj, orf_pattern)
}

#' @rdname orfs
#' @export
dstr_find_orfs <- dstr_locate_orfs

# dstr_locate_orfs("AGTTGA")
# dstr_extract_orfs("AGTTGA")
# dstr_locate_orfs("atgatgtga")
# dstr_extract_orfs("atgatgtga")

#' @rdname orfs
#' @export
dstr_extract_orfs <- function(dstrobj, orf_pattern = REGEX_ORF[2]) {
  . <- NULL
  dstr_locate_orfs(dstrobj, orf_pattern) %>%
    bstr_sub_all(dstrobj, .)
}

#' Check open reading frame validity
#' @inheritParams class_bstr
#' @param check_stop If TRUE, check the presence of stop codon.
#' @param negate If TRUE, return non-matching elements.
#' @export
#' @examples
#' (test <- dstr(c("ATGtga", "AAGTGA", "ATGGGGTGA", "ATGGGTGA", "ATGGGG","ATGGG")))
#' is_valid_orf(test)
#' is_valid_orf(test, check_stop = TRUE)
#'
is_valid_orf <- function(dstrobj, check_stop = FALSE, negate = FALSE) {
    dstrobj <- as_dstr(dstrobj) %>% bstr_to_upper()

    if(check_stop) bstr_detect(dstrobj, paste0("^", REGEX_ORF[2]))
    else bstr_detect(dstrobj, paste0("^", REGEX_ORF[1]))
}

