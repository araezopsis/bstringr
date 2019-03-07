
#' Compare 2 residues
#' @inheritParams calc_ident_mat
#' @param x character
compare <- function(x, gap_chr){
  if(any(x != gap_chr)) x[1] == x[2]
}

#' calculate \% of identical residues
#' @importFrom purrr partial
#' @importFrom stringr str_extract_all
#' @param x aligned sequence 1
#' @param y aligned sequence 2
#' @inheritParams calc_ident_mat
calc_ident <-
  function(x, y, gap_chr = "-"){
    . <- NULL
    f <- partial(compare, gap_chr = gap_chr)
    str_extract_all(c(x, y), ".", simplify = T) %>%
      apply(2, f) %>%
      unlist %>%
      {sum(.)/length(.)}
  }

#' calculate \% of identical residues
#' @importFrom utils combn
#' @param bstrobj bstrobj
#' @param gap_chr gap character
#' @examples
#' temp <- bstr(c("ATGC--CG", "ACGC-GGG", "--------", "AT"))
#' round(calc_ident_mat(temp), 2)
#' @export
calc_ident_mat <-
  function(bstrobj, gap_chr = "-"){
    l <- length(bstrobj)
    n <- names(bstrobj)
    mat <- matrix(nrow = l, ncol = l)
    row.names(mat) <- colnames(mat) <- n
    cb <- combn(seq_along(bstrobj), 2, simplify = F)
    for(i in cb){
      mat[i[1], i[2]] <-
        calc_ident(bstrobj[i[1]], bstrobj[i[2]], gap_chr = gap_chr)
    }
    return(t(mat))
  }

