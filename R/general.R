
#' subsetting bstr class object
#' @param x x
#' @param ... ...
#' @param drop drop
#' @export
#' @examples
#' (test <- dstr_rand_seq(3, 10, seed = 1))
#' test[1]
#' test[2:1]
#'
#' test[2] <- "mutated"
#' test
#'
"[.bstr" <-
  function(x, ..., drop = F){
    y <- NextMethod("[", "character")
    class(y) <- class(x)
    y
  }

#' Combine Values into a bstr class object
#' @param ... ...
#' @export
#' @examples
#' c(dstr_rand_seq(2, 5, seed = 1), dstr_rand_seq(3, 5, seed = 2))
#' c(dstr_rand_seq(3, 5, seed = 2), dstr_rand_seq(2, 5, seed = 1))
#'
#' c("hoge", dstr_rand_seq(2, 5, seed = 1))
#' # c(dstr_rand_seq(3, 5, seed = 2), "hoge") # Error
#' c(dstr_rand_seq(3, 5, seed = 2), as_bstr("hoge"))
#'
"c.bstr" <-
  function(...){
    if(all(unlist(lapply(list(...), is_bstr)))){
      y <- c(unlist(lapply(list(...), unclass)))
      y <- as_bstr(y)
    }else{
      stop("input contains not bstr class object")
    }
  }

#' sort bstr
#' @param x x
#' @param decreasing logical. FALSE
#' @param ... ...
#' @param by c("names", "length")
#' @export
#' @examples
#' test <- bstr_rand_seq(6, c(10, 5, 10, 12, 13, 3), seed = 1)
#' test
#' sort(test, decreasing = TRUE)
#' sort(test, by = "length")
#'
sort.bstr <-
  function(x, decreasing = FALSE, ..., by = c("names", "length")){
    by <- match.arg(by)
    xc <- class(x)
    class(x) <- "character"

    if(by == "names"){
      n <- sort(names(x), decreasing = decreasing)
    }else{
      n <- names(sort(nchar(x), decreasing = decreasing))
    }
    x <- x[n]
    class(x) <- xc
    x
  }

#' Convert sequence to Biostrings::*StringSet object
#' @importFrom Biostrings BStringSet
#' @importFrom Biostrings DNAStringSet
#' @importFrom Biostrings AAStringSet
#' @param x sequence
#' @export
bstr2BioString <-
  function(x){
    x <- as_bstr(x)
    x_class <- class(x)
    class(x) <- "character"

    x <-
      switch(x_class[1],
             "bstr" = BStringSet(x, use.names = T),
             "dstr" = DNAStringSet(x, use.names = T),
             "astr" = AAStringSet(x, use.names = T)
      )
    x
  }

#' Convert sequence to Biostrings::*StringSet object
#' @param x sequence
#' @export
Bio2bstr <-
  function(x){
    bstr(as.character(x), names(x), ucase = F)
  }
# Biostrings::DNAStringSet(c(hoge = "AGAG", hige = "atg")) %>% Bio2bstr()

