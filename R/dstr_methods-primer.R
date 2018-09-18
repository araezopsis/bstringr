
#' Caluculate Oligo GC\%
#' @importFrom stringr str_count
#' @param x A DNA sequence you want to calculate GC\%
#' @export
calc_GCper <-
  function(x){
    dstrobj <- as_dstr(x)
    str_count(dstrobj, "[GC]") / str_count(dstrobj)
  }


dinuc_dH <-
  c(
    "AA" = 9.1, "AT" = 8.6, "AG" = 7.8,  "AC" = 6.5,
    "TA" = 6.0, "TT" = 9.1, "TG" = 5.8,  "TC" = 5.6,
    "GA" = 5.6, "GT" = 6.5, "GG" = 11.0, "GC" = 11.1,
    "CA" = 5.8, "CT" = 7.8, "CG" = 11.9, "CC" = 11.0
  )
dinuc_dS <-
  c(
    "AA" = 24.0, "AT" = 23.9, "AG" = 20.8, "AC" = 17.3,
    "TA" = 16.9, "TT" = 24.0, "TG" = 12.9, "TC" = 13.5,
    "GA" = 13.5, "GT" = 17.3, "GG" = 26.6, "GC" = 26.7,
    "CA" = 12.9, "CT" = 20.8, "CG" = 27.8, "CC" = 26.6
  )

#' Calculate Oligo DNA Tm
#' @param numTotal hoge
#' @param numA hoge
#' @param numT hoge
#' @param numG hoge
#' @param numC hoge
#' @param dinuc1 hoge
#' @param dinuc2 hoge
mf <-
  function(numTotal, numA, numT, numG, numC, dinuc1, dinuc2){
    if(numTotal < 18){
      Tm <- 4 * (numG + numC) + 2 * (numA + numC)
    } else {
      dH <- 0
      dS <- 0

      dH <- dH + sum(dinuc_dH[dinuc1])
      dS <- dS + sum(dinuc_dS[dinuc1])

      dH <- dH + sum(dinuc_dH[dinuc2])
      dS <- dS + sum(dinuc_dS[dinuc2])

      dS <- ifelse((numG + numG) > 0, dS + 16.1212, dS + 19.3455)

      Tm <- ((dH * 1000) / (dS + 26.26)) - 273.15 - 21.5971
      Tm
    }
  }

#' Calculate Oligo DNA Tm
#' @importFrom stringr str_count
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_sub
#' @importFrom purrr pmap_dbl
#' @importFrom magrittr %>%
#' @param x A DNA sequence you want to calculate melting temperature
#' @export
calc_oligoDNATm <-
  function(x){
    dstrobj <- as_dstr(x)
    n <- names(dstrobj)

    numTotal <- str_count(dstrobj, ".")
    numA <- str_count(dstrobj, "A")
    numT <- str_count(dstrobj, "T")
    numG <- str_count(dstrobj, "G")
    numC <- str_count(dstrobj, "C")

    dinuc1 <- str_extract_all(dstrobj, "[ATGC]{2}")
    dinuc2 <- str_sub(dstrobj, 2) %>% str_extract_all("[ATGC]{2}")

    Tm <-
      pmap_dbl(
        list(numTotal, numA, numT, numG, numC, dinuc1, dinuc2),
        ~ mf(..1, ..2, ..3, ..4, ..5, ..6, ..7)
      )
    round(Tm, 2)
  }
