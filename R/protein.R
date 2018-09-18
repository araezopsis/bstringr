
#' Calculate protein molecular weight
#' @importFrom stringr str_count
#' @importFrom purrr map
#' @importFrom purrr transpose
#' @importFrom purrr set_names
#' @param pro Amino acids sequence you want to calculate molecular weight
#' @param needinf information "Sequence", "AAComposition", "AAFrequency", "MW"
#' @export
calc_MW_peptide <-
  function(pro, needinf = c("Sequence", "MW")){
    pro <- as_bstr(pro)
    n <- names(pro)
    p_comp <- map(as.list(AMINO_ACID), ~ str_count(pro, .))
    names(p_comp) <- AMINO_ACID

    p_comp <- p_comp %>% transpose()
    names(p_comp) <- n

    p_comp <- p_comp %>% map(~ set_names(as.integer(.), AMINO_ACID))
    p_freq <- p_comp %>% map(~ round(.x/sum(.x) * 100, 2))
    p_MW <- p_comp %>% map(~ (.x * AA_WEIGHT[AMINO_ACID]) %>% sum)
    list(
      "Sequence" = pro,
      "AAComposition" = p_comp,
      "AAFrequency" = p_freq,
      "MW" = p_MW
    )[needinf]
  }

#' Calculate double strand DNA molecular weight
#' @param dna dna sequence
#' @export
calc_MW_dsDNA <-
  function(dna){
    .data <- NULL
    dna <- as_dstr(dna)
    tibble::tibble(
      seq_name = names(dna),
      seq = as.character(dna),
      num_bases = nchar(dna),
      applox_MW = (.data$num_bases * 607.4) + 157.9
    )
  }

#' Calculate single strand RNA molecular weight
#' @param dna dna sequence
#' @export
calc_MW_ssRNA <-
  function(dna){
    .data <- NULL
    dna <- as_dstr(dna)
    tibble::tibble(
      seq_name = names(dna),
      seq = as.character(dna),
      num_bases = nchar(dna),
      applox_MW = (.data$num_bases * 320.5) + 159.0
    )
  }
