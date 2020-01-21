
#' Read Genbank file
#' @param fpath A genbank file path.
#' @param ucase logical
#' @export
#' @examples
#' inf <- system.file("extdata", "sample.gb", package = "bstringr")
#' read_genbank(inf)
#'
read_genbank <-
  function(fpath, ucase = F){
    . <- NULL
    gblines <- readr::read_lines(fpath)
    n <- parse_LOCUS_FIELD(gblines) %>% .["LocusName"]
    s <- parse_ORIGIN_FIELD(gblines)
    bstr(s, n, ucase = ucase)
  }

#' Write Genbank file
#' @param x x
#' @param fpath A fasta file path.
#' @export
#' @examples
#' (temp <- dstr_rand_seq(1, 100, seed = 1))
#' write_genbank(temp)
#'
write_genbank <- function(x, fpath) {
  . <- NULL
  name <- names(x)
  name_trunc <- name %>% stringr::str_sub(1, 10)
  len <- bstringr::bstr_length(x)
  Sys.setlocale("LC_TIME", "C")
  today <- format(Sys.Date(), "%Y-%b-%d")
  sequence <- x %>% stringr::str_extract_all(".{1,10}") %>% .[[1]]
  seq_li <- character()
  for(i in seq_along(sequence)) {
    line <- (i %/% 7) + 1
    if((i %% 7) == 1) {
      seq_li[line] <- paste((i-1)*10+1, sequence[i])
    } else {
      seq_li[line] <- paste(seq_li[line], sequence[i])
    }
  }
  max_col <- max(nchar(seq_li))
  seq_li <-
    c(
      stringr::str_pad(seq_li[1:(length(seq_li)-1)], max_col+9, side = "left"),
      stringr::str_c("         ", seq_li[length(seq_li)])
    ) %>%
    paste(collapse = "\n")
  temp <-
    stringr::str_glue(
      "
LOCUS       {name_trunc}        {len} bp ds-DNA     linear       {today}
DEFINITION  .
ACCESSION
VERSION
SOURCE      .
  ORGANISM  .
COMMENT     >{name}
ORIGIN
{seq_li}
//
    "
    )
  if(missing(fpath)){
    return(temp)
  }else{
    readr::write_lines(temp, fpath)
  }
}



#' Parse Genbank file LOCUS FIELD
#' @param gb_line character vector of a Genbank file.
#' @export
#' @examples
#' inf <- system.file("extdata", "sample.gb", package = "bstringr")
#' readLines(inf) %>% parse_LOCUS_FIELD()
#'
parse_LOCUS_FIELD <-
  function(gb_line) {
    . <- NULL
    # Get a LOCUS FIELD line
    locus_field_line <-
      stringr::str_detect(gb_line, "^LOCUS       ") %>%
      {gb_line[.]}

    # This closure removes the matched string and its left or right side of
    # characters from string of "locus_field_line".
    f <- function(chr, side = c("l", "r")) {
      if(!is.na(chr)) {
        if(side == "l")
          temp <- stringr::str_remove(locus_field_line, paste0(".*", chr))
        if(side == "r")
          temp <- stringr::str_remove(locus_field_line, paste0(chr, ".*"))
        locus_field_line <<- temp
      }
    }

    locus_name <-
      stringr::str_remove(locus_field_line, "^LOCUS       ") %>%
      stringr::str_sub(start = 1, end = 10)
    f(locus_name, "l")

    sequence_length <-
      stringr::str_extract(locus_field_line, "\\d+ bp")
    f(sequence_length, "l")

    modification_date <-
      stringr::str_extract(locus_field_line, "\\s\\S+$") %>%
      stringr::str_trim("both")
    f(modification_date, "r")

    genbank_division <-
      stringr::str_trim(locus_field_line, "both") %>%
      stringr::str_extract("[A-Z]{3}$")
    f(genbank_division, "r")

    molecule_type <- stringr::str_trim(locus_field_line, "both")

    return(c(
      LocusName = locus_name,
      SequenceLength = sequence_length,
      MoleculeType = molecule_type,
      GenbankDivision = genbank_division,
      ModificationDate = modification_date
    ))
  }

include_blank_header_line <-
  function(gb_line, header) {
    if(is.logical(header)) header <- which(header)

    blank_lines <- stringr::str_detect(gb_line, "^            ")
    num_blank <- 0
    for (i in blank_lines[(header + 1L):length(blank_lines)]) {
      if(!i) break()
      num_blank <- num_blank + 1
    }
    lines <- rep(FALSE, length(gb_line))
    lines[header:(header + num_blank)] <- TRUE
    lines
  }

#' Parse Genbank file DIFINITION FIELD
#' @param gb_line character vector of a Genbank file.
#' @export
#' @examples
#' inf <- system.file("extdata", "sample.gb", package = "bstringr")
#' readLines(inf) %>% parse_DEFINITION_FIELD()
#'
parse_DEFINITION_FIELD <-
  function(gb_line) {
    . <- NULL
    # Get DEFINITION FIELD lines
    definition_field_line <-
      stringr::str_detect(gb_line, "^DEFINITION  ") %>%
      include_blank_header_line(gb_line, header = .) %>%
      gb_line[.]

    definition <-
      stringr::str_remove(definition_field_line, "^DEFINITION  ") %>%
      stringr::str_trim("both")
    definition
  }

#' Parse Genbank file ACCESSION FIELD
#' @param gb_line character vector of a Genbank file.
#' @export
#' @examples
#' inf <- system.file("extdata", "sample.gb", package = "bstringr")
#' readLines(inf) %>% parse_ACCESSION_FIELD()
#'
parse_ACCESSION_FIELD <-
  function(gb_line) {
    . <- NULL
    # Get an ACCESSION FIELD line
    accession_field_line <-
      stringr::str_detect(gb_line, "^ACCESSION   ") %>%
      gb_line[.]

    accession <-
      stringr::str_remove(accession_field_line, "^ACCESSION   ") %>%
      stringr::str_trim("both")
    accession
  }

#' Parse Genbank file VERSION FIELD
#' @param gb_line character vector of a Genbank file.
#' @export
#' @examples
#' inf <- system.file("extdata", "sample.gb", package = "bstringr")
#' readLines(inf) %>% parse_VERSION_FIELD()
#'
parse_VERSION_FIELD <-
  function(gb_line) {
    . <- NULL
    # Get a VERSION FIELD line
    version_field_line <-
      stringr::str_detect(gb_line, "^VERSION     ") %>%
      gb_line[.]

    version <-
      stringr::str_remove(version_field_line, "^VERSION     ") %>%
      stringr::str_trim("both")
    version
  }

#' Parse Genbank file ORIGIN FIELD
#' @param gb_line character vector of a Genbank file.
#' @export
#' @examples
#' inf <- system.file("extdata", "sample.gb", package = "bstringr")
#' readLines(inf) %>% parse_ORIGIN_FIELD()
#'
parse_ORIGIN_FIELD <-
  function(gb_line) {
    . <- NULL
    # Get ORIGIN FIELD lines
    origin_field_line <-
      stringr::str_which(gb_line, "^ORIGIN") %>%
      {gb_line[.:length(gb_line)]}

    origin_field_line %>%
      {.[!stringr::str_detect(., "^ORIGIN")]} %>%
      {.[!stringr::str_detect(., "//")]} %>%
      stringr::str_remove_all("[\\s\\d]") %>%
      stringr::str_c(collapse = "")
  }

