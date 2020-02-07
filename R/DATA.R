
CODONS <-
  c(
    "TTT" = "F",  "TCT" = "S",  "TAT" = "Y",  "TGT" = "C",
    "TTC" = "F",  "TCC" = "S",  "TAC" = "Y",  "TGC" = "C",
    "TTA" = "L",  "TCA" = "S",  "TAA" = "*",  "TGA" = "*",
    "TTG" = "L",  "TCG" = "S",  "TAG" = "*",  "TGG" = "W",

    "CTT" = "L",  "CCT" = "P",  "CAT" = "H",  "CGT" = "R",
    "CTC" = "L",  "CCC" = "P",  "CAC" = "H",  "CGC" = "R",
    "CTA" = "L",  "CCA" = "P",  "CAA" = "R",  "CGA" = "R",
    "CTG" = "L",  "CCG" = "P",  "CAG" = "R",  "CGG" = "R",

    "ATT" = "I",  "ACT" = "T",  "AAT" = "N",  "AGT" = "S",
    "ATC" = "I",  "ACC" = "T",  "AAC" = "N",  "AGC" = "S",
    "ATA" = "I",  "ACA" = "T",  "AAA" = "K",  "AGA" = "R",
    "ATG" = "M",  "ACG" = "T",  "AAG" = "K",  "AGG" = "R",

    "GTT" = "V",  "GCT" = "A",  "GAT" = "D",  "GGT" = "G",
    "GTC" = "V",  "GCC" = "A",  "GAC" = "D",  "GGC" = "G",
    "GTA" = "V",  "GCA" = "A",  "GAA" = "E",  "GGA" = "G",
    "GTG" = "V",  "GCG" = "A",  "GAG" = "E",  "GGG" = "G"
  )

AA_DATA <-
  tibble::tribble(
    ~aa, ~h_index, ~three_letter,  ~aa_mw,
    "I",      4.5,         "Ile", 113.158,
    "V",      4.2,         "Val",  99.131,
    "L",      3.8,         "Leu", 113.158,
    "F",      2.8,         "Phe", 147.174,
    "C",      2.5,         "Cys", 103.143,
    "M",      1.9,         "Met", 131.196,
    "A",      1.8,         "Ala",  71.078,
    "G",     -0.4,         "Gly",  57.051,
    "T",     -0.7,         "Thr", 101.104,
    "S",     -0.8,         "Ser",  87.077,
    "W",     -0.9,         "Trp",  186.21,
    "Y",     -1.3,         "Tyr", 163.173,
    "P",     -1.6,         "Pro",  97.115,
    "H",     -3.2,         "His", 137.139,
    "E",     -3.5,         "Glu", 129.114,
    "Q",     -3.5,         "Gln", 128.173,
    "D",     -3.5,         "Asp", 115.087,
    "N",     -3.5,         "Asn", 114.103,
    "K",     -3.9,         "Lys", 128.172,
    "R",     -4.5,         "Arg", 156.186
  )

AA_ALPHABET <-
  c("A", "R", "N", "D", "C", "Q", "E", "G", "H", "I", "L", "K",
    "M", "F", "P", "S", "T", "W", "Y", "V", "X")

# AA_ALPHABET %>% stringr::str_c(collapse = "") %>% paste0("[", ., "]")
REGEX_AA_ALPHABET <-
  "(?i)[ARNDCQEGHILKMFPSTWYVX\\-\\+\\.\\*]"
REGEX_NOT_AA_ALPHABET <-
  "(?i)[^ARNDCQEGHILKMFPSTWYVX\\-\\+\\.\\*]"



# "https://en.wikipedia.org/wiki/Nucleic_acid_notation"
# | symbol | bases_represented | complement |          description          |
# |:------:|:-----------------:|:----------:|:-----------------------------:|
# |   A    |         A         |     T      |            Adenine            |
# |   C    |         C         |     G      |           Cytosine            |
# |   G    |         G         |     C      |            Guanine            |
# |   T    |         T         |     A      |            Thymine            |
# |   U    |         U         |     A      |            Uracil             |
# |   W    |        AT         |     W      |             Weak              |
# |   S    |        CG         |     S      |            Strong             |
# |   M    |        AC         |     K      |             aMino             |
# |   K    |        GT         |     M      |             Keto              |
# |   R    |        AG         |     Y      |            puRine             |
# |   Y    |        CT         |     R      |          pYrimidine           |
# |   B    |        CGT        |     V      |    not A (B comes after A)    |
# |   D    |        AGT        |     H      |    not C (D comes after C)    |
# |   H    |        ACT        |     D      |    not G (H comes after G)    |
# |   V    |        ACG        |     B      | not T (V comes after T and U) |
# |   N    |       ACGT        |     N      |  any Nucleotide (not a gap)   |
# |   Z    |                   |     Z      |             Zero              |


DNA_ALPHABET <-
  c("A", "C", "G", "T", "N", "-", "+", ".")
DNA_IUPAC_ALPHABET <-
  c("A", "C", "G", "T", "M", "R", "W", "S", "Y", "K",
    "V", "H", "D", "B", "N", "-", "+", ".")

# DNA_ALPHABET %>% stringr::str_c(collapse = "") %>% paste0("[", ., "]")
REGEX_DNA_ALPHABET <- "(?i)[ACGTN\\-\\+\\.]"
REGEX_DNA_IUPAC_ALPHABET <- "(?i)[ACGTMRWSYKVHDBN\\-\\+\\.]"
REGEX_NOT_DNA_IUPAC_ALPHABET <- "(?i)[^ACGTMRWSYKVHDBN\\-\\+\\.]"

RNA_ALPHABET <-
  c("A", "C", "G", "U", "N", "-", "+", ".")
RNA_IUPAC_ALPHABET <-
  c("A", "C", "G", "U", "M", "R", "W", "S", "Y", "K",
    "V", "H", "D", "B", "N", "-", "+", ".")
REGEX_RNA_ALPHABET <- "(?i)[ACGUN\\-\\+\\.]"
REGEX_RNA_IUPAC_ALPHABET <- "(?i)[ACGUMRWSYKVHDBN\\-\\+\\.]"
REGEX_NOT_RNA_IUPAC_ALPHABET <- "(?i)[^ACGUMRWSYKVHDBN\\-\\+\\.]"

