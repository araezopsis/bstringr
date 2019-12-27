
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

AA_WEIGHT <-
  c(
    "A" = 71.078,
    "R" = 156.186,
    "N" = 114.103,
    "D" = 115.087,
    "C" = 103.143,
    "Q" = 128.173,
    "E" = 129.114,
    "G" = 57.051,
    "H" = 137.139,
    "I" = 113.158,
    "L" = 113.158,
    "K" = 128.172,
    "M" = 131.196,
    "F" = 147.174,
    "P" = 97.115,
    "S" = 87.077,
    "T" = 101.104,
    "W" = 186.21,
    "Y" = 163.173,
    "V" = 99.131
    )

AMINO_ACID <-
  c(
    "A" = "Ala",
    "R" = "Arg",
    "N" = "Asn",
    "D" = "Asp",
    "C" = "Cys",
    "Q" = "Gln",
    "E" = "Glu",
    "G" = "Gly",
    "H" = "His",
    "I" = "Ile",
    "L" = "Leu",
    "K" = "Lys",
    "M" = "Met",
    "F" = "Phe",
    "P" = "Pro",
    "S" = "Ser",
    "T" = "Thr",
    "W" = "Trp",
    "Y" = "Tyr",
    "V" = "Val"
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

