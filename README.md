
<!-- README.md is generated from README.Rmd. Please edit that file -->
bstringr
========

[![Travis build status](https://travis-ci.org/t-arae/bstringr.svg?branch=master)](https://travis-ci.org/t-arae/bstringr) [![Coverage status](https://codecov.io/gh/t-arae/bstringr/branch/master/graph/badge.svg)](https://codecov.io/github/t-arae/bstringr?branch=master)

The goal of bstringr is to ...

Installation
------------

``` r
# install.packages("devtools")
devtools::install_github("t-arae/bstringr")
```

Usage
-----

``` r
library(bstringr)
```

### bstr-class object

``` r
# bstr object
(temp <- bstr(letters[1:5]))
#> class: bstr,character 
#> number of sequences: 5 
#> [1]      no name 1      : a                                                       1 
#> [2]      no name 2      : b                                                       1 
#> [3]      no name 3      : c                                                       1 
#> [4]      no name 4      : d                                                       1 
#> [5]      no name 5      : e                                                       1

class(temp)
#> [1] "bstr"      "character"

# dstr-class
dstr("ATGC")
#> class: dstr,bstr,character 
#> number of sequences: 1 
#> [1]      no name 1      : ATGC                                                    4

# dstr("E")
# Error in dstr("E") : input contains NOT DNA character
```

### FASTA file IO

``` r
inf <- system.file("extdata", package = "bstringr") %>% list.files(full.names = T)
readLines(inf)
#>  [1] ">TEST"                                                                       
#>  [2] "This is test."                                                               
#>  [3] ""                                                                            
#>  [4] ">test1"                                                                      
#>  [5] "AAAAAAAAAA AAAAAAAAAA AAAAAAAAAA AAAAAAAAAA AAAAAAAAAA AAAAAAAAAA AAAAAAAAAA"
#>  [6] ""                                                                            
#>  [7] ">test2"                                                                      
#>  [8] "AAAAAAAAAA"                                                                  
#>  [9] "AAAAAAAAAA"                                                                  
#> [10] ""                                                                            
#> [11] ">test3"                                                                      
#> [12] "ATGC"

# Read fasta file
(test_fa <- read_fasta(inf))
#> class: bstr,character 
#> number of sequences: 4 
#> [1]         TEST        : This is test.                                           13 
#> [2]        test1        : AAAAAAAAAA AAAAAAAAAA AAA....AAA AAAAAAAAAA AAAAAAAAAA  76 
#> [3]        test2        : AAAAAAAAAAAAAAAAAAAA                                    20 
#> [4]        test3        : ATGC                                                     4

# Write fasta file
test_fa %>% write_fasta(width = 5) %>% paste(collapse = "\n") %>% cat
#> >TEST
#> This 
#> is te
#> st.
#> >test1
#> AAAAA
#> AAAAA
#>  AAAA
#> AAAAA
#> A AAA
#> AAAAA
#> AA AA
#> AAAAA
#> AAA A
#> AAAAA
#> AAAA 
#> AAAAA
#> AAAAA
#>  AAAA
#> AAAAA
#> A
#> >test2
#> AAAAA
#> AAAAA
#> AAAAA
#> AAAAA
#> >test3
#> ATGC
```

### Functions for bstr

``` r
# Degapping
test_fa %>% bstr_remove_gap(gap_chr = " ")
#> class: bstr,character 
#> number of sequences: 4 
#> [1]         TEST        : Thisistest.                                             11 
#> [2]        test1        : AAAAAAAAAAAAAAAAAAAAAAAAA....AAAAAAAAAAAAAAAAAAAAAAAAA  70 
#> [3]        test2        : AAAAAAAAAAAAAAAAAAAA                                    20 
#> [4]        test3        : ATGC                                                     4
```

### Interface to the Biostrings::BStringSet-class

``` r
# bstr-class -> BioStringSet-class
test_fa %>% bstr2BioString()
#>   A BStringSet instance of length 4
#>     width seq                                          names               
#> [1]    13 This is test.                                TEST
#> [2]    76 AAAAAAAAAA AAAAAAAAAA...AAAAAAAAA AAAAAAAAAA test1
#> [3]    20 AAAAAAAAAAAAAAAAAAAA                         test2
#> [4]     4 ATGC                                         test3

# BioStringSet -> bstr-class
test_fa %>% bstr2BioString() %>% Bio2bstr()
#> class: bstr,character 
#> number of sequences: 4 
#> [1]         TEST        : This is test.                                           13 
#> [2]        test1        : AAAAAAAAAA AAAAAAAAAA AAA....AAA AAAAAAAAAA AAAAAAAAAA  76 
#> [3]        test2        : AAAAAAAAAAAAAAAAAAAA                                    20 
#> [4]        test3        : ATGC                                                     4
```
