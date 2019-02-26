
<!-- README.md is generated from README.Rmd. Please edit that file -->
bstringr
========

[![Travis build status](https://travis-ci.org/t-arae/bstringr.svg?branch=master)](https://travis-ci.org/t-arae/bstringr) [![Coverage status](https://codecov.io/gh/t-arae/bstringr/branch/master/graph/badge.svg)](https://codecov.io/github/t-arae/bstringr?branch=master)

The goal of bstringr is to ...

Installation
------------

``` r
devtools::install_github("araezopsis/bstringr")
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
#> [1]   No name sequence  : A                                                            
#> [2]   No name sequence  : B                                                            
#> [3]   No name sequence  : C                                                            
#> [4]   No name sequence  : D                                                            
#> [5]   No name sequence  : E

class(temp)
#> [1] "bstr"      "character"

# dstr-class
dstr("ATGC")
#> class: dstr,bstr,character 
#> number of sequences: 1 
#> [1]   No name sequence  : ATGC

# dstr("E")
# Error in dstr("E") : input contains NOT DNA character
```

### FASTA file IO

``` r
inf <- system.file("extdata", package = "bstringr") %>% list.files(full.names = T)
readLines(inf)
#> [1] ">TEST1"        "This is test."

# Read fasta file
(test_fa <- read_fasta(inf))
#> class: bstr,character 
#> number of sequences: 1 
#> [1]        TEST1        : This is test.

# Write fasta file
test_fa %>% write_fasta(width = 5) %>% paste(collapse = "\n") %>% cat
#> >TEST1
#> This 
#> is te
#> st.
```

### Functions for bstr

``` r
# Degapping
test_fa %>% bstr_degap(gap_chr = " ")
#> class: bstr,character 
#> number of sequences: 1 
#> [1]        TEST1        : Thisistest.
```

### Interface to the Biostrings::BStringSet-class

``` r
# bstr-class -> BioStringSet-class
test_fa %>% bstr2BioString()
#>   A BStringSet instance of length 1
#>     width seq                                          names               
#> [1]    13 This is test.                                TEST1

# BioStringSet -> bstr-class
test_fa %>% bstr2BioString() %>% Bio2bstr()
#> class: bstr,character 
#> number of sequences: 1 
#> [1]        TEST1        : This is test.
```
