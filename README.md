
<!-- README.md is generated from README.Rmd. Please edit that file -->
bstringr
========

[![Travis build status](https://travis-ci.org/araezopsis/bstringr.svg?branch=master)](https://travis-ci.org/araezopsis/bstringr) [![Coverage status](https://codecov.io/gh/araezopsis/bstringr/branch/master/graph/badge.svg)](https://codecov.io/github/araezopsis/bstringr?branch=master)

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
#> [1]      no name 1      : A                                                       1 
#> [2]      no name 2      : B                                                       1 
#> [3]      no name 3      : C                                                       1 
#> [4]      no name 4      : D                                                       1 
#> [5]      no name 5      : E                                                       1

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
#> [1] ">TEST1"        "This is test."

# Read fasta file
(test_fa <- read_fasta(inf))
#> class: bstr,character 
#> number of sequences: 1 
#> [1]        TEST1        : This is test.                                           13

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
#> [1]        TEST1        : Thisistest.                                             11
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
#> [1]        TEST1        : This is test.                                           13
```

### 関数対応状況

第一引数は文字列ベクトル型のオブジェクト。複数の文字列に対応 bstr(), dstr(), astr() as\_bstr(), as\_dstr(), as\_astr()

第一引数は任意のオブジェクト。 is\_bstr(), is\_dstr(), is\_astr()

-   \[ \] hage
-   \[x\] hige

### IO

### bstr-class

-   bstr\_length() \[OK\]
-   bstr\_sort() sort bstr strings
-   bstr\_detect() detect pattern from strings
-   bstr\_count() count pattern from strings
-   bstr\_locate() locate pattern from strings
-   bstr\_extract() extract pattern from strings
-   info
-   to\_lower小文字
-   to\_upper 大文字
-   toggle\_case
-   reverse 逆順
-   remove 文字列から任意の文字(列)を除く
-   remove\_number 文字列から数字を除く
-   remove\_notalpha 文字列からアルファベット以外を除く
-   replace 文字列から任意の文字(列)を除く
-   composition | base\_composition | residue\_composition
-   sub
-   trim\_left
-   trim\_right

-   align
-   sub\_aligned
-   degap | remove\_gap
-   calc\_identity
-   calc\_conservation
-   calc\_consensus | calc\_PSSM
-   calc\_occupancy

-   bstr2BString
-   BString2bstr

### dstr-class

-   to\_rna
-   to\_dna
-   complement
-   reverse\_complement
-   translate
-   trim\_stop
-   find\_orf

-   calc\_molweight
-   calc\_Tm

-   restriction\_map
-   digest
-   ligate

-   pcr
-   calc\_gcper
-   primer\_check
-   topo
-   lr
-   topo\_lr

### astr-class

-   calc\_molweight
