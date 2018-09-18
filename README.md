
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

``` r
# bstr object
temp <- bstr(letters[1:5])
print.default(temp)
#> No name sequence No name sequence No name sequence No name sequence 
#>              "A"              "B"              "C"              "D" 
#> No name sequence 
#>              "E" 
#> attr(,"class")
#> [1] "bstr"      "character"

# print method for bstr
print(temp)
#> class: bstr,character 
#> length: 5 
#> 1        No name sequence       : A                                                  
#> 2        No name sequence       : B                                                  
#> 3        No name sequence       : C                                                  
#> 4        No name sequence       : D                                                  
#> 5        No name sequence       : E

# c method for bstr
c(temp, temp)
#> class: bstr,character 
#> length: 10 
#> 1        No name sequence       : A                                                  
#> 2        No name sequence       : B                                                  
#> 3        No name sequence       : C                                                  
#> 4        No name sequence       : D                                                  
#> 5        No name sequence       : E                                                  
#> 6        No name sequence       : A

# [ method for bstr
temp[c(1,2,4)]
#> class: bstr,character 
#> length: 3 
#> 1        No name sequence       : A                                                  
#> 2        No name sequence       : B                                                  
#> 3        No name sequence       : D
rev(temp)
#> class: bstr,character 
#> length: 5 
#> 1        No name sequence       : E                                                  
#> 2        No name sequence       : D                                                  
#> 3        No name sequence       : C                                                  
#> 4        No name sequence       : B                                                  
#> 5        No name sequence       : A
head(temp)
#> class: bstr,character 
#> length: 5 
#> 1        No name sequence       : A                                                  
#> 2        No name sequence       : B                                                  
#> 3        No name sequence       : C                                                  
#> 4        No name sequence       : D                                                  
#> 5        No name sequence       : E
tail(temp)
#> class: bstr,character 
#> length: 5 
#> 1        No name sequence       : A                                                  
#> 2        No name sequence       : B                                                  
#> 3        No name sequence       : C                                                  
#> 4        No name sequence       : D                                                  
#> 5        No name sequence       : E
```

``` r
dstr("ATGC")
#> class: dstr,bstr,character 
#> length: 1 
#> 1        No name sequence       : ATGC
```
