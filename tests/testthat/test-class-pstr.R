context("test-class-pstr")

as_c <- c("pstr", "bstr", "character")
str1 <- c("ATGC")
str2 <- c("ATGZ")

test_that("pstr()", {
  expect_identical(
    pstr(str1, ucase = T),
    structure(str1, names = "no name 1", class = as_c)
  )

  expect_error(pstr(str2))
  expect_error(pstr(list("a")))
})

test_that("is_pstr()", {
  expect_true(is_pstr(pstr("")))
  expect_true(is_pstr(structure("", class = "pstr")))
  expect_false(is_pstr(structure("", class = "ast")))
})

test_that("as_pstr()", {
  expect_identical(
    as_pstr(str1),
    pstr(str1)
  )
})
