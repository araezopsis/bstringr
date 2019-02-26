context("test-class-astr")

as_c <- c("astr", "bstr", "character")
str1 <- c("ATGC")
str2 <- c("ATGZ")

test_that("astr()", {
  expect_identical(
    astr(str1, ucase = T),
    structure(str1, names = "no name 1", class = as_c)
  )

  expect_error(astr(str2))
  expect_error(astr(list("a")))
})

test_that("is_astr()", {
  expect_true(is_astr(astr("")))
  expect_true(is_astr(structure("", class = "astr")))
  expect_false(is_astr(structure("", class = "ast")))
})

test_that("as_astr()", {
  expect_identical(
    as_astr(str1),
    astr(str1)
  )
})
