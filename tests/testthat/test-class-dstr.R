context("test-class-dstr")

ds_c <- c("dstr", "bstr", "character")
str1 <- c("ATGC")
str2 <- c("ATGZ")

test_that("dstr()", {
  expect_identical(
    dstr(str1, ucase = T),
    structure(str1, names = "no name 1", class = ds_c)
  )

  expect_error(dstr(str2))
  expect_error(dstr(list("a")))
})

test_that("is_dstr()", {
  expect_true(is_dstr(dstr("")))
  expect_true(is_dstr(structure("", class = "dstr")))
  expect_false(is_dstr(structure("", class = "dst")))
})

test_that("as_dstr()", {
  expect_identical(
    as_dstr(str1),
    dstr(str1)
  )
})
