context("test-bstr-methods")

test_that("bstr_sub()", {
  expect_equal(
    bstr_sub(c("ABCDE", "A"), 2, 5),
    bstr(c("BCDE", ""))
    )
})

test_that("bstr_sub_true()",{
  expect_equal(
    bstr_sub_true(c("-ABC--DE-F", "ABCDEFGH"), 1, 1, 6),
    bstr(c("ABC--DE-F", "BCDEFGH"))
  )
})

test_that("bstr_remove_all()",{
  expect_equal(
    bstr_remove_all(c("-ABC--DE-F", "ABCDEFGH"), "[-GH]"),
    bstr(c("ABCDEF", "ABCDEF"))
  )
})

test_that("bstr_degap()",{
  expect_equal(
    bstr_degap(c("-ABC--DE-F", "ABCDEFGH")),
    bstr(c("ABCDEF", "ABCDEFGH"))
  )
})

test_that("bstr_reverse()",{
  expect_equal(
    bstr_reverse(dstr(c("ATG-C", "GT--C"))),
    dstr(c("C-GTA", "C--TG"))
  )
})
