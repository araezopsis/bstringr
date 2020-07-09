context("test-bstr-methods")

test_that("bstr_sub()", {
  expect_equal(
    bstr_sub(c("ABCDE", "A"), 2, 5),
    bstr(c("BCDE", ""), paste0("no name ", c("1 2-5", "2 2-2")))
    )
})

test_that("bstr_sub_true()",{
  expect_equal(
    bstr_sub_true(c("-ABC--DE-F", "ABCDEFGH"), 1, 1, 6),
    bstr(c("ABC--DE-F", "BCDEFGH"))
  )
})

test_that("bstr_remove()",{
  expect_equal(
    bstr_remove(c("-ABC--DE-F", "ABCDEFGH"), "[-GH]"),
    bstr(c("ABCDEF", "ABCDEF"))
  )

  expect_equal(
    bstr_remove("AaBb", "[Ab]", case_sensitive = T),
    bstr("aB")
  )
})

test_that("bstr_remove_gap()",{
  expect_equal(
    bstr_remove_gap(c("-ABC--DE-F", "ABCDEFGH")),
    bstr(c("ABCDEF", "ABCDEFGH"))
  )
})

