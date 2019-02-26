context("test-bstr-methods-trans")

test_that("bstr_to_lower()",{
  expect_equal(
    bstr_to_lower(bstr(c("ATG-C", "GT--C"))),
    bstr(c("atg-c", "gt--c"))
  )
})

test_that("bstr_to_upper()",{
  expect_equal(
    bstr_to_upper(bstr(c("atg-C", "Gt--c"))),
    bstr(c("ATG-C", "GT--C"))
  )
})

test_that("bstr_switch_case()",{
  expect_equal(
    bstr_switch_case(bstr(c("atg-C", "Gt--c"))),
    bstr(c("ATG-c", "gT--C"))
  )
})

test_that("bstr_reverse()",{
  expect_equal(
    bstr_reverse(bstr(c("ATG-C", "GT--C"))),
    bstr(c("C-GTA", "C--TG"))
  )
})
