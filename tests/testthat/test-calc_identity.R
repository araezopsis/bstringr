context("test-calc_identity")

test_that("calc_ident", {
  expect_equal(calc_ident("ATGC", "ATCC"), 0.75)
  expect_equal(calc_ident("ATG-C", "ATC-C"), 0.75)
  expect_equal(calc_ident("ATG-C", "ATC-C", "."), 0.8)
})
