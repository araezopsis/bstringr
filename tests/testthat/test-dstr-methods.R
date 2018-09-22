context("test-dstr-methods")

test_that("dstr_trim_stop()", {
  expect_equal(
    dstr_trim_stop(c("ATGTGA", "TAGATAG", "ATAA")),
    dstr(c("ATG", "TAGA", "A"))
  )
})

test_that("dstr_complement()", {
  expect_equal(
    dstr_complement(c("ATGC", "GCT-A")),
    dstr(c("TACG", "CGA-T"))
  )
})

test_that("dstr_rc()", {
  expect_equal(
    dstr_rc(c("ATGC", "GCT-A")),
    dstr(c("GCAT", "T-AGC"))
  )
})

# test_that("dstr_traslate()", {
#   expect_equal(
#     dstr_translate(c("ATGC", "GCT-A")),
#     dstr(c("GCAT", "T-AGC"))
#   )
# })
