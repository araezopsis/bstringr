context("test-dstr-methods")

test_that("dstr_remove_stop()", {
  expect_equal(
    dstr_remove_stop(c("ATGTGA", "TAGATAG", "ATAA")),
    dstr(c("ATG", "TAGA", "A"))
  )
})

test_that("dstr_complement()", {
  expect_equal(
    dstr_complement(c("ATGC", "GCT-A")),
    dstr(c("TACG", "CGA-T"))
  )

  expect_equal(
    dstr_complement(c("ATgc", "gCT-a")),
    dstr(c("TAcg", "cGA-t"))
  )
})

test_that("dstr_rev_comp()", {
  expect_equal(
    dstr_rev_comp(c("ATGC", "GCT-A")),
    dstr(c("GCAT", "T-AGC"))
  )
})

test_that("dstr_traslate()", {
  expect_equal(
    dstr_translate(c("atgTGa")),
    astr(c("M*"))
  )
})
