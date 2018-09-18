context("test-fasta-io")

test_file_path <- system.file("extdata", "aa_test.fas", package = "bioseqr")
# write_fasta(bstr("This is test.", "TEST1", F), test_file_path)

test_that("read_fasta()", {
  expect_equal(
    read_fasta(test_file_path),
    bstr("This is test.", "TEST1", F)
  )
})

test_that("write_fasta()", {
  expect_equal(
    write_fasta(read_fasta(test_file_path)),
    c(">TEST1", "This is test.")
  )
})
