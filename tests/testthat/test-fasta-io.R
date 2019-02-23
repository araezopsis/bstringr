context("test-fasta-io")

test_file_path <- system.file("extdata", "test.fas", package = "bstringr")
test_seq <- read_fasta(test_file_path)

test_that("read_fasta()", {
  expect_equal(length(test_seq), 4)
  expect_equal(
    test_seq[1],
    bstr("This is test.", "TEST", F)
  )
})

test_that("write_fasta()", {
  expect_equal(
    write_fasta(test_seq[1]),
    c(">TEST", "This is test.")
  )
})
