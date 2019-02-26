context("test-class-bstr")

bs_c <- c("bstr", "character")

test_that("bstr()", {
  expect_identical(
    bstr("hoge", ucase = T),
    structure("HOGE", names = "no name 1", class = bs_c)
  )

  expect_error(bstr(letters[1:5], c(NA, NULL, "Z")))

  # bstr("", "")
  # bstr(letters[1:3], c("", NULL, NA))
  # bstr(letters[1:3], c("", NULL))
  # bstr(letters[1], NULL)

  expect_error(bstr(1))
  expect_error(bstr(list("a")))
})

test_that("is_bstr()", {
  expect_true(is_bstr(bstr("")))
  expect_true(is_bstr(structure("", class = "bstr")))
  expect_false(is_bstr(structure("", class = "bst")))
})

test_that("as_bstr()", {
  expect_identical(
    as_bstr("hoge"),
    bstr("hoge")
  )
})
