test_that(".btwExtra_check_string works", {
  expect_invisible(.btwExtra_check_string("x"))
  expect_error(.btwExtra_check_string(1), "must be a single string")
  expect_error(.btwExtra_check_string(NULL), "must be a single string")
})

test_that(".btwExtra_check_character works", {
  expect_invisible(.btwExtra_check_character(c("a", "b")))
  expect_invisible(.btwExtra_check_character(NULL, allow_null = TRUE))
  expect_error(.btwExtra_check_character(1), "must be a character vector")
})

test_that(".btwExtra_check_number works", {
  expect_invisible(.btwExtra_check_number(1))
  expect_invisible(.btwExtra_check_number(NULL, allow_null = TRUE))
  expect_error(.btwExtra_check_number("a"), "must be a single numeric value")
})
