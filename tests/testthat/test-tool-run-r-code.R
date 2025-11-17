test_that("run_r_code returns a tool result and computes 1+1", {
  res <- btwExtra_tool_env_run_r_code("1 + 1")
  expect_true(any(grepl("BtwExtraToolResult", class(res))))
  expect_match(res@value, "2")
  expect_equal(res@extra$data$status, "ok")
})

test_that("run_r_code truncates long output when requested", {
  code <- "cat(paste0(1:50, collapse = '\\n'))"
  res <- btwExtra_tool_env_run_r_code(code, max_output_lines = 5)
  expect_true(any(grepl("BtwExtraToolResult", class(res))))
  expect_match(res@value, "Showing the first 5 lines", ignore.case = TRUE)
  expect_equal(res@extra$data$max_output_lines, 5)
})

test_that("run_r_code can return full output when max_output_lines = -1", {
  code <- "cat(paste0(1:10, collapse = '\\n'))"
  res <- btwExtra_tool_env_run_r_code(code, max_output_lines = -1)
  expect_true(any(grepl("BtwExtraToolResult", class(res))))
  expect_match(res@value, "\\b10\\b")
  expect_equal(res@extra$data$max_output_lines, -1)
})

test_that("run_r_code returns an error payload on failure", {
  res <- btwExtra_tool_env_run_r_code("stop('boom')")
  expect_true(any(grepl("BtwExtraToolResult", class(res))))
  expect_match(res@value, "Error executing R code")
  expect_equal(res@extra$data$status, "error")
})
