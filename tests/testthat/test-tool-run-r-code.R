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

test_that("run_r_code captures a plot as base64 media", {
  res <- btwExtra_tool_env_run_r_code("plot(1:3)")
  plot_data <- res@extra$data$plot

  expect_true(any(grepl("BtwExtraToolResult", class(res))))
  expect_type(plot_data$data, "character")
  expect_true(nchar(plot_data$data) > 0)
  expect_equal(plot_data$mime, "image/png")
  expect_true(fs::file_exists(plot_data$path))
  expect_match(res@value, "Plot captured \\(PNG\\)")

  display <- res@extra$display$content
  expect_equal(display[[2]]$type, "media")
  expect_equal(display[[2]]$mediaType, "image/png")
})

test_that("run_r_code can skip plot capture when requested", {
  if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")

  res <- btwExtra_tool_env_run_r_code("plot(1:3)", capture_plot = FALSE)

  expect_true(any(grepl("BtwExtraToolResult", class(res))))
  expect_false(any(grepl("Plot captured", res@value)))
  expect_null(res@extra$data$plot)
  expect_false(file.exists("Rplots.pdf"))
})
