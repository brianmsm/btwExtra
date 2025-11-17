test_that("btwExtra_tools contains the run-r-code tool", {
  tools <- btwExtra_tools()
  names <- vapply(tools, function(t) t@name, character(1))
  expect_true("btwExtra_tool_env_run_r_code" %in% names)
})

test_that("_intent is injected on tools", {
  tools <- btwExtra_tools()
  run_tool <- tools[[which(vapply(tools, function(t) t@name, character(1)) == "btwExtra_tool_env_run_r_code")]]
  expect_true("_intent" %in% names(run_tool@arguments@properties))
})
