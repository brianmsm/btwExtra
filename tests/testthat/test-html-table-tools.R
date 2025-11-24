test_that("html_table_to_df extracts from a plain data frame", {
  df <- head(mtcars)
  assign("tbl_plain_df", df, envir = .GlobalEnv)
  on.exit(rm("tbl_plain_df", envir = .GlobalEnv), add = TRUE)

  res <- btwExtra_tool_html_table_to_df("tbl_plain_df")

  expect_true(any(grepl("BtwExtraToolResult", class(res))))
  expect_equal(res@extra$data$n_rows, nrow(df))
  expect_equal(res@extra$data$n_cols, ncol(df))
  expect_s3_class(res@extra$data$data, "data.frame")
})

test_that("html_table_to_df falls back to parsing HTML", {
  skip_if_not_installed("rvest")

  html_tbl <- htmltools::tags$table(
    htmltools::tags$thead(
      htmltools::tags$tr(
        htmltools::tags$th("col1"),
        htmltools::tags$th("col2")
      )
    ),
    htmltools::tags$tbody(
      htmltools::tags$tr(
        htmltools::tags$td("a"),
        htmltools::tags$td("1")
      ),
      htmltools::tags$tr(
        htmltools::tags$td("b"),
        htmltools::tags$td("2")
      )
    )
  )

  assign("tbl_htmltools", html_tbl, envir = .GlobalEnv)
  on.exit(rm("tbl_htmltools", envir = .GlobalEnv), add = TRUE)

  res <- btwExtra_tool_html_table_to_df("tbl_htmltools")
  df <- res@extra$data$data

  expect_true(any(grepl("BtwExtraToolResult", class(res))))
  expect_equal(res@extra$data$method, "rvest::html_table")
  expect_equal(nrow(df), 2)
  expect_equal(ncol(df), 2)
})

test_that("html_table_screenshot captures a PNG", {
  skip_if_not_installed("webshot2")
  skip_if_not_installed("chromote")

  chrome_available <- tryCatch(
    {
      path <- chromote::find_chrome()
      length(path) > 0 && !is.na(path)
    },
    error = function(e) FALSE
  )
  if (!chrome_available) {
    skip("Headless Chrome not available for webshot2.")
  }

  html_tbl <- htmltools::tags$table(
    htmltools::tags$thead(
      htmltools::tags$tr(
        htmltools::tags$th("col1"),
        htmltools::tags$th("col2")
      )
    ),
    htmltools::tags$tbody(
      htmltools::tags$tr(
        htmltools::tags$td("a"),
        htmltools::tags$td("1")
      )
    )
  )

  assign("tbl_htmltools_shot", html_tbl, envir = .GlobalEnv)
  on.exit(rm("tbl_htmltools_shot", envir = .GlobalEnv), add = TRUE)

  res <- btwExtra_tool_html_table_screenshot("tbl_htmltools_shot")
  payload <- res@extra$data$screenshot

  expect_true(any(grepl("BtwExtraToolResult", class(res))))
  expect_equal(payload$mime, "image/png")
  expect_true(nchar(payload$data) > 0)
  expect_true(fs::file_exists(payload$path))

  display <- res@extra$display$content
  expect_equal(display[[2]]$type, "media")
  expect_equal(display[[2]]$mediaType, "image/png")
})

test_that("html_table_screenshot treats empty selector as default", {
  skip_if_not_installed("webshot2")
  skip_if_not_installed("chromote")
  skip_if_not_installed("jsonlite")

  html_tbl <- htmltools::tags$table(
    htmltools::tags$tr(
      htmltools::tags$td("a")
    )
  )

  assign("tbl_htmltools_shot_empty", html_tbl, envir = .GlobalEnv)
  on.exit(rm("tbl_htmltools_shot_empty", envir = .GlobalEnv), add = TRUE)

  res <- btwExtra_tool_html_table_screenshot("tbl_htmltools_shot_empty", selector = "")
  payload <- res@extra$data$screenshot

  expect_true(any(grepl("BtwExtraToolResult", class(res))))
  expect_equal(payload$mime, "image/png")
  expect_true(fs::file_exists(payload$path))
})

test_that("html_table_to_df extracts reactable data from embedded JSON", {
  skip_if_not_installed("jsonlite")

  df <- head(mtcars)
  reactable_like <- list(
    x = list(tag = list(attribs = list(data = jsonlite::toJSON(df)))),
    tag = list(attribs = list(data = jsonlite::toJSON(df)))
  )
  class(reactable_like) <- c("reactable", "htmlwidget")

  assign("tab_reactable_like", reactable_like, envir = .GlobalEnv)
  on.exit(rm("tab_reactable_like", envir = .GlobalEnv), add = TRUE)

  res <- btwExtra_tool_html_table_to_df("tab_reactable_like")
  extracted <- res@extra$data$data

  expect_true(any(grepl("BtwExtraToolResult", class(res))))
  expect_equal(res@extra$data$method, "reactable data")
  expect_equal(dim(extracted), dim(df))
  expect_equal(colnames(extracted), colnames(df))
})

test_that("html_table_to_df extracts flextable dataset", {
  skip_if_not_installed("flextable")

  ft <- flextable::flextable(head(mtcars))
  assign("tab_flextable", ft, envir = .GlobalEnv)
  on.exit(rm("tab_flextable", envir = .GlobalEnv), add = TRUE)

  res <- btwExtra_tool_html_table_to_df("tab_flextable")
  df <- res@extra$data$data

  expect_true(any(grepl("BtwExtraToolResult", class(res))))
  expect_equal(res@extra$data$method, "flextable body dataset")
  expect_equal(nrow(df), 6)
  expect_true(all(colnames(df) %in% colnames(mtcars)))
})
