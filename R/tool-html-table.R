#' Tool: HTML table to data frame
#'
#' @description
#' Extract the content of an HTML-style table object already present in the
#' connected R session and expose it as a data frame (and JSON-friendly list).
#' Use this when you care about the table *values* (rows/columns/missingness),
#' not styling. For styling/layout, use the screenshot tool instead.
#'
#' @param object_name Name of the object in the current R session that holds
#'   the HTML table (e.g. `\"tbl_gt\"`).
#' @param max_preview_rows Maximum number of rows to show in the textual
#'   preview. Use `0` to disable the preview. Defaults to 8.
#' @param _intent Optional free-text intent (automatically injected when called
#'   via ellmer tools; can be left empty when calling directly).
#' @export
btwExtra_tool_html_table_to_df <- function(object_name,
                                           max_preview_rows = 8L,
                                           `_intent` = "") {}

btwExtra_tool_html_table_to_df_impl <- function(object_name,
                                                max_preview_rows = 8L) {
  .btwExtra_check_string(object_name)
  .btwExtra_check_number(max_preview_rows)

  obj <- .btwExtra_get_object(object_name)

  extraction <- .btwExtra_extract_table_df(obj)
  df <- extraction$data

  preview_n <- max(0L, as.integer(max_preview_rows))
  preview <- if (preview_n > 0L) utils::head(df, n = preview_n) else NULL
  preview_lines <- if (preview_n > 0L) utils::capture.output(print(preview)) else character()

  n_rows <- nrow(df)
  n_cols <- ncol(df)

  header <- sprintf(
    "Extracted HTML table data from `%s` (%s rows x %s cols) using %s.",
    object_name, n_rows, n_cols, extraction$method
  )

  value_text <- if (length(preview_lines)) {
    paste0(
      header,
      "\n\nPreview (first ", min(preview_n, n_rows), " rows):\n",
      paste(preview_lines, collapse = "\n")
    )
  } else {
    header
  }

  btwExtra_tool_result(
    value = value_text,
    data = list(
      object_name = object_name,
      classes = class(obj),
      method = extraction$method,
      n_rows = n_rows,
      n_cols = n_cols,
      data = df,
      preview = preview
    )
  )
}

#' Tool: HTML table screenshot
#'
#' @description
#' Capture a PNG screenshot of an HTML-style table object for visual inspection
#' (headers, alignment, zebra stripes, footnotes, etc.). Use this when you care
#' about layout/styling; use `btwExtra_tool_html_table_to_df` when you care
#' about data content.
#'
#' @param object_name Name of the object in the current R session that holds
#'   the HTML table (e.g. `\"tbl_gt\"`).
#' @param selector Optional CSS selector to target a specific table element;
#'   defaults to the first `<table>` tag.
#' @param _intent Optional free-text intent (automatically injected when called
#'   via ellmer tools; can be left empty when calling directly).
#' @export
btwExtra_tool_html_table_screenshot <- function(object_name,
                                                selector = "table",
                                                `_intent` = "") {}

btwExtra_tool_html_table_screenshot_impl <- function(object_name,
                                                     selector = "table") {
  .btwExtra_check_string(object_name)
  .btwExtra_check_string(selector)

  obj <- .btwExtra_get_object(object_name)

  html_file <- .btwExtra_render_table_html(obj)
  png_file <- as.character(fs::file_temp("btwExtra-table-", ext = ".png"))

  screenshot_ok <- tryCatch(
    {
      if (!requireNamespace("webshot2", quietly = TRUE)) {
        cli::cli_abort("Package {.pkg webshot2} is required to capture screenshots.")
      }

      webshot2::webshot(
        url = html_file,
        file = png_file,
        selector = selector
      )
      TRUE
    },
    error = function(e) {
      cli::cli_abort("Could not capture screenshot: {conditionMessage(e)}")
    }
  )

  if (!isTRUE(screenshot_ok) || !fs::file_exists(png_file)) {
    cli::cli_abort("Screenshot file was not created.")
  }

  img_payload <- .btwExtra_plot_payload(
    path = png_file,
    width = NA,
    height = NA,
    res = NA
  )

  value_text <- sprintf(
    "Table screenshot captured from `%s` (saved at %s).",
    object_name, png_file
  )

  btwExtra_tool_result(
    value = value_text,
    data = list(
      object_name = object_name,
      classes = class(obj),
      selector = selector,
      html_file = html_file,
      screenshot = img_payload
    ),
    display = .btwExtra_plot_display(value_text, img_payload)
  )
}

.btwExtra_get_object <- function(object_name) {
  if (!exists(object_name, envir = .GlobalEnv, inherits = TRUE)) {
    cli::cli_abort("Object {.val {object_name}} was not found in the selected R session.")
  }
  get(object_name, envir = .GlobalEnv, inherits = TRUE)
}

.btwExtra_extract_table_df <- function(x) {
  as_df <- .btwExtra_try_as_data_frame(x)
  if (!is.null(as_df)) {
    return(list(data = as_df, method = "as.data.frame"))
  }

  handlers <- list(
    .btwExtra_extract_gt_tbl,
    .btwExtra_extract_gtsummary,
    .btwExtra_extract_datatable,
    .btwExtra_extract_reactable
  )

  for (handler in handlers) {
    res <- handler(x)
    if (!is.null(res)) {
      return(res)
    }
  }

  html_res <- .btwExtra_extract_table_via_html(x)
  if (!is.null(html_res)) {
    return(html_res)
  }

  cli::cli_abort(
    "Could not extract a data frame from object of class {.val {paste(class(x), collapse = ' / ')}}."
  )
}

.btwExtra_try_as_data_frame <- function(x) {
  has_method <- is.data.frame(x) ||
    is.matrix(x) ||
    any(vapply(class(x), function(cls) {
      !is.null(utils::getS3method("as.data.frame", cls, optional = TRUE))
    }, logical(1)))

  if (!isTRUE(has_method)) {
    return(NULL)
  }

  df <- tryCatch(
    as.data.frame(x),
    error = function(e) NULL
  )

  if (is.data.frame(df)) df else NULL
}

.btwExtra_extract_gt_tbl <- function(x) {
  if (!inherits(x, "gt_tbl")) {
    return(NULL)
  }

  df <- NULL

  df <- .btwExtra_try_as_data_frame(x)
  if (is.data.frame(df)) {
    return(list(data = df, method = "gt::as.data.frame"))
  }

  df <- x[["_data"]]
  if (is.data.frame(df)) {
    return(list(data = df, method = "gt internal data"))
  }

  NULL
}

.btwExtra_extract_gtsummary <- function(x) {
  if (!inherits(x, "gtsummary")) {
    return(NULL)
  }

  df <- .btwExtra_try_as_data_frame(x)
  if (is.data.frame(df)) {
    return(list(data = df, method = "gtsummary::as.data.frame"))
  }

  if (requireNamespace("gtsummary", quietly = TRUE)) {
    df <- tryCatch(
      {
        tib <- gtsummary::as_tibble(x)
        as.data.frame(tib)
      },
      error = function(e) NULL
    )
    if (is.data.frame(df)) {
      return(list(data = df, method = "gtsummary::as_tibble"))
    }
  }

  NULL
}

.btwExtra_extract_datatable <- function(x) {
  if (!(inherits(x, "datatables") || inherits(x, "datatable"))) {
    return(NULL)
  }

  data <- tryCatch(
    x[["x"]][["data"]],
    error = function(e) NULL
  )
  if (is.null(data)) {
    data <- tryCatch(
      x[["x"]][["origData"]],
      error = function(e) NULL
    )
  }

  if (is.data.frame(data)) {
    return(list(data = data, method = "DT::datatable$data"))
  }

  NULL
}

.btwExtra_extract_reactable <- function(x) {
  if (!inherits(x, "reactable_htmlwidget")) {
    return(NULL)
  }

  data <- tryCatch(
    x[["x"]][["data"]],
    error = function(e) NULL
  )
  if (is.null(data)) {
    data <- tryCatch(
      x[["x"]][["data_raw"]],
      error = function(e) NULL
    )
  }

  if (is.data.frame(data)) {
    return(list(data = data, method = "reactable data"))
  }

  NULL
}

.btwExtra_extract_table_via_html <- function(x) {
  html_file <- tryCatch(
    .btwExtra_render_table_html(x),
    error = function(e) NULL
  )
  if (is.null(html_file) || !fs::file_exists(html_file)) {
    return(NULL)
  }

  if (!requireNamespace("rvest", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg rvest} is required to parse HTML tables. Please install it."
    )
  }

  df <- tryCatch(
    {
      doc <- rvest::read_html(html_file)
      table_node <- rvest::html_element(doc, "table")
      if (is.null(table_node)) {
        return(NULL)
      }
      rvest::html_table(table_node)
    },
    error = function(e) NULL
  )

  if (is.data.frame(df)) {
    return(list(data = df, method = "rvest::html_table", html_file = html_file))
  }

  NULL
}

.btwExtra_render_table_html <- function(x) {
  html_file <- as.character(fs::file_temp("btwExtra-table-", ext = ".html"))

  # Reactable/DT/htmlwidgets classes
  if (inherits(x, "htmlwidget")) {
    if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
      cli::cli_abort("Package {.pkg htmlwidgets} is required to render this table.")
    }
    htmlwidgets::saveWidget(x, file = html_file, selfcontained = TRUE)
    return(html_file)
  }

  # gt: rely on as_raw_html if available
  if (inherits(x, "gt_tbl") && requireNamespace("gt", quietly = TRUE)) {
    html <- tryCatch(gt::as_raw_html(x), error = function(e) NULL)
    if (!is.null(html)) {
      htmltools::save_html(htmltools::HTML(html), file = html_file)
      return(html_file)
    }
  }

  # gtsummary: convert to gt then render
  if (inherits(x, "gtsummary") && requireNamespace("gtsummary", quietly = TRUE)) {
    gt_tbl <- tryCatch(gtsummary::as_gt(x), error = function(e) NULL)
    if (!is.null(gt_tbl)) {
      return(.btwExtra_render_table_html(gt_tbl))
    }
  }

  # htmltools/shiny tags or character HTML
  if (inherits(x, c("shiny.tag", "shiny.tag.list", "html")) || rlang::is_character(x)) {
    html_content <- if (rlang::is_character(x)) {
      htmltools::HTML(paste(x, collapse = "\n"))
    } else {
      x
    }
    htmltools::save_html(html_content, file = html_file)
    return(html_file)
  }

  # Last resort: coerce to HTML via as.character()
  htmltools::save_html(htmltools::HTML(as.character(x)), file = html_file)
  html_file
}

.btwExtra_add_to_tools(
  name = "btwExtra_tool_html_table_to_df",
  group = "env",
  tool = function() {
    ellmer::tool(
      btwExtra_tool_html_table_to_df_impl,
      name = "btwExtra_tool_html_table_to_df",
      description = 'Expose the content of an HTML-style table object as a data frame for reasoning about values (rows/cols/missingness), not styling.

      ## When to use
      - You already have a table object in the R session (e.g. `gt`, `gtsummary`, `DT`, `reactable`, `knitr::kable`).
      - You need the table *content* as a normal data frame / JSON to inspect values or compute summaries.
      - Prefer this over `run_r_code` for simple table inspection; pair with `btw_tool_env_describe_data_frame` if you want column-level stats.

      ## What it returns
      - A compact text preview (up to `max_preview_rows`).
      - `extra$data$data`: the full data frame.
      - Metadata: source object name/classes, extraction method, row/col counts.

      ## Cautions
      - Styling/footnotes/spanners are not preserved; this is for data only.
      - If no direct extractor exists, it renders to HTML and parses the first `<table>` element via `rvest` as a best-effort fallback.
      ',
      annotations = ellmer::tool_annotations(
        title = "HTML Table to Data Frame",
        read_only_hint = TRUE,
        open_world_hint = FALSE,
        idempotent_hint = TRUE,
        btw_can_register = function() TRUE
      ),
      arguments = list(
        object_name = ellmer::type_string(
          "Name of the table object in the current R session (e.g. `tbl_gt`)."
        ),
        max_preview_rows = ellmer::type_integer(
          "Maximum number of rows to show in the textual preview (use 0 to disable).",
          required = FALSE
        )
      )
    )
  }
)

.btwExtra_add_to_tools(
  name = "btwExtra_tool_html_table_screenshot",
  group = "env",
  tool = function() {
    ellmer::tool(
      btwExtra_tool_html_table_screenshot_impl,
      name = "btwExtra_tool_html_table_screenshot",
      description = 'Capture a PNG screenshot of an HTML-style table object when you need to check appearance (headers, alignment, zebra stripes, footnotes).

      ## When to use
      - You care about table styling/layout, not just the raw values.
      - Combine with `btwExtra_tool_html_table_to_df` when you also need the underlying data.
      - Re-run after adjusting the object via `run_r_code` if you change formatting.

      ## What it returns
      - A short text note plus:
      - `extra$data$screenshot`: base64-encoded PNG + temp file path.
      - `extra$display$content`: includes a `media` entry (`image/png`) for clients that can render images directly.

      ## Cautions
      - Requires {webshot2} (and a compatible headless browser via chromote) to capture the screenshot.
      - Captures the first `<table>` by default; override `selector` to target a specific element.
      ',
      annotations = ellmer::tool_annotations(
        title = "HTML Table Screenshot",
        read_only_hint = TRUE,
        open_world_hint = FALSE,
        idempotent_hint = FALSE,
        btw_can_register = function() TRUE
      ),
      arguments = list(
        object_name = ellmer::type_string(
          "Name of the table object in the current R session (e.g. `tbl_gt`)."
        ),
        selector = ellmer::type_string(
          "Optional CSS selector for the table element to capture; defaults to the first `<table>`."
        )
      )
    )
  }
)
