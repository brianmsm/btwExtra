#' Tool: Run arbitrary R code
#'
#' @description
#' Escape hatch to evaluate R code in the connected session, emulating the R
#' console: each expression is evaluated in order, assignments are not printed,
#' and any visible value is shown. Console output is captured and can be
#' truncated to save context.
#'
#' @param code String with the R code to execute.
#' @param max_output_lines Maximum number of output lines to display; use `-1`
#'   to disable truncation. Defaults to 20.
#' @param capture_plot Whether to capture a plot (PNG) produced by the code.
#'   Defaults to `TRUE`.
#' @param plot_width Width (in pixels) of the PNG device opened to capture the
#'   plot. Defaults to 800.
#' @param plot_height Height (in pixels) of the PNG device opened to capture
#'   the plot. Defaults to 600.
#' @param plot_res Resolution (in DPI) of the PNG device opened to capture the
#'   plot. Defaults to 96.
#' @param _intent Optional free-text intent (automatically injected when called
#'   via ellmer tools; can be left empty when calling directly).
#' @export
btwExtra_tool_env_run_r_code <- function(code,
                                         max_output_lines = 20L,
                                         capture_plot = TRUE,
                                         plot_width = 800,
                                         plot_height = 600,
                                         plot_res = 96,
                                         `_intent` = "") {}

btwExtra_tool_env_run_r_code_impl <- function(code,
                                              max_output_lines = 20L,
                                              capture_plot = TRUE,
                                              plot_width = 800,
                                              plot_height = 600,
                                              plot_res = 96) {
  .btwExtra_check_string(code)
  if (!is.null(max_output_lines)) {
    .btwExtra_check_number(max_output_lines)
  }
  if (!rlang::is_bool(capture_plot)) {
    cli::cli_abort("{.arg capture_plot} must be TRUE or FALSE.")
  }
  if (isTRUE(capture_plot)) {
    .btwExtra_check_number(plot_width)
    .btwExtra_check_number(plot_height)
    .btwExtra_check_number(plot_res)
  }

  exprs <- NULL
  last_val <- NULL
  out <- character()
  plot_file <- NULL
  cleanup_plot <- TRUE
  plot_device <- NULL

  close_plot_device <- function() {
    if (is.null(plot_device)) {
      return(invisible())
    }

    active_devices <- grDevices::dev.list()

    if (is.null(active_devices) || length(active_devices) == 0) {
      return(invisible())
    }

    if (plot_device %in% active_devices) {
      try(utils::capture.output(grDevices::dev.off(plot_device)), silent = TRUE)
    }
  }

  cleanup_plot_file <- function() {
    if (!isTRUE(cleanup_plot)) {
      return(invisible())
    }

    if (!is.null(plot_file) && fs::file_exists(plot_file)) {
      try(unlink(plot_file), silent = TRUE)
    }
  }

  on.exit(close_plot_device(), add = TRUE)
  on.exit(cleanup_plot_file(), add = TRUE)

  res <- tryCatch(
    {
      exprs <- parse(text = code)

      if (length(exprs) == 0L) {
        return(list(
          status = "ok",
          console = character(),
          result_class = NA_character_
        ))
      }

      active_devices_initial <- grDevices::dev.list()

      if (isTRUE(capture_plot)) {
        plot_file <- as.character(fs::file_temp("btwExtra-plot-", ext = ".png"))
        cleanup_plot <- FALSE
        grDevices::png(
          filename = plot_file,
          width = plot_width,
          height = plot_height,
          res = plot_res
        )
        plot_device <- grDevices::dev.cur()
      } else if (is.null(active_devices_initial)) {
        plot_file <- as.character(fs::file_temp("btwExtra-plot-nocapture-", ext = ".png"))
        grDevices::png(
          filename = plot_file,
          width = plot_width,
          height = plot_height,
          res = plot_res
        )
        plot_device <- grDevices::dev.cur()
      }

      out <- utils::capture.output({
        last_val <<- NULL

        for (i in seq_along(exprs)) {
          expr <- exprs[[i]]

          is_assignment_i <- is.call(expr) &&
            as.character(expr[[1L]]) %in% c("<-", "=", "<<-")

          vis <- withVisible(eval(expr, envir = .GlobalEnv))
          last_val <<- vis$value

          if (vis$visible && !is_assignment_i && !is.null(vis$value)) {
            print(vis$value)
          }
        }
      })

      list(
        status = "ok",
        console = out,
        result_class = if (is.null(last_val)) NA_character_
                       else paste(class(last_val), collapse = " / ")
      )
    },
    error = function(e) {
      list(
        status = "error",
        error_message = conditionMessage(e),
        console = out
      )
    }
  )

  if (isTRUE(capture_plot)) {
    close_plot_device()
  }

  plot_data <- NULL

  if (isTRUE(capture_plot)) {
    plot_data <- .btwExtra_plot_payload(
      path = plot_file,
      width = plot_width,
      height = plot_height,
      res = plot_res
    )
  }

  if (!identical(res$status, "ok")) {
    msg_lines <- c(
      paste0("Error executing R code: ", res$error_message),
      if (length(res$console)) c("R console output:", res$console) else character()
    )

    value_text <- paste(msg_lines, collapse = "\n")
    value_text <- .btwExtra_append_plot_hint(value_text, plot_data)

    return(btwExtra_tool_result(
      value = value_text,
      data  = c(res, list(plot = plot_data)),
      display = .btwExtra_plot_display(value_text, plot_data)
    ))
  }

  lines <- res$console

  if (length(lines) == 0L) {
    value_text <- "R code executed successfully (no visible output)."
    value_text <- .btwExtra_append_plot_hint(value_text, plot_data)

    return(btwExtra_tool_result(
      value = value_text,
      data  = c(res, list(plot = plot_data)),
      display = .btwExtra_plot_display(value_text, plot_data)
    ))
  }

  n_lines <- length(lines)

  if (is.null(max_output_lines) || is.na(max_output_lines)) {
    max_output_lines <- 20L
  }

  if (max_output_lines <= 0L) {
    value_text <- paste(lines, collapse = "\n")
    value_text <- .btwExtra_append_plot_hint(value_text, plot_data)

    return(btwExtra_tool_result(
      value = value_text,
      data  = c(res, list(
        n_output_lines   = n_lines,
        max_output_lines = max_output_lines,
        plot             = plot_data
      )),
      display = .btwExtra_plot_display(value_text, plot_data)
    ))
  }

  if (n_lines > max_output_lines) {
    preview <- lines[seq_len(max_output_lines)]

    notice <- .btwExtra_truncation_notice()

    value_text <- paste0(
      "R output has ", n_lines, " lines. Showing the first ",
      max_output_lines, " lines.\n\n",
      paste(preview, collapse = "\n"),
      if (!is.null(notice)) paste0("\n\n", notice) else ""
    )
  } else {
    value_text <- paste(lines, collapse = "\n")
  }

  value_text <- .btwExtra_append_plot_hint(value_text, plot_data)

  btwExtra_tool_result(
    value = value_text,
    data  = c(res, list(
      n_output_lines   = n_lines,
      max_output_lines = max_output_lines,
      plot             = plot_data
    )),
    display = .btwExtra_plot_display(value_text, plot_data)
  )
}

.btwExtra_plot_payload <- function(path, width, height, res) {
  if (is.null(path) || !fs::file_exists(path)) {
    return(NULL)
  }

  info <- fs::file_info(path)

  if (is.na(info$size) || info$size <= 0) {
    return(NULL)
  }

  list(
    data = base64enc::base64encode(path),
    mime = "image/png",
    path = path,
    width = width,
    height = height,
    res = res
  )
}

.btwExtra_append_plot_hint <- function(value_text, plot_data) {
  if (is.null(plot_data)) {
    return(value_text)
  }

  path <- plot_data$path %||% ""
  path_hint <- if (nzchar(path)) {
    sprintf("Plot captured (PNG) (saved at [%s](file://%s))", path, path)
  } else {
    "Plot captured (PNG)"
  }

  paste(value_text, path_hint, sep = "\n\n")
}

.btwExtra_plot_display <- function(value_text, plot_data) {
  if (is.null(plot_data)) {
    return(NULL)
  }

  list(
    content = list(
      list(type = "text", text = value_text),
      list(
        type = "media",
        data = plot_data$data,
        mediaType = plot_data$mime
      )
    )
  )
}

.btwExtra_add_to_tools(
  name = "btwExtra_tool_env_run_r_code",
  group = "env",
  tool = function() {
    ellmer::tool(
      btwExtra_tool_env_run_r_code_impl,
      name = "btwExtra_tool_env_run_r_code",
      description = 'Escape hatch for executing arbitrary R code in the connected R session.
      
      ## Usage guidelines (for the model)
      - Prefer more specific tools when available:
        - For exploring the *structure* of a data frame/tibble, `btw_tool_env_describe_data_frame` returns a compact summary (types, basic stats, missingness). Use it instead of printing many rows of a large data set with `run_r_code`.
        - Use `btw_tool_env_describe_environment` to list objects in the R session.
        - Use `btw_tool_files_*` to read/write files.
      - Use this tool when you really need to run custom R logic or to glue steps together in ways that existing tools cannot express directly.
      - If the object to inspect is (or could be) a plot (e.g. ggplot, base plot, lattice), call this with `capture_plot = TRUE` to render and capture the PNG. Then use the temporary PNG path in `extra$data$plot$path` with your client\'s image-viewing tool to visualize and describe the plot instead of printing its structure.
      
      ## Output policy
      - The tool emulates the R console: non-assignment expressions with a visible value are printed automatically (like running them at the R prompt).
      - Large outputs are truncated to `max_output_lines` lines by default to save tokens and context. Use `max_output_lines = -1` only if you truly need the full output (e.g., for a short model summary).
      - Avoid repeatedly printing the same large object; print it once and then reason from that result, or request more targeted summaries instead.
      - If `capture_plot = TRUE` and a plot is produced, the result includes an image payload with both base64-encoded PNG data (`extra$data$plot$data`) and a temporary file path to the PNG (`extra$data$plot$path`). Use whichever form your client supports (e.g., render base64 or open the file at `path`). 
      - If the user asks you to display the image, prefer loading it from the temporary path with `magick::image_read(<path>)` and describe it; avoid relying on base64 in clients that do not render images.
      ',
      annotations = ellmer::tool_annotations(
        title = "Run R Code",
        read_only_hint = FALSE,
        open_world_hint = FALSE,
        idempotent_hint = FALSE,
        btw_can_register = function() TRUE
      ),
      arguments = list(
        code = ellmer::type_string(
          "R code to execute in the global environment of the selected R session."
        ),
        max_output_lines = ellmer::type_integer(
          "Maximum number of output lines to display. Use -1 to disable truncation.",
          required = FALSE
        ),
        capture_plot = ellmer::type_boolean(
          paste(
            "Capture a plot when the code produces one. The result exposes both a",
            "base64-encoded PNG (`extra$data$plot$data`) and a temporary file path",
            "(`extra$data$plot$path`). Defaults to TRUE."
          ),
          required = FALSE
        ),
        plot_width = ellmer::type_integer(
          "PNG width in pixels when capturing a plot.",
          required = FALSE
        ),
        plot_height = ellmer::type_integer(
          "PNG height in pixels when capturing a plot.",
          required = FALSE
        ),
        plot_res = ellmer::type_integer(
          "PNG resolution (DPI) when capturing a plot.",
          required = FALSE
        )
      )
    )
  }
)
