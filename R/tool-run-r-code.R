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
#' @param _intent Optional free-text intent (automatically injected when called
#'   via ellmer tools; can be left empty when calling directly).
#' @export
btwExtra_tool_env_run_r_code <- function(code, max_output_lines = 20L, `_intent` = "") {}

btwExtra_tool_env_run_r_code_impl <- function(code, max_output_lines = 20L) {
  .btwExtra_check_string(code)
  if (!is.null(max_output_lines)) {
    .btwExtra_check_number(max_output_lines)
  }

  exprs <- NULL
  last_val <- NULL
  out <- character()

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

  if (!identical(res$status, "ok")) {
    msg_lines <- c(
      paste0("Error executing R code: ", res$error_message),
      if (length(res$console)) c("R console output:", res$console) else character()
    )

    value_text <- paste(msg_lines, collapse = "\n")

    return(btwExtra_tool_result(
      value = value_text,
      data  = res
    ))
  }

  lines <- res$console

  if (length(lines) == 0L) {
    value_text <- "R code executed successfully (no visible output)."

    return(btwExtra_tool_result(
      value = value_text,
      data  = res
    ))
  }

  n_lines <- length(lines)

  if (is.null(max_output_lines) || is.na(max_output_lines)) {
    max_output_lines <- 20L
  }

  if (max_output_lines <= 0L) {
    value_text <- paste(lines, collapse = "\n")

    return(btwExtra_tool_result(
      value = value_text,
      data  = c(res, list(
        n_output_lines   = n_lines,
        max_output_lines = max_output_lines
      ))
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

  btwExtra_tool_result(
    value = value_text,
    data  = c(res, list(
      n_output_lines   = n_lines,
      max_output_lines = max_output_lines
    ))
  )
}

.btwExtra_add_to_tools(
  name = "btwExtra_tool_env_run_r_code",
  group = "env",
  tool = function() {
    ellmer::tool(
      btwExtra_tool_env_run_r_code_impl,
      name = "btwExtra_tool_env_run_r_code",
      description = paste(
        "Escape hatch for executing arbitrary R code in the connected R session.",
        "",
        "Usage guidelines (for the model):",
        "- Prefer more specific tools when available:",
        "  - For exploring the *structure* of a data frame/tibble,",
        "    `btw_tool_env_describe_data_frame` returns a compact summary",
        "    (types, basic stats, missingness). Use it instead of printing many",
        "    rows of a large data set with `run_r_code`.",
        "  - Use `btw_tool_env_describe_environment` to list objects in the R session.",
        "  - Use `btw_tool_files_*` to read/write files.",
        "- Use this tool when you really need to run custom R logic or to glue steps",
        "  together in ways that existing tools cannot express directly.",
        "",
        "Output policy:",
        "- The tool emulates the R console: non-assignment expressions with a visible",
        "  value are printed automatically (like running them at the R prompt).",
        "- Large outputs are truncated to `max_output_lines` lines by default to save",
        "  tokens and context. Use `max_output_lines = -1` only if you truly need the",
        "  full output (e.g., for a short model summary).",
        "- Avoid repeatedly printing the same large object; print it once and then",
        "  reason from that result, or request more targeted summaries instead."
      ),
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
        )
      )
    )
  }
)
