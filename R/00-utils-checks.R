.btwExtra_check_string <- function(x, allow_null = FALSE, arg = rlang::caller_arg(x)) {
  if (is.null(x) && allow_null) {
    return(invisible(x))
  }

  if (rlang::is_string(x)) {
    return(invisible(x))
  }

  cli::cli_abort("{.arg {arg}} must be a single string.")
}

.btwExtra_check_character <- function(x, allow_null = FALSE, arg = rlang::caller_arg(x)) {
  if (is.null(x) && allow_null) {
    return(invisible(x))
  }

  if (is.character(x)) {
    return(invisible(x))
  }

  cli::cli_abort("{.arg {arg}} must be a character vector.")
}

.btwExtra_check_number <- function(x, allow_null = FALSE, arg = rlang::caller_arg(x)) {
  if (is.null(x) && allow_null) {
    return(invisible(x))
  }

  if (is.numeric(x) && length(x) == 1) {
    return(invisible(x))
  }

  cli::cli_abort("{.arg {arg}} must be a single numeric value.")
}

.btwExtra_truncation_notice <- local({
  shown <- FALSE
  function() {
    # Option controls once-per-session behavior (default TRUE: show once)
    show_once <- getOption("btwExtra.show_truncation_notice_once", TRUE)
    if (!isTRUE(show_once)) {
      return(.btwExtra_truncation_notice_text())
    }
    if (isTRUE(shown)) {
      return(NULL)
    }
    shown <<- TRUE
    .btwExtra_truncation_notice_text()
  }
})

.btwExtra_truncation_notice_text <- function() {
  paste(
    "If you need the full output for reasoning, re-run this tool with",
    "`max_output_lines = -1` (no truncation) or with a larger value.",
    "For large model summaries (e.g. lavaan, mirt, lm), consider using",
    "more focused calls (e.g. `summary(fit, fit.measures = TRUE)`,",
    "`coef(fit)`, or extracting specific components) instead of the",
    "entire summary."
  )
}
