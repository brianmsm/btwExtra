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
