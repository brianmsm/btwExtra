.btwExtra_tools <- list()

.btwExtra_add_to_tools <- function(name, group = name, tool) {
  .btwExtra_check_string(name)
  .btwExtra_check_string(group)

  if (!rlang::is_function(tool)) {
    cli::cli_abort(
      "`tool` must be a function so that `ellmer::tool()` is called at run time."
    )
  }

  if (name %in% names(.btwExtra_tools)) {
    cli::cli_abort("Tool names must be unique: {.val {name}}")
  }

  .btwExtra_tools[[name]] <<- list(
    name = name,
    group = group,
    tool = tool
  )

  invisible(tool)
}
