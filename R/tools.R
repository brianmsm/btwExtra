#' Tools: Register tools from btwExtra
#'
#' @description
#' `btwExtra_tools()` returns the list of tools registered in this package in
#' the format expected by `ellmer::chat$register_tools()`. It accepts filters by
#' tool names, groups, or names without prefix, mirroring `btw::btw_tools()`.
#'
#' @param ... Optional names of tools or groups. If omitted, all tools
#'   registered in btwExtra are included.
#'
#' @return List of `ellmer::tool()` objects.
#' @export
btwExtra_tools <- function(...) {
  tools <- c(...)
  .btwExtra_check_character(tools, allow_null = TRUE)

  if (length(tools) == 0) {
    tools <- names(.btwExtra_tools)
  } else {
    tool_names <- purrr::map_chr(.btwExtra_tools, function(x) x$name)
    tool_groups <- purrr::map_chr(.btwExtra_tools, function(x) x$group)

    allowed <- c(
      tool_groups,
      tool_names,
      sub("btwExtra_tool_", "", tool_names, fixed = TRUE)
    )
    allowed <- unique(allowed)

    tools <- tryCatch(
      rlang::arg_match(tools, allowed[!grepl("^btwExtra_", allowed)], multiple = TRUE),
      error = function(err_short) {
        tryCatch(
          rlang::arg_match(tools, allowed, multiple = TRUE),
          error = function(err_long) {
            class(err_short) <- c("btwExtra_unmatched_tool_error", class(err_short))
            rlang::cnd_signal(err_short)
          }
        )
      }
    )
  }

  tools_to_keep <- purrr::map_lgl(.btwExtra_tools, .btwExtra_is_tool_match, tools)
  res <- .btwExtra_tools[tools_to_keep]
  res <- .btwExtra_as_ellmer_tools(res)

  tools_can_register <- purrr::map_lgl(res, function(tool) {
    is.null(tool@annotations$btw_can_register) ||
      tool@annotations$btw_can_register()
  })

  res[tools_can_register]
}

.btwExtra_is_tool_match <- function(tool, labels = NULL) {
  if (is.null(labels)) {
    return(TRUE)
  }
  if (tool$name %in% labels) {
    return(TRUE)
  }
  if (tool$group %in% labels) {
    return(TRUE)
  }
  if (sub("btwExtra_tool_", "", tool$name) %in% labels) {
    return(TRUE)
  }
  FALSE
}

# Convierte desde .btwExtra_tools (o un subconjunto filtrado) al formato
# compatible con `client$set_tools()`.
.btwExtra_as_ellmer_tools <- function(x) {
  groups <- purrr::map_chr(x, function(.x) .x$group)
  tools <- purrr::compact(purrr::map(x, function(.x) .x$tool()))
  tools <- purrr::map2(tools, groups, .btwExtra_set_tool_icon)
  purrr::map(tools, .btwExtra_wrap_with_intent)
}

.btwExtra_wrap_with_intent <- function(tool) {
  if ("_intent" %in% names(tool@arguments@properties)) {
    return(tool)
  }

  tool_fun <- S7::S7_data(tool)
  wrapped_tool <- rlang::new_function(
    c(rlang::fn_fmls(tool_fun), list(`_intent` = "")),
    rlang::fn_body(tool_fun),
    env = rlang::fn_env(tool_fun)
  )
  S7::S7_data(tool) <- wrapped_tool
  tool@arguments@properties[["_intent"]] <- ellmer::type_string(
    paste(
      "The intent of the tool call that describes why you called this tool.",
      "This should be a single, short phrase that explains this tool call to the user."
    )
  )

  tool
}

.btwExtra_tool_group_icon <- function(group, default = NULL) {
  switch(
    group,
    "docs" = .btwExtra_tool_icon("dictionary"),
    "env" = .btwExtra_tool_icon("source-environment"),
    "files" = .btwExtra_tool_icon("folder-open"),
    "git" = .btwExtra_tool_icon("git"),
    "github" = .btwExtra_tool_icon("github"),
    "ide" = .btwExtra_tool_icon("code-blocks"),
    "search" = .btwExtra_tool_icon("search"),
    "session" = .btwExtra_tool_icon("screen-search-desktop"),
    "web" = .btwExtra_tool_icon("globe-book"),
    if (!is.null(default)) .btwExtra_tool_icon(default)
  )
}

.btwExtra_set_tool_icon <- function(tool, group) {
  if (!is.list(tool@annotations)) {
    tool@annotations <- list()
  }

  tool@annotations$icon <- .btwExtra_tool_group_icon(group)
  tool
}

.btwExtra_tool_icon <- local({
  icons <- list()
  function(name) {
    if (!is.null(icons[[name]])) {
      return(icons[[name]])
    }

    candidates <- list(
      function() {
        fs::path_package("btwExtra", "icons", paste0(name, ".svg"))
      },
      function() {
        fs::path_package("btw", "icons", paste0(name, ".svg"))
      }
    )

    for (path_fun in candidates) {
      icon <- tryCatch(
        {
          path <- path_fun()
          htmltools::HTML(readLines(path, warn = FALSE))
        },
        error = function(e) NULL
      )
      if (!is.null(icon)) {
        icons[[name]] <<- icon
        return(icon)
      }
    }

    NULL
  }
})
