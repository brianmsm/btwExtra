#' Combined tools for MCP with btw and btwExtra
#'
#' @description
#' `btwExtra_default_tools()` returns the MCP-friendly combination: all
#' `btw::btw_tools()` plus the additions from `btwExtra_tools()`.
#'
#' `btwExtra_mcp_server()` runs `btw::btw_mcp_server()` using that default set
#' (or a custom set if provided).
#'
#' @param tools List of `ellmer::tool()` or a path to an R script compatible
#'   with `mcptools::mcp_server()`. By default it uses `btwExtra_default_tools()`.
#'
#' @return List of tools (`btwExtra_default_tools()`) or the result of
#'   `btw::btw_mcp_server()` (`btwExtra_mcp_server()`).
#' @export
btwExtra_default_tools <- function() {
  c(btw::btw_tools(), btwExtra_tools())
}

#' @rdname btwExtra_default_tools
#' @export
btwExtra_mcp_server <- function(tools = btwExtra_default_tools()) {
  btw::btw_mcp_server(tools = tools)
}
