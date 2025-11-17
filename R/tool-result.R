BtwExtraToolResult <- S7::new_class(
  "BtwExtraToolResult",
  parent = ellmer::ContentToolResult
)

btwExtra_tool_result <- function(value, data = NULL, ..., cls = BtwExtraToolResult) {
  cls(
    value = value,
    extra = list(data = data, ...)
  )
}
