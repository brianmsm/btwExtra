# btwExtra – Developer Guide for Agents

This package extends the {btw} toolset with extra MCP tools. It is designed to be used by LLM agents (via ellmer/MCP) and human contributors. The key goal is to match the coding and tool-exposition philosophy of {btw}, while keeping a clear separation (no name collisions) and minimal dependencies.

## Purpose
- Provide additional tools not present in {btw}, following the same UX and registration model.
- Offer a convenient MCP combination (`btwExtra_default_tools()`) that merges {btw} tools with btwExtra tools.
- Keep tools discoverable and consistent for LLM-driven workflows.

## Coding conventions for tools
- **Naming**: prefix with `btwExtra_tool_...`. Use a group aligned with {btw} (env, docs, files, git, github, ide, search, session, web) for icon/UX consistency.
- **Structure per tool file**:
  1) A stub exported function (for docs/NAMESPACE) with `_intent` argument if applicable.
  2) An `*_impl` function with the actual logic.
  3) A call to `.btwExtra_add_to_tools()` that returns `ellmer::tool(...)`, sets description, annotations, and arguments with `ellmer::type_*`.
- **Registration**: use `.btwExtra_add_to_tools(name, group, tool = function() ellmer::tool(...))`. Tool names must be unique.
- **Intent wrapper**: `_intent` is injected automatically by `.btwExtra_wrap_with_intent()` if missing.
- **Icons**: `.btwExtra_set_tool_icon()` assigns an SVG by group. Icons fall back to {btw} if btwExtra has none.
- **Results**: return `btwExtra_tool_result()` (S7 subclass of `ellmer::ContentToolResult`). Enrich `extra`/`display` as needed.
- **Validation**: use the local helpers (`.btwExtra_check_string`, `.btwExtra_check_character`, `.btwExtra_check_number`). Avoid reaching into `btw:::` internals.
- **File order**: helpers and registry are in `00-...` / `01-...` to load before dependent files during `load_all()`. You can also enforce order with `@include`.
- **README source**: edit `README.Rmd` and render to `README.md` to keep both in sync.
- **README workflow**: never edit `README.md` directly. Apply changes to `README.Rmd` and render. If `README.Rmd` differs from the agent’s changes, assume the user modified it manually; preserve those manual edits and/or ask the user before overwriting.

## MCP defaults
- Use `btwExtra_default_tools()` to combine `btw::btw_tools()` and `btwExtra_tools()`.
- `btwExtra_mcp_server()` wraps `btw::btw_mcp_server(tools = btwExtra_default_tools())`.

## Style and docs
- Keep documentation and comments in English.
- Descriptions should include “when to use” and “cautions” where relevant, matching btw’s style.
- Examples should be minimal and deterministic.

## Dependencies
- Imports: btw, ellmer, S7, fs, cli, htmltools, purrr, rlang. Icons fall back to {btw}.
- Prefer local helpers over pulling new dependencies; if needed, justify additions in DESCRIPTION.

## Checklist for a new tool
- [ ] Name prefixed with `btwExtra_tool_...` and assigned to a group.
- [ ] Stub + `_impl` + `.btwExtra_add_to_tools(...)`.
- [ ] Arguments typed with `ellmer::type_*`; `_intent` handled automatically if absent.
- [ ] Description includes usage guidance and cautions; annotations set (`btw_can_register` as needed).
- [ ] Returns `btwExtra_tool_result` (or subclass) with useful `extra/display`.
- [ ] Tests/examples are short and deterministic; docstrings in English.
- [ ] Icons: rely on group defaults unless a custom icon is added to `inst/icons/`.

Following these conventions keeps btwExtra consistent with {btw} and ready for MCP/LLM use.***
