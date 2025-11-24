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
- **Agent workflow**: do not create commits unless the user explicitly asks for it. You may suggest commit contents/messages, but wait for user approval before committing.

## MCP defaults
- Use `btwExtra_default_tools()` to combine `btw::btw_tools()` and `btwExtra_tools()`.
- `btwExtra_mcp_server()` wraps `btw::btw_mcp_server(tools = btwExtra_default_tools())`.

## HTML / table / report roadmap for btwExtra

The following phases describe how btwExtra will gradually add support for HTML tables, reports and interactive outputs. This file is only a **high-level map**; each phase will later get its own, more detailed prompt.

### Phase 1 – HTML tables

**1A. Table content (data frame view)**  
- Goal: when a table object already exists in R (e.g. `gt`, `gtsummary`, `DT`, `reactable`, etc.), expose its **data content** as a normal data frame / JSON so the model can reason about it.  
- Behaviour: detect common HTML table classes, convert them to `data.frame`/`tibble` (via existing methods or a generic HTML fallback), and return either a compact preview or a structured JSON description.  
- Usage: use these tools when the user cares about *what* is in the table (values, columns, missingness, summaries), not formatting.

**1B. Table screenshots (visual inspection)**  
- Goal: when the user cares about **styling/layout** (headers, fonts, alignment, zebra stripes, etc.), capture a screenshot of the HTML table.  
- Behaviour: render the table to a temporary HTML file, screenshot it (e.g. `webshot2`), and return:
  - a `media` entry (PNG) for clients that render images, and  
  - a temporary PNG path for CLIs that can only open files.  
- Usage: use when the user asks “does this table look right?” or wants visual confirmation of formatting.

### Phase 2 – Quarto / R Markdown reports

**2A. Render + logs**  
- Goal: provide a clear story for rendering `.qmd` / `.Rmd` and checking whether the build succeeded.  
- Behaviour: high-level “render report” tool that calls `quarto::quarto_render()` or `rmarkdown::render()`, captures logs, and returns:
  - success/failure status,  
  - output paths,  
  - collected errors/warnings/messages.  
- Usage: use when the user says “render this report” or “why is my Quarto/Rmd failing?”.

**2B. Knit code in a separate environment**  
- Goal: run document code to create objects without polluting `.GlobalEnv`.  
- Behaviour: knit with `knitr::knit()` into a temporary `.md` using a fresh environment, expose an `env_id` plus a summary of objects created.  
- Usage: use when you need access to objects produced by a report, but the user doesn’t want them in their global workspace. Combine with `run_r_code` if the user later wants to copy objects into `.GlobalEnv`.

**2C. Report screenshots**  
- Goal: visually inspect the rendered HTML report (overall or specific sections).  
- Behaviour: screenshot the HTML output (full page or by selector/anchor) using the same pipeline as Phase 1B, returning media + PNG path.  
- Usage: use when the user wants to check layout, themes, header/footer appearance, or a specific section of the report.

### Phase 3 – Widgets, apps and slides

**3A. Static widgets (HTML widgets, maps, interactive plots)**  
- Goal: inspect how interactive widgets *look* (legends, axes, labels, map tiles, etc.) even if full interactivity is not exercised.  
- Behaviour: save widget to temporary HTML (`htmlwidgets::saveWidget()`), then screenshot it like any other HTML.  
- Usage: use when the user wants visual verification of a widget’s appearance.

**3B. Shiny apps, interactive sites, and HTML slides**  
- Goal: define how to combine R-side tools with browser automation (e.g. Playwright MCP) for apps and slide decks.  
- Behaviour:
  - Slides (revealjs/xaringan/Quarto): treat them as HTML reports – render, then screenshot selected slides/sections.  
  - Shiny apps:  
    - R-side tools help detect app structure and show how to run it (`shiny::runApp`).  
    - Functional testing (clicks, tabs, inputs) is delegated to browser-automation MCPs (Playwright); recommend `shinytest2` for reproducible tests.  
- Usage: use R tools for structure + basic screenshots; use Playwright MCP when real interaction is required.

### Tool descriptions (for every phase)

For each new tool added in these phases, its `description` should clearly state:

- **When to use it**  
  - e.g., “Use this for table content, not for styling” or “Use this for rendered HTML, not for raw R objects”.
- **What it returns**  
  - e.g., “Returns a preview data frame”, “Returns a JSON summary”, “Returns a PNG media entry and a temporary PNG path”.
- **How to combine it with other tools**  
  - e.g., “Prefer this over `run_r_code` for data frame inspection”,  
  - “Use with `btwExtra_tool_env_run_r_code` for custom summaries”,  
  - “Use together with Playwright MCP for interactive Shiny behaviour”.

A more detailed prompt for each phase will be added as those features are implemented; this section is only the high-level roadmap for the model.

## Style and docs
- Keep documentation and comments in English.
- Descriptions should include “when to use” and “cautions” where relevant, matching btw’s style.
- Tool descriptions should be written as literal markdown strings (no `paste()`), with explicit newlines. Use markdown headings (e.g., `## Usage guidelines`, `## Output policy`) to improve rendering in clients.
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
