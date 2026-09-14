# pxweb2r 0.0.0.9000

* First version as a package, extracted from `func_pxweb2.R` in
  `Region-Dalarna/funktioner`.
* New: `pxweb2_get_codelist()` fetches a single code list (value set or
  aggregation) by its global id, without needing a table -
  e.g. `pxweb2_get_codelist("vs_RegionKommun07")` for all municipalities.
* New: `pxweb2_list_codelists()` reads a table's metadata and lists every
  code list its variables offer (id, type, label).
* All function and parameter names are English. Enum values follow: the
  `deso_regso_versions` argument takes `"latest"` / `"sum"` (was
  `"senaste"` / `"summering"`). User-facing messages and code comments are
  English too.
* Table ids are no longer validated against the pattern `^TAB\d+$`. A minimal
  local check is done instead (non-empty string, length 1, no whitespace or
  `/`), and `pxweb2_get_metadata()` decides whether the table actually exists -
  HTTP 404 is translated into a clear error message.
* New: `pxweb2_table_exists()` to quickly check whether a table id exists on a
  given `base_url`.
* `base_url` is now passed all the way to the metadata calls, and is an
  argument on `pxweb2_table_updated()`, `pxweb2_table_needs_update()`,
  `pxweb2_query_list_template()` and `pxweb2_data_script_template()`.
* `pxweb2_data_script_template(to_clipboard = TRUE)` uses `clipr` (Suggests)
  instead of `writeLines(con = "clipboard")` and falls back to printing only
  when no clipboard is available, e.g. on headless Linux.
* New: `quiet` argument on `pxweb2_get_data()` and `pxweb2_get_values()`.
  `quiet = TRUE` suppresses the informational `message()`/`cat()` output these
  functions print about things a well-tested query already expects - the
  `include_aggregations = "auto"` summary, "latest period" substitution
  notices when fetching several tables at once, and the "invalid values
  removed" notices from `on_all_values_invalid`. It never suppresses errors
  or genuine `warning()`s (unknown variables, mismatched result structures
  across tables, `on_all_values_invalid = "stop"`), only the routine notices.
  Default `FALSE` keeps the existing (verbose) behaviour.
* Fix: `.pxweb2_get_time_values()` (used internally to resolve
  `latest_period_code` across one or more tables) called `pxweb2_get_values()`
  without arguments, which meant it always fetched aggregation metadata for
  every variable (not just the time variable it actually needed) and always
  printed an `include_aggregations = "auto"` message - even when the calling
  `pxweb2_get_data(quiet = TRUE)` asked it not to. Now calls with
  `include_aggregations = "none", quiet = TRUE` explicitly.
* Fix: `pxweb2_get_data(table = <vector of several tables>, auto_limit = ...)`
  never actually passed `auto_limit` on to `.pxweb2_get_multiple_tables()`,
  so a custom `auto_limit` was silently ignored (always used the default,
  30) when fetching more than one table at once.

## Name mapping from func_pxweb2.R

| func_pxweb2.R | pxweb2r |
|---|---|
| `pxweb2_hamta_data()` | `pxweb2_get_data()` |
| `pxweb2_meta()` | `pxweb2_get_metadata()` |
| `pxweb2_variabler()` | `pxweb2_get_variables()` |
| `pxweb2_varden()` | `pxweb2_get_values()` |
| `pxweb2_tabell_uppdaterades()` | `pxweb2_table_updated()` |
| `pxweb2_tabell_behover_uppdateras()` | `pxweb2_table_needs_update()` |
| `pxweb2_tabell_finns()` | `pxweb2_table_exists()` |
| `pxweb2_search_tables()` | `pxweb2_search_tables()` |
| `pxweb2_query_list_txt_create()` | `pxweb2_query_list_template()` |
| `pxweb2_get_data_script_create()` | `pxweb2_data_script_template()` |
