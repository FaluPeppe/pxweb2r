#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @importFrom utils globalVariables
## usethis namespace: end
NULL

# Kolumnnamn som används i data-masking (dplyr/tidyr) och därför ser ut som
# odefinierade globala variabler för R CMD check.
globalVariables(c(
  ".ar_deso", ".ar_deso_regso", ".ar_regso", ".grund_region_kod", ".har_data",
  ".har_parentes", ".har_version", ".region_kod_tmp", ".region_text_ren",
  ".region_tmp", ".row_id", ".value_num", ".version_ar", "agg_id", "agg_label",
  "code", "code_lower", "codelist", "id", "kommun", "kommun_kod", "label",
  "label_lower", "priority", "role", "role_lower", "show", "size", "type",
  "values_long_df"
))
