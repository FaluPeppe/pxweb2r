# Package dependencies are declared in DESCRIPTION (Imports) and NAMESPACE, not here.

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


# Minimal local check of a table id before it is put into a URL.
# Only checks that it is a non-empty string of length 1 without whitespace
# or '/' - i.e. catches a passed vector, NULL, NA or a whole URL.
# No naming-convention check ("TAB..."); whether the id actually exists is
# decided by the API via pxweb2_get_metadata() (404 => not on the base_url used).
intern_pxweb2_check_table_id <- function(table_id) {
  if (is.null(table_id) || length(table_id) != 1 || !is.character(table_id) ||
      is.na(table_id) || !nzchar(trimws(table_id))) {
    stop("table id must be a non-empty string of length 1.", call. = FALSE)
  }
  if (stringr::str_detect(table_id, "\\s|/")) {
    stop(
      "table id '", table_id, "' contains whitespace or '/'. ",
      "Pass only the table id, not a URL or path.",
      call. = FALSE
    )
  }
  invisible(table_id)
}

#' Get data from a PxWeb API v2
#'
#' Fetches data from one or more PxWeb tables (by default Statistics Sweden's
#' statistical database) and returns a tidy `tibble`. Handles, among other
#' things, automatic chunking of large requests, label values in queries,
#' wildcards, "latest period", and splitting DeSO/RegSO by municipality.
#'
#' @param table Table id (e.g. `"TAB6104"`), a vector of table ids, or a
#'   metadata object from [pxweb2_get_metadata()].
#' @param query Named list where each element is a variable and the value is the
#'   values to fetch. `NULL` fetches all values. Both codes and labels are
#'   accepted when `allow_label_values = TRUE`.
#' @param lang Language for metadata and labels, `"sv"` or `"en"`.
#' @param output_format Output format from the API, normally `"json-stat2"`.
#' @param base_url Base URL of the PxWeb API v2 (the tables endpoint).
#' @param on_all_values_invalid What happens when every supplied value for a
#'   variable is invalid: `"stop"`, `"*"` or `"null"`.
#' @param allow_label_values If `TRUE`, queries may contain labels instead of
#'   codes.
#' @param strip_code_in_label Removes codes that also appear in the label column.
#' @param latest_period_code Code that in a query means "fetch the latest time
#'   period". `NULL` disables the feature.
#' @param allow_api_wildcards If `TRUE`, `"*"` in queries is passed on as an API
#'   wildcard instead of being matched against valid values.
#' @param harmonise_variable_names Named vector for renaming variables so that
#'   several tables get the same variable name, e.g. `c(Region = "Kommun")`.
#' @param deso_regso_versions Handling of multiple DeSO/RegSO versions:
#'   `"latest"`, `"sum"` or `NULL`.
#' @param split_deso_regso_by_municipality If `TRUE`, `kommun_kod` and `kommun`
#'   are added as separate columns.
#' @param include_aggregations Controls fetching of aggregations, see
#'   [pxweb2_get_values()].
#' @param auto_limit Max number of code-list calls when
#'   `include_aggregations = "auto"`.
#'
#' @return A `tibble` with the fetched data.
#' @export
pxweb2_get_data <- function(
    table = NULL,
    query = NULL,               # a list where each name is a variable and the value is the values wanted; values may be vectors
    lang = "sv",
    output_format = "json-stat2",
    base_url = "https://statistikdatabasen.scb.se/api/v2/tables/",
    on_all_values_invalid = "stop",          # if all values for a variable are invalid: "stop" halts the request, "*" returns all values for the variable, "null" returns NULL (handy when fetching from several tables where different years or regions live in different tables)
    allow_label_values = TRUE,               # if TRUE, labels may be used for values in queries, i.e. both "20" and "Dalarnas lan" work
    strip_code_in_label = TRUE,              # removes codes that also appear in the label column. If only a code exists and no label, the code is kept as the label
    latest_period_code = "9999",             # this value fetches the latest value of the time column; NULL = not used
    allow_api_wildcards = TRUE,              # TRUE = if the query contains "*" the API's built-in wildcard is used; FALSE = "*" is treated as an ordinary value and matched against valid values in the table
    harmonise_variable_names = NULL,         # used when fetching several tables at once. Pass a named vector, e.g. c("Region" = "Kommun"), and the variable "Kommun" is renamed to "Region" so all tables share the same variable name for municipalities and municipality codes
    deso_regso_versions = "latest",          # if there are several DeSO/RegSO versions, "latest" uses the most recent version that has a value, "sum" sums all versions, NULL does nothing
    split_deso_regso_by_municipality = TRUE, # adds the columns kommun_kod and kommun and keeps only the RegSO/DeSO name in the region column
    include_aggregations = "none",           # controls aggregations in valid_values_list (see pxweb2_get_values): "none"/FALSE = none, "all"/TRUE = all, "auto" = auto, "codelists" = metadata only, or a named vector c(Region = "agg_RegionLA2018")
    auto_limit = 30L                         # max total number of code-list calls when include_aggregations = "auto"
){
  if (is.null(table)) stop("table_id must be supplied")
  if (!on_all_values_invalid %in% c("stop", "*", "null")) {
    stop("Invalid value for on_all_values_invalid. Allowed: \"stop\", \"*\", \"null\".")
  }
  
  # check whether more than one table was requested
  if (!is.list(table) && length(table) > 1) {
    return(
      intern_pxweb2_get_multiple_tables(
        tables = table,
        query = query,
        lang = lang,
        output_format = output_format,
        base_url = base_url,
        on_all_values_invalid = on_all_values_invalid,
        allow_label_values = allow_label_values,
        strip_code_in_label = strip_code_in_label,
        latest_period_code = latest_period_code,
        allow_api_wildcards = allow_api_wildcards,
        harmonise_variable_names = harmonise_variable_names,
        deso_regso_versions = deso_regso_versions,
        split_deso_regso_by_municipality = split_deso_regso_by_municipality,
        include_aggregations = include_aggregations
      )
    )
  }
  
  if (!is.null(deso_regso_versions)) {
    deso_regso_versions <- match.arg(
      deso_regso_versions,
      choices = c("latest", "sum")
    )
  }
  
  if (!is.list(table)) {
    intern_pxweb2_check_table_id(table)
    metadata <- pxweb2_get_metadata(table, base_url = base_url)
  } else {
    metadata <- table
    table <- metadata$extension$px$tableid
  }
  
  data_url <- paste0(base_url, table, "/data")
  
  variables_df <- pxweb2_get_variables(metadata)          # fetch all variables
  valid_values_list <- pxweb2_get_values(metadata, include_aggregations = include_aggregations, auto_limit = auto_limit)      # fetch all unique values for all variables (incl. aggregations if selected)
  
  # build a query list from the supplied query, or one covering all valid values
  query_list <- if (is.null(query)) {
    intern_pxweb2_create_variable_query_list(variables_df)
  } else {
    if (intern_pxweb2_is_pxweb_query_list(query)) {
      query
    } else {
      intern_pxweb2_list_to_query_list(
        variables_df,
        query,
        valid_values_list = valid_values_list,
        allow_label_values = allow_label_values
      )
    }
  }
  
  query_list <- query_list |>
    intern_pxweb2_resolve_latest_period(
      variables_df = variables_df,
      valid_values_list = valid_values_list,
      latest_period_code = latest_period_code
    ) |>
    intern_pxweb2_resolve_api_wildcards(
      valid_values_list = valid_values_list,
      allow_api_wildcards = allow_api_wildcards
    ) |>
    intern_pxweb2_sanitize_query_values(
      valid_values_list = valid_values_list,
      on_all_values_invalid = on_all_values_invalid
    )
  
  
  # if query_list = NULL, NULL is returned - useful when fetching a set of values
  # for variables that live in different tables

  # expand any aggregations / "**" into several requests
  if (any(purrr::map_lgl(query_list$selection, ~ is.null(.x$valueCodes)))) {
    return(NULL)
  }

  request_list <- intern_pxweb2_expand_requests_generic(query_list, valid_values_list)


  # drop requests that lack a selection or are NULL
  request_list <- purrr::compact(request_list)
  if (length(request_list) == 0) return(NULL)

  # split into chunks if the request has more than 150,000 cells
  query_chunks <- intern_pxweb2_make_request_chunks(
    variables_df,
    request_list,
    valid_values_list = valid_values_list
  )


  # 1) determine which code columns to add
  cols_to_add <- variables_df |>
    dplyr::filter(
      show == "code_value" |
        role == "geo" |
        stringr::str_to_lower(code) == "region" |
        stringr::str_to_lower(label) == "region"
    ) |>
    dplyr::transmute(
      code_col = code,
      txt_col = label,
      new_name = paste0(stringr::str_to_lower(label), "_kod")
    )
  
  # all data is fetched here ============
  result_table <- purrr::map(query_chunks, function(req) {

    # decide whether we must use GET (codelist/outputValues work there) or POST
    use_get <- length(req$extra_query) > 0

    if (use_get) {
      # build query parameters valueCodes[...] from req$body$selection
      vc_query <- purrr::map(req$body$selection, function(s) {
        var  <- s$variableCode
        vals <- unlist(s$valueCodes, use.names = FALSE)
        stats::setNames(list(paste(vals, collapse = ",")), paste0("valueCodes[", var, "]"))
      }) |>
        purrr::flatten()
      
      data_resp <- intern_pxweb2_GET(
        data_url,
        query = c(list(lang = lang, outputFormat = output_format), vc_query, req$extra_query),
        httr::accept_json()
      )
    } else {
      data_resp <- intern_pxweb2_POST(
        data_url,
        body = jsonlite::toJSON(req$body, auto_unbox = TRUE),
        query = list(lang = lang, outputFormat = output_format),
        encode = "raw",
        httr::content_type_json(),
        httr::accept_json()
      )
    }
    
    if (httr::status_code(data_resp) >= 400) {
      cat("HTTP ", httr::status_code(data_resp), "\n", sep = "")
      cat(httr::content(data_resp, "text", encoding = "UTF-8"), "\n", sep = "")
    }
    httr::stop_for_status(data_resp)
    
    json_text <- httr::content(data_resp, "text")
    
    df_labels <- rjstat::fromJSONstat(json_text, naming = "label")

    # if there are columns we should also fetch codes for, they are fetched here
    df_codes <- if (nrow(cols_to_add) > 0) {
      rjstat::fromJSONstat(json_text, naming = "id") |>
        dplyr::select(dplyr::all_of(stats::setNames(cols_to_add$code_col, cols_to_add$new_name)))
    } else NULL

    # combine label and code columns
    df_result <- dplyr::bind_cols(purrr::compact(list(df_codes, df_labels)))

    if (isTRUE(strip_code_in_label) && nrow(cols_to_add) > 0) {
      df_result <- purrr::reduce(seq_len(nrow(cols_to_add)), function(acc, i) {

        code_col <- cols_to_add$new_name[i]
        text_col <- cols_to_add$txt_col[i]

        if (!all(c(code_col, text_col) %in% names(acc))) {
          return(acc)
        }


        acc[[text_col]] <- intern_pxweb2_strip_code_in_label(
          label = acc[[text_col]],
          code = acc[[code_col]]
        )

        acc

      }, .init = df_result)

    }

    return(df_result)

  }, .progress = TRUE) |>
    purrr::list_rbind()

  # handle DeSO/RegSO versions here if requested (value "latest" or "sum", not NULL)
  result_table <- intern_pxweb2_handle_deso_regso_versions(
    result_table,
    mode = deso_regso_versions,
    value_col = "value"
  )

  # optionally split municipality code and municipality into their own columns and
  # keep only the RegSO name (and DeSO code as name) in the region column
  if (isTRUE(split_deso_regso_by_municipality)) {
    result_table <- intern_pxweb2_split_deso_regso_municipality(result_table)

    result_table <- intern_pxweb2_fill_deso_municipality_from_metadata(
      df = result_table,
      metadata = metadata
    )
  }

  # move each code column to just before its label column
  df_result <- purrr::reduce(seq_len(nrow(cols_to_add)), function(acc, i) {
    dplyr::relocate(acc, dplyr::all_of(cols_to_add$new_name[i]),
                    .before = dplyr::all_of(cols_to_add$txt_col[i]))
  }, .init = result_table)


  return(df_result)

}

#' Get when a table was last updated
#'
#' @param table Table id or metadata object.
#' @param base_url Base URL of the PxWeb API v2.
#'
#' @return Timestamp (ISO 8601) as a string, e.g. `"2026-06-02T00:59:31Z"`, or
#'   `NA` if the value is missing.
#' @export
pxweb2_table_updated <- function(
    table,
    base_url = "https://statistikdatabasen.scb.se/api/v2/tables/"
) {
  pxweb2_get_metadata(table, base_url = base_url)$updated
}

#' Check whether a table needs updating
#'
#' Compares the table's update time in PxWeb with a timestamp of your own.
#'
#' @param table Table id.
#' @param reference_datetime Timestamp to compare against, format
#'   `"YYYY-MM-DDTHH:MM:SSZ"`.
#' @param base_url Base URL of the PxWeb API v2.
#'
#' @return `TRUE` if the PxWeb table is newer than `reference_datetime`, `FALSE`
#'   otherwise, or `NA` if the table's update value is missing.
#' @export
pxweb2_table_needs_update <- function(
    table,
    reference_datetime,                 # date + time to compare against, in the same format as Statistics Sweden's `updated` in the metadata,
                                        # namely: 2026-06-02T00:59:31Z
    base_url = "https://statistikdatabasen.scb.se/api/v2/tables/"
) {

  intern_pxweb2_check_table_id(table)

  if (is.null(reference_datetime) || length(reference_datetime) != 1) {
    stop("reference_datetime must be a text value of length 1.", call. = FALSE)
  }

  format_ok <- "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$"

  if (!stringr::str_detect(reference_datetime, format_ok)) {
    stop(
      "reference_datetime must have the format 'YYYY-MM-DDTHH:MM:SSZ', e.g. '2025-05-28T06:00:00Z'.",
      call. = FALSE
    )
  }

  scb_updated <- pxweb2_table_updated(table, base_url = base_url)

  if (is.na(scb_updated)) {
    return(NA)
  }

  if (!stringr::str_detect(scb_updated, format_ok)) {
    stop(
      "The API's `updated` value does not have the expected format: ",
      scb_updated,
      call. = FALSE
    )
  }

  scb_updated > reference_datetime
} # end function pxweb2_table_needs_update


# split municipality code and municipality name into their own columns, and reduce
# the region column to just the RegSO/DeSO name (the DeSO name equals its code)
intern_pxweb2_split_deso_regso_municipality <- function(df,
                                                    region_col = "region",
                                                    region_code_col = "region_kod") {
  
  if (!all(c(region_col, region_code_col) %in% names(df))) {
    return(df)
  }
  
  region_koder <- as.character(df[[region_code_col]])
  
  grund_region_koder <- dplyr::if_else(
    stringr::str_detect(stringr::str_to_lower(region_koder), "deso|regso"),
    stringr::str_remove(region_koder, "_.*$"),
    region_koder
  )
  
  har_deso_regso <- stringr::str_detect(
    grund_region_koder,
    "^\\d{4}[ABC]\\d+|^\\d{4}R\\d+"
  ) |
    stringr::str_detect(
      stringr::str_to_lower(region_koder),
      "deso|regso"
    )
  
  if (!any(har_deso_regso, na.rm = TRUE)) {
    return(df)
  }
  
  df |>
    dplyr::mutate(
      .region_tmp = as.character(.data[[region_col]]),
      .region_kod_tmp = as.character(.data[[region_code_col]]),
      
      .grund_region_kod = dplyr::if_else(
        stringr::str_detect(stringr::str_to_lower(.region_kod_tmp), "deso|regso"),
        stringr::str_remove(.region_kod_tmp, "_.*$"),
        .region_kod_tmp
      ),
      
      .ar_deso = stringr::str_detect(.grund_region_kod, "^\\d{4}[ABC]\\d+"),
      .ar_regso = stringr::str_detect(.grund_region_kod, "^\\d{4}R\\d+"),
      .ar_deso_regso = .ar_deso | .ar_regso,
      
      .region_text_ren = .region_tmp,
      .region_text_ren = stringr::str_remove(
        .region_text_ren,
        paste0("^", stringr::str_escape(.region_kod_tmp), "\\s+")
      ),
      .region_text_ren = stringr::str_remove(
        .region_text_ren,
        paste0("^", stringr::str_escape(.grund_region_kod), "\\s+")
      ),
      .region_text_ren = stringr::str_trim(.region_text_ren),
      
      .har_parentes = stringr::str_detect(.region_text_ren, "\\([^()]+\\)"),
      
      kommun_kod = dplyr::if_else(
        .ar_deso_regso & stringr::str_detect(.grund_region_kod, "^\\d{4}"),
        stringr::str_sub(.grund_region_kod, 1, 4),
        NA_character_
      ),
      
      kommun = dplyr::case_when(
        .har_parentes ~ stringr::str_trim(
          stringr::str_remove(.region_text_ren, "\\s*\\([^()]+\\)\\s*$")
        ),
        .ar_deso & .region_text_ren != "" & .region_text_ren != .grund_region_kod ~ .region_text_ren,
        TRUE ~ NA_character_
      ),
      
      "{region_col}" := dplyr::case_when(
        .har_parentes ~ stringr::str_match(.region_text_ren, "\\(([^()]+)\\)")[, 2],
        .ar_deso ~ .grund_region_kod,
        .ar_regso & .region_text_ren != "" ~ .region_text_ren,
        .ar_regso ~ .grund_region_kod,
        TRUE ~ .region_tmp
      ),
      
      "{region_code_col}" := dplyr::if_else(
        .ar_deso_regso,
        .grund_region_kod,
        .region_kod_tmp
      )
    ) |>
    dplyr::select(
      -.region_tmp,
      -.region_kod_tmp,
      -.grund_region_kod,
      -.ar_deso,
      -.ar_regso,
      -.ar_deso_regso,
      -.region_text_ren,
      -.har_parentes
    ) |>
    dplyr::relocate(
      dplyr::any_of(c("kommun_kod", "kommun")),
      .after = dplyr::all_of(region_code_col)
    )
}

# handle several different versions of DeSO or RegSO in the same table
intern_pxweb2_handle_deso_regso_versions <- function(df,
                                                       mode = c("latest", "sum"),
                                                       region_col = "region",
                                                       region_code_col = "region_kod",
                                                       value_col = "value") {
  
  if (is.null(mode)) return(df)
  
  mode <- match.arg(mode)
  
  if (!all(c(region_col, region_code_col, value_col) %in% names(df))) {
    return(df)
  }
  
  tmp <- df |>
    dplyr::mutate(
      .row_id = dplyr::row_number(),
      .region_kod_tmp = as.character(.data[[region_code_col]]),
      .region_tmp = as.character(.data[[region_col]]),
      
      .grund_region_kod = stringr::str_remove(.region_kod_tmp, "_.*$"),
      
      .ar_deso_regso = stringr::str_detect(
        .grund_region_kod,
        "^\\d{4}[ABC]\\d+|^\\d{4}R\\d+"
      ),
      
      .version_ar = stringr::str_match(
        stringr::str_to_lower(.region_kod_tmp),
        "_(?:deso|regso)(\\d{4})"
      )[, 2],
      .version_ar = suppressWarnings(as.integer(.version_ar)),
      .version_ar = dplyr::if_else(is.na(.version_ar), 0L, .version_ar),
      
      .har_version = stringr::str_detect(
        stringr::str_to_lower(.region_kod_tmp),
        "_(?:deso|regso)\\d{4}"
      ),
      
      .value_num = suppressWarnings(as.numeric(.data[[value_col]])),
      .har_data = !is.na(.value_num) & .value_num != 0
    )
  
  tmp_other <- tmp |>
    dplyr::filter(!.ar_deso_regso)

  tmp_deso_regso <- tmp |>
    dplyr::filter(.ar_deso_regso)
  
  if (nrow(tmp_deso_regso) == 0) {
    return(
      tmp |>
        dplyr::select(
          -.row_id,
          -.region_kod_tmp,
          -.region_tmp,
          -.grund_region_kod,
          -.ar_deso_regso,
          -.version_ar,
          -.har_version,
          -.value_num,
          -.har_data
        )
    )
  }
  
  grouping_cols <- setdiff(
    names(tmp_deso_regso),
    c(
      region_code_col,
      region_col,
      value_col,
      "kommun_kod",
      "kommun",
      ".row_id",
      ".region_kod_tmp",
      ".region_tmp",
      ".grund_region_kod",
      ".ar_deso_regso",
      ".version_ar",
      ".har_version",
      ".value_num",
      ".har_data"
    )
  )
  
  grouping_cols <- c(".grund_region_kod", grouping_cols)

  if (mode == "latest") {

    tmp_deso_regso_done <- tmp_deso_regso |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) |>
      dplyr::arrange(
        dplyr::desc(.har_data),
        dplyr::desc(.version_ar),
        dplyr::desc(.har_version),
        .row_id,
        .by_group = TRUE
      ) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        "{region_code_col}" := .grund_region_kod
      )

  } else if (mode == "sum") {

    tmp_deso_regso_done <- tmp_deso_regso |>
      dplyr::mutate(
        "{region_code_col}" := .grund_region_kod
      ) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(region_code_col, grouping_cols)))) |>
      dplyr::arrange(
        dplyr::desc(.har_data),
        dplyr::desc(.version_ar),
        .row_id,
        .by_group = TRUE
      ) |>
      dplyr::summarise(
        "{region_col}" := dplyr::first(.data[[region_col]]),
        "{value_col}" := sum(.value_num, na.rm = TRUE),
        .row_id = dplyr::first(.row_id),
        .groups = "drop"
      )
  }
  
  dplyr::bind_rows(tmp_other, tmp_deso_regso_done) |>
    dplyr::arrange(.row_id) |>
    dplyr::select(
      -dplyr::any_of(c(
        ".row_id",
        ".region_kod_tmp",
        ".region_tmp",
        ".grund_region_kod",
        ".ar_deso_regso",
        ".version_ar",
        ".har_version",
        ".value_num",
        ".har_data"
      ))
    )
}


intern_pxweb2_find_municipality_valueset_url <- function(metadata,
                                                    region_var = "Region") {
  
  region_dim <- metadata$dimension[[region_var]]
  
  if (is.null(region_dim)) {
    return(NULL)
  }
  
  cl <- purrr::pluck(region_dim, "extension", "codelists", .default = NULL)
  
  if (is.null(cl) || length(cl) == 0) {
    return(NULL)
  }
  
  cl_df <- purrr::map_dfr(cl, function(item) {
    tibble::tibble(
      id = purrr::pluck(item, "id", .default = NA_character_),
      label = purrr::pluck(item, "label", .default = NA_character_),
      type = purrr::pluck(item, "type", .default = NA_character_),
      url = purrr::pluck(item, "links", 1, "href", .default = NA_character_)
    )
  })
  
  hit <- cl_df |>
    dplyr::filter(
      stringr::str_to_lower(type) == "valueset",
      stringr::str_detect(stringr::str_to_lower(label), "kommun") |
        stringr::str_detect(stringr::str_to_lower(id), "kommun")
    )
  # note: "kommun" is Statistics Sweden's own term for municipality and appears
  # verbatim in the API metadata, so it is matched literally here.

  if (nrow(hit) == 0) {
    return(NULL)
  }

  hit$url[[1]]
}


intern_pxweb2_municipality_key_from_metadata <- function(metadata,
                                                           region_var = "Region") {
  
  url <- intern_pxweb2_find_municipality_valueset_url(
    metadata = metadata,
    region_var = region_var
  )
  
  if (is.null(url) || is.na(url) || identical(url, "")) {
    return(NULL)
  }
  
  resp <- intern_pxweb2_GET(url, httr::accept_json())
  httr::stop_for_status(resp)
  
  cl <- jsonlite::fromJSON(
    httr::content(resp, "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
  
  values <- purrr::pluck(cl, "values", .default = NULL)
  
  if (is.null(values) || length(values) == 0) {
    return(NULL)
  }
  
  municipality_key <- purrr::map_dfr(values, function(x) {
    tibble::tibble(
      municipality_code = as.character(purrr::pluck(x, "code", .default = NA_character_)),
      municipality = as.character(purrr::pluck(x, "label", .default = NA_character_))
    )
  }) |>
    dplyr::filter(!is.na(municipality_code), !is.na(municipality)) |>
    dplyr::mutate(
      municipality = intern_pxweb2_strip_code_prefix_in_label(
        label = municipality,
        code = municipality_code
      )
    ) |>
    dplyr::distinct(municipality_code, .keep_all = TRUE)
  return(municipality_key)
}

intern_pxweb2_fill_deso_municipality_from_metadata <- function(df,
                                                         metadata,
                                                         region_code_col = "region_kod",
                                                         municipality_code_col = "kommun_kod",
                                                         municipality_col = "kommun") {
  
  if (!all(c(region_code_col, municipality_code_col, municipality_col) %in% names(df))) {
    return(df)
  }
  
  region_code <- as.character(df[[region_code_col]])

  is_deso <- stringr::str_detect(region_code, "^\\d{4}[ABC]\\d+")

  if (!any(is_deso, na.rm = TRUE)) {
    return(df)
  }

  missing_municipality <- is.na(df[[municipality_col]]) | df[[municipality_col]] == ""

  if (!any(is_deso & missing_municipality, na.rm = TRUE)) {
    return(df)
  }

  municipality_key <- intern_pxweb2_municipality_key_from_metadata(metadata)

  if (is.null(municipality_key) || nrow(municipality_key) == 0) {
    return(df)
  }

  match_idx <- match(
    as.character(df[[municipality_code_col]]),
    municipality_key$municipality_code
  )

  municipality_from_metadata <- municipality_key$municipality[match_idx]

  fill <- is_deso & missing_municipality & !is.na(municipality_from_metadata)

  df[[municipality_col]][fill] <- municipality_from_metadata[fill]

  df
}


intern_pxweb2_strip_code_prefix_in_label <- function(label, code) {

  label_chr <- as.character(label)
  code_chr <- as.character(code)

  purrr::map2_chr(label_chr, code_chr, function(lbl, k) {

    if (is.na(lbl) || is.na(k)) {
      return(lbl)
    }

    cleaned <- stringr::str_remove(
      lbl,
      paste0("^", stringr::str_escape(k), "\\s+")
    ) |>
      stringr::str_trim()

    if (identical(cleaned, "")) {
      lbl
    } else {
      cleaned
    }
  })
}


# helper: check whether the query contains "9999"
intern_pxweb2_query_has_latest_period <- function(query, latest_period_code = "9999") {
  
  if (is.null(query) || is.null(latest_period_code)) {
    return(FALSE)
  }
  
  if (intern_pxweb2_is_pxweb_query_list(query)) {
    return(
      any(
        purrr::map_lgl(query$selection, function(x) {
          latest_period_code %in% as.character(unlist(x$valueCodes, use.names = FALSE))
        })
      )
    )
  }
  
  any(
    purrr::map_lgl(query, function(x) {
      latest_period_code %in% as.character(x)
    })
  )
}

# helper: replace "9999" with the chosen common period
intern_pxweb2_replace_latest_period_in_query <- function(query, latest_period_code, latest_period) {
  
  if (is.null(query) || is.null(latest_period_code)) {
    return(query)
  }
  
  if (intern_pxweb2_is_pxweb_query_list(query)) {
    
    query$selection <- purrr::map(query$selection, function(x) {
      
      x$valueCodes <- purrr::map(x$valueCodes, function(v) {
        v <- as.character(v)
        v[v == latest_period_code] <- latest_period
        v
      })
      
      x
    })
    
    return(query)
  }
  
  purrr::map(query, function(x) {
    x <- as.character(x)
    x[x == latest_period_code] <- latest_period
    x
  })
}

# helper: find the time variable
# relies on pxweb2_get_variables() returning something with code, label, role.
intern_pxweb2_find_time_variable <- function(variables_df) {

  candidate <- variables_df |>
    dplyr::mutate(
      code_lower = stringr::str_to_lower(code),
      label_lower = stringr::str_to_lower(label),
      role_lower = stringr::str_to_lower(role)
    ) |>
    dplyr::filter(
      role_lower == "time" |
        code_lower %in% c("tid", "\u00e5r", "ar", "time", "m\u00e5nad", "manad") |
        label_lower %in% c("tid", "\u00e5r", "ar", "time", "m\u00e5nad", "manad")
    )

  if (nrow(candidate) == 0) {
    stop("Could not identify a time variable in the metadata.", call. = FALSE)
  }

  if (nrow(candidate) > 1) {
    stop(
      "Several possible time variables found: ",
      paste(candidate$code, collapse = ", "),
      call. = FALSE
    )
  }

  candidate$code[[1]]
}

# helper: fetch values for the time variable
intern_pxweb2_get_time_values <- function(metadata) {

  variables_df <- pxweb2_get_variables(metadata)
  valid_values_list <- pxweb2_get_values(metadata)

  time_var <- intern_pxweb2_find_time_variable(variables_df)

  if (!time_var %in% names(valid_values_list)) {
    stop(
      "The time variable ",
      time_var,
      " does not exist as a name in valid_values_list.",
      call. = FALSE
    )
  }
  
  time_df <- valid_values_list[[time_var]]

  if (!is.data.frame(time_df)) {
    return(as.character(time_df))
  }
  
  possible_code_cols <- c(
    "code",
    "value",
    "id",
    "values",
    "valueCode",
    "value_code",
    "kod"
  )

  code_col <- intersect(possible_code_cols, names(time_df))[1]

  if (is.na(code_col)) {
    code_col <- names(time_df)[1]

    warning(
      "Could not identify a code column for the time variable ",
      time_var,
      ". Using the first column: ",
      code_col,
      call. = FALSE
    )
  }

  time_df[[code_col]] |>
    as.character()
}


# find the latest period when several tables are passed
intern_pxweb2_latest_common_period <- function(metadata_list) {

  periods_list <- metadata_list |>
    purrr::map(intern_pxweb2_get_time_values)

  common_periods <- Reduce(intersect, periods_list)

  if (length(common_periods) == 0) {
    stop(
      "There is no common time period across the tables.",
      call. = FALSE
    )
  }

  common_periods |>
    sort(decreasing = TRUE) |>
    (\(x) x[[1]])()
}

intern_pxweb2_latest_period_all_tables <- function(metadata_list) {

  periods_list <- metadata_list |>
    purrr::map(intern_pxweb2_get_time_values)

  all_periods <- Reduce(union, periods_list)

  if (length(all_periods) == 0) {
    stop(
      "Could not find any time values in the tables.",
      call. = FALSE
    )
  }

  all_periods |>
    sort(decreasing = TRUE) |>
    (\(x) x[[1]])()
}


# helper: warn about differing structure
intern_pxweb2_warn_if_different_structure <- function(result_list) {

  columns_list <- result_list |>
    purrr::map(names)

  all_columns <- Reduce(union, columns_list)

  missing_list <- columns_list |>
    purrr::imap(function(columns, name) {
      setdiff(all_columns, columns)
    })

  has_differences <- any(lengths(missing_list) > 0)

  if (isTRUE(has_differences)) {

    details <- missing_list |>
      purrr::imap_chr(function(missing, name) {
        if (length(missing) == 0) {
          paste0(name, ": no missing columns")
        } else {
          paste0(name, ": missing ", paste(missing, collapse = ", "))
        }
      }) |>
      paste(collapse = "\n")

    warning(
      "The tables do not have an identical column structure. ",
      "Missing columns are filled with NA by dplyr::bind_rows().\n",
      details,
      call. = FALSE
    )
  }

  invisible(NULL)
}

# main function for several tables
intern_pxweb2_get_multiple_tables <- function(
    tables,
    query = NULL,
    lang = "sv",
    output_format = "json-stat2",
    base_url = "https://statistikdatabasen.scb.se/api/v2/tables/",
    on_all_values_invalid = "stop",
    allow_label_values = TRUE,
    strip_code_in_label = TRUE,
    latest_period_code = "9999",
    allow_api_wildcards = TRUE,
    harmonise_variable_names = NULL,
    deso_regso_versions = "latest",
    split_deso_regso_by_municipality = TRUE,
    include_aggregations = "none",
    auto_limit = 30L
) {

  if (!is.character(tables) || length(tables) < 1) {
    stop("tables must be a character vector with at least one table id.", call. = FALSE)
  }
  purrr::walk(tables, intern_pxweb2_check_table_id)

  metadata_list <- tables |>
    purrr::map(\(t) pxweb2_get_metadata(t, base_url = base_url))

  names(metadata_list) <- tables

  # resolve "auto" once, based on the total number of code lists across ALL tables
  if (identical(include_aggregations, "auto")) {
    intern_cl_count <- function(meta) {
      purrr::map_int(meta$dimension, function(dim_el) {
        cl <- purrr::pluck(dim_el, "extension", "codelists", .default = NULL)
        if (is.null(cl)) 0L else sum(purrr::map_lgl(cl, ~ tolower(purrr::pluck(.x, "type", .default = "")) == "aggregation"))
      }) |> sum()
    }
    total <- purrr::map_int(metadata_list, intern_cl_count) |> sum()
    include_aggregations <- if (total <= auto_limit) {
      message(
        "include_aggregations = \"auto\": found ", total, " code lists in total (",
        length(tables), " tables, limit ", auto_limit, ") and is fetching all aggregations."
      )
      "all"
    } else {
      message(
        "include_aggregations = \"auto\": found ", total, " code lists in total (",
        length(tables), " tables), which exceeds the limit ", auto_limit,
        ". Fetching code-list metadata only (\"codelists\"). Set \"all\" to fetch everything."
      )
      "codelists"
    }
  }

  has_latest_period <- intern_pxweb2_query_has_latest_period(
    query = query,
    latest_period_code = latest_period_code
  )

  query_adjusted <- query
  tables_to_fetch <- tables

  if (isTRUE(has_latest_period)) {

    periods_list <- metadata_list |>
      purrr::map(intern_pxweb2_get_time_values)

    latest_period <- periods_list |>
      Reduce(f = union) |>
      sort(decreasing = TRUE) |>
      (\(x) x[[1]])()

    query_adjusted <- intern_pxweb2_replace_latest_period_in_query(
      query = query,
      latest_period_code = latest_period_code,
      latest_period = latest_period
    )

    tables_to_fetch <- tables[
      purrr::map_lgl(tables, function(table_id) {
        latest_period %in% periods_list[[table_id]]
      })
    ]

    message(
      "latest_period_code = '",
      latest_period_code,
      "' was replaced with the latest period in the tables: ",
      latest_period
    )

    if (length(tables_to_fetch) < length(tables)) {
      tables_skipped <- setdiff(tables, tables_to_fetch)

      message(
        "The following table(s) do not contain ",
        latest_period,
        " and are therefore not fetched: ",
        paste(tables_skipped, collapse = ", ")
      )
    }
  }

  if (length(tables_to_fetch) == 0) {
    return(NULL)
  }

  result_list <- tables_to_fetch |>
    purrr::map(function(table_id) {

      metadata_table <- metadata_list[[table_id]]
      variables_df_table <- pxweb2_get_variables(metadata_table)

      query_table <- query_adjusted |>
        intern_pxweb2_harmonise_query_names(
          variables_df = variables_df_table,
          harmonise_variable_names = harmonise_variable_names
        )

      df_table <- pxweb2_get_data(
        table = metadata_table,
        query = query_table,
        lang = lang,
        output_format = output_format,
        base_url = base_url,
        on_all_values_invalid = on_all_values_invalid,
        allow_label_values = allow_label_values,
        strip_code_in_label = strip_code_in_label,
        latest_period_code = latest_period_code,
        allow_api_wildcards = allow_api_wildcards,
        deso_regso_versions = deso_regso_versions,
        split_deso_regso_by_municipality = split_deso_regso_by_municipality,
        include_aggregations = include_aggregations,
        auto_limit = auto_limit
      )

      # if the table returned NULL (e.g. due to "null" mode) -> skip harmonisation/mutate
      if (is.null(df_table)) return(NULL)

      df_table |>
        intern_pxweb2_harmonise_result_names(
          harmonise_variable_names = harmonise_variable_names
        ) |>
        dplyr::mutate(
          table_id = table_id,
          .before = 1
        )


    })

  names(result_list) <- tables_to_fetch

  result_list <- purrr::compact(result_list)
  
  if (length(result_list) == 0) {
    return(NULL)
  }
  
  intern_pxweb2_warn_if_different_structure(result_list)
  
  dplyr::bind_rows(result_list)
} # end function intern_pxweb2_get_multiple_tables


# helper: harmonise query names
# lets the user write Region = ... even if a given table actually has the variable Kommun.
intern_pxweb2_harmonise_query_names <- function(
    query,
    variables_df,
    harmonise_variable_names = NULL
) {

  if (is.null(query) || is.null(harmonise_variable_names)) {
    return(query)
  }

  if (intern_pxweb2_is_pxweb_query_list(query)) {
    return(query)
  }

  if (is.null(names(harmonise_variable_names))) {
    stop(
      "harmonise_variable_names must be a named vector, e.g. c(\"Region\" = \"Kommun\").",
      call. = FALSE
    )
  }

  table_variables <- variables_df$code
  table_variables_lc <- stringr::str_to_lower(table_variables)

  query_names <- names(query)
  query_names_lc <- stringr::str_to_lower(query_names)

  for (standard_name in names(harmonise_variable_names)) {

    alt_name <- unname(harmonise_variable_names[[standard_name]])

    standard_name_lc <- stringr::str_to_lower(standard_name)
    alt_name_lc <- stringr::str_to_lower(alt_name)

    query_standard_pos <- which(query_names_lc == standard_name_lc)
    table_standard_pos <- which(table_variables_lc == standard_name_lc)
    table_alt_pos <- which(table_variables_lc == alt_name_lc)

    if (
      length(query_standard_pos) > 0 &&
      length(table_standard_pos) == 0 &&
      length(table_alt_pos) > 0
    ) {
      names(query)[query_standard_pos] <- table_variables[table_alt_pos[[1]]]
    }
  }

  query
} # end function intern_pxweb2_harmonise_query_names


intern_pxweb2_strip_code_in_label <- function(label, code) {
  label_chr <- as.character(label)
  code_chr <- as.character(code)

  # if the code column happens to contain "2082 Sater" instead of just "2082",
  # take the first token as the candidate code.
  code_candidate <- stringr::str_extract(code_chr, "^\\S+")

  # only strip if code_candidate actually looks code-like, i.e. contains a digit.
  # this prevents "Stockholms lan" from becoming "lan" in tables where the label is already clean.
  code_looks_like_code <- stringr::str_detect(code_candidate, "\\d")

  cleaned <- dplyr::if_else(
    is.na(label_chr) | is.na(code_candidate) | !code_looks_like_code,
    label_chr,
    stringr::str_remove(
      label_chr,
      paste0("^", stringr::str_escape(code_candidate), "\\s+")
    )
  )

  cleaned <- stringr::str_trim(cleaned)

  # if the label was only a code, e.g. "0114A0010", keep the original
  dplyr::if_else(
    is.na(cleaned) | cleaned == "",
    label_chr,
    cleaned
  )
}


# helper: harmonise result names
# renames result columns after fetching, e.g. Kommun -> Region and kommun_kod -> region_kod
intern_pxweb2_harmonise_result_names <- function(
    df,
    harmonise_variable_names = NULL
) {

  if (is.null(df) || is.null(harmonise_variable_names)) {
    return(df)
  }

  if (is.null(names(harmonise_variable_names))) {
    stop(
      "harmonise_variable_names must be a named vector, e.g. c(\"Region\" = \"Kommun\").",
      call. = FALSE
    )
  }

  for (standard_name in names(harmonise_variable_names)) {

    alt_name <- unname(harmonise_variable_names[[standard_name]])

    standard_name_lc <- stringr::str_to_lower(standard_name)
    alt_name_lc <- stringr::str_to_lower(alt_name)

    names_lc <- stringr::str_to_lower(names(df))

    # label column, e.g. kommun -> region
    alt_pos <- which(names_lc == alt_name_lc)
    standard_pos <- which(names_lc == standard_name_lc)

    if (length(alt_pos) > 0 && length(standard_pos) == 0) {
      names(df)[alt_pos] <- standard_name_lc
    }

    # code column, e.g. kommun_kod -> region_kod
    alt_code <- paste0(alt_name_lc, "_kod")
    standard_code <- paste0(standard_name_lc, "_kod")

    names_lc <- stringr::str_to_lower(names(df))
    
    alt_code_pos <- which(names_lc == alt_code)
    standard_code_pos <- which(names_lc == standard_code)

    if (length(alt_code_pos) > 0 && length(standard_code_pos) == 0) {
      names(df)[alt_code_pos] <- standard_code
    }
  }

  df
} # end function intern_pxweb2_harmonise_result_names

intern_pxweb2_resolve_api_wildcards <- function(query_list,
                                                valid_values_list,
                                                allow_api_wildcards = TRUE) {
  
  if (!isTRUE(allow_api_wildcards)) return(query_list)
  
  wildcard_to_regex <- function(x) {
    x <- stringr::str_replace_all(x, "([\\.\\+\\^\\$\\(\\)\\[\\]\\{\\}\\|\\\\])", "\\\\\\1")
    x <- stringr::str_replace_all(x, "\\*", ".*")
    x <- stringr::str_replace_all(x, "\\?", ".")
    paste0("^", x, "$")
  }
  
  query_list$selection <- purrr::map(query_list$selection, function(s) {
    
    var <- s$variableCode
    vals <- unlist(s$valueCodes, use.names = FALSE)
    
    if (length(vals) == 0 || is.null(vals)) return(s)
    
    ok_tbl <- valid_values_list[[var]]
    if (is.null(ok_tbl) || !"code" %in% names(ok_tbl)) return(s)
    
    if ("type" %in% names(ok_tbl)) {
      ok_tbl <- ok_tbl |>
        dplyr::filter(type == "Variable")
    }
    
    ok_codes <- ok_tbl$code
    
    vals_expanded <- purrr::map(vals, function(v) {
      
      # leave full wildcard, top/bottom and aggregation specials alone
      if (identical(v, "*") ||
          identical(v, "**") ||
          stringr::str_detect(v, "^\\s*(top|bottom)\\s*\\(\\s*\\d+\\s*\\)\\s*$") ||
          stringr::str_detect(v, "^agg_")) {
        return(v)
      }

      # only expand if the value contains * or ?
      if (stringr::str_detect(v, "[\\*\\?]")) {
        pattern <- wildcard_to_regex(v)
        hits <- ok_codes[stringr::str_detect(ok_codes, pattern)]
        
        if (length(hits) == 0) {
          return(v)
        }
        
        return(hits)
      }
      
      v
    }) |>
      unlist(use.names = FALSE) |>
      unique()
    
    s$valueCodes <- as.list(vals_expanded)
    s
  })
  
  query_list
}


intern_pxweb2_resolve_latest_period <- function(query_list,
                                              variables_df,
                                              valid_values_list,
                                              latest_period_code = "9999") {
  
  if (is.null(latest_period_code)) return(query_list)
  
  time_vars <- variables_df |>
    dplyr::filter(role == "time") |>
    dplyr::pull(code)
  
  if (length(time_vars) == 0) return(query_list)
  
  query_list$selection <- purrr::map(query_list$selection, function(s) {
    
    var <- s$variableCode
    
    if (!var %in% time_vars) return(s)
    
    vals <- unlist(s$valueCodes, use.names = FALSE)
    
    if (!latest_period_code %in% vals) return(s)
    
    ok_tbl <- valid_values_list[[var]]
    
    if (is.null(ok_tbl) || !"code" %in% names(ok_tbl)) {
      stop("Cannot find valid time values for the variable: ", var)
    }

    # use only ordinary variable values, not aggregation rows
    if ("type" %in% names(ok_tbl)) {
      ok_tbl <- ok_tbl |>
        dplyr::filter(type == "Variable")
    }

    latest_value <- ok_tbl |>
      dplyr::pull(code) |>
      dplyr::last()

    if (is.na(latest_value) || length(latest_value) == 0) {
      stop("Cannot determine the latest time value for the variable: ", var)
    }

    vals <- dplyr::if_else(vals == latest_period_code, latest_value, vals)
    
    s$valueCodes <- as.list(vals)
    s
  })
  
  query_list
}

intern_pxweb2_sanitize_query_values <- function(query, valid_values_list, 
                                                on_all_values_invalid = "stop",
                                                warn = TRUE) {
  if (!on_all_values_invalid %in% c("stop", "*", "null")) {
    stop("Invalid value for on_all_values_invalid. Allowed: \"stop\", \"*\", \"null\".")
  }

  removed <- list()

  query$selection <- purrr::map(query$selection, function(s) {
    var  <- s$variableCode
    vals <- unlist(s$valueCodes, use.names = FALSE)

    # leave special expressions alone
    if (length(vals) == 1 && (identical(vals, "*") ||
                              grepl("^\\s*(top|bottom)\\s*\\(\\s*\\d+\\s*\\)\\s*$", vals, ignore.case = TRUE))) {
      return(s)
    }

    ok_tbl <- valid_values_list[[var]]
    if (is.null(ok_tbl) || !"code" %in% names(ok_tbl)) return(s)

    ok <- ok_tbl$code
    keep <- vals[vals %in% ok]
    bad  <- setdiff(vals, keep)

    if (length(bad) > 0) removed[[var]] <<- unique(c(removed[[var]], bad))

    # if everything was invalid: stop or replace with "*"
    if (length(keep) == 0) {

      if (on_all_values_invalid == "stop") {
        stop(
          "Invalid values in the query for the variable '", var, "': ",
          paste(vals, collapse = ", "),
          ". The run is stopped because of ",
          "'on_all_values_invalid = \"stop\"'."
        )
      }

      if (on_all_values_invalid == "*") {
        if (warn) {
          cat(
            "Invalid values in the query for the variable '", var,
            "'; all values are included because ",
            "'on_all_values_invalid = \"*\"'.\n",
            sep = ""
          )
        }
        s$valueCodes <- list("*")
        return(s)
      }

      if (on_all_values_invalid == "null") {
        if (warn) {
          cat(
            "Invalid values in the query for the variable '", var,
            "'; the variable is dropped because ",
            "'on_all_values_invalid = \"null\"'.\n",
            sep = ""
          )
        }
        s$valueCodes <- NULL
        return(s)
      }
    }

    s$valueCodes <- as.list(keep)
    s
  })

  if (warn && length(removed) > 0) {
    msg <- paste(
      purrr::imap_chr(removed, ~ paste0(.y, ": ", paste(.x, collapse = ", "))),
      collapse = " | "
    )
    cat(paste0("The following values do not exist in the table and were therefore removed:\n", msg, "\n"))
  }

  return(query)
}


intern_pxweb2_rate_limiter <- local({
  times <- numeric(0)              # timestamps (sec) of recent calls
  max_calls <- 30L
  window <- 10                      # seconds
  safety <- 0.05                    # small margin (50 ms)

  function() {
    now <- as.numeric(Sys.time())

    # discard calls older than the window
    times <<- times[now - times < window]

    # if we already have max_calls in the window: wait until the first falls out
    if (length(times) >= max_calls) {
      wait <- window - (now - times[1]) + safety
      if (wait > 0) Sys.sleep(wait)
      now <- as.numeric(Sys.time())
      times <<- times[now - times < window]
    }

    # register that we are making a call now
    times <<- c(times, now)
    invisible(NULL)
  }
})


.pxweb2_api_log <- new.env(parent = emptyenv())

.pxweb2_api_log$rows <- list()

intern_pxweb2_api_log_reset <- function() {
  .pxweb2_api_log$rows <- list()
  invisible(NULL)
}

intern_pxweb2_api_log_get <- function() {
  if (length(.pxweb2_api_log$rows) == 0) {
    return(tibble::tibble(
      time = as.POSIXct(character()),
      method = character(),
      url = character(),
      status = integer(),
      attempt = integer(),
      endpoint_type = character()
    ))
  }
  tibble::as_tibble(do.call(rbind, lapply(.pxweb2_api_log$rows, as.data.frame)))
}

intern_pxweb2_api_endpoint_type <- function(url) {
  url_chr <- as.character(url)
  
  if (grepl("/metadata", url_chr, fixed = TRUE)) {
    return("metadata")
  }
  
  if (grepl("/data", url_chr, fixed = TRUE)) {
    return("data")
  }
  
  if (grepl("/tables", url_chr, fixed = TRUE)) {
    return("tables")
  }
  
  if (grepl("/codelists", url_chr, fixed = TRUE) ||
      grepl("codelist", url_chr, ignore.case = TRUE)) {
    return("codelist")
  }

  "other"
}

intern_pxweb2_api_log_add <- function(method, url, status, attempt) {
  .pxweb2_api_log$rows <- c(
    .pxweb2_api_log$rows,
    list(list(
      time = Sys.time(),
      method = as.character(method),
      url = as.character(url),
      status = as.integer(status),
      attempt = as.integer(attempt),
      endpoint_type = intern_pxweb2_api_endpoint_type(url)
    ))
  )
  
  invisible(NULL)
}

intern_pxweb2_GET <- function(url, ..., max_tries = 3, retry_wait_default = 10) {
  
  resp <- NULL
  
  for (attempt in seq_len(max_tries)) {
    
    intern_pxweb2_rate_limiter()
    
    resp <- httr::GET(url, ...)
    
    intern_pxweb2_api_log_add(
      method = "GET",
      url = url,
      status = httr::status_code(resp),
      attempt = attempt
    )
    
    if (httr::status_code(resp) != 429) {
      return(resp)
    }
    
    retry_after <- suppressWarnings(as.numeric(httr::headers(resp)[["retry-after"]]))
    wait <- ifelse(is.na(retry_after), retry_wait_default, retry_after)
    Sys.sleep(wait + 0.2)
  }
  
  resp
}

intern_pxweb2_POST <- function(url, ..., max_tries = 3, retry_wait_default = 10) {
  
  resp <- NULL
  
  for (attempt in seq_len(max_tries)) {
    
    intern_pxweb2_rate_limiter()
    
    resp <- httr::POST(url, ...)
    
    intern_pxweb2_api_log_add(
      method = "POST",
      url = url,
      status = httr::status_code(resp),
      attempt = attempt
    )
    
    if (httr::status_code(resp) != 429) {
      return(resp)
    }
    
    retry_after <- suppressWarnings(as.numeric(httr::headers(resp)[["retry-after"]]))
    wait <- ifelse(is.na(retry_after), retry_wait_default, retry_after)
    Sys.sleep(wait + 0.2)
  }
  
  resp
}


intern_pxweb2_make_chunks <- function(variables_df, query, valid_values_list,
                                      max_cells = 150000) {
  total_cells <- intern_pxweb2_count_cells(variables_df, query)
  
  if (total_cells <= max_cells) {
    return(list(query))
  }
  
  split_var <- intern_pxweb2_choose_split_variable(variables_df, query)
  
  selected_vals <- intern_pxweb2_get_valuecodes(query, split_var)
  
  if (is.null(selected_vals) || length(selected_vals) == 0 ||
      (length(selected_vals) == 1 && identical(selected_vals, "*"))) {
    
    ok_tbl <- valid_values_list[[split_var]]
    
    if ("type" %in% names(ok_tbl)) {
      ok_tbl <- ok_tbl |>
        dplyr::filter(type == "Variable")
    }
    
    all_vals <- ok_tbl |>
      dplyr::pull(code)
    
  } else {
    
    all_vals <- selected_vals
  }
  
  
  # greedy packing (purrr::accumulate): fill as close to max_cells as possible
  state <- purrr::accumulate(
    all_vals,
    .init = list(
      cur_vals = character(),
      cur_cells = 0L,
      chunks = list()
    ),
    .f = function(st, v) {
      # the cost of adding v (incl. other dimensions)
      cost <- intern_pxweb2_count_cells(
        variables_df,
        intern_pxweb2_set_valuecodes_in_query(query, split_var, v)
      )

      # if v on its own is larger than max_cells (unlikely, but guard for it)
      if (cost > max_cells) {
        if (length(st$cur_vals) > 0) {
          st$chunks <- append(st$chunks, list(st$cur_vals))
          st$cur_vals <- character()
          st$cur_cells <- 0L
        }
        st$chunks <- append(st$chunks, list(v))
        return(st)
      }
      
      # if it does not fit in the current chunk: close the chunk and start a new one
      if (st$cur_cells + cost > max_cells && length(st$cur_vals) > 0) {
        st$chunks <- append(st$chunks, list(st$cur_vals))
        st$cur_vals <- v
        st$cur_cells <- cost
        return(st)
      }

      # otherwise add to the current chunk
      st$cur_vals <- c(st$cur_vals, v)
      st$cur_cells <- st$cur_cells + cost
      st
    }
  ) 
  
  state <- state[[length(state)]]
  
  # add the last chunk if there is one
  if (length(state$cur_vals) > 0) {
    state$chunks <- append(state$chunks, list(state$cur_vals))
  }
  
  purrr::map(state$chunks, ~ intern_pxweb2_set_valuecodes_in_query(query, split_var, .x))
}


intern_pxweb2_set_valuecodes_in_query <- function(query, variable, values) {
  idx <- purrr::detect_index(
    query$selection,
    ~ identical(.x$variableCode, variable)
  )
  
  if (idx == 0L) {
    stop("Variable missing from query: ", variable)
  }
  
  query$selection[[idx]]$valueCodes <- as.list(values)
  query
}


intern_pxweb2_choose_split_variable <- function(variables_df, query = list()) {
  # choose a variable to split on when a request has more than 150,000 rows

  candidates <- variables_df |>
    dplyr::filter(
      role != "time",
      role != "contents"
    )

  if (nrow(candidates) == 0) {
    stop("No suitable dimension to split on")
  }

  # prioritise geo, otherwise the largest
  candidates |>
    dplyr::mutate(priority = ifelse(role == "geo", 2, 1)) |>
    dplyr::arrange(dplyr::desc(priority), dplyr::desc(size)) |>
    dplyr::slice(1) |>
    dplyr::pull(code)
}

intern_pxweb2_list_to_query_list <- function(variables_df,
                                             query = list(),
                                             valid_values_list, 
                                             default_value = "*",
                                             allow_label_values = TRUE,
                                             warn = TRUE) {
  stopifnot(is.data.frame(variables_df))
  
  if (!all(c("code", "label", "elimination") %in% names(variables_df))) {
    stop("variables_df must at least have the columns: code, label, elimination")
  }

  # match exactly on code (fuzzy matching could be added later if wanted)
  valid_codes <- variables_df$code
  elim_map <- stats::setNames(as.logical(variables_df$elimination), variables_df$code)

  # allow both code and label (case-insensitive) in query names ---
  key_to_code <- c(
    stats::setNames(variables_df$code, tolower(variables_df$code)),
    stats::setNames(variables_df$code, tolower(variables_df$label))
  )

  # normalise the query: map names (code/label) -> code
  q_names <- names(query)
  q_keys  <- tolower(q_names)

  mapped_codes <- unname(key_to_code[q_keys])

  # unknown keys = those that could not be mapped
  unknown <- q_names[is.na(mapped_codes)]
  
  # build a normalised query keyed by code
  query_norm <- query[!is.na(mapped_codes)]
  names(query_norm) <- mapped_codes[!is.na(mapped_codes)]

  # --- allow labels as valueCodes (only if they are not already valid codes) ---
  if (isTRUE(allow_label_values) && !is.null(valid_values_list) && length(query_norm) > 0) {
    query_norm <- purrr::imap(query_norm, function(val, var) {
      # leave special cases alone
      if (is.null(val) || (length(val) == 1 && (is.na(val) || identical(val, "*"))) ||
          (is.character(val) && length(val) == 1 &&
           grepl("^\\s*(top|bottom)\\s*\\(\\s*\\d+\\s*\\)\\s*$", val, ignore.case = TRUE))) {
        return(val)
      }
      
      ok_tbl <- valid_values_list[[var]]
      if (is.null(ok_tbl) || !all(c("code", "label") %in% names(ok_tbl))) return(val)
      
      codes <- ok_tbl$code
      lbl_map <- stats::setNames(ok_tbl$code, tolower(ok_tbl$label))
      
      v <- as.character(val)
      v2 <- purrr::map_chr(v, function(x) {
        #if (x %in% codes) x else (lbl_map[[tolower(x)]] %||% x)
        if (x %in% codes) x else {
          hit <- unname(lbl_map[tolower(x)])
          if (length(hit) == 0 || is.na(hit) || identical(hit, character(0))) x else hit
        }
        
      })
      v2 <- unique(v2)
      return(v2)
    })
  }
  
  
  # warn if the same code was given more than once (via both code and label)
  dup_codes <- names(query_norm)[duplicated(names(query_norm))]
  if (warn && length(dup_codes) > 0) {
    warning("The same variable was given more than once (via code/label). Last wins: ",
            paste(unique(dup_codes), collapse = ", "))
  }
  query_norm <- query_norm[!duplicated(names(query_norm), fromLast = TRUE)]

  # if the user passes unknown variables -> warn but ignore
  if (warn && length(unknown) > 0) {
    warning("Unknown variables in `query` are ignored: ", paste(unknown, collapse = ", "))
  }

  # build the selection, but:
  # - default "*" for every variable that exists in the table
  # - if query[[var]] is NA -> drop the variable (if eliminable; otherwise warn)
  selection <- purrr::map(valid_codes, function(var) {
    val <- if (var %in% names(query_norm)) query_norm[[var]] else default_value

    # NA => drop (i.e. return NULL so we can purrr::compact() later)
    if (length(val) == 1 && is.na(val)) {
      if (warn && !isTRUE(elim_map[[var]])) {
        warning(
          "The variable `", var, "` was set to NA (dropped), but is not eliminable according to the metadata. ",
          "The API may still require a selection for this dimension."
        )
      }
      return(NULL)
    }

    # allow the user to write "*" or TOP(1)/Top(1)/top(1) etc.
    # valueCodes must always become a list(...) for JSON
    if (identical(val, "*")) {
      vc <- list("*")
    } else if (is.character(val) && length(val) == 1) {
      vc <- list(val)
    } else {
      # vector (e.g. c("20","21","17")) -> list("20","21","17")
      vc <- as.list(val)
    }
    
    list(
      variableCode = var,
      valueCodes = vc
    )
  }) |>
    purrr::compact()
  
  list(selection = selection)
} 


intern_pxweb2_create_variable_query_list <- function(variables_df,
                                                     default_value = "*",
                                                     overrides = list()) {
  selections <- purrr::map(
    variables_df$code,
    function(var) {
      v <- if (!is.null(overrides[[var]])) overrides[[var]] else default_value
      
      list(
        variableCode = var,
        valueCodes = if (length(v) == 1 && is.na(v)) {
          list()                       # empty = removed in the next step
        } else if (identical(v, "*") || (is.character(v) && length(v) == 1)) {
          list(v)                      # e.g. "*", "top(4)"
        } else {
          as.list(as.character(v))     # e.g. c("20","21","17") -> list("20","21","17")
        }
      )
    }
  ) |>
    purrr::keep(~ length(.x$valueCodes) > 0)  # drop the ones that became NA
  
  list(selection = selections)
}



#' Get metadata for a PxWeb table
#'
#' @param table_id Table id, e.g. `"TAB6104"`.
#' @param base_url Base URL of the PxWeb API v2.
#'
#' @return Metadata as a parsed list. An error is thrown if the table does not
#'   exist (HTTP 404) on the given `base_url`.
#' @export
pxweb2_get_metadata <- function(
    table_id = NULL,
    base_url = "https://statistikdatabasen.scb.se/api/v2/tables/"
){
  if (is.null(table_id)) stop("table_id must be supplied")
  intern_pxweb2_check_table_id(table_id)

  meta_url <- paste0(base_url, table_id, "/metadata")

  resp <- intern_pxweb2_GET(meta_url, httr::accept_json())

  if (httr::status_code(resp) == 404) {
    stop(
      "Table id '", table_id, "' was not found on ", base_url,
      " (HTTP 404). Check the table id or base_url.",
      call. = FALSE
    )
  }
  httr::stop_for_status(resp)

  meta <- httr::content(resp, as = "parsed", encoding = "UTF-8")

  return(meta)
}

#' Check whether a table id exists
#'
#' Quick check against the metadata endpoint on the given `base_url`. Errors
#' other than HTTP 404 (network, 500 ...) are re-thrown, since they do not mean
#' the table is missing.
#'
#' @param table_id Table id to check.
#' @param base_url Base URL of the PxWeb API v2.
#'
#' @return `TRUE` if the table exists, `FALSE` otherwise.
#' @export
pxweb2_table_exists <- function(
    table_id,
    base_url = "https://statistikdatabasen.scb.se/api/v2/tables/"
) {
  intern_pxweb2_check_table_id(table_id)

  meta_url <- paste0(base_url, table_id, "/metadata")
  resp <- intern_pxweb2_GET(meta_url, httr::accept_json())

  if (httr::status_code(resp) == 404) return(FALSE)
  httr::stop_for_status(resp)
  TRUE
}

# derive the API root ("https://.../api/v2/") from a tables base_url
intern_pxweb2_api_root <- function(base_url) {
  sub("tables/?$", "", base_url)
}

#' Get a single code list
#'
#' Fetches one code list (value set or aggregation) by id from the PxWeb API v2.
#' Code-list ids are global and do not require a table - e.g.
#' `"vs_RegionKommun07"` (municipalities), `"vs_RegionLän07"` (counties) or
#' `"agg_RegionLA2018"` (local labour-market areas). Use
#' [pxweb2_list_codelists()] to discover which ids a given table offers.
#'
#' @param codelist_id Code-list id, e.g. `"vs_RegionKommun07"`.
#' @param lang Language for labels, `"sv"` or `"en"`.
#' @param base_url Base URL of the PxWeb API v2 (the tables endpoint); the code
#'   lists endpoint is derived from it.
#'
#' @return A `tibble` with one row per value: `code`, `label`, and `value_map`
#'   (a list column - the member codes, which for a value set is just the code
#'   itself and for an aggregation is the set of aggregated codes).
#' @export
pxweb2_get_codelist <- function(
    codelist_id,
    lang = "sv",
    base_url = "https://statistikdatabasen.scb.se/api/v2/tables/"
) {
  if (is.null(codelist_id) || length(codelist_id) != 1 || !is.character(codelist_id) ||
      is.na(codelist_id) || !nzchar(trimws(codelist_id))) {
    stop("codelist_id must be a non-empty string of length 1.", call. = FALSE)
  }

  cl_url <- paste0(intern_pxweb2_api_root(base_url), "codelists/", codelist_id)
  resp <- intern_pxweb2_GET(cl_url, httr::accept_json(), query = list(lang = lang))

  if (httr::http_error(resp)) {
    stop(
      "Could not fetch code list '", codelist_id, "' from ",
      intern_pxweb2_api_root(base_url), "codelists/ (HTTP ",
      httr::status_code(resp), "). Check the code-list id.",
      call. = FALSE
    )
  }

  cl <- httr::content(resp, as = "parsed", encoding = "UTF-8")
  values <- purrr::pluck(cl, "values", .default = list())

  if (length(values) == 0) {
    return(tibble::tibble(
      code = character(), label = character(), value_map = list()
    ))
  }

  tibble::tibble(
    code = trimws(purrr::map_chr(values, ~ .x$code %||% NA_character_)),
    label = trimws(purrr::map_chr(values, ~ .x$label %||% NA_character_)),
    value_map = purrr::map(values, ~ as.character(.x$valueMap %||% .x$code))
  )
}

#' List the code lists available for a table
#'
#' Reads a table's metadata and returns every code list (value sets and
#' aggregations) offered by its variables. There is no global code-list
#' listing in the API - they are only discoverable per table.
#'
#' @param table Table id or a metadata object from [pxweb2_get_metadata()].
#' @param base_url Base URL of the PxWeb API v2.
#'
#' @return A `tibble` with one row per code list: `variable` (the variable it
#'   belongs to), `id`, `type` (`"Valueset"` or `"Aggregation"`) and `label`.
#'   Fetch a specific one with [pxweb2_get_codelist()].
#' @export
pxweb2_list_codelists <- function(
    table,
    base_url = "https://statistikdatabasen.scb.se/api/v2/tables/"
) {
  if (is.null(table)) stop("table must be supplied, either as a table id or a metadata object.")
  if (!is.list(table)) {
    intern_pxweb2_check_table_id(table)
    metadata <- pxweb2_get_metadata(table, base_url = base_url)
  } else metadata <- table

  empty <- tibble::tibble(
    variable = character(), id = character(),
    type = character(), label = character()
  )

  rows <- purrr::imap(metadata$dimension, function(dim_el, dim_name) {
    cl <- purrr::pluck(dim_el, "extension", "codelists", .default = NULL)
    if (is.null(cl) || length(cl) == 0) return(empty)
    purrr::map_dfr(cl, function(item) {
      lbl <- purrr::pluck(item, "label", .default = NA_character_)
      if (is.list(lbl)) {
        lbl <- purrr::pluck(lbl, "sv",
                            .default = (unlist(lbl, use.names = FALSE)[1] %||% NA_character_))
      }
      tibble::tibble(
        variable = dim_name,
        id       = purrr::pluck(item, "id",   .default = NA_character_),
        type     = purrr::pluck(item, "type", .default = NA_character_),
        label    = lbl %||% NA_character_
      )
    })
  })

  dplyr::bind_rows(rows)
}

intern_pxweb2_is_pxweb_query_list <- function(x) {
  is.list(x) &&
    !is.null(x$selection) &&
    is.list(x$selection) &&
    all(purrr::map_lgl(
      x$selection,
      ~ is.list(.x) && !is.null(.x$variableCode) && !is.null(.x$valueCodes)
    ))
}

intern_pxweb2_count_cells <- function(variables_df, query = list()) {
  
  # empty query => select all values (i.e. same as "*" for all dimensions)
  if (length(query) == 0) {
    return(prod(as.integer(variables_df$size)))
  }

  if (!intern_pxweb2_is_pxweb_query_list(query)) {
    stop("`query` must be a PxWeb query_list: list(selection = list(list(variableCode=..., valueCodes=list(...)), ...))")
  }

  if (!is.data.frame(variables_df) | !all(c("code", "label") %in% names(variables_df))) {
    stop("variables_df must be a data frame with the table's variables, obtainable with pxweb2_get_variables().")
  }

  # named map: selection_map[["Region"]] = c("20","21"), etc.
  selection_map <- purrr::map(
    query$selection,
    ~{
      var  <- .x$variableCode
      vals <- unlist(.x$valueCodes, use.names = FALSE)
      stats::setNames(list(vals), var)
    }
  ) |>
    purrr::flatten()
  
  sizes <- purrr::map_int(variables_df$code, function(id) {

    # if the variable is in the query but is NA => "deselected" => cell contribution = 1
    if (!is.null(selection_map[[id]]) &&
        length(selection_map[[id]]) == 1 &&
        is.na(selection_map[[id]][1])) {
      return(1L)
    }

    # if the variable is in the query with values
    if (!is.null(selection_map[[id]])) {
      vals <- selection_map[[id]]

      # wildcard => full size for that dimension
      if (length(vals) == 1 && identical(vals, "*")) {
        return(as.integer(variables_df$size[variables_df$code == id][1]))
      }

      # top(n)/bottom(n)
      if (length(vals) == 1 &&
          is.character(vals) &&
          grepl("^\\s*(top|bottom)\\s*\\(\\s*\\d+\\s*\\)\\s*$", vals, ignore.case = TRUE)) {
        n <- as.integer(gsub(".*\\(\\s*(\\d+)\\s*\\).*", "\\1", vals, perl = TRUE))
        return(n)
      }

      # explicit codes
      return(length(vals))
    }

    # if the variable is missing from the query => assume full size
    as.integer(variables_df$size[variables_df$code == id][1])
  })
  
  prod(sizes)
}


#' Get the variables of a PxWeb table
#'
#' @param table Table id or a metadata object from [pxweb2_get_metadata()].
#' @param base_url Base URL of the PxWeb API v2.
#'
#' @return A `tibble` with one row per variable (code, label, number of values,
#'   elimination, role, etc.).
#' @export
pxweb2_get_variables <- function(
    table = NULL,             # a table id or a metadata object
    base_url = "https://statistikdatabasen.scb.se/api/v2/tables/"
) {
  if (is.null(table)) stop("table must be supplied, either as a table id or a metadata object.")

  if (!is.list(table)) {
    intern_pxweb2_check_table_id(table)
    metadata <- pxweb2_get_metadata(table, base_url = base_url)
  } else metadata <- table

  # extract all variables with both code and label, plus how many unique values they have
  variables <- tibble::tibble(
    code       = names(metadata$dimension),
    label      = purrr::map_chr(metadata$dimension, ~ .x$label %||% NA_character_),
    size = purrr::map_int(metadata$dimension, ~ length(.x$category$label %||% list())),
    elimination = purrr::map_lgl(metadata$id, ~ isTRUE(metadata$dimension[[.x]]$extension$elimination)),
    role = purrr::map_chr(metadata$id, ~ {
      if (!is.null(metadata$role$time) && .x %in% metadata$role$time) return("time")
      if (!is.null(metadata$role$metric) && .x %in% metadata$role$metric) return("contents")
      if (!is.null(metadata$role$geo) && .x %in% metadata$role$geo) return("geo")
      "other"
    }),
    show = purrr::map_chr(metadata$dimension, ~ .x$extension$show %||% NA_character_)
  )
  
  variables <- variables |> 
    dplyr::mutate(role = dplyr::if_else(tolower(code) %in% c("region"), "geo", role))
  
  return(variables)
}

#' Get the valid values of a PxWeb table's variables
#'
#' @param table Table id or a metadata object from [pxweb2_get_metadata()].
#' @param variables Optional vector of variable names if you do not want values
#'   for all variables.
#' @param return_df_if_only_one_variable If `TRUE`, a `tibble` is returned
#'   instead of a list when only one variable is requested.
#' @param include_aggregations Controls fetching of aggregations: `"auto"`,
#'   `"all"`, `"none"`, `"codelists"`, or a named vector per variable.
#' @param auto_limit Max number of code-list calls for `"auto"`.
#' @param aggregation_member_sep Character(s) that separate members in an
#'   aggregation.
#' @param base_url Base URL of the PxWeb API v2.
#'
#' @return A named list (or `tibble`, see `return_df_if_only_one_variable`) of
#'   valid values per variable.
#' @export
pxweb2_get_values <- function(
    table,                               # a table id or a metadata object from pxweb2_get_metadata()
    variables = NULL,                    # variable names if you do not want values for all variables
    return_df_if_only_one_variable = TRUE,
    include_aggregations = "auto",       # "auto"      = fetch everything if the total number of code lists <= auto_limit, otherwise "codelists"
    # "all"       = always fetch all aggregations in full
    # "none"      = nothing, not even code-list metadata
    # "codelists" = only code-list metadata (id + label, type "AggregationCodelist"), no members
    # named vector, e.g. c(Region = "agg_RegionLA2018", Alder = "all", Tid = "codelists")
    #   => selective control per variable; the name can be a code or a label (case-insensitive)
    #   => value per variable: "all", "codelists", or one/several agg ids (e.g. "agg_RegionLA2018")
    auto_limit = 30L,                    # max number of code-list calls for "auto"
    aggregation_member_sep = ";",        # aggregation members are separated with this character(s)
    base_url = "https://statistikdatabasen.scb.se/api/v2/tables/"
) {

  if (is.null(table)) stop("table must be supplied, either as a table id or a metadata object.")
  if (!is.list(table)) {
    intern_pxweb2_check_table_id(table)
    metadata <- pxweb2_get_metadata(table, base_url = base_url)
  } else metadata <- table

  # all variables: code + label
  variables_df <- tibble::tibble(
    code  = names(metadata$dimension),
    label = purrr::map_chr(metadata$dimension, ~ .x$label %||% NA_character_)
  )

  table_variables <- metadata$dimension

  # filter variables if the variables parameter is set
  if (!is.null(variables)) {
    var_not_found <- variables[!tolower(variables) %in% tolower(variables_df$code) &
                                !tolower(variables) %in% tolower(variables_df$label)]
    var_found <- variables[tolower(variables) %in% tolower(variables_df$code) |
                             tolower(variables) %in% tolower(variables_df$label)]
    if (length(var_not_found) > 0) {
      cat(paste0("The variables ", paste0(var_not_found, collapse = ", "), " do not exist in the table and are therefore omitted."))
    }
    if (length(var_found) > 0) {
      table_variables <- table_variables[
        tolower(names(table_variables)) %in% tolower(var_found) |
          tolower(purrr::map_chr(table_variables, "label")) %in% tolower(var_found)
      ]
    }
  }

  # ---------------------------------------------------------------------------
  # helper: normalise variable name/code -> code
  intern_to_var_code <- function(name) {
    hits <- variables_df$code[
      tolower(variables_df$code)  %in% tolower(name) |
        tolower(variables_df$label) %in% tolower(name)
    ]
    hits
  }

  # helper: extract code-list info (id + label + url) from a dimension element
  intern_cl_info <- function(dim_el) {
    cl <- purrr::pluck(dim_el, "extension", "codelists", .default = NULL)
    if (is.null(cl) || length(cl) == 0) {
      return(tibble::tibble(code = character(), label = character(),
                            type = character(), url = character()))
    }
    purrr::map_df(cl, function(item) {
      lbl <- purrr::pluck(item, "label", .default = NA)
      if (is.list(lbl)) {
        lbl <- purrr::pluck(lbl, "sv",
                            .default = (unlist(lbl, use.names = FALSE)[1] %||% NA_character_))
      }
      tibble::tibble(
        code  = purrr::pluck(item, "id",    .default = NA_character_),
        label = lbl %||% NA_character_,
        type  = purrr::pluck(item, "type",  .default = NA_character_),
        url   = purrr::pluck(item, "links", 1, "href", .default = NA_character_)
      )
    }) |>
      dplyr::filter(tolower(type) == "aggregation")
  }
  
  # ---------------------------------------------------------------------------
  # count the total number of code lists in the table (for "auto" mode)
  total_codelists <- sum(purrr::map_int(table_variables, function(dim_el) {
    nrow(intern_cl_info(dim_el))
  }))

  # ---------------------------------------------------------------------------
  # interpret include_aggregations -> per-variable instruction
  # result: a named list  var_code -> mode
  #   mode is one of: "all", "codelists", "none", or a character vector of agg ids
  #
  ia <- include_aggregations

  # backwards compatibility: TRUE -> "all", FALSE -> "none"
  if (isTRUE(ia))        ia <- "all"
  if (identical(ia, FALSE)) ia <- "none"

  # validate scalar string values
  if (is.character(ia) && is.null(names(ia)) && length(ia) == 1) {
    if (!ia %in% c("auto", "all", "none", "codelists")) {
      stop('include_aggregations: invalid value "', ia,
           '". Allowed: "auto", "all", "none", "codelists", or a named vector.', call. = FALSE)
    }
  }

  # resolve "auto": choose "all" or "codelists" depending on the total number of code lists
  auto_chose_all <- FALSE
  if (identical(ia, "auto")) {
    if (total_codelists <= auto_limit) {
      ia <- "all"
      auto_chose_all <- TRUE
    } else {
      ia <- "codelists"
    }
  }

  # build the per-variable instruction vector
  # var_mode: named character vector  var_code -> "all" | "codelists" | "none" | "agg_..."
  if (is.character(ia) && is.null(names(ia))) {
    # scalar mode ("all", "none", "codelists") -> applies to all variables
    scalar_mode <- ia
    var_mode <- stats::setNames(rep(scalar_mode, length(table_variables)),
                                names(table_variables))
  } else if (is.character(ia) && !is.null(names(ia))) {
    # named vector: c(Region = "agg_RegionLA2018", Alder = "all", ...)
    # unknown variable names -> warning; variables not mentioned -> "codelists"
    var_mode <- stats::setNames(rep("codelists", length(table_variables)),
                                names(table_variables))

    ia_names <- names(ia)
    unknown  <- ia_names[
      !tolower(ia_names) %in% tolower(variables_df$code) &
        !tolower(ia_names) %in% tolower(variables_df$label)
    ]
    if (length(unknown) > 0) {
      warning("The following variable names in include_aggregations are not recognised and are ignored: ",
              paste(unknown, collapse = ", "), call. = FALSE)
    }

    # group by variable code (there can be several entries with the same name, e.g. Region = "agg_X", Region = "agg_Y")
    for (i in seq_along(ia)) {
      var_codes <- intern_to_var_code(ia_names[i])
      if (length(var_codes) == 0) next
      val <- ia[[i]]

      for (vc in var_codes) {
        existing <- var_mode[[vc]]
        if (val %in% c("all", "none", "codelists")) {
          # explicit mode: always overwrite
          var_mode[[vc]] <- val
        } else {
          # specific agg id: accumulate (there can be several entries)
          if (existing %in% c("codelists", "none")) {
            var_mode[[vc]] <- val          # first agg id for this variable
          } else if (!existing %in% c("all")) {
            var_mode[[vc]] <- paste(c(existing, val), collapse = "\n")  # append
          }
          # if existing == "all" -> leave it as "all"
        }
      }
    }
  } else {
    stop('include_aggregations must be "auto", "all", "none", "codelists" or a named vector.', call. = FALSE)
  }

  # ---------------------------------------------------------------------------
  # GET -> JSON with cache
  .codelist_cache <- new.env(parent = emptyenv())
  
  fetch_codelist <- purrr::possibly(function(u) {
    if (exists(u, envir = .codelist_cache, inherits = FALSE)) {
      return(get(u, envir = .codelist_cache, inherits = FALSE))
    }
    resp <- intern_pxweb2_GET(u, httr::accept_json())
    httr::stop_for_status(resp)
    out <- jsonlite::fromJSON(
      httr::content(resp, "text", encoding = "UTF-8"),
      simplifyVector = FALSE
    )
    assign(u, out, envir = .codelist_cache)
    out
  }, otherwise = NULL)
  
  # helper: fetch the full contents of a selection of code lists (filtered on agg ids if given)
  intern_fetch_agg_values <- function(cl_info_df, agg_ids_filter = NULL) {
    # agg_ids_filter: character vector of specific agg ids to fetch, NULL = all
    df <- cl_info_df
    if (!is.null(agg_ids_filter)) {
      df <- df |> dplyr::filter(tolower(code) %in% tolower(agg_ids_filter))
    }
    if (nrow(df) == 0) {
      return(tibble::tibble(code = character(), label = character(), type = character()))
    }
    
    aggr_parsed <- df |>
      dplyr::transmute(agg_id = code, agg_label = label, url) |>
      dplyr::mutate(
        codelist = purrr::map(url, fetch_codelist),
        values_long_df = purrr::map(codelist, ~ {
          v <- .x$values
          if (is.null(v) || length(v) == 0) {
            return(tibble::tibble(code = character(), label = character(), members = character()))
          }
          purrr::map_df(v, ~ tibble::tibble(
            code    = as.character(purrr::pluck(.x, "code",  .default = NA_character_)),
            label   = as.character(purrr::pluck(.x, "label", .default = NA_character_)),
            members = {
              vm <- purrr::pluck(.x, "valueMap", .default = NULL)
              if (is.null(vm)) NA_character_
              else paste(unlist(vm, use.names = FALSE), collapse = aggregation_member_sep)
            }
          )) |>
            dplyr::filter(!is.na(code))
        })
      ) |>
      dplyr::select(-codelist)
    
    if (nrow(aggr_parsed) == 0) {
      return(tibble::tibble(code = character(), label = character(), type = character()))
    }
    
    aggr_parsed |>
      dplyr::select(agg_id, agg_label, values_long_df) |>
      tidyr::unnest(values_long_df) |>
      dplyr::mutate(type = "Aggregation") |>
      dplyr::relocate(c(code, label, type), .before = 1)
  }
  
  # ---------------------------------------------------------------------------
  # build the list: one tibble per dimension element
  vars_with_codelist_only <- character(0)

  values <- purrr::imap(table_variables, function(dim_el, dim_name) {

    # --- variable values (type = "Variable")
    cat_lab <- purrr::pluck(dim_el, "category", "label", .default = NULL)
    cat_df <- if (is.null(cat_lab)) {
      tibble::tibble(code = character(), label = character(), type = character())
    } else {
      v <- unlist(cat_lab, use.names = TRUE)
      tibble::tibble(code = names(v), label = unname(v), type = "Variable")
    }

    mode <- var_mode[[dim_name]] %||% "codelists"
    cl_info <- intern_cl_info(dim_el)

    if (mode == "none") {
      return(cat_df)
    }

    if (mode == "codelists") {
      if (nrow(cl_info) == 0) return(cat_df)
      vars_with_codelist_only <<- c(vars_with_codelist_only, dim_name)
      codelist_meta <- cl_info |>
        dplyr::select(code, label, type) |>
        dplyr::mutate(type = "AggregationCodelist")
      return(dplyr::bind_rows(cat_df, codelist_meta))
    }

    if (mode == "all") {
      if (nrow(cl_info) == 0) return(cat_df)
      return(dplyr::bind_rows(cat_df, intern_fetch_agg_values(cl_info)))
    }

    # specific agg ids (one or several, newline-separated internally)
    agg_ids <- unlist(stringr::str_split(mode, "\n"), use.names = FALSE)
    unknown_ids <- agg_ids[!tolower(agg_ids) %in% tolower(cl_info$code)]
    if (length(unknown_ids) > 0) {
      warning("The following agg ids are not recognised for the variable '", dim_name, "' and are ignored: ",
              paste(unknown_ids, collapse = ", "), call. = FALSE)
    }
    agg_ids <- agg_ids[tolower(agg_ids) %in% tolower(cl_info$code)]

    if (length(agg_ids) == 0) {
      # no valid agg ids -> fall back on code-list metadata
      if (nrow(cl_info) > 0) {
        vars_with_codelist_only <<- c(vars_with_codelist_only, dim_name)
        codelist_meta <- cl_info |>
          dplyr::select(code, label, type) |>
          dplyr::mutate(type = "AggregationCodelist")
        return(dplyr::bind_rows(cat_df, codelist_meta))
      }
      return(cat_df)
    }

    dplyr::bind_rows(cat_df, intern_fetch_agg_values(cl_info, agg_ids_filter = agg_ids))
  })

  # ---------------------------------------------------------------------------
  # messages
  if (auto_chose_all) {
    message(
      "include_aggregations = \"auto\": found ", total_codelists,
      " code lists (<= ", auto_limit, ") and fetched all aggregations automatically."
    )
  } else if (length(vars_with_codelist_only) > 0) {
    message(
      "The following variables have aggregation code lists that were not fetched in full: ",
      paste(vars_with_codelist_only, collapse = ", "), ".\n",
      "Set include_aggregations = \"all\" (or name the variables with specific agg ids) ",
      "to fetch all aggregations and their values."
    )
  }

  if (return_df_if_only_one_variable && length(values) == 1) {
    values <- tibble::as_tibble(values[[1]])
  }

  return(values)
}




#' Build a query-list template as text for a script
#'
#' Builds the text representation of a `query` list from a table's variables, to
#' paste into a script. Printed with `cat()`.
#'
#' @param table_id Table id.
#' @param default_value Value used for variables without an override, normally
#'   `"*"`.
#' @param overrides Named list of values per variable.
#' @param object_name Name of the list object in the generated text.
#' @param base_url Base URL of the PxWeb API v2.
#'
#' @return Called for its side effect (`cat()`). Returns `NULL` invisibly.
#' @export
pxweb2_query_list_template <- function(table_id,
                                         default_value = "*",
                                         overrides = list(),
                                         object_name = "query_list",
                                         base_url = "https://statistikdatabasen.scb.se/api/v2/tables/"
) {
  # create the text for a query list for a script, based on the given table
  variables_df <- pxweb2_get_variables(table = table_id, base_url = base_url)
  vars <- variables_df$code
  
  vals <- purrr::map_chr(vars, function(var) {
    v <- if (!is.null(overrides[[var]])) overrides[[var]] else default_value
    
    if (length(v) == 1 && is.na(v)) return(NA_character_)
    
    if (is.character(v) && length(v) == 1) {
      paste0(var, ' = "', v, '"')
    } else {
      paste0(
        var, " = c(",
        paste(sprintf('"%s"', as.character(v)), collapse = ", "),
        ")"
      )
    }
  })
  
  vals <- vals[!is.na(vals)]
  
  cat(
    paste0(
      object_name, " <- list(\n  ",
      paste(vals, collapse = ",\n  "),
      "\n)"
    )
  )
}

#' Build a ready-made pxweb2_get_data() call as text
#'
#' Generates a complete `pxweb2_get_data()` call for a table, as text. Printed
#' with `cat()` and, by default, copied to the clipboard.
#'
#' @param table_id Table id.
#' @param default_value Value used for variables without an override, normally
#'   `"*"`.
#' @param overrides Named list of values per variable.
#' @param to_clipboard If `TRUE`, the text is copied to the clipboard. Requires
#'   the `clipr` package and an available clipboard (on Linux `xclip`, `xsel` or
#'   `wl-clipboard` plus an active display). If not available, the text is only
#'   printed.
#' @param base_url Base URL of the PxWeb API v2.
#'
#' @return Called for its side effect (`cat()`). Returns `NULL` invisibly.
#' @export
pxweb2_data_script_template <- function(table_id,
                                          default_value = "*",
                                          overrides = list(),
                                          to_clipboard = TRUE,
                                          base_url = "https://statistikdatabasen.scb.se/api/v2/tables/"
) {

  meta <- pxweb2_get_metadata(table_id, base_url = base_url)
  title_txt <- meta$label
  variables_df <- pxweb2_get_variables(meta)
  values_df <- pxweb2_get_values(meta)
  vars <- variables_df$code
  
  vals <- purrr::map_chr(vars, function(var) {
    v <- if (!is.null(overrides[[var]])) overrides[[var]] else default_value
    
    if (length(v) == 1 && is.na(v)) return(NA_character_)
    
    if (is.character(v) && length(v) == 1) {
      paste0(var, ' = "', v, '"')
    } else {
      paste0(
        var, " = c(",
        paste(sprintf('"%s"', as.character(v)), collapse = ", "),
        ")"
      )
    }
  })
  
  vals <- vals[!is.na(vals)]
  
  return_txt <- paste0(
    "# ", title_txt, "\n",
    "dataset_df <- pxweb2_get_data(\n",
    '\ttable = "', table_id, '",\n',
    "\tquery = list(\n\t\t",
    paste(vals, collapse = ",\n\t\t"),
    "\n\t\t))"
  )

  if (to_clipboard) {
    if (requireNamespace("clipr", quietly = TRUE) && clipr::clipr_available()) {
      clipr::write_clip(return_txt)
    } else {
      message("Clipboard not available - the text is only printed.")
    }
  }

  cat(return_txt)
}


#' Search among tables in a PxWeb API v2
#'
#' Queries the tables endpoint and pages through all matches.
#'
#' @param query Free-text search. `NULL` lists all tables.
#' @param base_url Base URL of the PxWeb API v2 (the tables endpoint).
#' @param lang Language, `"sv"` or `"en"`.
#' @param pastDays Restrict to tables updated in the last N days.
#' @param includeDiscontinued Whether discontinued tables should be included.
#' @param pageSize Number of matches per page.
#' @param timeout_sec Timeout per call in seconds.
#' @param max_pages Max number of pages to fetch.
#'
#' @return A `tibble` with one row per table.
#' @export
pxweb2_search_tables <- function(query = NULL,
                                 base_url = "https://statistikdatabasen.scb.se/api/v2/tables",
                                 lang = "sv",
                                 pastDays = NULL,
                                 includeDiscontinued = NULL,
                                 pageSize = 200,
                                 timeout_sec = 60,
                                 max_pages = Inf) {
  
  base_url <- sub("/+$", "", base_url)
  
  qs_base <- purrr::compact(list(
    lang = lang,
    query = query,
    pastDays = pastDays,
    includeDiscontinued = includeDiscontinued,
    pageSize = pageSize
  ))
  
  fetch_page <- function(pageNumber) {
    resp <- intern_pxweb2_GET(
      url = base_url,
      query = c(qs_base, list(pageNumber = pageNumber)),
      httr::accept_json(),
      httr::timeout(timeout_sec)
    )
    httr::stop_for_status(resp)
    
    txt <- httr::content(resp, "text", encoding = "UTF-8")
    jsonlite::fromJSON(txt, simplifyVector = TRUE)
  }
  
  out1 <- fetch_page(1)
  
  # pick out tables whether the response is in $tables or "directly"
  extract_tables <- function(out) {
    if (is.data.frame(out)) return(tibble::as_tibble(out))
    if (is.list(out) && "tables" %in% names(out)) {
      if (is.data.frame(out$tables)) return(tibble::as_tibble(out$tables))
      if (is.list(out$tables)) return(tibble::as_tibble(out$tables))
    }
    if (is.list(out)) return(tibble::as_tibble(out))
    tibble::tibble()
  }
  
  page1 <- extract_tables(out1)
  
  # decide whether there are more pages: if we get < pageSize we are done
  if (nrow(page1) == 0) return(page1)
  
  pages <- list(page1)
  page <- 2
  
  while (page <= max_pages) {
    if (nrow(pages[[length(pages)]]) < pageSize) break
    
    outn <- fetch_page(page)
    pgn <- extract_tables(outn)
    
    if (nrow(pgn) == 0) break
    pages[[length(pages) + 1]] <- pgn
    
    if (nrow(pgn) < pageSize) break
    page <- page + 1
  }
  
  dplyr::bind_rows(pages) |> dplyr::distinct()
}

# function helpers to create queries for aggregations as well
intern_pxweb2_request <- function(body, extra_query = list(), tag = list()) {
  list(body = body, extra_query = extra_query, tag = tag)
}

intern_pxweb2_get_valuecodes <- function(query_list, var) {
  i <- which(purrr::map_chr(query_list$selection, "variableCode") == var)
  if (length(i) == 0) return(NULL)
  unlist(query_list$selection[[i]]$valueCodes, use.names = FALSE)
}

intern_pxweb2_set_valuecodes <- function(query_list, var, vals) {
  i <- which(purrr::map_chr(query_list$selection, "variableCode") == var)
  if (length(i) == 0) return(query_list)
  
  query_list$selection[[i]]$valueCodes <- if (length(vals) == 1 && is.na(vals)) {
    NULL
  } else if (length(vals) == 0) {
    list()
  } else if (length(vals) == 1 && identical(vals, "*")) {
    list("*")
  } else if (is.character(vals) && length(vals) == 1) {
    list(vals)
  } else {
    as.list(as.character(vals))
  }
  
  query_list
}


intern_pxweb2_make_request_chunks <- function(variables_df, request_list, valid_values_list, max_cells = 150000) {
  
  request_list <- purrr::compact(request_list)
  
  request_list |>
    purrr::map(function(.req) {
      bodies <- intern_pxweb2_make_chunks(
        variables_df,
        .req$body,
        valid_values_list = valid_values_list,
        max_cells = max_cells
      )
      bodies <- purrr::compact(bodies)
      
      purrr::map(bodies, ~ intern_pxweb2_request(
        body = .x,
        extra_query = .req$extra_query,
        tag = .req$tag
      ))
    }) |>
    purrr::flatten()
}

intern_pxweb2_is_special_value <- function(vals) {
  length(vals) == 1 && (
    identical(vals, "*") ||
      identical(vals, "**") ||
      grepl("^\\s*(top|bottom)\\s*\\(\\s*\\d+\\s*\\)\\s*$", vals, ignore.case = TRUE) ||
      grepl("^agg_", vals)
  )
}

intern_pxweb2_var_alternatives <- function(query_list,
                                           valid_values_list,
                                           var,
                                           output_values = "aggregated",    # can also be single
                                           warn = TRUE) {
  
  vals <- intern_pxweb2_get_valuecodes(query_list, var)
  if (is.null(vals) || length(vals) == 0) {
    return(list(list(var = var, vals = NULL, extra_query = list())))
  }
  
  vals <- as.character(vals)
  
  # special: top/bottom/* -> do nothing (no codelist)
  if (length(vals) == 1 && (identical(vals, "*") ||
                            grepl("^\\s*(top|bottom)\\s*\\(\\s*\\d+\\s*\\)\\s*$", vals, ignore.case = TRUE))) {
    return(list(list(var = var, vals = vals, extra_query = list())))
  }
  
  ok_tbl <- valid_values_list[[var]]
  if (is.null(ok_tbl) || !all(c("code", "type") %in% names(ok_tbl))) {
    return(list(list(var = var, vals = vals, extra_query = list())))
  }
  
  agg_tbl <- ok_tbl |>
    dplyr::filter(tolower(type) == "aggregation") |>
    dplyr::select(dplyr::any_of(c("code", "agg_id","agg_label")))
  
  has_agg <- all(c("code", "agg_id") %in% names(agg_tbl)) && nrow(agg_tbl) > 0
  
  agg_code_to_id <- if (has_agg) stats::setNames(agg_tbl$agg_id, agg_tbl$code) else character()
  
  # 1) var="**" => default "*" + one per agg_id with "*"
  if (length(vals) == 1 && identical(vals, "**")) {
    if (!has_agg) {
      if (warn) warning("The variable `", var, "` has no aggregations; '**' is interpreted as '*'.")
      vals <- "*"
    } else {
      all_agg_ids <- unique(agg_tbl$agg_id)
      
      out <- list(
        list(var = var, vals = "*", extra_query = list())  # default
      )
      
      out <- c(out, purrr::map(all_agg_ids, ~ list(
        var = var,
        vals = "*",
        extra_query = stats::setNames(list(.x), paste0("codelist[", var, "]")) |>
          c(stats::setNames(list(output_values), paste0("outputValues[", var, "]")))
      )))
      
      return(out)
    }
  }
  
  # 2) var="agg_..." => one alt: vals="*" + codelist[var]=agg_...
  if (length(vals) == 1 && grepl("^agg_", vals)) {
    if (!has_agg) {
      if (warn) warning("The variable `", var, "` has no aggregations; `", vals, "` is ignored.")
      return(list(list(var = var, vals = "*", extra_query = list())))
    }
    return(list(list(
      var = var,
      vals = "*",
      extra_query = stats::setNames(list(vals), paste0("codelist[", var, "]")) |>
        c(stats::setNames(list(output_values), paste0("outputValues[", var, "]")))
    )))
  }
  
  
  # 3) mixed codes: split into default + per agg_id (for agg codes)
  is_agg <- vals %in% names(agg_code_to_id)
  default_vals <- vals[!is_agg]
  agg_vals <- vals[is_agg]
  
  out <- list()
  
  if (length(default_vals) > 0) {
    out <- c(out, list(list(var = var, vals = default_vals, extra_query = list())))
  }
  
  if (length(agg_vals) > 0) {
    by_id <- split(agg_vals, agg_code_to_id[agg_vals])
    out <- c(out, purrr::imap(by_id, ~ list(
      var = var,
      vals = .x,
      extra_query = stats::setNames(list(.y), paste0("codelist[", var, "]")) |>
        c(stats::setNames(list(output_values), paste0("outputValues[", var, "]")))
    )))
  }
  
  # if no agg matched -> default only
  if (length(out) == 0) out <- list(list(var = var, vals = vals, extra_query = list()))

  # if several agg_id in the same variable (codes from different aggs) => return several alternatives
  out
}


intern_pxweb2_expand_requests_generic <- function(query_list, valid_values_list,
                                                  output_values = "aggregated",    # can also be single
                                                  warn = TRUE) {

  vars_in_body <- purrr::map_chr(query_list$selection, "variableCode")

  # build alternatives per variable
  alts <- purrr::map(vars_in_body, ~ intern_pxweb2_var_alternatives(
    query_list, valid_values_list, var = .x,
    output_values = output_values, warn = warn
  ))
  names(alts) <- vars_in_body

  # cartesian product of alternatives (usually this is 1)
  combos <- tidyr::expand_grid(!!!alts) |> purrr::transpose()
  
  purrr::map(combos, function(choice) {
    body  <- query_list
    extra <- list()
    
    for (opt in choice) {
      if (!is.null(opt$vals)) {
        body <- intern_pxweb2_set_valuecodes(body, opt$var, opt$vals)
      }
      if (length(opt$extra_query) > 0) {
        extra <- c(extra, opt$extra_query)
      }
    }
    
    intern_pxweb2_request(body = body, extra_query = extra, tag = list(kind = "expanded"))
  })
}
