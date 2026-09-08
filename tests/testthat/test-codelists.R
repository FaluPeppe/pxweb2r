test_that("pxweb2_get_codelist validates its input", {
  expect_error(pxweb2_get_codelist(NULL))
  expect_error(pxweb2_get_codelist(NA_character_))
  expect_error(pxweb2_get_codelist(""))
  expect_error(pxweb2_get_codelist(c("a", "b")))
})

test_that("intern_pxweb2_api_root strips the tables segment", {
  expect_identical(
    pxweb2r:::intern_pxweb2_api_root("https://statistikdatabasen.scb.se/api/v2/tables/"),
    "https://statistikdatabasen.scb.se/api/v2/"
  )
  expect_identical(
    pxweb2r:::intern_pxweb2_api_root("https://example.org/api/v2/tables"),
    "https://example.org/api/v2/"
  )
})

test_that("pxweb2_get_codelist fetches a value set (needs network)", {
  skip_on_cran()
  skip_if_offline()

  kommuner <- pxweb2_get_codelist("vs_RegionKommun07")
  expect_s3_class(kommuner, "tbl_df")
  expect_named(kommuner, c("code", "label", "value_map"))
  expect_gt(nrow(kommuner), 250)
  expect_true("0114" %in% kommuner$code)
})

test_that("pxweb2_list_codelists lists a table's code lists (needs network)", {
  skip_on_cran()
  skip_if_offline()

  cl <- pxweb2_list_codelists("TAB638")
  expect_s3_class(cl, "tbl_df")
  expect_named(cl, c("variable", "id", "type", "label"))
  expect_true("vs_RegionKommun07" %in% cl$id)
  expect_true(all(cl$type[cl$id == "vs_RegionKommun07"] == "Valueset"))
})
