test_that(".pxweb2_check_table_id accepts reasonable ids", {
  expect_invisible(pxweb2r:::.pxweb2_check_table_id("TAB6104"))
  expect_identical(
    pxweb2r:::.pxweb2_check_table_id("statfin_vaerak_pxt_11ra"),
    "statfin_vaerak_pxt_11ra"
  )
})

test_that(".pxweb2_check_table_id rejects bad input", {
  expect_error(pxweb2r:::.pxweb2_check_table_id(NULL))
  expect_error(pxweb2r:::.pxweb2_check_table_id(NA_character_))
  expect_error(pxweb2r:::.pxweb2_check_table_id(""))
  expect_error(pxweb2r:::.pxweb2_check_table_id(c("TAB1", "TAB2")))
  expect_error(pxweb2r:::.pxweb2_check_table_id(6104L))
  expect_error(
    pxweb2r:::.pxweb2_check_table_id(
      "https://statistikdatabasen.scb.se/api/v2/tables/TAB6104"
    )
  )
  expect_error(pxweb2r:::.pxweb2_check_table_id("TAB 6104"))
})
