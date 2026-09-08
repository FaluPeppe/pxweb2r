test_that("intern_pxweb2_kontrollera_tabell_id godtar rimliga id:n", {
  expect_invisible(pxweb2r:::intern_pxweb2_kontrollera_tabell_id("TAB6104"))
  expect_identical(
    pxweb2r:::intern_pxweb2_kontrollera_tabell_id("statfin_vaerak_pxt_11ra"),
    "statfin_vaerak_pxt_11ra"
  )
})

test_that("intern_pxweb2_kontrollera_tabell_id avvisar felaktig indata", {
  expect_error(pxweb2r:::intern_pxweb2_kontrollera_tabell_id(NULL))
  expect_error(pxweb2r:::intern_pxweb2_kontrollera_tabell_id(NA_character_))
  expect_error(pxweb2r:::intern_pxweb2_kontrollera_tabell_id(""))
  expect_error(pxweb2r:::intern_pxweb2_kontrollera_tabell_id(c("TAB1", "TAB2")))
  expect_error(pxweb2r:::intern_pxweb2_kontrollera_tabell_id(6104L))
  expect_error(
    pxweb2r:::intern_pxweb2_kontrollera_tabell_id(
      "https://statistikdatabasen.scb.se/api/v2/tables/TAB6104"
    )
  )
  expect_error(pxweb2r:::intern_pxweb2_kontrollera_tabell_id("TAB 6104"))
})
