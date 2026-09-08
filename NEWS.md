# pxweb2r 0.0.0.9000

* Första versionen som paket, utbruten ur `func_pxweb2.R` i
  `Region-Dalarna/funktioner`.
* Tabell-id valideras inte längre mot mönstret `^TAB\d+$`. En lokal
  minimalkontroll (icke-tom sträng, längd 1, inga blanksteg eller `/`) görs i
  stället, och `pxweb2_meta()` avgör om tabellen faktiskt finns – HTTP 404
  översätts till ett begripligt felmeddelande.
* Nytt: `pxweb2_tabell_finns()` för att snabbt kontrollera om ett tabell-id
  finns på en given `base_url`.
* `base_url` skickas nu hela vägen till metadata-anropen, och finns som
  argument på `pxweb2_tabell_uppdaterades()`,
  `pxweb2_tabell_behover_uppdateras()`, `pxweb2_query_list_txt_create()` och
  `pxweb2_get_data_script_create()`.
