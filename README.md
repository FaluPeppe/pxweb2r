# pxweb2r

<!-- badges: start -->
<!-- badges: end -->

`pxweb2r` hämtar data och metadata från ett **PxWeb API v2** – som standard
[SCB:s statistikdatabas](https://www.statistikdatabasen.scb.se/) – och
returnerar tidy `tibble`-tabeller.

Paketet är utbrutet ur funktionsfilen `func_pxweb2.R` i
[`Region-Dalarna/funktioner`](https://github.com/Region-Dalarna/funktioner).

## Installation

```r
# install.packages("remotes")
remotes::install_github("FaluPeppe/pxweb2r")
```

## Kom igång

```r
library(pxweb2r)

# Hämta data
befolkning <- pxweb2_hamta_data(
  tabell = "TAB6104",
  query = list(
    Region = c("Dalarnas län"),
    Kon    = "*",
    ContentsCode = "*",
    Tid    = "9999"          # senaste tidsperiod
  )
)

# Metadata och variabler
meta <- pxweb2_meta("TAB6104")
pxweb2_variabler(meta)
pxweb2_varden(meta)

# Sök tabeller
pxweb2_search_tables("befolkning")

# Finns tabellen?
pxweb2_tabell_finns("TAB6104")
```

## Andra PxWeb-instanser

Alla funktioner tar ett `base_url`-argument. Tabell-id valideras inte mot
SCB:s namngivning – om ett id inte finns på den angivna `base_url` ger
`pxweb2_meta()` ett tydligt fel (HTTP 404).

## Licens

MIT © Peter Möller
