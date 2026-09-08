# pxweb2r

<!-- badges: start -->
<!-- badges: end -->

`pxweb2r` fetches data and metadata from a **PxWeb API v2** - by default
[Statistics Sweden's statistical database](https://www.statistikdatabasen.scb.se/) -
and returns tidy `tibble` tables.

The package is extracted from the function file `func_pxweb2.R` in
[`Region-Dalarna/funktioner`](https://github.com/Region-Dalarna/funktioner).

## Installation

```r
# install.packages("remotes")
remotes::install_github("FaluPeppe/pxweb2r")
```

## Getting started

```r
library(pxweb2r)

# Fetch data
population <- pxweb2_get_data(
  table = "TAB6104",
  query = list(
    Region       = c("Dalarnas lan"),
    Kon          = "*",
    ContentsCode = "*",
    Tid          = "9999"          # latest period
  )
)

# Metadata and variables
meta <- pxweb2_get_metadata("TAB6104")
pxweb2_get_variables(meta)
pxweb2_get_values(meta)

# Search tables
pxweb2_search_tables("befolkning")

# Does the table exist?
pxweb2_table_exists("TAB6104")
```

## Other PxWeb instances

Every function takes a `base_url` argument. Table ids are not validated against
Statistics Sweden's naming - if an id does not exist on the given `base_url`,
`pxweb2_get_metadata()` raises a clear error (HTTP 404).

## Licence

MIT (c) Peter Möller
