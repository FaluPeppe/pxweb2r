#' Converts the GET URL from SCB to table_id and selected query into R
#' 
#' Extracts table_id and valuecodes from a Statistics Sweden API URL
#' and converts them to a list suitable for use with pxweb_get_data()
#' 
#' @param url A SCB GET API URL. Located under Spara -> API-uttag -> GET
#' 
#' @return A list with two elements:
#' \describe{
#'  \item{table}{Table id.}
#'  \item{query}{Query list suitable for pxweb_get_data().}
#' }
#' 
#' @examples 
#' x <- pxweb2_GET_url_to_px(url = "https://statistikdatabasen.scb.se/api/v2/tables/TAB5457/data?lang=sv&outputFormat=json-stat2&valuecodes[ContentsCode]=*&valuecodes[Tid]=2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025&valuecodes[Region]=21&codelist[Region]=vs_A_L%C3%A4nRMI&heading=ContentsCode,Tid&stub=Region")
#' 
#' x$table
#' x$query
#' 
#' @export
pxweb2_GET_url_to_px <- function(url){
  
  table <- sub(".*/tables/([^/]+)/data.*", "\\1", url)
  
  params <- strsplit(sub(".*\\?", "", url), "&")[[1]]
  
  x <- grep("^valuecodes\\[", params, value = TRUE)
  
  keys <- sub("^valuecodes\\[([^]]+)\\]=.*$", "\\1", x)
  
  vals <- utils::URLdecode(
    sub("^valuecodes\\[[^]]+\\]=(.*)$", "\\1", x)
  )
  
  vals <- lapply(vals, function(x){
    if(grepl(",", x, fixed = TRUE)){
      trimws(strsplit(x, ",", fixed = TRUE)[[1]])
    } else {
      x
    }
  })
  
  list(
    table = table,
    query = stats::setNames(vals, keys)
  )
  
}