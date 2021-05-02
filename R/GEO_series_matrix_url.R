#' GEO series matrix url
#'
#' @param ... one or more gse number
#'
#' @return url of series matrix
#' @export
#'
#' @examples
#' \donttest{
#' GEO_series_matrix_url('GSE1','GSE100')
#'
#'
#' }
GEO_series_matrix_url <- function(...){
    gse=c(...)
    url <- GEO_series_matrix(gse)
    html <- xml2::read_html(url)
    sm <- html %>%
        rvest::html_text() %>%
        strsplit(split = '\n') %>%
        unlist() %>%
        do::Replace0(from = c(paste0('.*',toupper(gse)),'\r'))
    paste0(url,gse,sm)
}

GEO_series_matrix <- function(...){
    gse <- toupper(c(...))
    url <- 'ftp://ftp.ncbi.nlm.nih.gov/geo/series/%s/%s/matrix/'
    res <- c()
    for(i in gse){
        if (nchar(i) <= 6){
            urli <- sprintf(url,'GSEnnn',i)
        }else{
            urli <- sprintf(url,paste0(do::knife_right(i,3),'nnn'),i)
        }
        res <- c(res, urli)
    }
    names(res) <- gse
    res
}

