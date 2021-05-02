#' download cytoscope addin by r
#'
#' @param url urls by cytoscope_url()
#'
#' @return download by r
#' @export
#'
#' @examples
#' \donttest{
#' url <- cytoscope_url()
#' cytoscope_down(url)
#' }
cytoscope_down <- function(url){
    for (i in 1:length(url)){
        cat(i,'/',length(url),"\n")
        html = tryCatch(read_html(url[i]), error =function(e) "e")
        if (is.character(html)) next(i)
        v = html%>%
            html_nodes(xpath = '//a[@href]') %>%
            html_text()  %>%
            .[do::right(. , 4) == ".jar"]
        jar =paste0(url[i],v)
        for (j in 1:length(jar)){
            download.file(jar[j], v[j])
        }
    }
}
