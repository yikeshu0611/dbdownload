#' Get all cytoscope addin urls
#'
#' @return cytoscope addin urls
#' @export
#'
#' @examples
#' \donttest{
#' url <- cytoscope_url()
#' }
cytoscope_url <- function(){
    html <- GET('https://apps.cytoscape.org/media/')
    x=read_html('https://apps.cytoscape.org/media/')%>%
        html_nodes(xpath = '//a[@href]') %>%
        html_text()
    x2=x[-(1:5)]
    url= paste0('https://apps.cytoscape.org/media/',x2,"releases/")
    pb <- txtProgressBar(max=length(url),width = 30,style = 3)
    for (i in 1:length(url)){
        setTxtProgressBar(pb,i)
        html = tryCatch(read_html(url[i]), error =function(e) "e")
        if (is.character(html)) next(i)
        v = html%>%
            html_nodes(xpath = '//a[@href]') %>%
            html_text()  %>%
            .[length(.)]
        v
        tar.i = paste0(url[i],v)
        tar=c(tar,tar.i)
    }
    tar
}
