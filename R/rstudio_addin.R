#' download rstudio addin by chrome
#' download from https://github.com/daattali/addinslist
#' @param browser browser
#'
#' @return download by web browser
#'
#' @export
#'
#' @examples
#' \donttest{
#' rstudio_addin()
#' }
rstudio_addin <- function(browser = getOption("browser")){
    html = read_html("https://github.com/daattali/addinslist")
    url = html %>%
        html_nodes(xpath = '//*[@id="readme"]/div[3]/article/table/tbody//td[4]/a') %>%
        html_attr('href')
    durl = paste0(gsub('https://','https://codeload.',url),'/zip/refs/heads/master')
    j=0
    for (i in durl) {
        j=j+1
        cat(j,'\n')
        browseURL(i,browser = browser)
        Sys.sleep(3)
    }
}
