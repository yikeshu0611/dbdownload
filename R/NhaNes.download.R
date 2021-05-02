#' download NhaNes data base by r directory
#'
#' @param BeginYear year of begin
#' @param yearn year begin number
#' @param dirn directory begin number
#' @param filen file begin number
#'
#' @return download by r directory
#' @export
#'
#' @examples
#' \donttest{
#' NhaNes.download(BeginYear = NULL, yearn=2, dirn=1, filen=1)
#' }
NhaNes.download <- function(BeginYear = NULL, yearn=2, dirn=1, filen=1){
  home_url = 'https://wwwn.cdc.gov/nchs/nhanes'
  html = read_html(home_url)
  year_href = html %>%
    html_nodes(xpath = '//div[@class="col-md-3 d-flex"]/a') %>%
    html_attr('href')
  year_url = paste0(home_url,'/',year_href)
  bgy = as.numeric(do::Replace0(year_url,'.*BeginYear='))
  if (is.null(BeginYear)){
    BeginYear = bgy
  }else{
    year_url = year_url[bgy %in% BeginYear]
  }
  for (i in yearn:length(year_url)) {
    year_html = read_html(year_url[i])
    data_href = year_html %>%
      html_nodes(xpath = '//a[@class="list-title td-none td-ul-hover"]') %>%
      set::grep_and('Component=') %>%
      html_attr('href')
    data_href = set::grep_not_and(data_href,'LimitedAccess')
    if (length(data_href) ==0){
      message('==========================\n')
      message('    No Data For Year',(bgy[bgy %in% BeginYear])[i])
      next(i)
    }
    data_url = paste0('https://wwwn.cdc.gov/',data_href)
    for (j in dirn:length(data_url)) {
      if (j == length(data_url)) dirn=1
      data_html = read_html(data_url[j])
      file_href = data_html %>%
        html_nodes(xpath = '//td[@class="text-center"]/a') %>%
        set::grep_and('XPT') %>%
        html_attr('href')
      dirs = data_html %>%
        html_nodes(xpath = '//span[@id="PageHeading_lblHeading"]') %>%
        html_text()
      dir1 = do::Trim(stringr::str_extract(dirs,'.*[0-9]{4}-[0-9]{4}'))
      dir2 = do::Trim(do::Replace0(dirs,dir1))
      dirs2 = c(dir1,dir2)
      for (m in 1:length(dirs2)) {
        dirm=paste0(dirs2[1:m],collapse = '/')
        suppressWarnings(dir.create(dirm))
      }
      file_url = paste0('https://wwwn.cdc.gov',file_href)
      filename = paste0(dirm,'/',do::Replace0(file_url,'.*[0-9]{4}-[0-9]{4}/'))
      for (k in filen:length(file_url)) {
        if (k==length(file_url)) filen=1
        cat(crayon::red(paste0(' yearn: ',length(year_url),','),
                        paste0('dirn: ',length(data_url),','),
                        paste0('filen: ',length(file_url))))
        cat('\n')
        cat(paste0(', yearn=',i),
            paste0(', dirn=',j),
            paste0(', filen=',k),
            '-',dir1,'-',dir2)
        cat('\n')
        download.file(file_url[k],filename[k])
      }
    }
  }
}
