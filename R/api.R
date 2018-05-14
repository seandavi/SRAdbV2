#' @importFrom lubridate as_datetime
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' 
#' @keywords internal
.search_handler = function(response) {
  tmp = jsonlite::fromJSON(httr::content(response, type='text', encoding='UTF-8'))
  res = as_tibble(tmp$hits$hits$`_source`)
  datecols = grep(paste(c('Received', 'Published', "LastUpdate", "LastMetaUpdate"), collapse="|"), 
                  colnames(res), value=TRUE)
  res[datecols] = lapply(res[datecols], function(d) {lubridate::as_datetime(d/1000)})
  attr(res, 'count') = tmp$hits$total
  return(res)
}


#' @importFrom httr build_url
get_url = function(path) httr::build_url(list('https://api-omicidx.cancerdatasci.org', path))

#' @importFrom httr parse_url build_url accept_json add_headers content_type
.sra_get_search_function = function(path, q='*', from = 0, size = 10, fields = NULL, .headers=NULL) {
  url = httr::parse_url(file.path("https://api-omicidx.cancerdatasci.org", path))
  url$query = list(q=q, from=from, size=size)
  if(!is.null(fields)) {
    url$query = c(url$query, setNames(as.list(fields),rep("fields",length(fields))))
  }
  url = httr::build_url(url)
  message(url)
  result <- httr::GET(url, httr::content_type("application/json"),
                      httr::accept_json(), httr::add_headers(.headers = .headers))
  res = .search_handler(result)
  attr(res,'from') = from
  attr(res,'q') = q
  class(res) = c('sra_search_result',class(res))
  return(res)
}


#' search SRA metadata
#' 
#' @param q a lucene query string
#' @param from integer(1) for paging results (unit is the number of records, not number of pages)
#' @param size integer(1) for the number of results to return per page
#' @param fields character() vector with fields to return. The default \code{NULL}
#'     will return all available fields
#'
#' @return a \code{tibble} of records, with some columns potentially 
#'     containing a list.
#'
#' @examples 
#' sra_run_search()
#'
#' @export
sra_experiment_search = function(q = '*', from = 0, size = 10, fields = NULL) {
  .sra_get_search_function(path = '/sra/experiment/search', q, from, size, fields)
}

#' @describeIn sra_experiment_search search runs
#' @export
sra_run_search = function(q = '*', from = 0, size = 10, fields = NULL) {
  .sra_get_search_function(path = '/sra/run/search', q, from, size, fields)
}

#' @describeIn sra_experiment_search search studies
#' @export
sra_study_search = function(q = '*', from = 0, size = 10, fields = NULL) {
  .sra_get_search_function(path = '/sra/study/search', q, from, size, fields)
}


#' @describeIn sra_experiment_search search studies
#' @export
sra_sample_search = function(q = '*', from = 0, size = 10, fields = NULL) {
  .sra_get_search_function(path = '/sra/sample/search', q, from, size, fields)
}

#' @describeIn sra_experiment_search search studies
#' @export
sra_full_search = function(q = '*', from = 0, size = 10, fields = NULL) {
  .sra_get_search_function(path = '/sra/full/search', q, from, size, fields)
}

