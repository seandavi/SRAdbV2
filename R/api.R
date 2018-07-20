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
  attr(res, 'scroll_id') = tmp$`_scroll_id`
  return(res)
}


#' Get mapping from endpoint
#' @importFrom jsonlite fromJSON
#' @keywords internal
.mapping_handler = function(entity='full') {
    l = jsonlite::fromJSON(sprintf('https://api-omicidx.cancerdatasci.org/sra/1.0/mapping/%s',entity))
    res = Filter(function(x) !is.null(x),lapply(l, '[[', 'type'))
    res2 = Filter(is.null,lapply(l, '[[', 'type'))
    res = c(res, sapply(names(res2), function(x) {
        unlist(lapply(l[[x]]$properties, '[[', 'type'))
    }))
    return(res)
}


#' @importFrom httr build_url
get_url = function(path) httr::build_url(list(.base_url, path))

#' @importFrom httr parse_url build_url accept_json add_headers content_type
.sra_get_search_function = function(path, q='*', start = 0, size = 10, fields = NULL, .headers=NULL) {
  url = httr::parse_url(paste0("https://api-omicidx.cancerdatasci.org/sra/1.0", path))
  url$query = list(q=q, start=start, size=size)
  if(!is.null(fields)) {
    url$query$fields = paste(fields,collapse=',')
  }
  url = httr::build_url(url)
  if(getOption('omicidx.verbose',FALSE)) message(url)
  result <- httr::GET(url, httr::content_type("application/json"),
                      httr::accept_json(), httr::add_headers(.headers = .headers))
  res = .search_handler(result)
  attr(res,'start') = start
  attr(res,'q') = q
  class(res) = c('sra_search_result',class(res))
  return(res)
}

.sra_scroll = function(scroll_id, scroll='1m') {
  path = '/scroll'
  url = httr::parse_url(paste0("https://api-omicidx.cancerdatasci.org/sra/1.0", path))
  url$query = list(scroll_id = scroll_id, scroll = scroll)
  url = httr::build_url(url)
  result <- httr::GET(url, httr::content_type("application/json"),
                      httr::accept_json())
  res = .search_handler(result)
  return(res)
}


#' search SRA metadata
#' 
#' @param q a lucene query string
#' @param start integer(1) for paging results (unit is the number of records, not number of pages)
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
sra_experiment_search = function(q = '*', start = 0, size = 10, fields = NULL) {
  .sra_get_search_function(path = '/search/experiment', q, start, size, fields)
}

#' @describeIn sra_experiment_search search runs
#' @export
sra_run_search = function(q = '*', start = 0, size = 10, fields = NULL) {
  .sra_get_search_function(path = '/search/run', q, start, size, fields)
}

#' @describeIn sra_experiment_search search studies
#' @export
sra_study_search = function(q = '*', start = 0, size = 10, fields = NULL) {
  .sra_get_search_function(path = '/search/study', q, start, size, fields)
}


#' @describeIn sra_experiment_search search studies
#' @export
sra_sample_search = function(q = '*', start = 0, size = 10, fields = NULL) {
  .sra_get_search_function(path = '/search/sample', q, start, size, fields)
}

#' @describeIn sra_experiment_search search studies
#' @export
sra_full_search = function(q = '*', start = 0, size = 10, fields = NULL) {
  .sra_get_search_function(path = '/search/full', q, start, size, fields)
}

#' Browse the API interactively
#' 
#' The SRA API is a swagger 2.0 compliant API and has a simple
#' user interface for experimenting with the API itself. The
#' API could be used from any language or even command-line
#' utilities for accessing web resources. 
#' 
#' @param version character(1), the API version
#' 
#' @examples 
#' sra_browse_API()
#' 
#' @export
sra_browse_API = function(version = "1.0"){
  stopifnot(is.character(version) & length(version)==1)
  browseURL(sprintf('https://api-omicidx.cancerdatasci.org/sra/%s/ui', version))
}


#' URL for the swagger json API description
#' 
#' The SRA API is a swagger 2.0 compliant API. As such, it is
#' described by a json file that can be used to build additional
#' clients in many other languages. See \url{https://swagger.io/}
#' for more details. 
#' 
#' @param version character(1), the API version
#' 
#' @return character(1), the URL of the \code{swagger.json} file.
#' 
#' @examples 
#' sra_get_swagger_json_url()
#' 
#' @export
sra_get_swagger_json_url = function(version = "1.0") {
  stopifnot(is.character(version) & length(version)==1)
  return(
    sprintf('https://api-omicidx.cancerdatasci.org/sra/%s/swagger.json', version))
}

#' mappings
#' 
#' @param entity one of the possible SRA entities (run, sample,
#'     study, experiment, full)
#'
#' @export
sra_get_mappings = function(entity = 'full') {
  
}
