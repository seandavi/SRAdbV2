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


get_url = function(path) httr::build_url(list('https://xdolg4kkf3.execute-api.us-east-1.amazonaws.com/dev', path))

.sra_get_search_function = function(path, q='*', from = 0, size = 10, fields = NULL, .headers=NULL) {
  url = httr::parse_url(file.path("https://xdolg4kkf3.execute-api.us-east-1.amazonaws.com/dev", path))
  url$query = list(q=q, from=from, size=size)
  if(!is.null(fields)) {
    url$query$fields = fields
  }
  url = httr::build_url(url)
  message(url)
  result <- httr::GET(url, httr::content_type("application/json"),
                      httr::accept_json(), httr::add_headers(.headers = .headers))
  return(.search_handler(result))
}

sra_experiment_search = function(q = '*', from = 0, size = 10, fields = NULL) {
  .sra_get_search_function(path = '/sra/experiment/search', q, from, size, fields)
}

sra_run_search = function(q = '*', from = 0, size = 10, fields = NULL) {
  .sra_get_search_function(path = '/sra/run/search', q, from, size, fields)
}

sra_study_search = function(q = '*', from = 0, size = 10, fields = NULL) {
  .sra_get_search_function(path = '/sra/study/search', q, from, size, fields)
}

sra_sample_search = function(q = '*', from = 0, size = 10, fields = NULL) {
  .sra_get_search_function(path = '/sra/sample/search', q, from, size, fields)
}

sra_full_search = function(q = '*', from = 0, size = 10, fields = NULL) {
  .sra_get_search_function(path = '/sra/full/search', q, from, size, fields)
}


#' Initialize an SRAdb API client
#'
#' @importFrom rapiclient get_api get_operations
#'   
#' @return 
#' an client
#' 
#' @examples 
#' client = sradb_get_client()
#' # self-describing
#' names(client)
#' # self-documenting
#' client[[1]]
#' 
#' @export
sradb_get_client = function() {
  library(rapiclient)
  sra_api = get_api("https://xdolg4kkf3.execute-api.us-east-1.amazonaws.com/dev/swagger.json")
  paths = names(sra_api$paths)
  df_paths = Filter(function(x) grepl('search',x), paths)
  client = lapply(df_paths, function(path) get_operations(sra_api, path = path,
                                                          handle_response = .search_handler)[[1]])
  return(client)
}