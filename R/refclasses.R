setClassUnion('CharacterOrNull', c("character", "NULL"))

OidxSearch = setRefClass("OidxSearch",
            fields = list(entity='character',
                          q = 'CharacterOrNull',
                          size = "integer",
                          start = "integer",
                          return_fields = "CharacterOrNull"),
            methods = list(
              initialize = function(entity='full', q = NULL, size = 100L, start = 0L, return_fields=NULL) {
                "Initialize a new OidxSearch object."
                entity <<- entity
                q      <<- q
                size   <<- size
                start  <<- start
                return_fields <<- return_fields
              },
              count = function() {
                "Return a simple count of records that meet the search criteria"
                path = paste0('/search/',.self$entity)
                return(attr(.sra_get_search_function(path, q = .self$q, size = 1, start = 0, fields=return_fields),"count"))
              }
            ))


Omicidx = setRefClass("Omicidx", methods = list(
  search = function(entity = 'full', q='*', start=0L, size=100L, return_fields = NULL) {
    "Build a new search of the Omicidx API"
    return(OidxSearch$new(entity, q, start, size, return_fields))
  }
))
