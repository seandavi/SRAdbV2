setClassUnion('CharacterOrNull', c("character", "NULL"))

OidxSearch = setRefClass("OidxSearch",
            fields = list(entity='character',
                          q = 'CharacterOrNull',
                          size = "integer",
                          start = "integer",
                          return_fields = "CharacterOrNull"),
            methods = list(
              initialize = function(entity='full', q = NULL, size = 100L, start = 0L, return_fields=NULL) {
                entity <<- entity
                q      <<- q
                size   <<- size
                start  <<- start
                return_fields <<- return_fields
              },
              count = function() {
                path = paste0('/search/',.self$entity)
                return(attr(.sra_get_search_function(path, q = .self$q, size = 1, start = 0, fields=return_fields),"count"))
              }
            ))


Omicidx = setRefClass("Omicidx", methods = list(
  search = function(q='*', start=0, size=10, fields = NULL) {
    sra_full_search(q, start, size, fields)
  }
))
