#' Class Scroller
#' 
#' Scroll through large result sets
#' 
#' This class is not meant to be called by an end 
#' user. Instead, start with a Searcher object and 
#' then call \code{searcher$scroll()}.
#' 
#' 
#' 
#' @export
Scroller = R6Class(
    "Scroller",
    public =  list(
        
        initialize = function(search, progress = interactive()) {
            private$scroll_id <- NULL
            private$search <- search
            private$progress <- progress
            private$.last  <- FALSE
            private$scroll = "1m"
            private$.fetched = 0
        },
        
        has_next = function() {
            !private$.last
        },
        
        yield = function() {
            if(is.null(private$scroll_id)) {
                res = private$search$head()
            }
            else if(!is.null(private$scroll_id)) {
                res = .sra_scroll(scroll_id = private$scroll_id, scroll = private$scroll)
            }
            private$scroll_id <- attr(res, 'scroll_id')
            if(nrow(res)==0) {
                private$.last <- TRUE
                return(NULL)
            }
            private$.fetched = private$.fetched + nrow(res)
            return(res)
        },
        
        collate = function(limit = Inf) {
            private$scroll_id <- NULL
            count = min(self$count, limit)
            size  = private$search$size
            iters = ceiling(count/size)
            if(private$progress) {
                pb = progress::progress_bar$new(
                                                format = " working: ( :spin ) [:bar] :percent eta: :eta",
                                                total = iters, clear = FALSE, width= getOption('width'))
            }
            l = lapply(seq_len(iters), function(n) {
                if(private$progress) pb$tick()
                return(self$yield())
            })
            dplyr::bind_rows(l)
        },

        reset = function() {
            private$scroll_id = NULL
            private$.fetched   = 0
            private$.last     = FALSE
        }
    ),
    private = list(
        scroll    = NULL,
        .count = NULL,
        scroll_id = NULL,
        search = NULL,
        progress = NULL,
        .last  = NULL,
        .fetched = NULL
    ),
    active = list(
        count = function() {
            if(is.null(private$.count))
                private$.count = private$search$count()
            return(private$.count)
        },
        fetched = function() {
            return(private$.fetched)
        }
    )
)

#' @export
Searcher = R6Class(
    "Searcher", list(
                    q = NULL,
                    entity= NULL,
                    size = NULL,
                    start = NULL,
                    return_fields = NULL,
                    initialize = function(q='*', entity='full', size = 100L, start = 0L, return_fields=NULL) {
                        "Initialize a new OidxSearch object."
                        self$entity <- entity
                        self$q      <- q
                        self$size   <- size
                        self$start  <- start
                        self$return_fields <- return_fields
                    },
                    count = function() {
                        "Return a simple count of records that meet the search criteria"
                        path = paste0('/search/',self$entity)
                        return(attr(.sra_get_search_function(path, q = self$q, size = 1, 
                                                             start = 0, fields=self$return_fields),"count"))
                    },
                    head = function() {
                        path = paste0('/search/',self$entity)
                        return(.sra_get_search_function(path, q = self$q, size = self$size, 
                                                        start = 0, fields=self$return_fields))
                    },
                    
                    scroll = function() {
                        return(Scroller$new(self$clone(deep=TRUE)))
                    }
                ))

#' @export
Omicidx = R6Class(
    "Omicidx", list(
                   search = function(q='*', entity = 'full', start=0L, size=100L, return_fields = NULL) {
                       "Build a new search of the Omicidx API"
                       return(Searcher$new(q, entity, start = 0L, 
                                           size = size, return_fields = return_fields))
                   },
                   mapping = function(entity='full') {
                       .mapping_handler(entity)
                   }
               )
)

    
