makeCached <- function(name,funcs) {
    cache <- replicate(length(funcs),NULL)
    names(cache) <- names(funcs)
    make_fun <- function(x) {
        force(x)
        
        set <- function(fname,y) {
            x <<- y
            cache[fname] <<- NULL
        }
        get <- function() {
            x
        }
        set_cached <- function(fname,cv) {
            cache[[fname]] <<- cv
        }
        get_cached <- function(fname) {
            cache[[fname]]
        }
        
        list(set = set,
             get = get,
             set_cached = set_cached,
             get_cached = get_cached)
    }
    assign(name,make_fun,envir=parent.env(environment()))
    for (fname in names(funcs)) {
        
        z <- function () {
            funcname <- fname
            f <- function(x) {
                print(cache)
                cv <- cache[[funcname]]
                if (is.null(cv)) {
                    print(paste("Calculating ", funcname))
                    data <- x$get()
                    cv <- funcs[[funcname]](data)
                    x$set_cached(funcname,cv)
                } else {
                    print(paste("Getting cached value ", funcname))
                }
                cv
            }
            assign(fname,f,pos=parent.env(parent.env(environment())))
        }
        z()
        
        
        #assign(fname,f,pos=parent.env(environment()))
        #assign("funcname",paste(fname))
        #print(environment(f))
        #print(environment())
    }
}

makeCached("makeCachedMatrix2",list(cacheSolve2=function(x) {solve(x)},
                                    cacheSum2=function(x) {mean(x)}))
