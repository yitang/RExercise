##'  helper function 
##'
##' remove NAs 
##' @param x anything really 
##' @return part of x 
##' @export 
##' @author Yi Tang
remove.na <-
    function(x) {
        x[!(is.na(x))]
    }
