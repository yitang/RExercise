##'  Travel distance 
##'
##' give a set of location L, it calculate the travelling distance, i.e. sum of all the distance from L(i) to L(i-1).
##' @param x_lon lon
##' @param x_lat lat 
##' @param y_lon lon 
##' @param y_lat lat
##' @return double
##' @export 
##' @author Yi Tang
GCD_pairs <-
    function(x_lon, x_lat, y_lon, y_lat) {
        n <- length(x_lon)
        dist <- sapply(seq_len(n), function(i) {
                           spDistsN1(cbind(x_lon[i], x_lat[i]),
                                     cbind(y_lon[i], y_lat[i]), longlat = TRUE)
                       })
        return(dist)
    }
