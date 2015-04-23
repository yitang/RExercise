##' Parse GPX file 
##'
##' A warpper function for Parse_GPX. 
##' @param data.dir a folder that contains GPX files 
##' @param app RunKeeper, Strave or others?
##' @param add.city add city and country 
##' @return a list 
##' @export 
##' @author Yi Tang
Parse_GPX_all <- function(data.dir = NULL, app = "Strava", add.city = TRUE)  {
    if (!is.null(data.dir))
        setwd(data.dir)
    data.files <- list.files(pattern = "gpx$")
    all.data <- lapply(data.files, Parse_GPX)
    location <- lapply(all.data, "[[", 1)
    summary <- rbindlist(lapply(all.data, "[[", 2))
    data.files <- gsub("\\.gpx", "", data.files)
    summary[, id := data.files]
    names(location) <- summary$id

    ## special treat for data from RunKeeper and Strava
    if (app == "Strava") {
        tmp <- lapply(strsplit(data.files, "-"), function(tmp) {
                          c(id = paste0(tmp[1:2], collapse = "-"),
                            type =  tmp[3]) })
        summary[, id := sapply(tmp, "[", 1)]
        summary[, activity := sapply(tmp, "[", 2)]
        names(location) <- summary$id
    } else {
        summary[, activity := sapply(strsplit(summary$name, " "), "[", 1)]
    }

    if (add.city == TRUE) {
        cities <- lapply(location, function(x) {
                             lonlat <- x[1, c(lon, lat)]
                             loc.info <- revgeocode(lonlat, output = "more")
                             c(as.character(loc.info$locality), as.character(loc.info$country))
                         })
        cities <- do.call(rbind, cities)
        summary[, city := cities[, 1]]
        summary[, country := cities[, 2]]
    }
    list(location, summary)
}
