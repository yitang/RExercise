##'  Parse GPX file
##'
##' main functions to parse GPX file, see package description for more details
##' @param file.path file path to .GPX file
##' @return list 
##' @export 
##' @author Yi Tang
Parse_GPX <-
    function(file.path) {
        gpx.list <- readGPX(file.path)
        track.info <- do.call(rbind, gpx.list$tracks[[1]]) ## stack loops, some paused activities 
        setDT(track.info)
        ## track.poly <- track.info[, Polygons(list(Polygon(cbind(lon, lat))), ID = file.path)]
        track.info[, time := gsub(pattern = "T", " ", time)]
        track.info[, time := gsub(pattern = "Z", "", time)]
        track.info[, ele := as.numeric(ele)]
        

        ## activity information 
        time2 <- track.info[, strptime(time, format = "%Y-%m-%d %H:%M:%S")]
        start.time <- format(time2[1], "%H:%M:%s")
        date <- format(time2[1], "%Y-%m-%d")
        duration <- sum(as.numeric(time2[length(time2)] - time2[1], unit = "hours"))
        n <- nrow(track.info)
        distance <- sum(track.info[, GCD_pairs(lon[-1], lat[-1], lon[-n], lat[-n])])
        speed <- distance / duration 
        climb <- sum(abs(diff(track.info$ele)))
        ele <- mean(track.info$ele)

        activity.info <- data.table(date,
                                    start.time,
                                    name = remove.na(names(gpx.list$tracks[[1]])),
                                    "duration (h)" = duration,
                                    "distance (km)" = distance,
                                    "speed (km/h)" = speed,
                                    "elevation (m)" = ele,
                                    "climb (m)" = climb)
        res <- list(track.info,
                    activity.info)
        res
    }
