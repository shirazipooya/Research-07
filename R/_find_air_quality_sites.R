
# Find Air Qality Site Code And Other Meta Data

find_air_quality_sites <- function(lat = NA,
                                   lon = NA,
                                   n = 10,
                                   state = NA,
                                   end.year = 2017:2018,
                                   plot = TRUE,
                                   returnMap = FALSE,
                                   parameter = c(81102,88101,42401,42602,44201,42101),
                                   num_para = 6,
                                   map = NULL)
{
        
        # Step 01: Find Site
        
        # create temp directory
        temp_dir <- tempdir()
        
        # url for site file
        aqs_sites_url <- "https://aqs.epa.gov/aqsweb/airdata/aqs_sites.zip"
        
        # create temp file
        temp_file <- paste0(temp_dir, basename(path = aqs_sites_url))
        
        # download file
        bin <- try(download.file(url = aqs_sites_url,
                                 destfile = temp_file,
                                 quiet = TRUE,
                                 mode = "wb"))
        
        # check for exist file
        if (inherits(bin, "try-error"))
        {
                warning(call. = FALSE, paste0("Data Does Not Exist On Server!"))
                return()
        }
        
        # read aqs_sites.csv file
        aqs_sites <- data.table::fread(file = unzip(zipfile = temp_file,
                                                    files = "aqs_sites.csv",
                                                    exdir = temp_dir),
                                       header = TRUE,
                                       fill = TRUE)
        
        # uniform state, conuty and site code
        aqs_sites$`State Code` <- formatC(x = aqs_sites$`State Code`, width = 2, format = "d", flag = "0")
        aqs_sites$`County Code` <- formatC(x = aqs_sites$`County Code`, width = 3, format = "d", flag = "0")
        aqs_sites$`Site Number` <- formatC(x = aqs_sites$`Site Number`, width = 4, format = "d", flag = "0")
        
        # select column
        aqs_sites <- aqs_sites %>%
                mutate(Code = paste(`State Code`, `County Code`, `Site Number`, sep = "-")) %>% 
                select(`State Name`, `County Name`, `City Name`, Code, Latitude, Longitude, Elevation,
                       `Site Established Date`, `Site Closed Date`)
        
        # set date
        aqs_sites$`Site Established Date` <- as.Date(x = aqs_sites$`Site Established Date`, "%Y-%m-%d")
        aqs_sites$`Site Closed Date` <- as.Date(x = aqs_sites$`Site Closed Date`, "%Y-%m-%d")
        aqs_sites[which(is.na(aqs_sites$`Site Closed Date`)), "Site Closed Date"] <- as.Date(x = "2018-06-30", "%Y-%m-%d")
        
        # check year:
        if (!any(end.year %in% c("current", "all")))
        {
                if (!is.numeric(x = end.year))
                {
                        stop("end.year should be one of 'current', 'all' or a numeric 4-digit year such as 2016.")
                }
        }
        
        # current year as the max available in the meta data:
        if ("current" %in% end.year)
        {
                end.year <- max(as.numeric(x = format(x = aqs_sites$`Site Closed Date`, "%Y")),
                                na.rm = TRUE)
        }
        
        if ("all" %in% end.year)
        {
                end.year <- 1900:2100
        }
        
        # Step 02: Find Monitors
        
        # url for monitors file
        aqs_monitors_url <- "https://aqs.epa.gov/aqsweb/airdata/aqs_monitors.zip"
        
        # create temp file
        temp_file <- paste0(temp_dir, basename(path = aqs_monitors_url))
        
        # download file
        bin <- try(download.file(url = aqs_monitors_url,
                                 destfile = temp_file,
                                 quiet = TRUE,
                                 mode = "wb"))
        
        # check for exist file
        if (inherits(bin, "try-error"))
        {
                warning(call. = FALSE, paste0("Data Does Not Exist On Server!"))
                return()
        }
        
        # read aqs_sites.csv file
        aqs_monitors <- data.table::fread(file = unzip(zipfile = temp_file,
                                                       files = "aqs_monitors.csv",
                                                       exdir = temp_dir),
                                          header = TRUE,
                                          fill = TRUE)
        
        # unlink(x = temp_dir, recursive = TRUE)
        
        # uniform state, conuty and site code
        aqs_monitors$`State Code` <- formatC(x = aqs_monitors$`State Code`, width = 2, format = "d", flag = "0")
        aqs_monitors$`County Code` <- formatC(x = aqs_monitors$`County Code`, width = 3, format = "d", flag = "0")
        aqs_monitors$`Site Number` <- formatC(x = aqs_monitors$`Site Number`, width = 4, format = "d", flag = "0")
        
        # select column
        aqs_monitors <- aqs_monitors %>%
                mutate(Code = paste(`State Code`, `County Code`, `Site Number`, sep = "-")) %>% 
                select(Code, `Parameter Code`, `Parameter Name`)
        
        aqs_monitors <- aqs_monitors %>%
                filter(Code %in% aqs_sites$Code)
        
        # Step 03: Join Sites and Monitors
        
        aqs <- left_join(x = aqs_monitors, y = aqs_sites, by = "Code") %>% 
                select(`State Name`, `County Name`, `City Name`, Code, `Parameter Name`, `Parameter Code`,
                       `Site Established Date`, `Site Closed Date`, Latitude, Longitude, Elevation) %>% 
                distinct()
        
        # Search Based On State Codes:
        if (!is.na(state))
        {
                # Search For State:
                id <- which(x = toupper(aqs$`State Name`) %in% toupper(x = state))
                aqs <- aqs[id, ]
        }
        
        # Make Sure No Missing Lon:
        id <- which(x = is.na(x = aqs$Longitude))
        if (length(x = id) > 0)
        {
                aqs <- aqs[-id, ]
        }
        
        # Make Sure No Missing Lat:
        id <- which(x = is.na(x = aqs$Latitude))
        if (length(x = id) > 0)
        {
                aqs <- aqs[-id, ]
        }
        
        # Filter By End Year:
        id <- which(x = format(x = aqs$`Site Closed Date`, "%Y") %in% end.year)
        aqs <- aqs[id, ]
        
        # select parameter
        if (!is.null(parameter))
        {
                aqs <- aqs %>% filter(`Parameter Code` %in% parameter)
        }
        
        num_parameter <- aqs %>% group_by(Code) %>% summarise(n()) %>% filter(`n()` >= num_para)
        
        dat <- left_join(x = num_parameter, y = aqs_sites, by = "Code")
        
        
        # Approximate Distance To Site:
        if (!missing(x = lat) && !missing(x = lon))
        {
                # Radius Of The Earth:
                r <- 6371
                
                # Coordinates Need To Be In Radians:
                dat$longR <- dat$Longitude * pi / 180
                dat$latR <- dat$Latitude * pi / 180
                
                LON <- lon * pi / 180
                LAT <- lat * pi / 180
                
                # Calclate Distance:
                dat$dist <- acos(x = sin(x = LAT) * sin(x = dat$latR) + cos(x = LAT) *
                                         cos(x = dat$latR) * cos(x = dat$longR - LON)) * r
                
                # Sort And Retrun Top N Nearest:
                dat <- head(x = openair:::sortDataFrame(x = dat,
                                                        key = "dist"),
                            n = n)
        }
        
        # Step 03: plot
        
        if (!is.null(map))
        {
                m <- map
        }
        
        if (plot)
        {
                icon <- leaflet::makeIcon(iconUrl = "data/map_marker/Map_Marker_Green.png",
                                          iconWidth = 40, iconHeight = 40,
                                          iconAnchorX = 20, iconAnchorY = 40,
                )
                
                if (!"dist" %in% names(x = dat))
                {
                        dat$dist <- NA
                }
                
                content <- paste(paste("Code:", dat$Code),
                                 paste("Number of Parameter:", dat$`n()`),
                                 paste("State:", dat$`State Name`),
                                 paste("County:", dat$`County Name`),
                                 paste("City:", dat$`City Name`),
                                 paste("Start:", dat$`Site Established Date`),
                                 paste("End:", dat$`Site Closed Date`),
                                 paste("Elevation:", dat$Elevation),
                                 paste("Distance (km)", round(dat$dist, 1)),
                                 sep = "<br/>")
                
                m <- m %>% leaflet::addMarkers(data = dat,
                                               icon = icon,
                                               lng = ~ Longitude,
                                               lat = ~ Latitude,
                                               popup = content,)
                
                if (!is.na(x = lat) && !is.na(x = lon)) 
                {
                        m <- m %>% leaflet::addCircles(lng = lon, 
                                                       lat = lat, 
                                                       weight = 20, 
                                                       radius = 200,
                                                       stroke = TRUE,
                                                       color = "red",
                                                       popup = paste("Search location",
                                                                     paste("Lat =", lat),
                                                                     paste("Lon =", lon),
                                                                     sep = "<br/>"))
                }
                
                print(m)
        }
        
        if (returnMap)
        {
                return(m)
        } else {
                return(dat)
        }
        
}