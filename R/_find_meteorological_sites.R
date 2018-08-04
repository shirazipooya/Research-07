
# Find ISD Site Code And Other Meta Data

find_meteorological_sites <- function(lat = NA,
                                      lon = NA,
                                      n = 10,
                                      country = NA,
                                      state = NA,
                                      site = "heathrow",
                                      end.year = "current",
                                      plot = TRUE,
                                      returnMap = FALSE)
{
     
     # Import Surface Meteorological Data from NOAA Integrated Surface Database (ISD):
     meta <- updateISD()
     
     # Check Year:
     if (!any(end.year %in% c("current", "all")))
     {
          if (!is.numeric(x = end.year))
          {
               stop("end.year should be one of 'current', 'all' or a numeric 4-digit year such as 2016.")
          }
     }
     
     # Current Year As The Max Available In The Meta Data:
     if ("current" %in% end.year)
     {
          end.year <- max(as.numeric(x = format(x = meta$END, "%Y")),
                          na.rm = TRUE)
     }
     
     if ("all" %in% end.year)
     {
          end.year <- 1900:2100
     }
     
     # Search Based On Name Of Site:
     if (!missing(x = site))
     {
          meta <- meta[grep(pattern = site,
                            x = meta$STATION,
                            ignore.case = TRUE), ]
     }
     
     # Search Based On Country Codes:
     if (!missing(x = country) && !is.na(x = country))
     {
          # Search For Country:
          id <- which(x = meta$CTRY %in% toupper(x = country))
          meta <- meta[id, ]
     }
     
     # Search Based On State Codes:
     if (!missing(state))
     {
          # Search For State:
          id <- which(x = meta$ST %in% toupper(x = state))
          meta <- meta[id, ]
     }
     
     # Make Sure No Missing Lon:
     id <- which(x = is.na(x = meta$LON))
     if (length(x = id) > 0)
     {
          meta <- meta[-id, ]
     }
     
     # Make Sure No Missing Lat:
     id <- which(x = is.na(x = meta$LAT))
     if (length(x = id) > 0)
     {
          meta <- meta[-id, ]
     }
     
     # Filter By End Year:
     id <- which(x = format(x = meta$END, "%Y") %in% end.year)
     meta <- meta[id, ]
     
     # Approximate Distance To Site:
     if (!missing(x = lat) && !missing(x = lon))
     {
          # Radius Of The Earth:
          r <- 6371
          
          # Coordinates Need To Be In Radians:
          meta$longR <- meta$LON * pi / 180
          meta$latR <- meta$LAT * pi / 180
          
          LON <- lon * pi / 180
          LAT <- lat * pi / 180
          
          # Calclate Distance:
          meta$dist <- acos(x = sin(x = LAT) * sin(x = meta$latR) + cos(x = LAT) *
                                 cos(x = meta$latR) * cos(x = meta$longR - LON)) * r
          
          # Sort And Retrun Top N Nearest:
          meta <- head(x = openair:::sortDataFrame(x = meta,
                                                   key = "dist"),
                       n = n)
     }
     
     dat <- dplyr::rename(meta, latitude = LAT, longitude = LON)
     
     if (plot)
     {
          if (!"dist" %in% names(x = dat))
          {
               dat$dist <- NA
          }
          
          content <- paste(paste(dat$STATION, 
                                 paste("Code:", dat$CODE),
                                 paste("Start:", dat$BEGIN),
                                 paste("End:", dat$END),
                                 paste("Distance (km)", round(dat$dist, 1)),
                                 sep = "<br/>"))
          
          m <- leaflet::leaflet(data = dat) %>%
               leaflet::addProviderTiles(provider = leaflet::providers$Stamen.Terrain) %>% 
               leaflet::addMarkers(lng = ~ longitude,
                                   lat = ~ latitude,
                                   popup = content)
          
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