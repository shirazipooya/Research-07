
# Step 00: Requirements ---------------------------------------------------------------------------------

# Remove All Objects From Environment:
rm(list = ls())

# Load Libraries:
library(dplyr)      # a grammar of data manipulation
# library(GSODR)      # global surface summary of the day (GSOD) weather data from R
# library(openair)    # tools for the analysis of air pollution data
# library(leaflet)    # create interactive web maps with the JavaScript "Leaflet" library
# library(readr)      # read rectangular text data
# library(data.table) # extension of "data.frame"

# Load Functions:
source(file = "./R/_updateISD.R")
source(file = "./R/_find_meteorological_sites.R")
source(file = "./R/_daily_met_data_cleansing.R")
source(file = "./R/_find_air_quality_sites.R")

lat = +034.0522
lon = -118.2437

# Step 01: Meteorology Data -----------------------------------------------------------------------------

# 01-01 find ISD site code: United States - Arizona - Phoenix
phoenix_met_site <- find_meteorological_sites(lat = lat,
                                              lon = lon,
                                              n = 20,
                                              end.year = "current",
                                              plot = FALSE,
                                              returnMap = FALSE)

phoenix_met_map <- find_meteorological_sites(lat = lat,
                                             lon = lon,
                                             n = 20,
                                             end.year = "current",
                                             plot = TRUE,
                                             returnMap = TRUE)

# 01-02 Import Meteorology Data:
phoenix_met_data <- GSODR::get_GSOD(years = 2010:2017,
                                    station = phoenix_met_site$CODE)

# 01-03 Meteorology Data Cleansing:
phoenix_met_data <- phoenix_met_data %>%
        select(YEARMODA, STNID, LAT, LON, ELEV_M, TEMP, DEWP, SLP,
               STP, VISIB, WDSP, MXSPD, MAX, MIN, PRCP, EA, ES, RH)

colnames(phoenix_met_data) <- c("date", "site", "lat", "lon", "elev", "tmean", "dp", "slp", "stp",
                                "visib", "ws", "ws_max", "tmax", "tmin", "prec", "ea", "es", "rh")

phoenix_sites <- unique(phoenix_met_data$site)

phoenix_data <- list()

for (i in phoenix_sites)
{
        data <- phoenix_met_data %>%
                filter(site == i)
        
        data <- daily_met_data_cleansing(data = data,
                                         start.date = "2010-01-01",
                                         end.date = "2017-12-31",
                                         format.date = "%Y-%m-%d")
        data$site <- i
        
        data$lat <- max(data$lat, na.rm = TRUE)
        
        data$lon <- max(data$lon, na.rm = TRUE)
        
        data$elev <- max(data$elev, na.rm = TRUE)
        
        phoenix_data[[i]] <- data
}

phoenix_data <- do.call(what = rbind, 
                        args = phoenix_data)

rownames(phoenix_data) <- NULL

# 01-04 Save phoenix_data:
phoenix_data$site <- as.factor(phoenix_data$site)

for (i in phoenix_sites)
{
        print(i)
        print(phoenix_data %>%
                      filter(site == i) %>%
                      summarise_all(funs(sum(is.na(.)))))
}

saveRDS(object = phoenix_data,
        file = "./data/met/phoenix_data.RDS")

# Step 02: Air Quality Data -----------------------------------------------------------------------------

# 01-02 find air quality site code: United States - Arizona - Phoenix
# c(81102,88101,42401,42602,44201,42101)
phoenix_air_quality_site <- find_air_quality_sites(lat = lat,
                                                   lon = lon,
                                                   n = 20,
                                                   state = NA,
                                                   end.year = 2017:2018,
                                                   parameter = c(81102,42401,42602,44201,42101),
                                                   num_para = 5,
                                                   plot = TRUE,
                                                   returnMap = FALSE,
                                                   map = phoenix_met_map)

