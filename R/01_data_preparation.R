
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

# United States - California - Los Angeles
lat = +034.0522
lon = -118.2437

# my met stations:
stations <- c("722950-23174", "722956-03167", "722874-93134", "722955-03174", "722970-23129", "722976-03166")

# Step 01: Meteorology Data -----------------------------------------------------------------------------

# 01-01 find ISD site code:
find_met_site <- find_meteorological_sites(lat = lat,
                                           lon = lon,
                                           n = 2,
                                           end.year = "current",
                                           plot = FALSE,
                                           returnMap = FALSE)

find_met_map <- find_meteorological_sites(lat = lat,
                                          lon = lon,
                                          n = 2,
                                          end.year = "current",
                                          plot = TRUE,
                                          returnMap = TRUE)

# 01-02 Import Meteorology Data:
download_met_data <- GSODR::get_GSOD(years = 2010:2017,
                                     # station = find_met_site$CODE,
                                     station = stations)

# 01-03 Meteorology Data Cleansing:
download_met_data <- download_met_data %>%
        select(YEARMODA, STNID, LAT, LON, ELEV_M, TEMP, DEWP, SLP,
               STP, VISIB, WDSP, MXSPD, MAX, MIN, PRCP, EA, ES, RH)

colnames(download_met_data) <- c("date", "site", "lat", "lon", "elev", "tmean", "dp", "slp", "stp",
                                 "visib", "ws", "ws_max", "tmax", "tmin", "prec", "ea", "es", "rh")

met_sites <- unique(download_met_data$site)

met_data <- list()

for (i in met_sites)
{
        data <- download_met_data %>%
                filter(site == i)
        
        data <- daily_met_data_cleansing(data = data,
                                         start.date = "2010-01-01",
                                         end.date = "2017-12-31",
                                         format.date = "%Y-%m-%d")
        data$site <- i
        
        data$lat <- max(data$lat, na.rm = TRUE)
        
        data$lon <- max(data$lon, na.rm = TRUE)
        
        data$elev <- max(data$elev, na.rm = TRUE)
        
        met_data[[i]] <- data
}

met_data <- do.call(what = rbind, 
                    args = met_data)

rownames(met_data) <- NULL

# 01-04 Save phoenix_data:
met_data$site <- as.factor(met_data$site)

for (i in met_sites)
{
        print(i)
        print(met_data %>%
                      filter(site == i) %>%
                      summarise_all(funs(sum(is.na(.)))))
}

saveRDS(object = met_data,
        file = "./data/met/met_data.RDS")

# Step 02: Air Quality Data -----------------------------------------------------------------------------

# 01-02 find air quality site code: United States - Arizona - Phoenix
# c(81102,88101,42401,42602,44201,42101)
find_airquality_site <- find_air_quality_sites(lat = lat,
                                               lon = lon,
                                               n = 5,
                                               state = NA,
                                               end.year = 2017:2018,
                                               parameter = c(81102,42401,42602,44201,42101),
                                               num_para = 5,
                                               plot = TRUE,
                                               returnMap = FALSE,
                                               map = find_met_map)

