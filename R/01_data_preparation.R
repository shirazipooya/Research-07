

# Step 00: Requirements ---------------------------------------------------------------------------------

# Remove All Objects From Environment:
rm(list = ls())

# Load Libraries:
library(dplyr)      # a grammar of data manipulation
library(GSODR)      # global surface summary of the day (GSOD) weather data from R

# Load Functions:
source(file = "R/_updateISD.R")
source(file = "R/_find_meteorological_sites.R")
source(file = "R/_daily_met_data_cleansing.R")

# Step 01: Meteorology Data -----------------------------------------------------------------------------

# 01-01 find ISD site code: United States - Arizona - Phoenix
phoenix_met_site <- find_meteorological_sites(lat = +033.4484,
                                              lon = -112.0740,
                                              n = 9,
                                              end.year = "current",
                                              plot = TRUE,
                                              returnMap = FALSE)

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
        file = "data/met/phoenix_data.RDS")

# Step 02: Air Quality Data -----------------------------------------------------------------------------



