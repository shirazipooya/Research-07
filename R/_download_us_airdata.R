
# Download US Air Quality Data

Download_AirQualityData <- function(period = "daily",
                                    year = 2015,
                                    site = c("06-037-5005"),
                                    criteria_gases = TRUE,
                                    particulates = TRUE,
                                    toxics_precursors_lead = FALSE,
                                    meteorological = TRUE)
{
        # pollutant
        pollutant <- c()
        
        # Criteria Gases
        if (criteria_gases)
        {
                CG <- c("Ozone" = "44201",
                        "SO2"   = "42401",
                        "CO"    = "42101",
                        "NO2"   = "42602")
                
                pollutant <- c(pollutant, CG)
        }
        
        # Particulates
        if (particulates)
        {
                P <- c("PM10_Mass"  = "81102",
                       "PM2.5_Mass" = "88101")
                
                pollutant <- c(pollutant, P)
        }
        
        # Toxics, Precursors, and Lead
        if (toxics_precursors_lead)
        {
                TPL <- c("HAPS"     = "HAPS",
                         "VOCS"     = "VOCS",
                         "NONOxNOy" = "NONOxNOy",
                         "LEAD"     = "LEAD")
                
                pollutant <- c(pollutant, TPL)
        }
        
        # Meteorological
        if (meteorological)
        {
                M <- c("Winds"       = "WIND",
                       "Temperature" = "TEMP",
                       "Pressure"    = "PRESS",
                       "RH_Dewpoint" = "RH_DP")
                
                pollutant <- c(pollutant, M)
        }
        
        pollutant_process <- expand.grid(period = period,
                                         year = year,
                                         pollutant = pollutant,
                                         stringsAsFactors = FALSE)
        
        data <- rowwise(data = pollutant_process) %>%
                do(getAirData(period = .$period,
                              year = .$year,
                              pollutant = .$pollutant))
        
        
}

getAirData <- function(period,
                       year,
                       pollutant)
{
        url <- "https://aqs.epa.gov/aqsweb/airdata/"
        
        file_name <- paste(period, "_", pollutant[1], "_", year, ".zip", sep = "")
        
        download_link <- paste(url, file_name, sep = "")
        
        temp <- tempdir()
        
        tmp <- paste0(temp, basename(path = download_link))
        
        bin <- try(download.file(url = download_link,
                                 destfile = tmp,
                                 quiet = TRUE,
                                 mode = "wb"))
        
        if (inherits(bin, "try-error"))
        {
                warning(call. = FALSE,
                        paste0("Data for ", year, " does not exist on server"))
                return()
        }
        
        data_temp <- data.table::fread(file = unzip(zipfile = tmp,
                                                    files = paste(period, "_", pollutant[1], "_", year, ".csv", sep = ""),
                                                    exdir = temp),
                                       header = TRUE,
                                       fill = TRUE)
        
        unlink(x = temp, recursive = TRUE)
        
        return(data_temp)
}

# Step 01: ------------------------------------------------------------------------------------

us_data <- Download_AirQualityData(period = "daily",
                                   year = 2016,
                                   site = c("06-037-5005"),
                                   criteria_gases = TRUE,
                                   particulates = TRUE,
                                   toxics_precursors_lead = FALSE,
                                   meteorological = TRUE)

us_data_ca <- us_data %>%
        filter(`State Name` == "California")

A <- distinct(us_data_ca[, c(26,27,3,4,6,7,9)]) %>% 
        filter(`Parameter Name` %in% c("Ozone", "Sulfur dioxide", "Carbon monoxide",
                                       "Nitrogen dioxide (NO2)", "PM10 Total 0-10um STP",
                                       "PM10 - LC", "PM2.5 - Local Conditions"))

B <- A[, c(1,2,3,5,6,7)] %>% 
        group_by(`County Name`, `City Name`,`Site Num`, `Latitude`, `Longitude`) %>% 
        summarise(n()) %>% 
        filter(`n()` >= 4)

write.csv(x = B, file = "B.csv")

us_data_az <- us_data %>%
        filter(`State Name` == "Arizona")
