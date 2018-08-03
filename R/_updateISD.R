
# Import Surface Meteorological Data from NOAA Integrated Surface Database (ISD)

updateISD <- function()
{
     url <- "https://www1.ncdc.noaa.gov/pub/data/noaa/isd-history.csv"
     
     meta <- suppressMessages(expr = readr::read_csv(file = url,
                                                     col_names = TRUE))
     
     names(meta) <- c("USAF", "WBAN", "STATION", "CTRY", "ST", 
                      "CALL", "LAT", "LON", "ELEV", "BEGIN", "END")
     
     meta$USAF <- formatC(x = meta$USAF, 
                          width = 6,
                          format = "d",
                          flag = "0")
     
     meta$WBAN <- formatC(x = meta$WBAN,
                          width = 5,
                          format = "d",
                          flag = "0")
     
     meta$BEGIN <- as.Date(x = as.character(x = meta$BEGIN),
                           format = "%Y%m%d")
     
     meta$END <- as.Date(x = as.character(x = meta$END),
                         format = "%Y%m%d")
     
     meta$CODE <- paste(meta$USAF,
                        meta$WBAN,
                        sep = "-")
     
     return(meta)
}