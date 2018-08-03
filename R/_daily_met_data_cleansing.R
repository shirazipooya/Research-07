
# Daily Meteorology Data Cleansing:
daily_met_data_cleansing <- function(data,
                                     start.date  = "2010-01-01",
                                     end.date    = "2017-12-31",
                                     format.date = "%Y-%m-%d")
{
     base_date <- data.frame(date = seq(from = as.Date(x = start.date, format.date),
                                        to   = as.Date(x = end.date, format.date),
                                        by   = "day"))
     
     data <- left_join(x  = base_date,
                       y  = data,
                       by = "date")
     
     return(data)
}