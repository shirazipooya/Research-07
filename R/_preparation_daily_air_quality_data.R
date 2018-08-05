
# Step 00: Requirements ---------------------------------------------------------------------------------

# Remove All Objects From Environment:
rm(list = ls())

# Load Libraries:
library(dplyr)      # a grammar of data manipulation
library(openair)    # tools for the analysis of air pollution data
library(leaflet)    # create interactive web maps with the JavaScript "Leaflet" library
library(readr)      # read rectangular text data
library(data.table) # extension of "data.frame"

# Load Functions:

# Step 01: Daily Air Quality Data -------------------------------------------------------------

data_folder <- choose.dir(caption = "Choose Daily Air Quality Data")

result <- list()

for (i in list.dirs(path = data_folder)[-1])
{
     data_file <- list.files(path = i, pattern = ".csv")
     
     data <- c()
     
     for (j in data_file)
     {
          data_link <- paste(i, j, sep = "/")
          
          data <- rbind(data, read.csv(file = data_link, header = TRUE)[,c(1,2,4,5,9,10,17,18)])
          
     }
     
     colnames(data) <- c("date", "site", "concentration", "unit", "para_code", "para_name", "lat", "lon")
     
     result[[substr(x = i, start = (nchar(list.dirs(path = data_folder)[1]) + 2), stop = nchar(i))]] <- data
     
}

result <- do.call(what = rbind,
                  args = result)

rownames(result) <- NULL

result$date <- as.Date(x = as.character(result$date), "%m/%d/%Y")

base_date <- data.frame(date = seq(from = as.Date(x = "2010-01-01", "%Y-%m-%d"),
                                   to   = as.Date(x = "2017-12-31", "%Y-%m-%d"),
                                   by   = "day"))

result$site <- paste("0", as.character(result$site), sep = "")

# select paramete
for (i in unique(result$para_code))
{
     data <- result %>% filter(para_code == i)
     
     dat <- list()
     
     # select site
     for (j in unique(data$site))
     {
          data_site <- data %>% filter(site == j)
          
          lat <- unique(data_site$lat)
          
          lon <- unique(data_site$lon)
          
          unit_i <- unique(data_site$unit)

          para_name <- unique(data_site$para_name)

          data_site <- left_join(x = base_date,
                           y = data_site,
                           by = "date")
          
          data_site$site <- j
          data_site$unit <- unit_i
          data_site$para_code <- i
          data_site$para_name <- para_name
          data_site$lat <- lat
          data_site$lon <- lon
          
          dat[[j]] <- data_site
     }
     
     dat <- do.call(what = rbind,
                       args = dat)
     
     library(reshape)
     mdata <- melt(dat, id=c("id","time"))

     library(tidyr)
     dd <- separate(data = dat, "")
}







for (i in unique(result$site))
{
     data <- result %>% filter(site == i)
     
     data$date <- as.Date(x = as.character(data$date), "%m/%d/%Y")
     
     para <- unique(data$para_code)
     
     for (j in para)
     {
          dat <- data %>% filter(para_code == j)
          
          lat <- unique(dat$lat)
          
          lon <- unique(dat$lon)
          
          concentration <- unique(dat$concentration)
          
          para_name <- unique(dat$para_name)
          
          para_name <- unique(dat$para_name)
          
          unit_i <- unique(dat$unit)
          
          dat <- left_join(x = Base_Date,
                            y = dat,
                            by = "date")
          
          dat$site <- i
          dat$concentration <- concentration
          dat$unit <- unit_i
          dat$para_code <- j
          dat$para_name <- para_name
          dat$lat <- lat
          dat$lon <- lon
     }
}


     
site <- result %>% select(site, para_code) %>% distinct() %>%
     group_by(site) %>% summarise(n()) %>% 
     filter(`n()` >= 5) %>% select(site)