library(tidyverse)
library(dplyr)
library(magrittr)
library(xts)
library(dygraphs)
library(readr)
library(lubridate)
library(RColorBrewer)

w6_precip_chem <- read_csv("D:/Duke/Work(Environ)/Data/w6_precip_chem.txt")
w6_stream_chem <- read_csv("D:/Duke/Work(Environ)/Data/w6_stream_chem.txt")

#dplyr unite given year and month
w6_precip_chem <- unite(w6_precip_chem, year, year:mo, sep = "/")

#format date to have day
w6_precip_chem$year <- as.character(w6_precip_chem$year)
w6_precip_chem['year'] <- apply(w6_precip_chem[, 'year', drop = F], 2,
                                function(x){paste0(x, '/01')})
#format as Date
as.Date(w6_precip_chem$year)

#re-adjust date to correlate to water year (june 1 = jan 1)
w6_precip_chem$year <- as.Date(w6_precip_chem$year) %m-% months(5)

#split down to just the year in order to sum
as.character(w6_precip_chem$year)
w6_precip_chem <- separate(w6_precip_chem, 
                           col = year, into = c("year", "month", "day"), sep = "-")

#aggregate precip to get mm/yr
pwateryear <- group_by(w6_precip_chem, year) %>%
  summarise_each(funs(sum), precip) %>%
  as.data.frame

#format as date (add as first day of first month in water year) to plug into xts
pwateryear$year <- as.character(pwateryear$year)
pwateryear['year'] <- apply(pwateryear[, 'year', drop = F], 2, function(x){paste0(x, '/01/01')})
pwateryear
as.Date(pwateryear$year)

#change precip num amounts to flux amounts (x = x2-x1)


#adjust the dates for w6_stream_chem in the same way
##OPTIMIZATION NOTE - this would be faster to combine tables then do these adjustments
#dplyr unite given year and month
w6_stream_chem <- unite(w6_stream_chem, year, year:mo, sep = "/")

#format date to have day
w6_stream_chem$year <- as.character(w6_stream_chem$year)
w6_stream_chem['year'] <- apply(w6_stream_chem[, 'year', drop = F], 2,
                                function(x){paste0(x, '/01')})
#format as Date
as.Date(w6_stream_chem$year)

#re-adjust date to correlate to water year (june 1 = jan 1)
w6_stream_chem$year <- as.Date(w6_stream_chem$year) %m-% months(5)

#split down to just the year in order to sum
as.character(w6_stream_chem$year)
w6_stream_chem <- separate(w6_stream_chem, 
                           col = year, into = c("year", "month", "day"), sep = "-")

#aggregate precip to get mm/yr
qwateryear <- group_by(w6_stream_chem, year) %>%
  summarise_each(funs(sum), flow) %>%
  as.data.frame

#format as date (add as first day of first month in water year) to plug into xts
qwateryear$year <- as.character(qwateryear$year)
qwateryear['year'] <- apply(qwateryear[, 'year', drop = F], 2, function(x){paste0(x, '/01/01')})
qwateryear
as.Date(qwateryear$year)

#change discharge num amounts to flux amounts (x = x2-x1)


##AGAIN - optimize into one long table with discharge/precip in same column


#use xts format and create dygraph of precip
waterflux6 <- cbind(xts(pwateryear$precip, 
             #change the dates from water year adjustments back to actual dates (jan->july)
                        order.by = (as.Date(pwateryear$year)%m+% months(5))),
                    xts(qwateryear$flow, 
                        order.by = (as.Date(qwateryear$year) %m+% months(5))),
                    xts((pwateryear$precip - qwateryear$flow), 
                        order.by = (as.Date(pwateryear$year) %m+% months(5))))
  dygraph(waterflux6, main = "W6", ylab = "Annual Water Flux mm/yr")%>%
    dyAxis("x", label = "Water Year", valueRange = c(1950-2020)) %>%
    dyAxis("y", label = "Annual Water Flux (mm/yr)") %>%
    dySeries(name = "..1", label = "P", 
             drawPoints = T, pointSize = 4, color = "blue") %>%
    dySeries(name = "..2", label = "Q",
             drawPoints = T, pointSize = 4, color = "blue") %>%
    dySeries(name = "..3", label = "ET",
             drawPoints = T, pointSize = 4, color = "green") %>%
    dyOptions(gridLineColor = "white", includeZero = T)%>%
    dyLegend(show = "follow")
#add discharge onto same dygraph







