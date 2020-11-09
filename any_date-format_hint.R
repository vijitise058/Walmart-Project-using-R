install.packages('lubridate')
library(lubridate) 
data <- data.frame(initialDiagnose = c("14.01.2009", "9/22/2005", 
                                       "4/21/2010", "28.01.2010", "09.01.2009", "3/28/2005", 
                                       "04.01.2005", "04.01.2005", "Created on 9/17/2010", "03 01 2010", "2020 01 09"))
parse_date_time(data$initialDiagnose, orders = c('mdy', 'dmy','ymd'))